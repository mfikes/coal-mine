(ns coal-mine.problem-95
  (:require [coal-mine.checks :refer [defcheck-95] :rename {defcheck-95 defcheck}]
            [clojure.test]
            [clojure.walk]))

(defcheck solution-100ea11a
  (fn f [t]
    (if (coll? t)
      (and (= 3 (count t))
           (f (nth t 1))
           (f (nth t 2)))
      (= t nil))))

(defcheck solution-1031e6df
  (fn tree?- [coll]
    ^{:doc "Write a predicate which checks whether or not a given
  sequence represents a binary tree."}
    (if (nil? coll)
      true
      (and (coll? coll) (= 3 (count coll)) (tree?- (second coll)) (tree?- (nth coll 2))))))

(defcheck solution-10334cc9
  (fn is-bintree [thing]
    (cond
      (nil? thing)
      true

      (not (sequential? thing))
      false

      (= (count thing) 3)
      (and (is-bintree (nth thing 1))
           (is-bintree (nth thing 2)))

      :else
      false)))

(defcheck solution-103da06b
  (fn btree? [xs]
    (if (coll? xs)
      (and (= 3 (count xs)) (every? btree? xs))
      (not (= xs false)))))

(defcheck solution-10af8f
  (fn tr? [tree]
    (and
     (coll? tree)
     (= 3 (count tree))
     (or (= nil (nth tree 1)) (tr? (nth tree 1)))
     (or (= nil (nth tree 2)) (tr? (nth tree 2))))))

(defcheck solution-118d35f7
  (fn binary-tree? [t]
    (if (coll? t)
      (if (= 3 (count t))
        (and (binary-tree? (nth t 1)) (binary-tree? (nth t 2)))
        false)
      (if (nil? t)
        true
        false))))

(defcheck solution-11bf3c4e
  (fn ! [coll]
    (or
     (nil? coll)
     (and (coll? coll) (= (count coll) 3) (every? ! (rest coll))))))

(defcheck solution-11fab87
  (fn bt? [s]
    (if-not (= 3 (count s))
      false
      (letfn [(f [i xs]
                (let [x (nth xs i)]
                  (cond (nil? x) true, (sequential? x) (bt? x), :else false)))]
        (and (f 1 s) (f 2 s))))))

(defcheck solution-1281c8ed
  (fn istree
    [node]
    (if (= node false)
      false
      (if (= nil node)                                      ;if it's nil, return true
        true
        (and (= 3 (count node))
             (istree (nth node 1))
             (istree (nth node 2)))))))

(defcheck solution-12c5fc1e
  (fn is-tree [el]
    (if (coll? el)
      (let [size (count el)]
        (if (not= 3 size)
          false
          (let [right (second el) left (nth el 2)]
            (and
             (or (= nil right) (is-tree right))
             (or (= nil left) (is-tree left))
             )
            )
          )
        )
      false
      )
    ))

(defcheck solution-13245238
  (fn binary-tree?
    [[x y z & xs :as coll]]
    (cond
      (and (coll? coll) (empty? coll)) false
      x (cond
          (or (some false? coll) (not= (count coll) 3)) false
          :else (and (binary-tree? y) (binary-tree? z)))
      :else true)))

(defcheck solution-133b0015
  (fn f [v]
    (or (nil? v)
        (and
         (coll? v)
         (= 3 (count v))
         (f (nth v 1))
         (f (nth v 2))))))

(defcheck solution-13587850
  (fn t? [t]
    (cond (nil? t) true
          (not (sequential? t)) false
          (not (= 3 (count t))) false
          :else (and (t? (second t)) (t? (nth t 2))))))

(defcheck solution-1367f2ed
  (fn tree? [tree]
    (cond (not (sequential? tree)) (if (nil? tree) true false)
          (= (count tree) 3) (and (tree? (second tree)) (tree? (nth tree 2)))
          :else false)))

(defcheck solution-13975452
  #((fn [c] (and (not= 1 c) (odd? c))) (count (filter nil? (flatten %)))))

(defcheck solution-13ba3c07
  (fn is-bin [tree]
    (if (and (not (vector? tree)) (not (list? tree))) (= nil tree)
                                                      (if (not= 3 (count tree)) false
                                                                                (let [[vl left right] tree]
                                                                                  (and (is-bin left) (is-bin right)))))))

(defcheck solution-140a7c7d
  (fn is-bin-tree?
    [t]
    (if (= 3 (count t))
      (let [sec (second t) thir (last t)]
        (if (or (nil? sec) (and (coll? sec) (is-bin-tree? sec)))
          (if (or (nil? thir) (and (coll? thir) (is-bin-tree? thir)))
            true false)
          false))
      false)))

(defcheck solution-14115565
  (fn T [t] (or (= t nil) (and (coll? t) (= 3 (count t)) (T (nth t 1)) (T (nth t 2))))))

(defcheck solution-14f95c30
  (fn tree? [coll]
    (let [tree-or-nil? #(or (nil? %) (tree? %))]
      (and
       (sequential? coll)
       (= 3 (count coll))
       (tree-or-nil? (nth coll 1))
       (tree-or-nil? (nth coll 2))))))

(defcheck solution-15068cd5
  (fn fr [ys]
    (letfn [(fsub [rt xs]
              (if rt (if (not (= 3 (count xs))) false
                                                (let [f1 (first xs) f2 (second xs) f3 (second (rest xs))]
                                                  (if (= nil f1) false (and (fsub false f2) (fsub false f3)))
                                                  ))
                     (if (sequential? xs) (if (not (= 3 (count xs))) false
                                                                     (let [f1 (first xs) f2 (second xs) f3 (second (rest xs))]
                                                                       (if (= f1 nil) false
                                                                                      (cond (and (= f2 nil) (= f3 nil)) true
                                                                                            (= f2 nil) (fsub false f3)
                                                                                            (= f3 nil) (fsub false f2)
                                                                                            :else (and (fsub false f2) (fsub false f3))
                                                                                            )))
                                                                     )
                                          (if (= nil xs) true false)
                                          )))]
      (fsub true ys)
      )))

(defcheck solution-155d2898
  (fn node? [c]
    (if (nil? c)
      true
      (if (and (sequential? c) (= (count c) 3))
        (and (node? (first (rest c))) (node? (second (rest c))))
        false))))

(defcheck solution-15630219
  (fn bin-tree? [s]
    (or
     (nil? s)
     (and
      (coll? s)
      (= (count s) 3)
      (bin-tree? (second s))
      (bin-tree? (nth s 2))
      ))))

(defcheck solution-1564173e
  (fn tree? [x] (or (nil? x) (and (coll? x) (= 3 (count x)) (and (tree? (nth x 1)) (tree? (nth x 2)))))))

(defcheck solution-156600e8
  (fn [c]
    (every? #(or (nil? %) (and (coll? %) (= (count %) 3))) (tree-seq coll? next c))))

(defcheck solution-158bef55
  (fn tree? [x]
    (or (nil? x)
        (and (sequential? x)
             (= 3 (count x))
             (tree? (second x))
             (tree? (last x))))))

(defcheck solution-159da908
  (fn binary-tree?
    [coll]
    (if (not= (count coll) 3)
      false
      (let [f (first coll)
            s (second coll)
            t (second (next coll))]
        (and
         (not (nil? f))
         (or (nil? s) (and (coll? s) (binary-tree? s)))
         (or (nil? t) (and (coll? t) (binary-tree? t))))))))

(defcheck solution-15bdbd33
  (fn tree? [coll]
    (or (nil? coll)
        (and (coll? coll)
             (= 3 (count coll))
             (let [[_ l r] coll]
               (and (tree? l) (tree? r)))))))

(defcheck solution-16293540
  (fn istree? [branch]
    (if (and
         (sequential? branch)
         (= 3 (count branch)))
      (let [b1 (first (rest branch))
            b2 (second (rest branch))]
        (and (if (nil? b1)
               true
               (istree? b1))
             (if (nil? b2)
               true
               (istree? b2))))
      false)))

(defcheck solution-163ea744
  (fn [tree] (every? true? (cons
                             (every? nil? ((group-by coll? (tree-seq coll? rest tree)) false))
                             (flatten (map #(list (= 3 (count %))
                                              (not (nil? (first %)))
                                              (every? coll? (filter (fn [coll] (not (nil? coll))) (rest %)))
                                              ) ((group-by coll? (tree-seq coll? rest tree)) true)))
                             ))))

(defcheck solution-1674e961
  (fn bt? [t]
    (cond
      (nil? t) true
      (and (sequential? t) (= (count t) 3) (every? bt? (rest t))) true
      :else false)))

(defcheck solution-168f0644
  (fn is-tree [x] (or (nil? x) (and
                                (sequential? x)
                                (= 3 (count x))
                                (is-tree (nth x 1))
                                (is-tree (nth x 2))))))

(defcheck solution-171d4949
  (fn [t] (every? (some-fn (every-pred sequential? #(= 3 (count %))) nil?) (tree-seq sequential? next t))))

(defcheck solution-179227cc
  (fn bin? [x]
    (or (nil? x)
        (and (coll? x)
             (= 3 (count x))
             (every? bin? (rest x))))))

(defcheck solution-17ad8e6b
  (fn check
    ([] false)
    ([f] (if (coll? f) (apply check f) false))
    ([f s] false)
    ([f s t]
     (cond
       (coll? f) false
       (or (false? f) (false? s) (false? t)) false
       (and (coll? s) (coll? t)) (and (apply check s) (apply check t))
       (coll? s) (apply check s)
       (coll? t) (apply check t)
       :else true))
    ([f s t & y]
     (if (zero? (mod (count y) 3))
       (and (check f s t) (apply check y))
       false))))

(defcheck solution-1817bd46
  (fn tree? [t] (if (nil? t)
                  true
                  (if (sequential? t)
                    (let [[n l r] t]
                      (and (= 3 (count t))
                           (tree? l)
                           (tree? r))
                      )

                    false
                    )
                  )))

(defcheck solution-187cf67f
  (fn f [l]
    (and (coll? l)
         (let [[_ b c] l]
           (and (= (count l) 3) (or (nil? b) (f b)) (or (nil? c) (f c)))
           ))
    ))

(defcheck solution-187f334a
  (fn f [t]
    (or (nil? t)
        (and (coll? t)
             (= 3 (count t))
             (f (nth t 1))
             (f (nth t 2))))))

(defcheck solution-1895162a
  (fn f [t] (if (coll? t)
              (if (= (count t) 3)
                (every? f t)
                false)
              (or (nil? t) t))))

(defcheck solution-18a514bd
  (fn t [x]
    (cond
      (= 1 (count x)) (nil? (first x))
      (= 3 (count x)) (and
                       (not (nil? (first x)))
                       (or
                        (nil? (second x))
                        (and (sequential? (second x)) (t (second x))))
                       (or
                        (nil? (last x))
                        (and (sequential? (last x)) (t (last x))))
                       )

      :else false
      )))

(defcheck solution-18ad46a6
  (fn checktree [tree]
    (let [isNode  (fn [node]
                    (and (not (empty? node))
                         (not (coll? (first node)))
                         )
                    ),
          isLeaf  (fn [node]
                    (cond (nil? node) true
                          (coll? node) (checktree node)
                          :else false
                          )
                    ),
          isLeft  (fn [tree]
                    (if (empty? (rest tree))
                      false
                      (isLeaf (first (rest tree)))
                      )
                    ),
          isRight (fn [tree]
                    (if (empty? (rest tree))
                      false
                      (let [other (rest (rest tree))]
                        (cond (empty? other) false
                              (not (empty? (rest other))) false
                              :else (isLeaf (first other))
                              )
                        )
                      )
                    )]
      (cond (not (coll? tree)) false
            (empty? tree) false
            :else (and (isNode tree)
                       (isLeft tree)
                       (isRight tree))
            )
      )
    ))

(defcheck solution-18d8ebe9
  (fn is-tree [t]
    (and (coll? t) (= (count t) 3)
         (or (nil? (second t)) (is-tree (second t)))
         (or (nil? (nth t 2)) (is-tree (nth t 2))))))

(defcheck solution-1923b4f8
  (fn is-tree?
    ([l]
     (if (not (coll? l))
       (nil? l)
       (case (count l)
         3 (every? is-tree? (rest l))
         false)))))

(defcheck solution-1955dae3
  (fn tree? [x]
    (if (and (coll? x) (= (count x) 3))
      (let [left  (second x)
            right (nth x 2)]
        (and
         (if (nil? left) true (tree? left))
         (if (nil? right) true (tree? right))))
      false)))

(defcheck solution-197b1070
  (fn tree?
    [tree]
    (or (nil? tree)
        (if (coll? tree)
          (and (= 3 (count tree))
               (tree? (second tree))
               (tree? (last tree)))
          false))))

(defcheck solution-19cb94aa
  (fn tree? [x]
    (if (coll? x)
      (if (= 3 (count x))
        (and (tree? (second x)) (tree? (last x)))
        false)
      (nil? x))))

(defcheck solution-19f2ecc
  (fn tree [s]
    (if (= nil s)
      true
      (and (coll? s) (= 3 (count s)) (tree (second s)) (tree (last s))))))

(defcheck solution-19f3c89
  (fn ex95 [tree]
    (if (not (coll? tree))
      (not (false? tree))
      (and (= 3 (count tree))
           (ex95 (nth tree 0))
           (ex95 (nth tree 1))
           (ex95 (nth tree 2))))))

(defcheck solution-1a59c4e4
  (fn f [tree]
    (letfn [(valid-branch? [b]
              (or (nil? b)
                  (and
                   (sequential? b)
                   (f b))))]
      (and (= 3 (count tree))
           (let [[_ l r] tree]
             (and
              (valid-branch? l)
              (valid-branch? r)))))))

(defcheck solution-1aa521a5
  (fn [t]
    (loop [v t, l (second t), r (last t), not-checked []]
      (cond
        (not= 3 (count v)) false
        (and (coll? l) (coll? r)) (recur l (second l) (last l) (conj not-checked r))
        (coll? l) (recur l (second l) (last l) not-checked)
        (coll? r) (recur r (second r) (last r) not-checked)
        (not-empty not-checked) (let [r (first not-checked)]
                                  (recur r (second r) (last r) (rest not-checked)))
        :else (= nil l r)))))

(defcheck solution-1ab00076
  (fn is-btree? [x]
    (if (nil? x)
      true
      (if (and (sequential? x)
               (= (count x) 3))
        (and (is-btree? (-> x
                          rest
                          first))
             (is-btree? (-> x
                          rest
                          rest
                          first)))
        false))))

(defcheck solution-1ac9e68f
  (fn bin-tree? [root]
    (or (nil? root)
        (and (coll? root)
             (= 3 (count root))
             (bin-tree? (second root))
             (bin-tree? (last root))))))

(defcheck solution-1ada1888
  (fn ctree [xs]
    (if (not= (count (seq xs)) 3)
      false
      (let [[n ls rs & rst] xs]
        (and (not (seq? n))
             (or (nil? ls) (and (and (sequential? ls) (ctree ls))))
             (or (nil? rs) (and (and (sequential? rs) (ctree rs)))))))))

(defcheck solution-1aef3aa6
  (fn x [[_ l r :as n]]
    (let [f #(if (coll? %) (x %) (nil? %))]
      (and (= (count n) 3) (f l) (f r)))))

(defcheck solution-1b3b28fa
  (fn tree? [s]
    (or (nil? s)
        (and (sequential? s)
             (= 3 (count s))
             (tree? (second s))
             (tree? (last s))))))

(defcheck solution-1b836ccb
  (fn f [xs]
    (cond
      (nil? xs) true
      (and (sequential? xs) (= (count xs) 3)) (and (f (second xs)) (f (last xs)))
      :else false)))

(defcheck solution-1b960b7
  (fn it [[k l r :as t]]
    (and (= 3 (count t))
         (if (coll? l) (it l) (nil? l))
         (if (coll? r) (it r) (nil? r)))))

(defcheck solution-1c00f8e1
  (fn bin-tree? [tree] (or (nil? tree) (and (sequential? tree) (= 3 (count tree)) (bin-tree? (nth tree 1)) (bin-tree? (nth tree 2))))))

(defcheck solution-1c588393
  (fn f [n]
    (if (nil? n)
      true
      (and (coll? n)
           (= (count n) 3)
           (f (second n))
           (f (last n))))))

(defcheck solution-1d1ec7b8
  (fn c [n]
    (cond (not (coll? n)) (nil? n)
          (not= 3 (count n)) false
          :else (and (c (second n)) (c (second (rest n)))))))

(defcheck solution-1d5b06cf
  (fn tree [t]
    (and (coll? t)
         (= 3 (count t))
         (or (nil? (second t)) (tree (second t)))
         (or (nil? (last t)) (tree (last t))))))

(defcheck solution-1d76b52e
  (fn bin [tree]
    (if (not (sequential? tree))
      (nil? tree)
      (if (not (= (count tree) 3))
        false
        (and (bin (second tree)) (bin (nth tree 2)))))))

(defcheck solution-1dc2127b
  (fn binary-tree? [s]
    (if-not (= 3 (count s))
      false
      (let [[a b c] (vec s)]
        (cond
          (sequential? a) (binary-tree? a)
          (sequential? b) (binary-tree? b)
          (sequential? c) (binary-tree? c)
          (false? a) false
          (false? b) false
          (false? c) false
          :else true)))))

(defcheck solution-1dd00d12
  (fn f [s]
    (cond (-> s coll? not) true
          (not= (count s) 3) false
          (some coll? s) (every? true? (map f s))
          :else (every? #(or (coll? %) (nil? %)) (drop 1 s)))))

(defcheck solution-1dde10f6
  (fn binary? [tree]
    (if (and (sequential? tree) (= 3 (count tree)))
      (let [[x y z] tree]
        (and
         (not (sequential? x))
         (or (nil? y) (binary? y))
         (or (nil? z) (binary? z))
         )
        )
      false
      )
    ))

(defcheck solution-1e035b82
  (let [count_colls_and_nils (fn [l] (count (filter #(or (nil? %) (coll? %)) l)))]
    (fn node? [l] (if (not= 3 (count l)) false
                                         (if (< (count_colls_and_nils l) 2) false
                                                                            (every? node? (filter coll? l)))))))

(defcheck solution-1e39d59d
  (fn btree? [t] (
                   and (= (count t) 3) (not (false? (nth t 1))) (not (false? (nth t 2)))
                       (or (not (coll? (nth t 1))) (btree? (nth t 1)))
                       (or (not (coll? (nth t 2))) (btree? (nth t 2)))
                       )))

(defcheck solution-1e46c982
  (fn is-a-tree [t]
    (let [true-when-nil (fn [node]
                          (if (nil? node)
                            true
                            (if (sequential? node)
                              (is-a-tree node)
                              false)))]
      (if-not (= 3 (count t))
        false
        (and (true-when-nil (second t))
             (true-when-nil (last t)))))))

(defcheck solution-1ea95822
  (fn tree? [node]
    (or (= nil node)
        (and (or (coll? node) (seq? node))
             (= 3 (count node))
             (first node)
             (tree? (second node))
             (tree? (last node))))))

(defcheck solution-1f53a4cd
  (fn t [args]
    (or (nil? args)
        (and (sequential? args)
             (= 3 (count args))
             (every? #(t %) (rest args))))))

(defcheck solution-1f829e3c
  (fn f [n] (if (coll? n)
              (and (= 3 (count n))
                   (f (second n))
                   (f (last n))) (= nil n))))

(defcheck solution-1f9539f6
  (fn istree [coll]
    (if (coll? coll)
      (if (and
           (= (count coll) 3)
           (istree (second coll))
           (istree (last coll)))
        true
        false
        )
      (if (nil? coll)
        true
        false)
      )
    ))

(defcheck solution-1fc7bdaa
  (fn is-tree? [x]
    (letfn [(check-branch [y]
              (cond (coll? y) (is-tree? y)
                    (nil? y) true
                    :else false)
              )]
      (cond (not (= 3 (count x))) false
            :else (and
                   (check-branch (second x))
                   (check-branch (nth x 2))
                   )
            )
      )
    ))

(defcheck solution-1ff1945d
  (fn t? [r]
    (or (nil? r)
        (and (coll? r)
             (= 3 (count r))
             (every? t? (rest r))))))

(defcheck solution-20170bca
  (fn bintree? [rt]
    (cond
      (nil? rt) true
      (coll? rt) (and
                  (= 3 (count rt))
                  (not (coll? (first rt)))
                  (bintree? (second rt))
                  (bintree? (last rt)))
      :else false)))

(defcheck solution-20232b9d
  (fn bintree? [t]
    (or (nil? t)
        (and (sequential? t)
             (= (count t) 3)
             (bintree? (nth t 1))
             (bintree? (nth t 2))))))

(defcheck solution-202984bc
  (fn ! [t]
    (and
     (sequential? t)
     (= 3 (count t))
     (let [[val left right] t]
       (and
        (not (sequential? val))
        (not (nil? val))
        (or (nil? left) (! left))
        (or (nil? right) (! right)))))))

(defcheck solution-204421ad
  (fn tr [x]
    (if (coll? x)
      (and (= 3 (count x)) (not (empty? x)) (not (coll? (first x))) (tr (second x)) (tr (last x)))
      (nil? x))))

(defcheck solution-20968003
  (fn node? [thing]
    (cond
      (nil? thing)
      true
      (and (sequential? thing) (= 3 (count thing)))
      (and (node? (nth thing 1)) (node? (nth thing 2)))
      :else
      false
      )))

(defcheck solution-210236b6
  (fn t [[v & br]]
    (and (= 2 (count (filter #(or (nil? %) (and % (t %))) br))))))

(defcheck solution-2123fb71
  (fn f [tree]
    (and (= 3 (count tree)) (every? #(if (coll? %) (f %) (not (false? %))) tree))))

(defcheck solution-2182a2ce
  (fn btree? [xs]
    (if (coll? xs)                                          ; if xs = collection
      (if (= (count xs) 3)                                  ;   if xs = collection of 3 (value, child, child)
        (apply = (map btree? xs))                           ;     then apply btree? to all in collection
        false)                                              ;     else result is false
      (if (not (false? xs))                                 ;   if xs = not a collection and not false
        true                                                ;     then result is true
        false)))                                            ;     else result is false
  )

(defcheck solution-22b9f658
  (fn ! [l]
    (if (= l nil) true
                  (if (= l false) false
                                  (if (not (= (count l) 3)) false
                                                            (and (! (second l)) (! (last l)))))))

  ; the false part not totally get
  )

(defcheck solution-22c73df1
  (fn f [tree]
    (if (nil? tree)
      true
      (and (coll? tree)
           (= 3 (count tree))
           (f (nth tree 1))
           (f (nth tree 2))))))

(defcheck solution-2328083d
  (fn tree? [x] (or (nil? x) (and (sequential? x) (= 3 (count x)) (every? tree? (rest x))))))

(defcheck solution-23398159
  (fn tree? [x]
    (cond
      (nil? x) true
      (not (coll? x)) false
      (not= 3 (count x)) false
      :else (and (tree? (nth x 1)) (tree? (nth x 2))))))

(defcheck solution-2462ce98
  (fn f [c]
    (if (coll? c)
      (and (= 3 (count c))
           (reduce #(and %1 %2) (map f c)))
      (not (boolean? c))
      )))

(defcheck solution-259e4367
  (fn b
    [t]
    (if (nil? t)
      true
      (and
       (coll? t)
       (= (count t) 3)
       (b (nth t 1))
       (b (nth t 2)))
      )))

(defcheck solution-266430b4
  (fn binary-tree? [bt]
    (let [tree  (tree-seq sequential? seq bt)
          heads (map first (filter sequential? tree))
          leafs (filter #(and (not (sequential? %)) (not= nil %)) tree)]
      (and (= (set heads) (set leafs)) (every? #(= 3 (count %)) (filter sequential? tree))))))

(defcheck solution-268ed7dc
  (fn f [n]
    (every? #(if (coll? %)
               (= (count %) 3)
               (not (contains? #{false '()} %)))
      (tree-seq coll? rest n))))

(defcheck solution-269708ba
  (fn cn [n]
    (every? true? (flatten (cons (= (count (filter #(not (false? %)) n)) 3)
                             (map #(if (coll? %)
                                     (cn %)
                                     true) n))))))

(defcheck solution-26971f12
  (fn t [[v r l & x :as a]]
    (let [nl (nil? l)
          nr (nil? r)
          sl (coll? l)
          sr (coll? r)
          wc (not= (count a) 3)]
      (if (or wc (and (not sr) (not nr)) (and (not sl) (not nl)))
        false
        (if (and nr nl (nil? x))
          true
          (and (or nl (t l))
               (or nr (t r))))))))

(defcheck solution-26cc9265
  (fn binary-tree? [a-seq]
    (if (or
         (not= 3 (count a-seq))
         (coll? (first a-seq))
         (some false? a-seq)
         (some true? a-seq))
      false
      (let [node-2       (second a-seq)
            node-3       (second (rest a-seq))
            process-node (fn [x] (if (coll? x) (binary-tree? x) true))]
        (and (process-node node-2) (process-node node-3))))))

(defcheck solution-26d3b60
  (fn tree? [x]
    (or (= x nil)
        (and (coll? x)
             (= (count x) 3)
             (every? tree? (rest x))))))

(defcheck solution-27948724
  (fn t? [x]
    (or (nil? x)
        (and (coll? x)
             (= 3 (count x))
             (every? t? (rest x))))))

(defcheck solution-285c13d9
  (fn tree?
    [xs]
    (if (nil? xs)
      true
      (and (sequential? xs) (= 3 (count xs)) (tree? (nth xs 1)) (tree? (nth xs 2))))))

(defcheck solution-286d1bc2
  (fn f [v] (and (= (count v) 3)
                 (every? #(if (sequential? %)
                            (f %)
                            (nil? %))
                   (rest v)))))

(defcheck solution-288f8dba
  (fn
    [coll]
    (letfn [(isTree [coll] (or (nil? coll) (and (coll? coll)
                                                (= 3 (count coll))
                                                (first coll)
                                                (isTree (second coll))
                                                (isTree (nth coll 2)))))]
      (isTree coll))))

(defcheck solution-28c852f0
  (fn ? [x]
    (if (coll? x)
      (let [[_ y z] x]
        (boolean (and (= (count x) 3)
                      (? y)
                      (? z))))
      (nil? x))))

(defcheck solution-28e8236e
  (fn is-tree? [t]
    (if (nil? t)
      true
      (if (and (coll? t) (= 3 (count t)) (is-tree? (nth t 1)) (is-tree? (nth t 2)))
        true
        false))))

(defcheck solution-2944dfc4
  (fn b [y]
    (if (or (list? y) (vector? y))
      (if (= 3 (count y))
        (if (false? (nth y 1))
          false
          (every? b y)
          )
        false
        )
      (not (false? y))
      )
    ))

(defcheck solution-294bc89
  (fn [root]
    (let [bintree (fn bintree [node]
                    (if (and (coll? node) (= 3 (count node)))
                      (lazy-seq
                        (cons true
                          (mapcat bintree (rest node))))
                      (list (nil? node))))]
      (every? identity (bintree root)))))

(defcheck solution-294dc853
  (fn tree? [t]
    (if (= nil t)
      true
      (if (or (not t) (not (= 3 (count t))))
        false
        (and (tree? (second t)) (tree? (last t)))))))

(defcheck solution-29a8b7b1
  (fn check-bin [node]
    (if (sequential? node)
      (let [[v left right] node]
        (cond (not= 3 (count node))
              false
              (and v (nil? left) (nil? right))
              true
              (and (or (nil? left) (sequential? left))
                   (or (nil? right) (sequential? right)))
              (and (if left (check-bin left) true) (if right (check-bin right) true))
              :else
              false
              ))
      false)))

(defcheck solution-2a2f092c
  (fn node? [x] (and (coll? x) (= 3 (count x)) (not (false? (second x))) (or (not (coll? (second x))) (node? (second x))) (or (not (coll? (last x))) (node? (last x))))))

(defcheck solution-2a4644ff
  (fn [t] (every? #(or (nil? %) (and (sequential? %) (= (count %) 3)))
            (tree-seq sequential? rest t))))

(defcheck solution-2acb8f43
  (fn tree [coll]
    (let [inner-coll (first (filter coll? coll))
          is-binary  (odd? (count (filter #(and (not= % true) (not= % false)) (flatten coll))))]
      (if (and is-binary (= (count inner-coll) 3))
        (tree inner-coll)
        is-binary))))

(defcheck solution-2add75f0
  (fn [t]
    (reduce #(and %1 %2)
      (map #(or (and (coll? %) (= 3 (count %))) (nil? %))
        (tree-seq identity rest t)))))

(defcheck solution-2afeb629
  (fn isTree? [x] (or (nil? x) (and (coll? x) (= 3 (count x)) (not (coll? (first x))) (isTree? (second x)) (isTree? (last x))))))

(defcheck solution-2b177c2f
  (fn is-tree [t]
    (if-not (sequential? t) (nil? t)
                            (if-not (= 3 (count t)) false
                                                    (let [[v l r] t]
                                                      (and (not (sequential? v))
                                                           (is-tree l)
                                                           (is-tree r)))))))

(defcheck solution-2b8ce7ca
  (fn tree? [tr]
    (cond
      (nil? tr) true
      (and (sequential? tr) (= 3 (count tr))) (and (tree? (second tr)) (tree? (second (rest tr))))
      :else false)))

(defcheck solution-2c371d6e
  (fn f [x] (if (coll? x) (and (= (count x) 3) (f (nth x 1)) (f (nth x 2))) (nil? x))))

(defcheck solution-2c425ab9
  (fn [coll]
    (letfn [(pred [s]
              (and
               (coll? s)
               (= 3 (count s))
               (let [[v l r] s]
                 (and v
                      (or (nil? l) (pred l))
                      (or (nil? r) (pred r))))))]
      (pred coll))))

(defcheck solution-2c472605
  (fn [coll]
    (clojure.walk/postwalk
      (fn [form]
        (if (or (and (coll? form)
                     (= 3 (count form))
                     (every? true? form))
                (and (not (coll? form))
                     (not (false? form))))
          true
          false))
      coll)))

(defcheck solution-2c6cfdb7
  (fn fg [ls]
    (cond
      (nil? ls) true
      (or (vector? ls) (list? ls)) (if (= 3 (count ls))
                                     (and (fg (nth ls 1)) (fg (nth ls 2)))
                                     false)
      (false? ls) false)))

(defcheck solution-2c6d9f73
  (fn [s] (letfn [(binary-tree? [s]
                    (and
                     (= (count s) 3)
                     (let [a (second s) b (last s)]
                       (and
                        (or
                         (nil? a)
                         (and
                          (sequential? a)
                          (binary-tree? a)))
                        (or
                         (nil? b)
                         (and
                          (sequential? b)
                          (binary-tree? b)))))))]
            (binary-tree? s))))

(defcheck solution-2c746666
  #(let [f (fn f [s]
             (if (sequential? s)
               (if (= 3 (count (remove false? s)))
                 (and (nil? (f (nth s 1))) (f (nth s 2)))
                 false)))]
     (nil? (f %))))

(defcheck solution-2ca6ea18
  (fn tree [xs]
    (if (nil? xs) true
                  (if (or
                       (not (coll? xs))
                       (not= (count xs) 3)) false
                                            (let [[v l r] xs]
                                              (and (tree l) (tree r)))))))

(defcheck solution-2cd9ac26
  (fn binary? [ys] (if (nil? ys) true (if (or (not (sequential? ys)) (empty? (drop 2 ys))) false
                                                                                           (let [[h x y & xs] ys] (and (binary? x) (binary? y) (empty? xs)))))))

(defcheck solution-2d21408
  (fn t [n]
    (or (nil? n)
        (and (coll? n) (= 3 (count n)) (t (second n)) (t (nth n 2))))))

(defcheck solution-2d7d5895
  (fn valid-tree? [tree]
    (let [left  (second tree)
          right (last tree)]
      (and (= (count tree) 3)
           (not (seq? (first tree)))
           (and (not (boolean? left))
                (not (boolean? right)))
           (or (nil? left) (valid-tree? left))
           (or (nil? right) (valid-tree? right))))))

(defcheck solution-2dbd2ec6
  (fn check-tree [node]
    (cond
      (nil? node) true
      (not (coll? node)) false
      (not= 3 (count node)) false
      :else (and (check-tree (second node)) (check-tree (last node)))
      )
    ))

(defcheck solution-2dd57dc1
  (fn tree? [xs]
    (if (and (coll? xs) (= (count xs) 3))
      (and (tree? (second xs)) (tree? (last xs)))
      (nil? xs))))

(defcheck solution-2dda705d
  (fn tree? [s]
    (or (nil? s)
        (and (sequential? s)
             (= (count s) 3)
             (tree? (second s))
             (tree? (nth s 2))))))

(defcheck solution-2e37ca1c
  (fn is-bin-tree? [root]
    (every? #(or (nil? %) (if (coll? %) (= (count %) 3) false)) (tree-seq identity rest root))))

(defcheck solution-2e38c010
  (fn is-tree? [node]
    (or (nil? node)
        (and (sequential? node)
             (= (count node) 3)
             (let [[_ left right] node]
               (and (is-tree? left) (is-tree? right)))))))

(defcheck solution-2e50ed21
  (fn bin? [root]
    (if (nil? root)
      true
      (if (and (coll? root) (= 3 (count root)))
        (every? bin? (rest root))
        false))))

(defcheck solution-2e629c19
  (letfn [(binary-tree? [tree] (if (nil? tree) true
                                               (and (coll? tree) (-> tree count (= 3))
                                                    (let [subtrees (next tree)] (and (-> subtrees first binary-tree?)
                                                                                     (-> subtrees second binary-tree?)))
                                                    )))] binary-tree?))

(defcheck solution-2e6a9833
  (fn btree? [coll]
    (or (nil? coll)
        (and coll
             (= 3 (count coll))
             (every? btree? (rest coll))))))

(defcheck solution-2eb1833e
  (fn t [xs]
    (let [[v l r] xs]
      (and (= 3 (count xs))
           (if (coll? l) (t l) (not= false l))
           (if (coll? r) (t r) (not= false r))))))

(defcheck solution-3019b260
  (fn [root]
    (every? (fn [node] (if (sequential? node)
                         (= 3 (count node))
                         (= node nil))) (tree-seq sequential? rest root))))

(defcheck solution-3099fedd
  (fn is-tree [t]
    (cond
      (nil? t) true
      (not (sequential? t)) false
      (and (= 3 (count t)) (every? is-tree (rest t))) true
      true false)))

(defcheck solution-309acee8
  (fn istree [x]
    (if (nil? x) true
                 (if (not (coll? x)) false
                                     (if (not (= (count x) 3)) false
                                                               (and (istree (second x)) (istree (nth x 2))))))
    ))

(defcheck solution-30ee7ac6
  (fn bin-tree? [[val & children]]
    (and (= (count children) 2)
         (every? #(if (sequential? %) (bin-tree? %) (nil? %)) children))))

(defcheck solution-30f2995f
  (fn f [tree] (if (nil? tree)
                 true
                 (if (or (not (coll? tree))
                         (not= (count tree) 3)
                         )
                   false
                   (if
                    (nil? (first tree))
                     false
                     (and (f (nth tree 2)) (f (nth tree 1))))))))

(defcheck solution-30f9cfbc
  (fn t? [s]
    (cond
      (not= 3 (count s)) false
      (some false? s) false
      (coll? (second s)) (if (t? (second s))
                           (if (coll? (last s)) (t? (last s)) true)
                           false)
      (coll? (last s)) (t? (last s))
      :else true)))

(defcheck solution-312446d0
  (fn c
    ([] false)
    ([x] (if (coll? x) (apply c x) (nil? x)))
    ([x a] false)
    ([x a b] (and (c a) (c b)))
    ([x a b & r] false)))

(defcheck solution-314a93c2
  (fn bintree? [t]
    (cond
      (nil? t) true
      (or (true? t) (false? t)) false
      :else (and
             (= 3 (count t))
             (bintree? (nth t 1))
             (bintree? (nth t 2)))
      )
    ))

(defcheck solution-315e0b8
  (fn f [x] (cond (nil? x) true
                  (not (sequential? x)) false
                  (not= (count x) 3) false
                  :else (and (f (nth x 1)) (f (nth x 2))))))

(defcheck solution-31bea0ed
  (fn tree? [t]
    (if (nil? t) true
                 (if (sequential? t)
                   (if (= 3 (count t))
                     (let [left (nth t 1) right (nth t 2)]
                       (and (tree? left) (tree? right)))
                     false)
                   false))))

(defcheck solution-31ce7495
  (fn is-tree [[_ l r :as t]]
    (and (= 3 (count t))
         (if (nil? l) true (and (coll? l) (is-tree l)))
         (if (nil? r) true (and (coll? r) (is-tree r))))))

(defcheck solution-32237cdd
  (fn tree? [s]
    (cond (nil? s) true
          (not (coll? s)) false
          (not= (count s) 3) false
          :else (and (tree? (second s))
                     (tree? (nth s 2))))))

(defcheck solution-3280c909
  (fn btree? [xs]
    (if (= 3 (count xs))
      (not (some false? (map #(if (and (coll? %))
                                (btree? %)
                                (if (= false %) false true)) xs)))
      false)))

(defcheck solution-32968f5c
  (fn istree [t]
    (if (sequential? t)
      (and (= (count t) 3)
           (not (sequential? (first t)))
           (istree (second t))
           (istree (nth t 2)))
      (nil? t))
    ))

(defcheck solution-32c0429b
  (fn tree? [s]
    (let
     [seq-has-3? #(and (sequential? %) (= 3 (count %)))
      valueable? #(or (keyword? %) (number? %))]
      (and (seq-has-3? s)
           (let [[value left-child right-child] s]
             (and (valueable? value)
                  (or (nil? left-child) (tree? left-child))
                  (or (nil? right-child) (tree? right-child))))))))

(defcheck solution-32f1bab1
  (fn f [t]
    (if (and (sequential? t) (= 3 (count t)))
      (let [[_ l r] t]
        (and (f l) (f r)))
      (nil? t))))

(defcheck solution-3345c79e
  (fn walk [[h & r]]
    (if (not= 2 (count r)) false
                           (every? true? (for [i r]
                                           (if (coll? i) (walk i) (nil? i))
                                           )))))

(defcheck solution-339a50e1
  (fn f [n]
    (cond
      (nil? n) true
      (not (coll? n)) false
      (= 3 (count n)) (and (f (nth n 1)) (f (nth n 2)))
      f false)
    ))

(defcheck solution-33d3bfc0
  (fn f [a]
    (cond
      (= false a) false
      (or (nil? a) (not (coll? a))) true
      (= 3 (count a)) (and (f (nth a 0)) (f (nth a 1)) (f (nth a 2)))
      :else false)))

(defcheck solution-34504198
  (fn f [c] (or (nil? c) (and (coll? c) (= 3 (count c))
                              (f (second c)) (f (last c))))))

(defcheck solution-349fc868
  (fn binary [t]
    (or (nil? t)
        (and (sequential? t)
             (= (count t) 3)
             (binary (nth t 1))
             (binary (nth t 2))))))

(defcheck solution-3513d3ba
  (fn [tree]
    (->> (tree-seq coll? next tree)
      (every? #(or (nil? %)
                   (and (counted? %)
                        (-> % count (= 3))))))))

(defcheck solution-3568779b
  (fn tree? [t]
    (if (or (= nil t) (and (sequential? t) (= 3 (count t)) (every? tree? (rest t))))
      true
      false)))

(defcheck solution-356ea2bb
  (fn binary-tree? [args]
    (if (nil? args)
      true
      (if (and (coll? args) (= 3 (count args)))
        (let [val   (first args)
              left  (second args)
              right (nth args 2)]
          (if (and
               (or (nil? left) (binary-tree? left))
               (or (nil? right) (binary-tree? right)))
            true
            false))
        false))))

(defcheck solution-35ec9bdf
  (fn bt? [coll]
    (or (nil? coll)
        (and (sequential? coll) (= 3 (count coll)) (bt? (second coll)) (bt? (last coll))))))

(defcheck solution-35ede634
  (fn is-tree [t]
    (cond
      (nil? t) true
      (coll? t) (and (= 3 (count t)) (is-tree (nth t 1)) (is-tree (nth t 2)))
      :else false)))

(defcheck solution-360698d1
  (fn bt [t]
    (if (nil? t)
      true
      (and (coll? t) (= (count t) 3) (bt (second t)) (bt (nth t 2))))))

(defcheck solution-3615c4b
  (fn isBt [bt]
    (if (coll? bt)
      (and
       (= 3 (count bt))
       (isBt (nth bt 0))
       (or (nil? (nth bt 1)) (coll? (nth bt 1))) (isBt (nth bt 1))
       (or (nil? (nth bt 2)) (coll? (nth bt 2))) (isBt (nth bt 2)))
      true)))

(defcheck solution-361604f5
  (fn btree? [t]
    (cond
      (nil? t) true
      (not (coll? t)) false
      (not= 3 (count t)) false
      :else (and (btree? (second t)) (btree? (last t))))))

(defcheck solution-36a46dd6
  (fn binary-tree? [tree]
    (cond
      (nil? tree) true
      (not (sequential? tree)) false
      :else (and (== 3 (count tree))
                 (every? binary-tree? (rest tree))))))

(defcheck solution-36d00ad1
  (fn [s]
    (every?
      #(or (nil? %) (and (coll? %) (= 3 (count %))))
      (tree-seq coll? rest s))))

(defcheck solution-36fd3799
  (fn binary-tree?
    [t]
    (cond
      (nil? t) true
      (not (sequential? t)) false
      :else (let [[_ left right] t]
              (and (= (count t) 3)
                   (binary-tree? left)
                   (binary-tree? right))))))

(defcheck solution-371288d0
  (fn tree? [s] (or (nil? s) (and (sequential? s) (= (count s) 3) (every? tree? (rest s))))))

(defcheck solution-37363860
  (fn tree? [args]
    (and (coll? args)
         (= (count args) 3)
         (or (nil? (second args)) (tree? (second args)))
         (or (nil? (nth args 2)) (tree? (nth args 2))))))

(defcheck solution-377908ed
  (fn is-tree? [root]
    (if (sequential? root)
      (and (= 3 (count root)) (is-tree? (first (rest root))) (is-tree? (first (rest (rest root)))))
      (= root nil))))

(defcheck solution-378fb0a2
  (fn binary-tree?
    [x]
    (or (= x nil)
        (and (coll? x)
             (= (count x) 3)
             (every? binary-tree? (rest x))))))

(defcheck solution-378fcd4c
  (fn do-check [t]
    (or (= t nil)
        (and (sequential? t)
             (= (count t) 3)
             (do-check (second t))
             (do-check (nth t 2))))))

(defcheck solution-37c2ee61
  (fn btree? [node]
    (if (nil? node)
      true
      (if (and (sequential? node)
               (= 3 (count node))
               (btree? (second node))
               (btree? (nth node 2)))
        true
        false))))

(defcheck solution-37cee483
  (fn tr?
    [tr]
    (if (and (sequential? tr) (= (count tr) 3))
      (let [[val l r] tr
            t1 (not (nil? val))
            t2 (or (nil? l) (tr? l))
            t3 (or (nil? r) (tr? r))]
        (and t1 t2 t3))
      false)))

(defcheck solution-37ecf4e4
  (fn tree? [xs] (and (coll? xs) (= 3 (count xs)) (every? #(or (nil? %) (tree? %)) (rest xs)))))

(defcheck solution-382cd095
  (fn tree? [t]
    (if
     (or (nil? t)
         (and (sequential? t)
              (= (count t) 3)
              (tree? (second t))
              (tree? (last t)))) true false)))

(defcheck solution-38850fe8
  (fn tree? [coll]
    (cond
      (nil? coll) true
      (not (coll? coll)) false
      (and (= (count coll) 3)
           (tree? (second coll))
           (tree? (nth coll 2))) true
      true false)))

(defcheck solution-38cc5a72
  ; I don't know why the second last one is false...
  ; but, just make the test pass..
  (fn is-bin-tree? [tree]
    (if (not= 3 (count tree))
      false
      (let [val   (first tree)
            left  (second tree)
            right (nth tree 2)]
        (and
         (not (coll? val))
         (if (coll? left) (is-bin-tree? left) (and (not (coll? left)) (not (false? left))))
         (if (coll? right) (is-bin-tree? right) (not (coll? right))))))))

(defcheck solution-38e6f979
  (fn tree? [t]
    (or (nil? t)
        (and (coll? t)
             (= 3 (count t))
             (tree? (second t))
             (tree? (last t))))))

(defcheck solution-3947a45a
  (fn isn? [t]
    (or (nil? t)
        (and (coll? t)
             (= 3 (count t))
             (isn? (second t))
             (isn? (last t))))))

(defcheck solution-39eac8e4
  (fn tree? [t]
    (or (nil? t)
        (and (sequential? t)
             (= 3 (count t))
             (tree? (nth t 1))
             (tree? (nth t 2))))))

(defcheck solution-3aa2c88d
  (fn tree? [seq]
    (or (nil? seq)
        (and (coll? seq)
             (= 3 (count seq))
             (tree? (second seq))
             (tree? (first (drop 2 seq)))))))

(defcheck solution-3ac8c1f4
  (fn t [c]
    (let [s sequential? n nil? f false]
      (if (s c)
        (if (== 3 (count c))
          (let [sc (second c)
                lc (last c)]
            (if (and (or (n sc) (s sc))
                     (or (n lc) (s lc)))
              (and (t sc) (t lc))
              f))
          f)
        true))))

(defcheck solution-3adae6f3
  (fn f [x]
    (if (sequential? x)
      (and (= 3 (count x)) (f (second x)) (f (last x)))
      (nil? x))))

(defcheck solution-3b1e9971
  (fn t [node]
    (if (and (coll? node)
             (= 3 (count node)))
      (let [[val left right] node]
        (and
         (not (nil? val))
         (or (nil? left)
             (t left))
         (or (nil? right)
             (t right))))
      false)))

(defcheck solution-3bc4507
  (fn tree? [t]
    (or (nil? t)
        (and
         (coll? t)
         (= 3 (count t))
         (tree? (nth t 1))
         (tree? (nth t 2))))))

(defcheck solution-3cac99a0
  (fn ct [tr]
    (if (nil? tr) true
                  (if (not (coll? tr)) false
                                       (let [[r le ri] tr]
                                         (if (not= 3 (count tr)) false
                                                                 (and (ct le) (ct ri))))))))

(defcheck solution-3d692bf3
  (fn is-tree [lst]
    (if (not (sequential? lst))
      (not= false lst)
      (let [rst (rest lst)]
        (if (= 2 (count rst))
          (and (is-tree (first rst))
               (is-tree (last rst)))
          false)))))

(defcheck solution-3d729034
  (fn binTree? [s]
    (cond
      (not (coll? s)) false
      (< (count s) 3) false
      (> (count s) 3) false
      (and (not (nil? (second s)))
           (not (nil? (nth s 2)))) (and (binTree? (second s))
                                        (binTree? (nth s 2)))
      (and (nil? (second s))
           (nil? (nth s 2))) true
      (nil? (second s)) (binTree? (nth s 2))
      (nil? (nth s 2)) (binTree? (second s))
      )))

(defcheck solution-3e1253d5
  (fn bt [t] (
               if (nil? t)
               true
               (
                 if (and (counted? t) (= 3 (count t)))
                 (and (bt (nth t 1)) (bt (nth t 2)))
                 false
                 )
               )))

(defcheck solution-3e264a0c
  (fn tree? [x]
    (or (nil? x)
        (and (coll? x)
             (= (count x) 3)
             (tree? (second x))
             (tree? (last x))))))

(defcheck solution-3e522a11
  (fn is-binary-tree? [t]
    (letfn [(child-type? [x]
              (or (sequential? x) (nil? x)))
            (valid-node? [n]
              (if (sequential? n)
                (and (= (count n) 3)
                     (not (child-type? (first n)))
                     (every? child-type? (rest n)))
                true))]
      (every? valid-node? (tree-seq sequential? seq t)))))

(defcheck solution-3e62ab1
  (fn tree? [node]
    (or (nil? node)
        (and (sequential? node)
             (= 3 (count node))
             (every? tree? (next node))))))

(defcheck solution-3ea1a378
  (fn ? [t]
    (or (nil? t)
        (and (coll? t)
             (= (count t) 3)
             (every? ? (rest t))))))

(defcheck solution-3ed42a17
  (fn istree [coll]
    (if (= nil coll)
      true
      (if (and (coll? coll) (= 3 (count coll)))
        (let [[v l r] coll]
          (and (istree l) (istree r))
          )
        false
        ))
    ))

(defcheck solution-3f20caf2
  (fn bin-tree? [t] (or (nil? t)
                        (and (coll? t)
                             (let [[_ left right] t]
                               (and (= 3 (count t))
                                    (bin-tree? left)
                                    (bin-tree? right)))))))

(defcheck solution-3fcc4e72
  (fn to-tree-or-not-to-tree [tree]
    (and
     (not (coll? (first tree)))
     (= 3 (count tree))

     ((comp
       (partial every? #(= true %))
       (partial map (fn [x]
                      (cond (nil? x) true
                            (coll? x) (to-tree-or-not-to-tree x)
                            :else false)
                      ))

       ) (rest tree)))))

(defcheck solution-40225b14
  (fn p [t]
    (or (nil? t)
        (and (coll? t) (= 3 (count t))
             (every? p (rest t))))))

(defcheck solution-40242ce8
  (fn tree? [c]
    (if (sequential? c)
      (and (= (count c) 3)
           (tree? (first (rest c)))
           (tree? (second (rest c))))
      (nil? c))))

(defcheck solution-409b86a1
  (fn is-b [t]
    (cond (nil? t) true
          (coll? t) (let [[n l r] t]
                      (and (= (count t) 3) (not (nil? n)) (is-b l) (is-b r)))
          :else false)
    ))

(defcheck solution-40eb5811
  (fn binary-tree? [tree]
    (let [[val left right] tree]
      (and (= (count tree) 3)
           (or (nil? left) (and (coll? left) (binary-tree? left)))
           (or (nil? right) (and (coll? right) (binary-tree? right)))))))

(defcheck solution-41323beb
  (fn tree?
    [tree]
    (or (nil? tree)
        (and (coll? tree)
             (= 3 (count tree))
             (tree? (second tree))
             (tree? (last tree))))))

(defcheck solution-414f9b35
  (fn fun [t]
    (and
     (= (count t) 3)
     (not (coll? (first t)))
     (if (coll? (second t))
       (fun (second t))
       (nil? (second t)))
     (if (coll? (first (next (next t))))
       (fun (first (next (next t))))
       (nil? (first (next (next t))))))))

(defcheck solution-41c1922d
  (fn tree? [x]
    (if (nil? x) true
                 (if (not (coll? x)) false
                                     (if (not (= (count x) 3)) false
                                                               (and (tree? (second x)) (tree? (nth x 2))))))))

(defcheck solution-4214b2ff
  (fn tree? [form]
    (or
     (nil? form)
     (and
      (coll? form)
      (= 3 (count form))
      (tree? (nth form 1))
      (tree? (nth form 2))))))

(defcheck solution-423ba600
  (letfn [(my-val? [a] true)
          (my-node? [x]
            (or
             (nil? x)
             (and
              (coll? x)
              (= 3 (count x))
              (my-val? (first x))
              (my-node? (second x))
              (my-node? (second (rest x))))))]
    (fn [y] (my-node? y))))

(defcheck solution-42e6c30c
  (fn tree? [tree]
    (cond
      (nil? tree) true
      (and (coll? tree) (= 3 (count tree))) (and
                                             (tree? (nth tree 1))
                                             (tree? (nth tree 2)))
      :else false)))

(defcheck solution-434a37fc
  (fn [t] (every? #(and (counted? %) (= 3 (count %)))
            (filter (comp not nil?) (tree-seq identity next t)))))

(defcheck solution-437a5db4
  (fn check-tree [coll]
    (cond (nil? coll) true
          (not (coll? coll)) false
          (not= (count coll) 3) false
          :else (and (check-tree (second coll))
                     (check-tree (last coll)))
          )
    ))

(defcheck solution-437fb34d
  (fn bin? [a] (or (nil? a) (and (not= false a) (= 3 (count a)) (bin? (second a)) (bin? (last a))))))

(defcheck solution-438ed72b
  (fn tree? [v]
    (if (= nil v)
      true
      (if (sequential? v)
        (if (not= 3 (count v))
          false
          (let [value (nth v 0)
                left  (nth v 1)
                right (nth v 2)
                ]
            (if (sequential? value)
              false
              (and (tree? left) (tree? right))
              )
            )
          )
        false
        ))
    ))

(defcheck solution-43a12486
  (fn bin-tree? [node]
    (or (nil? node)
        (and (coll? node)
             (= (count node) 3)
             (let [[v l r] node]
               (and (bin-tree? l)
                    (bin-tree? r)))))))

(defcheck solution-43fa8a25
  (fn istree [xs]
    (if (nil? xs) true
                  (and (coll? xs) (= 3 (count xs)) (istree (second xs)) (istree (nth xs 2))))))

(defcheck solution-44b11624
  (fn btree? [c]
    (if (coll? c)
      (and (= 3 (count c))
           (first c)
           (every? true? (map btree? (rest c))))
      (nil? c))))

(defcheck solution-44fccbd8
  (fn tree? [node]
    (or (nil? node)
        (and (sequential? node)
             (= (count node) 3)
             (every? tree? (rest node))))))

(defcheck solution-45327781
  (fn btree? [xs]
    (cond
      (nil? xs) true
      (not (coll? xs)) false
      (not= (count xs) 3) false
      (nil? (first xs)) false
      :else (and (btree? (nth xs 1)) (btree? (nth xs 2))))))

(defcheck solution-45f3349d
  (fn f [t]
    (or (nil? t)
        (and (coll? t) (= 3 (count t))
             (every? f (next t))))))

(defcheck solution-4616023b
  (fn f [s]
    (if (sequential? s)
      (let [[_ a b] s]
        (and (= 3 (count s)) (f a) (f b)))
      (nil? s))))

(defcheck solution-4629007
  (fn tree?
    [t]
    (if (and (coll? t) (= 3 (count t)))
      (let [[_ & ab] t]
        (every? #(or (nil? %) (tree? %)) ab))
      false)))

(defcheck solution-46305de4
  (fn tree? [s]
    (cond
      (= s nil) true
      (not (coll? s)) false
      (not= (count s) 3) false
      true (let [[x0 x1 x2] s]
             (and (tree? x1) (tree? x2))))))

(defcheck solution-463b2a8a
  (fn tree?
    [coll]
    (letfn [(check-node [n]
              (cond
                (sequential? n) (tree? n)
                (nil? n) true
                :else false))]
      (and
       (= 3 (count coll))
       (check-node (second coll))
       (check-node (second (rest coll)))))))

(defcheck solution-46586409
  (fn tree? [s]
    (cond
      (or (seq? s) (vector? s))
      (and (= (count s) 3) (tree? (second s)) (tree? (last s)))
      (nil? s) true
      :else false)))

(defcheck solution-4677293
  (fn f [tree]
    (and (= 3 (count tree)) (every? identity (map #(if (sequential? %) (f %) (not (false? %))) tree)))))

(defcheck solution-46884eaa
  (fn t? [x] (or (= x nil)
                 (and (coll? x)
                      (= (count x) 3)
                      (t? (nth x 1))
                      (t? (nth x 2))))))

(defcheck solution-469fd96a
  (fn f [sq]
    (let [l (rest (remove #(nil? %) sq))]
      (cond (some empty? l) false
            (some false? (flatten l)) false
            (and (= 3 (count sq)) (not (empty? l))) (= 1 (count (distinct (map f l))))
            (and (= 3 (count sq)) (empty? l)) true
            :else false))))

(defcheck solution-46c2377
  (fn is-binary-tree? [tree]
    (letfn [(left-node [tree] (second tree))
            (right-node [tree] (nth tree 2))]
      (and (coll? tree)
           (= (count tree) 3)
           (or (nil? (left-node tree))
               (is-binary-tree? (left-node tree)))
           (or (nil? (right-node tree))
               (is-binary-tree? (right-node tree)))))))

(defcheck solution-471050e
  (fn [a]
    (->> a
      (tree-seq sequential?
        (comp #(remove nil? %) rest))
      (every? (every-pred sequential?
                #(= 3 (count %)))))))

(defcheck solution-4712a5b5
  (fn tree? [coll]
    (and (= 3 (count coll))
         (every? tree? (filter coll? coll))
         (every? #(or (nil? %) (coll? %)) (rest coll)))))

(defcheck solution-47498bd
  (fn __ [s]
    (or (nil? s)
        (and (coll? s)
             (= (count s) 3)
             (not (nil? (first s)))
             (not (coll? (first s)))
             (__ (first (rest s)))
             (__ (second (rest s)))
             )
        )
    ))

(defcheck solution-4758a80f
  (fn f [t]
    (or
     (and
      (coll? t)
      (= 3 (count t))
      (f (get t 1))
      (f (get (vec t) 2)))
     (nil? t))))

(defcheck solution-47827191
  (fn [bt]
    (let [bt? (fn [bt] (and (sequential? bt)
                            (= 3 (count bt)))),
          f   (fn [x]
                (loop [v x]
                  (cond (empty? v) true
                        :else (let [f (first v)]
                                (if (bt? f)
                                  (recur (conj (rest v) (nth f 0) (nth f 1) (nth f 2)))
                                  (if (or (and (seq? f) (empty? f)) (false? f))
                                    false
                                    (recur (rest v))))))))]
      (if (bt? bt) (f bt)
                   false))))

(defcheck solution-480978d5
  (fn f [x]
    (
      if (coll? x)
      (and (= 3 (count x)) (every? f (rest x)))
      (nil? x)
      )
    ))

(defcheck solution-48cbc5d5
  (fn tree? [t]
    (if (nil? t)
      true
      (if (or (not (coll? t)) (not= (count t) 3))
        false
        (and (tree? (second t)) (tree? (last t)))))))

(defcheck solution-48d240b7
  (fn isbin [x]
    (if (nil? x) true
                 (if (not (sequential? x)) false
                                           (if (not= (count x) 3) false
                                                                  (and (isbin (last x)) (isbin (last (butlast x)))))))))

(defcheck solution-49260d68
  (fn bt? [s] (and (coll? s)
                   (= 3 (count s))
                   (or (nil? (nth s 1)) (bt? (nth s 1)))
                   (or (nil? (nth s 2)) (bt? (nth s 2))))))

(defcheck solution-4939471a
  (fn f [xs]
    (or (= xs nil)
        (and (coll? xs) (= 3 (count xs)) (f (second xs)) (f (nth xs 2))))))

(defcheck solution-497b6736
  (fn isBTree? [t]
    (cond
      (and (sequential? t) (= 3 (count t))) (and (isBTree? (second t)) (isBTree? (last t)))
      (nil? t) true
      :else false)))

(defcheck solution-49e70ec3
  (fn is-binary-tree [nodes]
    (if (nil? nodes)
      true
      (if (sequential? nodes)
        (if (= 1 (count nodes))
          (if (every? nil? nodes)
            true
            false
            )
          (if (= 3 (count nodes))
            (and (is-binary-tree (nth nodes 1)) (is-binary-tree (nth nodes 2)))
            false
            )
          )
        false
        )
      )
    ))

(defcheck solution-4a3b7bcd
  (fn tree? [a]
    (or
     (nil? a)
     (and
      (coll? a)
      (= (count a) 3)
      (every? tree? (rest a))))))

(defcheck solution-4a480b5f
  (fn [col] (every? #(if (coll? %) (= 3 (count %))) (->> col (tree-seq coll? rest) (remove nil?)))))

(defcheck solution-4a96920d
  (fn istree [x]
    (and x
         (= 3 (count x))
         (every?
           istree
           (keep
             identity
             (rest x))))))

(defcheck solution-4a998ecd
  (fn tree? [t]
    (or (nil? t)
        (and (coll? t)
             (= 3 (count t))
             (every? tree? (rest t))))))

(defcheck solution-4aa84098
  (fn lcount [l]
    (and (= 3 (count l))
         (every? #(if (coll? %)
                    (lcount %)
                    (not (false? %)))
           l))))

(defcheck solution-4af99a20
  (fn bin-tree? [tree]
    (or (nil? tree)
        (and (coll? tree)
             (= 3 (count tree))
             (and (bin-tree? (nth tree 1))
                  (bin-tree? (nth tree 2)))))))

(defcheck solution-4b2a904a
  (fn check-tree [node] (cond
                          (not (coll? node)) (nil? node)
                          (not (= 3 (count node))) false
                          :else (and (check-tree (nth node 1)) (check-tree (nth node 2))))))

(defcheck solution-4b34b1fd
  (fn b [n]
    (if (and (coll? n) (= (count n) 3))
      (every? true? (map b (filter #(not (nil? %)) (rest n))))

      false)))

(defcheck solution-4b9b0bc0
  (fn bintree [ls]
    (or (nil? ls)
        (and (sequential? ls)
             (= 3 (count ls))
             (bintree (second ls))
             (bintree (second (rest ls)))))))

(defcheck solution-4c0ebeaa
  (fn binary-tree? [coll]
    (let [value (first coll) left (second coll) right (last coll)]
      (and (= 3 (count coll))
           (or (nil? left) (and (sequential? left) (binary-tree? left)))
           (or (nil? right) (and (sequential? right) (binary-tree? right)))
           ))))

(defcheck solution-4c369ade
  (fn tree? [t]
    (cond (nil? t) true
          (coll? t) (and (= (count t) 3)
                         ((complement coll?) (first t))
                         (tree? (second t))
                         (tree? (last t)))
          :else false)))

(defcheck solution-4d147ff8
  (fn f [s]
    (or (nil? s)
        (and (or (seq? s) (vector? s))
             (= (count s) 3)
             (f (nth s 1))
             (f (nth s 2))))))

(defcheck solution-4d4acdfd
  (fn is-tree [lst]
    (if (or (not (coll? lst)) (not= 3 (count lst))) false
                                                    (let [lc (fnext lst) rc (last lst)]
                                                      (cond
                                                        (and (nil? lc) (nil? rc)) true
                                                        (and (nil? lc) (not (nil? rc))) (is-tree rc)
                                                        (and (nil? rc) (not (nil? lc))) (is-tree lc)
                                                        :else (and (is-tree rc) (is-tree lc)))))))

(defcheck solution-4d5999ad
  (fn f [t]
    (or (nil? t)
        (and t
             (= [true true] (map f (rest t)))))))

(defcheck solution-4d6d8f5a
  (fn tree? [tree]
    (let [seq-or-nil? (fn [x] (or (sequential? x) (nil? x)))]
      (cond
        (nil? tree) true
        (not (= [true true] (map seq-or-nil? (drop 1 tree)))) false
        :else (and (-> tree second tree?) (-> tree last tree?))))))

(defcheck solution-4d90b174
  (fn p95 [lst]
    (letfn [(tree? [lst]
              (or (nil? lst)
                  (and (coll? lst) (= 3 (count lst))
                       (let [[v l r] lst] (and (tree? l) (tree? r))))))] (tree? lst))))

(defcheck solution-4de602d9
  #(letfn [
           (tree? [t]
             (if (sequential? t)
               (let [[v l r] t]
                 (if (and (sequential? t) (= 3 (count t)))
                   (and (or (nil? l) (tree? l))
                        (or (nil? r) (tree? r)))
                   false))
               false))]
     (tree? %)))

(defcheck solution-4e011681
  (fn ff [[root l r :as tree]]
    (and (= 3 (count tree))
         (not (sequential? root))
         (or (nil? l) (and (sequential? l) (ff l)))
         (or (nil? r) (and (sequential? r) (ff r))))))

(defcheck solution-4e5be34
  (fn a [l]
    (if (nil? l)
      true
      (if (coll? l)
        (if (= (count l) 3)
          (and (a (nth l 1)) (a (nth l 2)))
          false)
        false))))

(defcheck solution-4ec1e5c0
  (fn chk [lst]

    (let [[v lc rc & more] lst]
      (and (not (nil? v))
           (if (coll? lc) (chk lc) (not (false? lc)))
           (if (coll? rc) (chk rc) (not (false? rc)))
           (not more)
           (= (count lst) 3)
           )
      )
    ))

(defcheck solution-4ef0f931
  (fn [tree]
    (loop [t tree]
      (if (not= 3 (count t)) false
                             (if (sequential? (second t))
                               (recur (second t))
                               (if (= false (second t))
                                 false
                                 (if (sequential? (last t))
                                   (recur (last t)) true)))))))

(defcheck solution-4f2d025b
  (fn bt [t] (if (nil? t) true (if
                                (and (sequential? t) (= 3 (count t))) (and (bt (nth t 1)) (bt (nth t 2))) false))))

(defcheck solution-4f2e93cd
  (fn t [c]
    (or
     (and
      (coll? c)
      (= (count c) 3)
      (t (second c))
      (t (last c)))
     (nil? c))))

(defcheck solution-4fd6dca7
  (fn [coll]
    (letfn [(node? [t] (and (coll? t)
                            (= 3 (count t))
                            (not (nil? (first t)))))
            (tree? [t]
              (cond (nil? t) true
                    (node? t) (and (tree? (nth t 1))
                                   (tree? (nth t 2)))
                    :else false))]
      (tree? coll))))

(defcheck solution-509d3d69
  (fn is-tree
    [tree]
    (or
     (nil? tree)
     (if (and (not (boolean? tree)) (= 3 (count tree)))
       (let [[_ left right] tree]
         (and (is-tree left) (is-tree right))) false))))

(defcheck solution-50a2eed2
  (fn btree? [coll]
    (and (not= false coll) (not= () coll)
         (or (not (sequential? coll))
             (and (= 3 (count coll)) (every? btree? coll))))))

(defcheck solution-510c550c
  (fn bintree? [tree]
    (if (and (sequential? tree) (= 3 (count tree)))
      (if-not (nil? (second tree))
        (bintree? (second tree))
        (if-not (nil? (nth tree 2))
          (bintree? (nth tree 2))
          true))
      false)))

(defcheck solution-514cfce5
  (fn q4q095-2 [t]
    "Write a predicate which checks whether or not a given sequence represents a binary tree. Each node in the tree must have a value, a left child, and a right child."
    (letfn
     [(bin [my-t]
        (and
         (coll? my-t)
         (= 3 (count my-t))
         (every? #(or (coll? %) (nil? %)) (rest my-t))))]
      (bin
        (clojure.walk/postwalk
          (fn [inner-t]
            (if (coll? inner-t)
              (if (bin inner-t)
                inner-t
                false)
              inner-t))
          t)))))

(defcheck solution-51adbc1e
  (fn f [t] ({nil 1} t (and (coll? t) (= 3 (count t)) (every? f (rest t))))))

(defcheck solution-51eff1af
  (fn b-tree [coll]
    (if (= 3 (count coll))
      (if-let [rst (seq (filter sequential? (rest coll)))]
        (every? b-tree rst)
        (every? nil? (rest coll)))
      false)))

(defcheck solution-521a02d8
  (fn t [s] (let [l #(or (nil? %) (and (coll? %) (t %)))] (and (= 3 (count s)) (l (first (next s))) (l (last s))))))

(defcheck solution-52448e00
  (fn [x] (let [f (flatten x)]
            (=
              (dec (count
                     (filter nil? f)))
              (count
                (filter #(not (nil? %)) f))
              )
            )))

(defcheck solution-527ddd17
  (fn is-tree? [node]
    (or (nil? node)
        (and (coll? node)
             (= (count node) 3)
             (is-tree? (nth node 1))
             (is-tree? (nth node 2))))))

(defcheck solution-529ea4ba
  (fn bin-tree? [t]
    (or (and (coll? t)
             (= (count t) 3)
             (every? bin-tree? (rest t)))
        (nil? t))))

(defcheck solution-52b9e9d3
  (fn tree? [coll]
    (or (nil? coll)
        (and (sequential? coll)
             (= 3 (count coll))
             (every? tree? (rest coll))))))

(defcheck solution-52df95a0
  (fn isTree [[r l p :as vst]]
    (if (= (count vst) 3)
      (cond
        (and (not (coll? l)) (not (nil? l))) false
        (and (not (coll? p)) (not (nil? p))) false
        (and (not (coll? r)) (nil? l) (nil? p)) true
        (and (coll? l) (nil? p)) (isTree l)
        (and (nil? l) (coll? p)) (isTree p)
        :else (and (isTree l) (isTree p)))
      false)))

(defcheck solution-53bc351a
  (fn binary-tree? [data]
    (and
     (sequential? data)
     (= (count data) 3)
     (or (nil? (nth data 1)) (binary-tree? (nth data 1)))
     (or (nil? (nth data 2)) (binary-tree? (nth data 2)))
     )))

(defcheck solution-54250cfb
  (fn tree [x]
    (or (nil? x)
        (if (or (not (coll? x)) (not= (count x) 3)) false
                                                    (and (tree (second x)) (tree (last x)))))))

(defcheck solution-54306aa0
  (fn tree? [col]
    (or (nil? col)
        (and (sequential? col)
             (= 3 (count col))
             (every? tree? (rest col))))))

(defcheck solution-54aadf66
  (fn b [xs]
    (or (nil? xs) (and (sequential? xs) (let [[_ l r] xs] (and (== (count xs) 3) (b l) (b r)))))))

(defcheck solution-56b8110f
  (fn [l]
    (cond
      (or (even? (count (flatten l)))
          (some false? (flatten l)))
      false
      :else true)))

(defcheck solution-56cdedf3
  (fn treep [s]
    (or (nil? s)
        (and (coll? s)
             (= (count s) 3)
             (treep (nth s 1))
             (treep (nth s 2))))))

(defcheck solution-56cefaa0
  (fn f
    [l]
    (cond
      (nil? l) true
      (not (coll? l)) false
      (= (count l) 3) (let [[v l r] l]
                        (and (f l) (f r)))
      :else false)))

(defcheck solution-57118d45
  (fn binary-tree?
    [tree]
    (let [next-fn (fn [x] (or (nil? x) (and (sequential? x) (binary-tree? x))))]
      (if (and (first tree) (= 3 (count tree)))
        (and (next-fn (second tree)) (next-fn (last tree)))
        false))))

(defcheck solution-57741994
  (fn is-tree? [xs]
    (and (sequential? xs)
         (= 3 (count xs))
         (not (sequential? (first xs)))
         (not (nil? (first xs)))
         (or (nil? (second xs)) (is-tree? (second xs)))
         (or (nil? (last xs)) (is-tree? (last xs))))))

(defcheck solution-579964ed
  (fn is-binary-tree? [input]
    (cond (nil? input) true
          (not (coll? input)) false
          (= 1 (count input)) true
          (= 3 (count input)) (and (is-binary-tree? (second input))
                                   (is-binary-tree? (nth input 2)))
          :else false)))

(defcheck solution-57fcdb44
  (fn tree? [lst]
    (cond
      (nil? lst) true
      (not (sequential? lst)) false
      (not= 3 (count lst)) false
      :else (every? tree? (rest lst)))))

(defcheck solution-58270723
  (fn t? [t]
    (or (nil? t)
        (and (coll? t)
             (= 3 (count t))
             (t? (nth t 1))
             (t? (nth t 2))))))

(defcheck solution-5858ad66
  (fn binary-tree? [t] (let [[v l r] t node? #(or (nil? %) %)] (or (nil? t) (and (= 3 (count t)) (node? l) (binary-tree? l) (node? r) (binary-tree? r))))))

(defcheck solution-586f4f88
  (fn tree? [coll]
    (cond (nil? coll) true
          (not (coll? coll)) false
          :else (let [[n l r] coll]
                  (and (not (nil? n))
                       (= 3 (count coll))
                       (or (nil? l) (tree? l))
                       (or (nil? r) (tree? r)))))))

(defcheck solution-58e4bbe8
  (fn isbt? [root]
    (or (nil? root)
        (letfn [(coll-or-nil? [coll] (or (coll? coll) (nil? coll)))
                (node? [[value left right :as coll]] (and (coll? coll)
                                                          (= 3 (count coll))
                                                          (coll-or-nil? left)
                                                          (coll-or-nil? right)))]
          (and (node? root)
               (isbt? (nth root 1))
               (isbt? (nth root 2)))))))

(defcheck solution-58e70ee9
  (fn tree? [t]
    (cond (nil? t) true
          (false? t) false
          (not= (count t) 3) false
          :else (and true (and (tree? (second t)) (tree? (second (rest t))))))))

(defcheck solution-593aaee3
  (fn binary-tree? [x]
    (and x
         (= (count x) 3)
         (let [[node l r] x]
           (and (not (nil? node))
                (or (nil? l) (binary-tree? l))
                (or (nil? r) (binary-tree? r)))))))

(defcheck solution-595d8139
  (fn tree? [t]
    (or
     (nil? t)
     (and
      (coll? t)
      (= (count t) 3)
      (every? tree? (rest t))))))

(defcheck solution-5963db99
  (fn tree?
    ([[v l r :as a]] (and (= 3 (count a)) (or (nil? l) (and (sequential? l) (tree? l))) (or (nil? r) (tree? r))))))

(defcheck solution-5973ea94
  (fn t [n]
    (or (nil? n)
        (and (coll? n)
             (= 3 (count n))
             (every? t (rest n))))))

(defcheck solution-59e6b7d5
  (fn binary-tree? [tree]
    (cond (nil? tree) true
          (and (coll? tree)
               (= (count tree) 3)) (and (binary-tree? (second tree))
                                        (binary-tree? (second (rest tree))))
          :else false)))

(defcheck solution-5a4e0ee3
  (fn tree [coll]
    (if (coll? coll)
      (if (= 3 (count coll))
        (and (tree (second coll)) (tree (last coll)))
        false)
      (or (nil? coll) (number? coll)))))

(defcheck solution-5a6bc214
  (fn [t]
    (letfn [(valid? [node]
              (if (coll? node)
                (and (= 3 (count node)) (not (nil? (first node))) (valid? (second node)) (valid? (nth node 2)))
                (nil? node)))]
      (valid? t))))

(defcheck solution-5b914f67
  (fn is_bin? [x]
    (if (or (seq? x) (vector? x))
      (if (and (= 3 (count x))
               (not (seq? (nth x 0)))
               (is_bin? (nth x 1))
               (is_bin? (nth x 2))) true false)
      (if (nil? x) true false))))

(defcheck solution-5bad93c8
  (fn tree? [xs]
    (cond
      (nil? xs) true
      (not (coll? xs)) false
      :else (and (= (count xs) 3) (tree? (second xs)) (tree? (last xs))))))

(defcheck solution-5bd3a0b6
  (fn f [node] (and (= (count node) 3) (let [[p l r] node c #(or (nil? %) (and (coll? %) (f %)))] (and (c l) (c r))))))

(defcheck solution-5be1c9cc
  (fn is-tree? [t]
    (cond
      (nil? t) true
      (and (coll? t) (== 3 (count t))) (and (is-tree? (nth t 1))
                                            (is-tree? (last t)))
      :else false)))

(defcheck solution-5bee15af
  (fn tree-
    [collumn]
    (cond
      (coll? collumn) (and (= 3 (count collumn))
                           (let [[k v1 v2] collumn]
                             (let [x1 (tree- v1)
                                   x2 (tree- v2)]
                               (and x1 x2))))
      (nil? collumn) true
      :else false)))

(defcheck solution-5c6f8747
  (fn tree? [possibletree] (if (nil? possibletree) true
                                                   (and
                                                    (coll? possibletree)
                                                    (= 3 (count possibletree))
                                                    (every? tree? (rest possibletree)))
                                                   )))

(defcheck solution-5c9851e2
  (fn isTree
    [seq]
    (if (= (count seq) 3)
      (if (and (coll? (second seq)) (coll? (last seq)))
        (and (isTree (second seq)) (isTree (last seq)))
        (if (and (= nil (second seq)) (coll? (last seq)))
          (recur (last seq))
          (if (and (= nil (last seq)) (coll? (second seq)))
            (recur (second seq))
            (if (and (= nil (last seq)) (= nil (second seq)))
              true
              false)
            )
          )
        )
      false)
    ))

(defcheck solution-5d50c9a7
  (fn bin-tree? [t]
    (cond
      (nil? t) true
      (number? t) true
      (or (list? t) (vector? t))
      (if (not= (count t) 3)
        false
        (and (bin-tree? (second t))
             (bin-tree? (second (rest t)))))
      :else false)))

(defcheck solution-5d88a5a
  (letfn [(bt? [xs]
            (if (and (coll? xs) (not= 3 (count xs)))
              false
              (let [[v l r] xs
                    valid? #(if (coll? %) (bt? %) (not= false %))]
                (and (valid? l) (valid? r)))))]
    bt?))

(defcheck solution-5dae5957
  (fn a [tree]
    (if (or (not (coll? tree))
            (not= (count tree) 3))
      false
      (and
       (or (nil? (nth tree 1))
           (a (nth tree 1)))
       (or (nil? (nth tree 2))
           (a (nth tree 2)))))))

(defcheck solution-5dbeb281
  (comp
   (partial = 0)
   count
   (partial filter #(if (coll? %)
                      (or (nil? (first %)) (not (= 3 (count %))))
                      (not (nil? %))))
   (partial tree-seq coll? rest)))

(defcheck solution-5eaae538
  (fn istree? [tree]
    (cond
      (nil? tree) true
      (sequential? tree)
      (let [children (rest tree)]
        (and (= 2 (count children))
             (istree? (first children))
             (istree? (last children))
             )
        )
      :else false
      )))

(defcheck solution-5ec1ca47
  (fn is-tree [s]
    (and (sequential? s)
         (= 3 (count s))
         (let [l (second s)
               r (nth s 2)]
           (and (or (nil? l) (is-tree l))
                (or (nil? r) (is-tree r))
                )))))

(defcheck solution-5ecdfd52
  (fn is-tree [xs]
    (and
     (coll? xs)
     (=
       (count xs)
       3
       )
     (every?
       (fn [x]
         (or
          (nil? x)
          (integer? x)
          (keyword? x)
          (is-tree x)
          )
         )
       xs
       )
     )
    ))

(defcheck solution-5ed7006c
  (fn [ms]
    (letfn [(go [ns]
              (if (not= 3 (count ns))
                false
                (let [[v l r] ns]
                  (and (or (nil? l) (and (sequential? l) (go l)))
                       (or (nil? r) (and (sequential? r) (go r)))))))]
      (go ms))))

(defcheck solution-5ee6684c
  (fn __ [tree]
    (odd? (count (filter #(not= false %) (flatten tree))))))

(defcheck solution-5f62370d
  (fn f [t]
    (cond ((complement coll?) t) true
          (not= (count t) 3) false
          ((comp not every?) #(or (nil? %) (coll? %)) (rest t)) false
          :else (every? identity (map f t)))))

(defcheck solution-5f8c0fd
  (fn tree? [x]
    (and (coll? x) (= (count x) 3)
         (let [[_ a b] x]
           (and
            (or (nil? a) (tree? a))
            (or (nil? b) (tree? b)))))))

(defcheck solution-5ffd7b4f
  (fn btree? [node]
    (if (nil? node)
      true
      (if (and (sequential? node) (= 3 (count node)))
        (let [[v l r] node]
          (and (btree? l)
               (btree? r)))
        false))))

(defcheck solution-604022d5
  (fn [s] (if (seq? s) (= 0 (compare [3] (vec (distinct (map count (filter seq? (replace {false [nil nil]} (tree-seq seq? seq s))))))))
                       (= 0 (compare [3] (vec (distinct (map count (filter vector? (replace {false [nil nil], () [nil nil]} (tree-seq vector? vec s))))))))
                       )))

(defcheck solution-605c151b
  (fn rec [t]

    (cond
      (nil? t)
      true

      (and (or (vector? t)
               (seq? t))
           (= 3 (count t)))

      (let [[root lhs rhs] t]
        (and
         (rec lhs)
         (rec rhs)))

      :else
      false)))

(defcheck solution-607fcf6e
  (fn tree? [t]
    (or (nil? t)
        (and (sequential? t)
             (= (count t) 3)
             (not (sequential? (first t)))
             (tree? (second t))
             (tree? (nth t 2))))))

(defcheck solution-60c949ea
  (fn tree? [t]
    (if (nil? t)
      true
      (and (sequential? t)
           (= 3 (count t))
           (tree? (nth t 1))
           (tree? (nth t 2))))))

(defcheck solution-60f481f4
  (fn tree? [s]
    (and (sequential? s)
         (= (count s) 3)
         (not (sequential? (first s)))
         (or (nil? (second s)) (tree? (second s)))
         (or (nil? (last s)) (tree? (last s))))))

(defcheck solution-6129f03a
  (fn btree? [x]
    (or
     (nil? x)
     (and (coll? x)
          (= 3 (count x))
          (every? btree? (rest x)))
     false)))

(defcheck solution-617df205
  (fn bt? [t] (or (nil? t)
                  (and
                   (sequential? t)
                   (= 3 (count t))
                   (bt? (nth t 1))
                   (bt? (nth t 2))))))

(defcheck solution-617f381e
  (fn f [s]
    (or (nil? s) (and (coll? s) (= 3 (count s)) (f (second s)) (f (second (next s)))))))

(defcheck solution-619c1303
  (fn peu [x] (if (= nil x) true (if (and (not= x false) (= 3 (count x))) (and (peu (second x)) (peu (last x))) false))))

(defcheck solution-61f00cf2
  (fn tree? [e]
    (or (nil? e)
        (and (counted? e)
             (= 3 (count e))
             (every? tree? (next e))))))

(defcheck solution-62096538
  (fn btree? [[n & xs]]
    (and
     (= 2 (count xs))
     (every?
       (fn [i] (or (nil? i) (and (or (seq? i) (vector? i)) (btree? i))))
       xs))))

(defcheck solution-62258e02
  (fn tree? [coll]
    (let [node? (fn [node] (or (nil? node) (tree? node)))]
      (if (and
           (or (seq? coll) (vector? coll))
           (= (count coll) 3)
           (first coll)
           (node? (second coll))
           (node? (nth coll 2)))
        true
        false))))

(defcheck solution-624d422f
  (fn treep [tree]
    (or (nil? tree)
        (and (coll? tree)
             (= 3 (count tree))
             (every? treep (rest tree))))))

(defcheck solution-6265f859
  (fn tree? [t]
    (or (nil? t)
        (and (coll? t)
             (= (count t) 3)
             (tree? (second t))
             (tree? (last t))))))

(defcheck solution-6324d909
  (fn bintree? [v]
    (if (nil? v)
      true
      (if (or (not (coll? v)) (not= (count v) 3))
        false
        (and (bintree? (second v)) (bintree? (last v)))
        )
      )
    ))

(defcheck solution-63750583
  (fn binary-tree? [x]
    (if (not (sequential? x))
      (not (false? x))
      (and (= (count x) 3)
           (every? true? (map binary-tree? x))))))

(defcheck solution-6386de8c
  (fn bt [t] (or (nil? t) (and (coll? t) (= 3 (count t)) (and (bt (nth t 1)) (bt (nth t 2)))))))

(defcheck solution-63ab9edf
  (fn is-tree
    [n]
    (if (and (coll? n) (= (count n) 3))
      (let [[v l r] n]
        (and (if (nil? l) true (is-tree l))
             (if (nil? r) true (is-tree r))))
      false)))

(defcheck solution-63e4db4e
  (fn test-tree
    [s]
    (let [left  (second s)
          right (last s)]
      (cond
        (= (count s) 1) true
        (= (count s) 3) (and
                         (if (coll? left) (test-tree left) (not (false? left)))
                         (if (coll? right) (test-tree right) (not (false? right))))
        :else false))))

(defcheck solution-63f76113
  (fn tree? [t]
    (cond (nil? t) true
          (not (coll? t)) false
          (= (count t) 3) (and (tree? (nth t 1)) (tree? (nth t 2)))
          :else false)))

(defcheck solution-643fd084
  (fn t? [[v l r :as t]] (and (= (count t) 3) (every? #(or (nil? %) (and (coll? %) (t? %))) [l r]))))

(defcheck solution-648b1809
  (fn istree? [root]
    (cond

      (= root nil) true
      (= (coll? root) false) false
      (= (count root) 3)
      (and
       (istree? (nth root 1))
       (istree? (nth root 2)))
      :else false)))

(defcheck solution-649778d0
  (fn branch? [tree]
    (if (coll? tree)
      (and (= 3 (count tree)) (every? branch? (rest tree)))
      (not (contains? #{'() false} tree)))))

(defcheck solution-649a7d18
  (fn t [n] (if (nil? n) true (and (coll? n) (= 3 (count n)) (t (second n)) (t (last n))))))

(defcheck solution-65367d39
  (fn tree? [s]
    (cond (nil? s) true
          (not (coll? s)) false
          (not= 3 (count s)) false
          :default (let [[v l r] s]
                     (and (tree? l) (tree? r))))))

(defcheck solution-6578a400
  (fn tree? [coll]
    (or (nil? coll)
        (and (sequential? coll)
             (= 3 (count coll))
             (tree? (nth coll 1))
             (tree? (nth coll 2))))))

(defcheck solution-65f71545
  (fn check [node]
    (cond
      (and (coll? node) (= (count node) 3)) (and (check (second node)) (check (last node)))
      (nil? node) true
      :else false)))

(defcheck solution-66a2e7aa
  (fn bt [a] (if (coll? a) (and (reduce #(and % (bt %2)) true (rest a)) (= 2 (count (filter #(or (= % nil) (coll? (seq %))) (rest a))))) (= a nil))))

(defcheck solution-66b9b056
  (fn [t]
    ((fn ok? [x]
       (cond
         (nil? x) true
         (not (coll? x)) false
         (= 3 (count x)) (and (ok? (second x)) (ok? (last x)))
         :else false
         )) t)))

(defcheck solution-66db0bae
  (fn isTree [l]
    (or (nil? l)
        (and (sequential? l)
             (= (count l) 3)
             (let [[a b c] l]
               (and (isTree b)
                    (isTree c)))))))

(defcheck solution-66f585ac
  (fn bt? [t]
    (cond
      (nil? t) true
      (and (sequential? t) (= (count t) 3)) (let [[x y z] t] (and (bt? y) (bt? z)))
      :else false)))

(defcheck solution-672ca0e5
  (fn ? [[h s & [t & r :as a]]]
    (let [f #(or (nil? %) (and (coll? %) (? %)))]
      (boolean (and (not r) a h (f s) (f t))))))

(defcheck solution-6750be34
  (fn trii? [coll]
    (let [length (if (sequential? coll) (count coll) 0)]
      (condp = length
        0 (nil? coll)
        3 (and (trii? (nth coll 1))
               (trii? (nth coll 2)))
        false))))

(defcheck solution-6770964d
  (fn ? [[a b c :as x]]
    (and
     (= 3 (count x))
     (if (coll? b) (? b) (nil? b))
     (if (coll? c) (? c) (nil? c)))))

(defcheck solution-679585df
  (fn tree?- [coll]
    (if (sequential? coll)
      (and (= 3 (count coll))
           (tree?- (first coll))
           (tree?- (second coll))
           (tree?- (last coll)))
      (if (or (nil? coll) coll)
        true false))))

(defcheck solution-67bf5767
  (fn btree? [branch]
    (and (sequential? branch)
         (= (count branch) 3)
         (every? identity (map #(or (nil? %) (btree? %)) (rest branch))))))

(defcheck solution-67e357ed
  (fn tree [[_ l r :as a]]
    (and
     (= 3 (count a))
     (if (sequential? l)
       (tree l)
       (nil? l))
     (if (sequential? r)
       (tree r)
       (nil? r)))))

(defcheck solution-67f5384c
  (fn isbin [tree] (if (or (seq? tree) (vector? tree) (= tree nil)) (let [balanced (= 3 (count tree)) leaf (= tree nil)] (do (if-not (or balanced leaf) false (if leaf true (and (isbin (nth tree 1)) (isbin (nth tree 2))))))) false)))

(defcheck solution-684ddf62
  (fn this [s]
    (true?
      (when (and (counted? s) (= 3 (count s)))
        (let [[v left right] s]
          (and
           (or (nil? left) (this left))
           (or (nil? right) (this right))))))))

(defcheck solution-68bbd9cb
  (fn binary-tree? [s]
    (or (nil? s)
        (and (sequential? s)
             (= (count s) 3)
             (not (nil? (first s)))
             (every? binary-tree? (rest s))))))

(defcheck solution-68eb32bb
  (fn is-tree? [v]
    (or (nil? v)
        (and (coll? v) (= 3 (count v)) (not (coll? (first v))) (is-tree? (second v)) (is-tree? (last v))))))

(defcheck solution-69126dde
  (fn* tree? [x]
    (if (coll? x)
      (and (= (count x) 3) (tree? (second x)) (tree? (last x)))
      (nil? x))))

(defcheck solution-69cc0689
  (fn tree? [n]
    (or (nil? n)
        (and (sequential? n)
             (= (count n) 3)
             (let [[a b c] n]
               (and
                (not (sequential? a))
                (tree? b)
                (tree? c)))))))

(defcheck solution-69df0486
  (fn binary-tree? [t]
    (cond
      (= nil t) true
      (and (sequential? t) (= 3 (count t))) (and (binary-tree? (first (rest t))) (binary-tree? (fnext (rest t))))
      :else false)))

(defcheck solution-69fc5d6e
  (fn bt? [n]
    (or
     (nil? n)
     (and (coll? n)
          (= (count n) 3)
          (bt? (second n))
          (bt? (nth n 2))))))

(defcheck solution-6a2db548
  (fn tree? [l]
    (if (nil? l)
      true
      (and (coll? l)
           (= 3 (count l))
           (tree? (second l))
           (tree? (last l))
           true))))

(defcheck solution-6a3061fc
  (fn binary? [t]
    (or (nil? t)
        (and (sequential? t)
             (= (count t) 3)
             (every? binary? (rest t))))))

(defcheck solution-6a3d2687
  (fn tree? [s]
    (or (nil? s)
        (and (coll? s)
             (= (count s) 3)
             (every? tree? (rest s))))))

(defcheck solution-6b7b4d3f
  (fn tree? [input]
    (if (= input nil) true
                      (cond (or (false? input)
                                (not (= 3 (count input)))) false
                            :else (let [[rt ln rn] input]
                                    (and (not (seq? rt))
                                         (tree? ln)
                                         (tree? rn)))))))

(defcheck solution-6b8d9012
  (fn binary? [tree]
    (cond
      (nil? tree) true
      (or (not (sequential? tree)) (not= (count tree) 3)) false
      :else
      (let [[v l r] tree]
        (boolean (and (not (sequential? v))
                      (binary? l)
                      (binary? r)))))))

(defcheck solution-6bc82665
  (fn is-tree? [s]
    (if (coll? s)
      (and (= 3 (count s))
           (reduce #(and % %2) (map #(is-tree? %) s)))
      (not= false s))))

(defcheck solution-6c15cc2c
  (fn tree? [s]
    (and (= 3 (count s)) (not= false (second s)) (every? true? (map tree? (filter sequential? s))))))

(defcheck solution-6c7017b1
  (fn binary-tree? [tr]
    (cond (coll? tr) (let [[nd l r & rst] tr
                           len (count tr)]
                       (and (= 3 len)
                            (binary-tree? l)
                            (binary-tree? r)))
          (nil? tr) true
          :else false)))

(defcheck solution-6c841219
  (fn tree? [t]
    (cond
      (nil? t) true
      (false? t) false
      (seq t) (and (= 3 (count t))
                   (tree? (second t))
                   (tree? (nth t 2)))
      :else false)))

(defcheck solution-6d0a7339
  (fn is-a-binary-tree
    [s]
    (if (nil? s) true
                 (if (not (coll? s))
                   false
                   (if (not (= (count s) 3))
                     false
                     (reduce #(and %1 %2) (map #(is-a-binary-tree %) (concat (take-nth 3 (rest s)) (take-nth 3 (rest (rest s)))))))))))

(defcheck solution-6d17c575
  (fn tree? [l]
    (if (and (not (seq? l)) (not (vector? l)))
      (not= false l)
      (if (= (count l) 3)
        (and (not (seq? (first l))) (tree? (second l)) (tree? (nth l 2)))
        false))))

(defcheck solution-6d54d40d
  (fn tree? [node]
    (let [lchild (fn [p] (first (rest p)))
          rchild (fn [p] (first (rest (rest p))))]
      (or (nil? node)
          (and
           (coll? node)
           (= (count node) 3)
           (tree? (lchild node))
           (tree? (rchild node)))))))

(defcheck solution-6d81ba61
  (fn check
    [[v l r :as a]]
    (and (= 3 (count a))
         (or (and (sequential? l) (check l)) (nil? l))
         (or (and (sequential? r) (check r)) (nil? r)))))

(defcheck solution-6d8beac0
  (fn f [l]
    (or (nil? l)
        (and (coll? l)
             (= (count l) 3)
             (every? f (rest l))))))

(defcheck solution-6da1445e
  (fn is-binary-tree? [[value left right :as all]] (and (= 3 (count all)) (or (nil? left) (and (sequential? left) (is-binary-tree? left))) (or (nil? right) (and (sequential? right) (is-binary-tree? right))))))

(defcheck solution-6dd103c1
  (fn binary-tree? [tree]
    (let [seq-branch? (fn [node]
                        (if (and (sequential? node)
                                 (= (count node) 3))
                          true
                          false))
          result-seqs (tree-seq seq-branch? rest tree)
          seq1        (filter sequential? result-seqs)
          seq2        (filter (complement sequential?) result-seqs)]
      (and
       (every? #(= 3 (count %)) seq1)
       (every? nil? seq2)))))

(defcheck solution-6deb9266
  (fn bt? [s]
    (cond
      (nil? s) true
      (sequential? s)
      (and
       (= 3 (count s))
       (bt? (second s))
       (bt? (last s)))
      :else false)))

(defcheck solution-6e0e8204
  (fn [coll]
    (let [branch? #(or (nil? %) (coll? %))]
      (every? #(and (= 3 (count %)) (branch? (nth % 1)) (branch? (nth % 2)))
        (filter coll? (tree-seq coll? identity coll))))))

(defcheck solution-6e2002c1
  (fn tree? [s]
    (if (nil? s)
      true
      (and (sequential? s)
           (= (count s) 3)
           (tree? (nth s 1))
           (tree? (nth s 2))))))

(defcheck solution-6e2394e9
  (fn tree-p [coll]
    (or (nil? coll)
        (letfn [(check-node [coll]
                  (and (coll? coll)
                       (= 3 (count coll))
                       (or (nil? (nth coll 1)) (check-node (nth coll 1)))
                       (or (nil? (nth coll 2)) (check-node (nth coll 2)))))]
          (check-node coll)))))

(defcheck solution-6e44bfdc
  (fn node? [c]
    (or (nil? c)
        (and
         (sequential? c)
         (= 3 (count c))
         (every? node? (rest c))
         ))))

(defcheck solution-6e96f3fe
  (fn is-tree? [s]
    (or (nil? s) (number? s) (keyword? s)
        (and (coll? s) (= (count s) 3)
             (if-let [[v l r] s]
               (and (is-tree? v) (is-tree? r) (is-tree? l)))))))

(defcheck solution-6ed6f968
  (fn tree? [tree]
    (cond
      (nil? tree) true
      (and (sequential? tree)
           (= (count tree) 3))
      (let [[v l r] tree]
        (and (tree? l)
             (tree? r)))
      :else false)))

(defcheck solution-6ef74b09
  (fn bt [t] (if (sequential? t) (and (= (count t) 3) (not (sequential? (nth t 0))) (bt (nth t 1)) (bt (nth t 2))) (not (false? t)))))

(defcheck solution-6f18cc3a
  (fn func [node]
    (if (coll? node)
      (and (= (count node) 3) (= [true true] (map func (rest node))))
      (= node nil))))

(defcheck solution-6f74157c
  (fn bin-tree? [coll]
    (and (= 3 (count coll))
         (let [[x a b & coll] coll]
           (and (not (= a false))                           ;;ad-hoc work-around
                (not (coll? x))
                (or (not (coll? a))
                    (bin-tree? a))
                (or (not (coll? b))
                    (bin-tree? b)))))))

(defcheck solution-6ff51e79
  (fn bt? [t]
    (or (nil? t)
        (and (coll? t)
             (= (count t) 3)
             (bt? (nth t 1))
             (bt? (nth t 2))))))

(defcheck solution-6ff8af06
  (fn f [t]
    (boolean (or (nil? t)
                 (when-let [[v t1 t2] t]
                   (and (= 3 (count t)) (f t1) (f t2)))))))

(defcheck solution-702edcc4
  (fn tree [x]
    (if (nil? x) true
                 (if (coll? x)
                   (if (not (= 3 (count x))) false
                                             (let [[l r] [(fnext x) (first (nnext x))]]
                                               (and (tree l) (tree r))))
                   false))))

(defcheck solution-702ffc27
  (fn tree? [t]
    (cond
      (nil? t) true
      (not (coll? t)) false
      :else (and (= (count t) 3)
                 (tree? (second t))
                 (tree? (last t))))))

(defcheck solution-706056e2
  (fn balanced? [coll]
    (let [triple? (fn t [coll]
                    (if (coll? coll)
                      [(= 3 (count coll)) (map t (rest coll))]
                      (nil? coll)))]
      (= #{true} (set (flatten (triple? coll)))))))

(defcheck solution-70c95cc7
  (fn binary-tree? [seq]
    (or (nil? seq)
        (and (sequential? seq)
             (= (count seq) 3)
             (binary-tree? (second seq))
             (binary-tree? (last seq))))))

(defcheck solution-71aa3e
  (fn t? [x]
    (or (nil? x)
        (and (coll? x) (= 3 (count x)) (t? (second x)) (t? (last x))))))

(defcheck solution-7204cfc4
  (fn binary-tree? [tree]
    (letfn [(tree-nodes [tree]
              (filter sequential?
                (tree-seq sequential? identity tree)))
            (binary-tree-node? [[first & rest :as node]]
              (and (= (count node) 3)
                   (every? #(or (nil? %) (sequential? %)) rest)
                   (#(or (number? %) (keyword? %)) first)))]
      (->> (tree-nodes tree)
        (map binary-tree-node?)
        (every? true?)))))

(defcheck solution-72339efa
  (fn bt? [sq]
    (if (nil? sq)
      true
      (and (sequential? sq)
           (= (count sq) 3)
           (every? bt? (rest sq))))))

(defcheck solution-724e1c03
  (fn [tree]
    (letfn [(is-tree [tr]
              (if (nil? tr)
                true
                (and (sequential? tr) (= 3 (count tr)) (is-tree (second tr)) (is-tree (second (rest tr))))))]
      (is-tree tree))

    ))

(defcheck solution-72a73705
  (fn tree [l]
    (if (coll? l)
      (and
       (= (count l) 3)
       (every? tree l)
       (or (coll? (second l)) (nil? (second l)))
       (or (coll? (last l)) (nil? (last l))))
      true)))

(defcheck solution-733c748
  (fn tree? [t]
    (or
     (nil? t)
     (and (coll? t) (= 3 (count t)) (every? tree? (rest t))))))

(defcheck solution-733dba2a
  (fn chk [n]
    (if (some true? (map #(% n) [list? seq? vector?]))
      (and (= (count n) 3) (chk (nth n 1)) (chk (nth n 2)))
      (nil? n))
    ))

(defcheck solution-7391d5cf
  (fn bt? [s]
    (and (coll? s)
         (= (count s) 3)
         (every? (some-fn nil? (and coll? bt?)) (rest s)))))

(defcheck solution-73e5cbfd
  (fn tree? [xs]
    (letfn [(leaf? [x] (not (or (coll? x) (false? x))))
            (leaf-or-tree? [x] (or (leaf? x) (tree? x)))]
      (and
       (coll? xs)
       (= 3 (count xs))
       (leaf-or-tree? (second xs))
       (leaf-or-tree? (last xs))
       ))))

(defcheck solution-74272f52
  (fn isTree [xs]
    (cond
      (sequential? xs) (and (= (count xs) 3) ((complement sequential?) (first xs)) (isTree (second xs)) (isTree (nth xs 2)))
      (nil? xs) true
      :else false)))

(defcheck solution-7461e621
  (fn binary? [s]
    (and
     (sequential? s)
     (= (count s) 3)
     (let [left  (second s)
           right (last s)]
       (and
        (or
         (nil? left)
         (binary? left))
        (or
         (nil? right)
         (binary? right)))))))

(defcheck solution-756e35ce
  (fn binary-tree? [t]
    (or (nil? t)
        (and
         (sequential? t)
         (= (count t) 3)
         (not (sequential? (first t)))
         (every? binary-tree? (rest t))))))

(defcheck solution-75b028a8
  (fn tree? [x]
    (or (nil? x) (and (sequential? x)
                      (= 3 (count x))
                      (not (sequential? (first x)))
                      (tree? (second x))
                      (tree? (last x))))))

(defcheck solution-75d43eab
  (fn t [s] (cond
              (nil? s) true
              (not (sequential? s)) false
              :else (let [v (first s) l (fnext s) r (last s)]
                      (and (= 3 (count s))
                           (not (sequential? v))
                           (t l) (t r))))))

(defcheck solution-76ae09cf
  (fn binary? [coll]
    (if ((comp not coll?) coll)
      (nil? coll)
      (let [[_ left right] coll]
        (and (= (count coll) 3) (binary? left) (binary? right))))))

(defcheck solution-76d3f035
  (fn chk [x]
    (cond
      (coll? x) (and
                 (every? chk (rest x))
                 (= (count x) 3))
      (nil? x) true
      :else false)))

(defcheck solution-76eb7154
  (fn b [[_ l r :as n]]
    (if (and n (= 3 (count n)))
      (and (if (nil? l) true (if l (b l) false))
           (if (nil? r) true (if r (b r) false)))
      false)))

(defcheck solution-77163aac
  (fn tree? [t]
    (or (nil? t)
        (and
         (counted? t)
         (= 3 (count t))
         (let [[v l r] t]
           (and (tree? l) (tree? r)))))))

(defcheck solution-772895c8
  (fn f [t]
    (if (nil? t) true
                 (if (and (coll? t) (= (count t) 3))
                   (let [[_ b c] t] (and (f b) (f c))) false))))

(defcheck solution-772d4ccf
  (fn is-tree [s]
    (or (nil? s)
        (and (coll? s)
             (= 3 (count s))
             (not (coll? (first s)))
             (is-tree (second s))
             (is-tree (last s))))))

(defcheck solution-774ccc63
  (fn check-vlr [[v l r :as t]]
    (cond (not= (count t) 3) false
          (and (not= nil v) (nil? l) (nil? v)) true
          (not (every? #(or (nil? %) (coll? %)) [l r])) false
          (some #(and (coll? %) (empty? %)) [l r]) false
          :else (every? true?
                  (map #(if (coll? %) (check-vlr %) true)
                    [l r])))))

(defcheck solution-777bb530
  (fn f [n]
    (or (nil? n)
        (and (coll? n)
             (= (count n) 3)
             (every? f (next n))))))

(defcheck solution-779876f3
  (fn istree [t]
    (if (sequential? t)
      (and (= 3 (count (seq t)))
           (istree (nth t 1))
           (istree (nth t 2)))
      (= nil t))))

(defcheck solution-780336b0
  (fn tree? [tree]
    (cond
      (nil? tree) true
      (not (coll? tree)) false
      (not= 3 (count tree)) false
      :else (and (tree? (second tree)) (tree? (last tree))))))

(defcheck solution-7875b419
  (fn tree? [xs]
    (if (nil? xs)
      true
      (if (coll? xs)
        (if (= (count xs) 3)
          (and (tree? (nth xs 1)) (tree? (nth xs 2)))
          false)
        false))))

(defcheck solution-788fff46
  (fn tree? [seq]
    (if (= seq nil)
      true
      (if (= seq ())
        false
        (and (coll? seq)
             (or (empty? seq)
                 (and (= (count seq) 3)
                      (tree? (second seq))
                      (tree? (last seq)))))))))

(defcheck solution-78a960a9
  (fn f [a]
    (if (coll? a)
      (and (= 3 (count a)) (f (nth a 1)) (f (nth a 2)))
      (not (false? a)))))

(defcheck solution-78df63d5
  (fn tree? [coll]
    (cond
      (or (seq? coll) (vector? coll))
      (and (= 3 (count coll)) (tree? (nth coll 1)) (tree? (nth coll 2)))
      (nil? coll) true
      :else false)))

(defcheck solution-78f6d44b
  #(odd? (count
           (remove false? (flatten %)))))

(defcheck solution-78facba9
  (fn tree? [t]
    (cond (nil? t) true
          (not (coll? t)) false
          (not (= (count t) 3)) false
          :else (and (tree? (nth t 1)) (tree? (nth t 2))))))

(defcheck solution-79103826
  (fn tree? [l]
    (cond
      (nil? l) true
      (coll? l)
      (and
       (= 3 (count l))
       ((complement coll?) (first l))
       (tree? (second l))
       (tree? (last l)))
      :else false)))

(defcheck solution-791bc6eb
  (fn checker

    [arg]

    (let [args (into [] arg)]

      (cond

        (= 3 (count args))

        (let [[node-name lchild rchild] args]
          (cond
            (and (coll? lchild) (coll? rchild)) (and (checker rchild) (checker lchild))
            (and (nil? lchild) (coll? rchild)) (checker rchild)
            (and (nil? rchild) (coll? lchild)) (checker lchild)
            (and (nil? lchild) (nil? rchild)) true
            :else false
            )
          )

        :else false
        )
      )
    ))

(defcheck solution-7925036a
  (fn my-tree? [xs]
    (if (not (coll? xs))
      (if (false? xs) false true)
      (if (not (= (count xs) 3))
        false
        (every? identity (map my-tree? xs)))))

  )

(defcheck solution-79281778
  (fn tree? [node]
    (let [third #(first (rest (rest %)))]
      (if (nil? node)
        true
        (if (and (coll? node) (= (count node) 3))
          (and (tree? (second node)) (tree? (third node)))
          false)))))

(defcheck solution-795678cd
  #(if (or (= (some #{()} (flatten %)) '()) (some false? (flatten %)))
     false
     (odd? (count (flatten %)))))

(defcheck solution-79658011
  (fn _ [n]
    (or (nil? n)
        (and (coll? n)
             (= 3 (count n))
             (let [[n' n1 n2] n]
               (and
                (_ n1)
                (_ n2)))))))

(defcheck solution-7a12e17
  (fn correct [xs]
    (or
     (not (sequential? xs))
     (and
      (=
        (count (filter #(or (nil? %) %) xs))
        3)
      (every? identity (map correct xs))))))

(defcheck solution-7aa4af60
  (fn btree [s]
    (every? true?
      (cons (= 3 (count s))
        (map #(if (coll? %) (btree %) (not (false? %))) s)))))

(defcheck solution-7abef030
  (fn node?
    [node]
    (or (nil? node)
        (and
         (coll? node)
         (= 3 (count node))
         (-> node first nil? not)
         (-> node second node?)
         (-> node last node?)))))

(defcheck solution-7aeba579
  (fn tree? [x]
    (cond
      (nil? x) true
      (not (coll? x)) false
      (not= (count x) 3) false
      :else (and (tree? (second x))
                 (tree? (second (next x)))
                 (not (coll? (first x)))
                 )
      )
    ))

(defcheck solution-7bb2e18e
  (fn [x]
    (let [t (tree-seq #(and (coll? %) (= (count %) 3)) identity x)
          ]
      (if (some #(= false %) t) false
                                (not (coll? (last t)))))))

(defcheck solution-7bc4b3a2
  (fn bintree [s]
    (or (nil? s)
        (and (coll? s)
             (= 3 (count s))
             (every? bintree (rest s))))))

(defcheck solution-7c7ae9e2
  (fn valid [n]
    (if (nil? n)
      true
      (if (coll? n)
        (if (= (count n) 3)
          (and (valid (nth n 1)) (valid (nth n 2)))
          false)
        false))))

(defcheck solution-7caab606
  (fn bt? [n]
    (or (nil? n)
        (and (coll? n)
             (= 3 (count n))
             (every? bt? (rest n))))))

(defcheck solution-7cd564e2
  (fn [t]
    (letfn [(isnode [node]
              (or (nil? node)
                  (and (coll? node)
                       (= (count node) 3)
                       (isnode (second node))
                       (isnode (last node))
                       )))]
      (isnode t))))

(defcheck solution-7cee2ef7
  (fn bin-tree? [t]
    (if (nil? t)
      true
      (and (coll? t)
           (= (count t) 3)
           (bin-tree? (second t))
           (bin-tree? (nth t 2))))))

(defcheck solution-7d010e4a
  (fn binary? [node]
    (if-not (coll? node)
      (nil? node)
      (let [ct (count node)]
        (if (and (not= ct 1) (not= ct 3))
          false
          (let [lhs (second node)
                rhs (last node)]
            (and (binary? lhs) (binary? rhs))))))))

(defcheck solution-7d4d7e02
  (fn tree? [t]
    (cond
      (not (sequential? t))
      false

      (not (= 3 (count t)))
      false

      :else
      (let [[item left right] t]
        (cond (and (nil? left)
                   (nil? right))
              true

              (nil? left)
              (tree? right)

              (nil? right)
              (tree? left)

              :else
              (and (tree? left)
                   (tree? right)))))))

(defcheck solution-7d5ec260
  (fn c95
    [t]
    (if (coll? t)
      (if (not= (count t) 3)
        false
        (and (if (coll? (first t))
               (c95 (first t))
               true)
             (c95 (second t))
             (c95 (last t))))
      (if (nil? t)
        true
        false))))

(defcheck solution-7e84c3dd
  (fn tree? [tr]
    (or (nil? tr)
        (and (coll? tr)
             (= 3 (count tr))
             (every? tree? (rest tr))))))

(defcheck solution-7eb6ec8e
  (fn t [l]
    (or (nil? l)
        (and (sequential? l)
             (= (count l) 3)
             (t (second l))
             (t (first (nnext l)))))))

(defcheck solution-7ebaf2e5
  (fn valid? [t] (let [valid-node (fn [i] (or (nil? i) (coll? i)))] (or (nil? t) (and (not (nil? (first t))) (= 3 (count t)) (valid-node (nth t 1)) (valid-node (nth t 2)) (valid? (nth t 1)) (valid? (nth t 2)))))))

(defcheck solution-7f35316d
  (fn btree? [s]
    (or (= s nil)
        (and (sequential? s)
             (= (count s) 3)
             (btree? (nth s 1))
             (btree? (nth s 2))))))

(defcheck solution-7f4e0c66
  (fn binary-tree? [t]
    (if-not (and (sequential? t) (= (count t) 3))
      false
      (let [[v l r] t
            valid-child? (fn [c] (or (nil? c) (binary-tree? c)))]
        (and (valid-child? l)
             (valid-child? r))))))

(defcheck solution-7f50fe54
  (fn tree? [x]
    (or
     (nil? x)
     (and
      (sequential? x)
      (= 3 (count x))
      (not (sequential? (first x)))
      (tree? (second x))
      (tree? (nth x 2))
      ))))

(defcheck solution-7f552fe7
  (fn c? [t]
    (or
     (nil? t)
     (and
      (coll? t)
      (= 3 (count t))
      (every? c? (rest t))))))

(defcheck solution-7f55760b
  (fn check-node [node]
    (cond
      (nil? node) true
      (and (coll? node)
           (= (count node) 3)
           (check-node (first (next node)))
           (check-node (first (nnext node)))) true
      :else false
      )
    ))

(defcheck solution-7f6b7d6d
  (fn tree? [coll]
    (if (not= (count coll) 3) false
                              (let [left  (nth coll 1)
                                    right (nth coll 2)]
                                (cond
                                  (coll? (first coll)) false
                                  (and (coll? left) (coll? right)) (and (tree? left)
                                                                        (tree? right))
                                  (and (not (coll? left)) (not (coll? right))) (and (nil? left)
                                                                                    (nil? right))
                                  (coll? left) (and (tree? left) (nil? right))
                                  (coll? right) (and (tree? right) (nil? left))
                                  :else true)))))

(defcheck solution-7fc6275a
  (fn [tree]
    (letfn [(binary-tree? [tree]
              (cond
                (nil? tree) true
                (not (sequential? tree)) false
                (empty? tree) false
                :else (let [[key right left] tree]
                        (if
                         (and (= 3 (count tree))
                              (not (nil? key))
                              (binary-tree? right)
                              (binary-tree? left))
                          true
                          false))))]
      (binary-tree? tree))))

(defcheck solution-80072f15
  (fn tree [coll] (if (not (coll? coll))
                    (nil? coll)
                    (case (count coll)
                      1 (nil? (first coll))
                      3 (and (not (nil? (first coll))) (tree (second coll)) (tree (last coll)))
                      false))))

(defcheck solution-8019f3b
  (fn tree? [coll]
    (cond
      (coll? coll)
      (and (= 3 (count coll)) (tree? (nth coll 1)) (tree? (nth coll 2)))
      (nil? coll) true
      :else false)))

(defcheck solution-808421a6
  (fn binary-tree? [l]
    (if (nil? l)
      true
      (if (and (sequential? l) (= (count l) 3))
        (let [v    (first l) r (rest l)
              left (first r) right (last r)]
          (and (binary-tree? left) (binary-tree? right)))
        false))))

(defcheck solution-808bd8f9
  (fn _ [c]
    (and
     (= (count c) 3)
     (if (sequential? (second c)) (_ (second c)) (nil? (second c)))
     (if (sequential? (last c)) (_ (last c)) (nil? (last c))))))

(defcheck solution-80b32979
  (fn tree? [t]
    (if (nil? t)
      true
      (if (and (sequential? t) (= 3 (count t)))
        (and (tree? (nth t 1)) (tree? (nth t 2)))
        false
        ))))

(defcheck solution-80c82b26
  (fn t [x]
    (or (nil? x) (and (sequential? x) (= 3 (count x)) (t (second x)) (t (nth x 2))) false)))

(defcheck solution-80ca6403
  #(->> (tree-seq coll? identity %)
     (filter coll?)
     (every? (fn [[a b c :as s]] (and (= 3 (count s)) (not (coll? a))
                                      (or (coll? b) (nil? b)) (or (coll? c) (nil? c)))))))

(defcheck solution-812ecace
  (fn is-tree? [[v l r :as nd]]
    (and
     (sequential? nd)
     (= 3 (count nd))
     (if (sequential? l) (is-tree? l) (or (nil? l) l))
     (if (sequential? r) (is-tree? r) (or (nil? r) r)))))

(defcheck solution-816c4e6e
  (fn [t]
    (empty?
      (keep #(if (and (coll? %) (= 3 (count %)))
               nil
               %)
        (tree-seq coll? next t)))))

(defcheck solution-819ea112
  (fn t [s] (and (= 3 (count s))
                 (every? #(or (nil? %)
                              (and (coll? %)
                                   (t %)))
                   (rest s)))))

(defcheck solution-81a3aa9f
  (fn t [c] (cond (nil? c) true (not (sequential? c)) false (not= 3 (count c)) false (not (t (nth c 1))) false (not (t (nth c 2))) false :else true)))

(defcheck solution-81c5897b
  (fn istree? [t]
    (cond
      (nil? t) true
      (not (sequential? t)) false
      (= 3 (count t))
      (let [a (first t) l (second t) r (nth t 2)]
        (and (not (seq? a)) (istree? l) (istree? r))
        )
      :else false
      )
    ))

(defcheck solution-81f1e8f3
  (fn [node]
    (let [recp
          (fn tree [node]
            (if (sequential? node)
              (if (= (count node) 3)
                (cons true (lazy-seq (concat (tree (nth node 1)) (tree (nth node 2)))))
                (list false))
              (list (nil? node))))]
      (every? identity (recp node)))))

(defcheck solution-82167ba7
  (fn bintree [s]
    (if (nil? s)
      true
      (if (and (coll? s) (= 3 (count s)))
        (every? bintree (rest s))
        false))))

(defcheck solution-826fc291
  (fn bin
    [n]
    (let [items (->> (flatten n)
                  count)
          nils  (->> (flatten n)
                  (filter nil?)
                  count)]
      (and (odd? items) (or (= nils 2) (and (odd? nils) (> nils 1)))))))

(defcheck solution-827eca8e
  (fn binary-tree? [tree]
    (if (or (vector? tree) (seq? tree))
      (and
       (= 3 (count tree))
       (not (seq? (first tree)))
       (every? binary-tree? (rest tree)))
      (nil? tree))))

(defcheck solution-82b5e1f6
  (fn istree? [xs]
    (if (= 3 (count xs))
      (let [[a b c] xs]
        (and ((complement seq?) a)
             (if (coll? b) (istree? b)
                           (nil? b))
             (if (coll? c) (istree? c)
                           (nil? c))))
      false)))

(defcheck solution-82e5938b
  (fn binary? [tree]
    (if (coll? tree)
      (and (= 3 (count tree))
           (every? binary? (rest tree)))
      (nil? tree))))

(defcheck solution-8319f80c
  (fn ! [coll]
    (if (nil? coll)
      true
      (if (not (or (list? coll) (vector? coll)))
        false
        (let [root (first coll) children (rest coll)]
          (if (or (nil? root) (not (= (count children) 2)))
            false
            (and (! (first children))
                 (! (second children)))))))))

(defcheck solution-8326f79b
  (fn [z] (letfn [(parse [acc l] (if (and (= 3 (count l)) (not (or (nil? (first l)) (sequential? (first l)))))
                                   (if (sequential? (second l))
                                     (and (parse acc (second l)) (if (sequential? (last l)) (parse acc (last l))
                                                                                            (if (nil? (last l)) true false)))
                                     (if (nil? (second l))
                                       (if (sequential? (last l)) (parse acc (last l))
                                                                  (if (nil? (last l)) true false))
                                       false))
                                   false))]
            (parse true z))))

(defcheck solution-83285dd2
  (letfn [(binary-tree? [t]
            (and (= (count t) 3)
                 (every? node? (rest t))))
          (node? [x]
            (or (nil? x)
                (and (coll? x)
                     (binary-tree? x))))]
    binary-tree?))

(defcheck solution-833fc356
  (fn t [xs]
    (cond
      (nil? xs) true
      (not (coll? xs)) false
      (not= 3 (count xs)) false
      :else (and (t (nth xs 1)) (t (nth xs 2))))))

(defcheck solution-8356101e
  (fn bt? [s]
    (or
     (nil? s)
     (and
      (coll? s)
      (= 3 (count s))
      (bt? (nth s 1))
      (bt? (nth s 2))))))

(defcheck solution-835a6eb9
  (fn [t] (every?
            (fn [c] (and (= (count c) 3)
                         (every? #(or (nil? %) (coll? %)) (rest c))))
            (filter coll? (tree-seq coll? rest t))
            )))

(defcheck solution-835dc5e3
  (fn [col]
    (let [check (fn f [col]
                  (if (coll? col)
                    (if (= 3 (count col))
                      (and (f (second col)) (f (second (rest col))))
                      false)
                    (if (and (not (nil? col)) (false? col))
                      false
                      true)))]
      (if (check col)
        (and (check (second col)) (check (second (rest col))))
        false))))

(defcheck solution-83c8c8eb
  (fn tree? [v]
    (or (nil? v)
        (and (coll? v)
             (= 3 (count v))
             (tree? (second v))
             (tree? (last v))))))

(defcheck solution-83dd1fad
  (fn [coll] (odd? (count (remove false? (flatten coll))))))

(defcheck solution-849b78d8
  (fn f [t] (and (coll? t) (= (count t) 3) (every? #(or (nil? %) (f %)) (rest t)))))

(defcheck solution-84cbb7a5
  (fn f [s]
    (and (sequential? s)
         (= (count s) 3)
         (or (nil? (second s)) (f (second s)))
         (or (nil? (nth s 2)) (f (nth s 2))))))

(defcheck solution-84dae13c
  (fn t [x]
    (or (nil? x)
        (and (sequential? x)
             (= 3 (count x)) (every? t (next x))
             ))))

(defcheck solution-8544738e
  (fn binary-tree? [tree]
    (or (nil? tree)
        (and (coll? tree)
             (= 3 (count tree))
             (binary-tree? (second tree))
             (binary-tree? (last tree))))))

(defcheck solution-859943fa
  (fn tree? [t]
    (or (nil? t)
        (and (sequential? t)
             (= (count t) 3)
             (tree? (second t))
             (tree? (last t))))))

(defcheck solution-85f8b9e6
  (fn check-node [[value & children]]
    (and
     ((complement nil?) value)
     (= (count children) 2)
     (every? identity
       (map
         #(cond (nil? %) true
                (coll? %) (check-node %)
                :else false)
         children)))))

(defcheck solution-8601ead4
  (fn traverse [tree]
    (if
     (coll? tree)
      (if
       (= 3 (count tree))
        (let
         [[value left right] tree]
          (and
           (or (keyword? value) (number? value))
           (if (nil? left) true (if (coll? left) (traverse left) false))
           (if (nil? right) true (if (coll? right) (traverse right) false))
           )
          )
        false
        )
      false
      )
    ))

(defcheck solution-86069dd8
  (fn tree? [t]
    (if (= 3 (count t))
      (every? #(or (nil? %) (and (sequential? %) (tree? %))) (rest t))
      false)))

(defcheck solution-863ef1d0
  (fn self [col]
    (or (nil? col)
        (and (sequential? col)
             (= (count col) 3)
             (every? self (rest col))))))

(defcheck solution-86888132
  (fn __ [se]
    (or
     (nil? se)
     (and
      (sequential? se)
      (= (count se) 3)
      (boolean (first se))
      (__ (nth se 1))
      (__ (nth se 2))))))

(defcheck solution-87122964
  (fn b-tree? [tree]
    (and
     (sequential? tree)
     (= (count tree) 3)
     (or (nil? (nth tree 1)) (b-tree? (nth tree 1)))
     (or (nil? (nth tree 2)) (b-tree? (nth tree 2))))))

(defcheck solution-88217026
  (fn bintree? [node]
    (or (nil? node)
        (and (coll? node)
             (let [[value & more] node]
               (and (= 2 (count more))
                    (every? bintree? more)
                    )
               )
             )
        )
    ))

(defcheck solution-88bc4d66
  (fn tree? [x]
    (or
     (nil? x)
     (and
      (sequential? x)
      (= 3 (count x))
      (tree? (second x))
      (tree? (last x))))))

(defcheck solution-893feb85
  (fn btree? [col] (and (coll? col) (= (count col) 3) (or (nil? (second col)) (btree? (second col))) (or (nil? (last col)) (btree? (last col))))
    ))

(defcheck solution-89fcdbb8
  (fn isTree? [coll]
    (or (nil? coll) (and (= (count coll) 3)
                         (or (nil? (second coll))
                             (coll? (second coll)))
                         (or (nil? (last coll))
                             (coll? (last coll)))
                         (isTree? (second coll))
                         (isTree? (last coll))))))

(defcheck solution-8ac588c2
  (fn mbinarytree [tr]
    (if (or ((complement sequential?) tr) (not= 3 (count tr)))
      false
      (let [s (second tr)
            l (last tr)]
        (cond
          (= nil s l) true
          (and (nil? s) (sequential? l)) (recur l)
          (and (nil? l) (sequential? s)) (recur s)
          (and (sequential? l) (sequential? s)) (and (mbinarytree l) (mbinarytree s))
          :d false)))))

(defcheck solution-8ac78c1d
  (fn tree? [t]
    (or (nil? t)
        (and (coll? t)
             (= (count t) 3)
             (let [[root l-child r-child] t]
               (and (not (coll? root))
                    (tree? l-child)
                    (tree? r-child)))))))

(defcheck solution-8afae184
  (fn tree? [t]
    (cond (nil? t) true
          (and (coll? t) (not= 3 (count t))) false
          (not (coll? t)) false
          :else (let [[n r l] t]
                  (and (not (coll? n))
                       (tree? l)
                       (tree? r))))))

(defcheck solution-8b1eb99a
  (fn [root-node]
    (letfn [(node? [node]
              (if (not (and (sequential? node)
                            (= 3 (count node))))
                false
                (let [child? (fn [val]
                               (or (nil? val)
                                   (node? val)))
                      left   (second node)
                      right  (last node)]
                  (and (child? left)
                       (child? right)))))]
      (node? root-node))))

(defcheck solution-8b6193fd
  (fn is-tree?
    [coll]
    (let [v           (vec coll)
          left        (get v 1)
          right       (get v 2)
          is-subtree? #(or (nil? %) (and (coll? %) (is-tree? %)))]
      (and (= (count v) 3)
           (is-subtree? left)
           (is-subtree? right)))))

(defcheck solution-8ba6a54f
  (fn tnode? [coll]
    (or (nil? coll)
        (and (coll? coll)
             (= 3 (count coll))
             (every? tnode? (next coll))))))

(defcheck solution-8bb7b4d3
  (fn tree? [x]
    (or (nil? x) (and (sequential? x) (= (count x) 3) (every? tree? (rest x))))))

(defcheck solution-8bbac098
  (fn p [col]
    (cond
      (nil? col) true
      (false? col) false
      (and (not (nil? col)) (empty? col)) false
      (and (not= (count col) 3)) false
      (or (vec col) (list? col)) (and (p (nth col 1))
                                      (p (nth col 2)))
      :else true)))

(defcheck solution-8be8c229
  (fn is_tree [t]
    (letfn [(rlist? [l] (or (list? l) (vector? l)))]
      (cond
        (and (rlist? t) (= 3 (count t)))
        (every? is_tree (next t))
        (nil? t) true
        :else
        false))))

(defcheck solution-8c42c072
  (fn ? [n]
    (or (nil? n)
        (and
         (coll? n)
         (= 3 (count n))
         (every? ? (rest n))))))

(defcheck solution-8ceb571b
  (fn btree? [coll]
    (if-let [[root left right] coll]
      (if (= 3 (count coll))
        (and (btree? left) (btree? right))
        false)
      (nil? coll))))

(defcheck solution-8d0c553b
  (fn tree? [xs]
    (or (nil? xs)
        (and
         (coll? xs)
         (= 3 (count xs))
         (tree? (nth xs 1))
         (tree? (nth xs 2))))))

(defcheck solution-8e3f2187
  (fn is-tree? [s]
    (or
     (nil? s)
     (and
      (sequential? s)
      (= 3 (count s))
      ((complement sequential?) (first s))
      (or
       (every? nil? (rest s))
       (every? #(and (not (nil? %)) ((complement sequential?) %)) (rest s))
       (every? is-tree? (rest s)))))))

(defcheck solution-8ea699e
  #(letfn [(bt? [coll] (if (not (coll? coll)) ((complement false?) coll) (if (or (not= 3 (count coll)) (coll? (first coll))) false (and (bt? (first (next coll))) (bt? (last coll))))))] (bt? %)))

(defcheck solution-8ebacfec
  (letfn [(binary-tree? [t]
            (and (= (count t) 3)
                 (every? node? (rest t))))
          (node? [x]
            (or (nil? x)
                (and (coll? x)
                     (binary-tree? x))))]
    binary-tree?))

(defcheck solution-8ed177b3
  (let
   [applicable? (fn [l] (or (seq? l) (vector? l)))]
    (fn node-valid?
      ([a b c]
       ; (println [a b c])
       (cond
         (and (nil? b) (nil? c)) true
         (nil? b) (if (applicable? c) (apply node-valid? c) false)
         (nil? c) (if (applicable? b) (apply node-valid? b) false)
         (and (applicable? b) (applicable? c)) (and (apply node-valid? b) (apply node-valid? c))
         :else false))
      ([a]
       ; (println [a])
       (cond
         (nil? a) true
         (applicable? a) (apply node-valid? a)
         :else false))
      ([] false)
      ([_ _] false)
      ([_ _ _ & _] false)
      )
    ))

(defcheck solution-8ef6ec79
  (fn t? [n]
    (if (nil? n) true
                 (and (coll? n)
                      (= 3 (count n))
                      (let [[v a b] n]
                        (and (t? a) (t? b)))))))

(defcheck solution-8fefd422
  (fn bt [s]
    (and (coll? s)
         (= 3 (count s))
         (let [[a b c] s]
           (and (not (nil? a))
                (or (nil? b) (bt b))
                (or (nil? c) (bt c)))))))

(defcheck solution-902722e0
  (fn binarytree? [x]
    (if (sequential? x)
      (if (== (count x) 3)
        (if (sequential? (first x))
          false
          (and (binarytree? (first (rest x))) (binarytree? (last x))))
        false)
      (if (nil? x)
        true
        false))))

(defcheck solution-903f0c17
  (fn t [x]
    (if (sequential? x)
      (if (= (count x) 3)
        (and (t (second x))
             (t (last x)))
        false)
      (nil? x))))

(defcheck solution-907e53c
  (fn test-btree [[val l-chld r-chld :as node]]
    (cond
      (= node nil) true
      (or (not (or (= nil l-chld) (coll? l-chld)))
          (not (or (= nil r-chld) (coll? r-chld)))
          (= val nil)
          (not= (count node) 3))
      false
      :else (and (test-btree l-chld) (test-btree r-chld)))))

(defcheck solution-90ea68fa
  (fn f [[v l r :as all]]
    (and
     (= 3 (count all))
     (or (nil? l) (and (coll? l) (f l)))
     (or (nil? r) (and (coll? r) (f r))))))

(defcheck solution-91284ea3
  (fn tree? [col]
    (let [leaf? (fn [x] (nil? x))]
      (cond
        (not (coll? col)) false
        (not= 3 (count col)) false
        (and (not (coll? (first col)))
             (if (coll? (second col))
               (tree? (second col))
               (not= false (second col)))
             (if (coll? (last col))
               (tree? (last col))
               (not= false (last col)))) true
        :else false))))

(defcheck solution-914bc9c7
  (fn tree? [s]
    (if (sequential? s)
      (let [v (first s)
            l (second s)
            r (second (next s))]
        (and (= 3 (count s))
             v
             (or (nil? l) (tree? l))
             (or (nil? r) (tree? r))))
      false
      )))

(defcheck solution-9179652e
  (fn tree? [node]
    (if (coll? node)
      (and
       (= 3 (count node))
       (reduce #(and %1 (tree? %2)) true node))
      (not= node false))))

(defcheck solution-91824bcf
  (fn tree? [s]
    (cond
      (= s nil) true
      (not (sequential? s)) false
      (not= (count s) 3) false
      :else (and (not (sequential? (first s))) (every? identity (map tree? (rest s))))
      )
    ))

(defcheck solution-918d9334
  (fn t? [xs]
    (if (coll? xs)
      (and (= 3 (count xs))
           (let [[_ a b] xs]
             (and (t? a) (t? b))))
      ;; not sure why false isn't a valid value for a leaf,
      ;; but this is necessary to reject [1 [2 [3 [4 false nil] nil] nil] nil]
      ;; as not a tree
      (not (false? xs)))))

(defcheck solution-91a09e02
  (fn tree? [z]
    (if (sequential? z)
      (let [[n l r] z]
        (and (= 3 (count z))
             (tree? l)
             (tree? r)))
      (nil? z))))

(defcheck solution-91c0c2cc
  (fn b [n]
    (if (coll? n)
      (and
       (= 3 (count n))
       (b (first n))
       (b (second n))
       (b (nth n 2)))
      (not= false n))))

(defcheck solution-923ff517
  (fn [x]
    (letfn [(collnotempty [x] (and (coll? x) (not (empty? x))))
            (nil-coll-nonempty? [x] (or (nil? x) (collnotempty x)))
            (treelike [x] (and (= (count x) 3)
                               (nil-coll-nonempty? (nth x 1))
                               (nil-coll-nonempty? (nth x 2))))]
      (loop [curr x]
        (cond
          (and (treelike curr) (not (empty? curr))) (recur (first (filter collnotempty curr)))
          (empty? curr) true
          :else false)))))

(defcheck solution-9254852a
  (fn ibt [t]
    (if (and (coll? t) (= (count t) 3))
      (let [[v l r] t]
        (and (not (coll? v))
             (or (nil? l) (ibt l))
             (or (nil? r) (ibt r))
             ))

      false)
    ))

(defcheck solution-92f3bf68
  (fn ! [node]
    (cond (nil? node) true
          (or (not (coll? node))
              (not= 3 (count node))) false
          :else (and (! (second node))
                     (! (last node))))))

(defcheck solution-92f5b0a7
  (fn [t]
    (empty?
      (filter #(or (not= 3 (count %))
                   (not (every? (fn [x] (or (nil? x) (sequential? x))) (rest %1))))
        (tree-seq #(sequential? %)
          #(filter sequential? (rest %)) t)))))

(defcheck solution-931ff41
  (fn binary-tree? [c]
    (and
     (coll? c)
     (== 3 (count c))
     (not (nil? (first c)))
     (or (nil? (second c))
         (binary-tree? (second c)))
     (or (nil? (last c))
         (binary-tree? (last c))))))

(defcheck solution-936bcd1d
  (fn check-tree [a]
    (or (nil? a)
        (and (sequential? a)
             (= 3 (count a))
             (first a)
             (check-tree (second a))
             (check-tree (last a))))))

(defcheck solution-9411bfb8
  (fn binary-tree? [tree]
    (or (nil? tree)
        (and (sequential? tree)
             (= 3 (count tree))
             (every? binary-tree? (rest tree))))))

(defcheck solution-944841b
  (fn bt? [tree]
    (cond (not (coll? tree)) (nil? tree)
          (= 3 (count tree)) (and (bt? (second tree)) (bt? (last tree)))
          :else false)))

(defcheck solution-9450fcfe
  (fn bt? [sq]
    (let [k (nth sq 0 :nil)
          l (nth sq 1 :nil)
          r (nth sq 2 :nil)
          o (nth sq 3 :nil)]
      (cond
        (not= :nil o) false
        (some #(= :nil %) [k l r]) false
        :else (and
               (if (sequential? l) (bt? l) (nil? l))
               (if (sequential? r) (bt? r) (nil? r)))))))

(defcheck solution-9509f70e
  (fn f [x] (or (nil? x) (and
                          (coll? x)
                          (= (count x) 3)
                          (f (nth x 1))
                          (f (nth x 2))))))

(defcheck solution-95123af1
  (fn tree-check [lst]
    (if (not (coll? lst)) (nil? lst)
                          (if (or (not= (count lst) 3) (not (first lst))) false
                                                                          (and
                                                                           (tree-check (nth lst 1))
                                                                           (tree-check (nth lst 2)))))))

(defcheck solution-951acbbe
  (fn b-node? [s]
    (or
     (nil? s)
     (and
      (coll? s)
      (= 3 (count s))
      (every? b-node? (rest s))))))

(defcheck solution-95c2ceb2
  (fn bintree [s]
    (or (nil? s)
        (and (sequential? s)
             (= 3 (count s))
             (bintree (second s))
             (bintree (first (nnext s)))))))

(defcheck solution-95f34a9b
  (fn tree? [d]
    (if-not (coll? d)
      (nil? d)
      (and (= 3 (count d))
           (not (nil? (first d)))
           (tree? (nth d 1))
           (tree? (nth d 2))))))

(defcheck solution-9611fbb7
  (fn node? [n]
    (cond
      (not (coll? n)) false
      (not= (count n) 3) false
      (coll? (first n)) false
      :else (every? #(if (nil? %) true (node? %)) (rest n)))))

(defcheck solution-96138a80
  (fn tree?
    [root]
    (if (nil? root)
      true
      (and (coll? root) (= 3 (count root)) (tree? (nth root 1)) (tree? (last root))))))

(defcheck solution-9630e86f
  (fn isTree
    [x]
    (cond
      (nil? x) true
      (not (sequential? x)) false
      (not (= (count x) 3)) false
      :else (and (isTree (nth x 1)) (isTree (nth x 2))))))

(defcheck solution-9718371f
  (fn tree? [t]
    (and
     (coll? t)
     (let [[head left right :as coll] t]
       (and
        (= 3 (count coll))
        (not (nil? head))
        (or (nil? left) (tree? left))
        (or (nil? right) (tree? right)))))))

(defcheck solution-9731736f
  (fn t [x]
    (or (nil? x)
        (and (coll? x) (= (count x) 3) (t (second x)) (t (last x))))))

(defcheck solution-976c2324
  (fn f [t]
    (cond
      (nil? t) true
      (and (coll? t) (= 3 (count t))) (and
                                       (first t)
                                       (f (nth t 1))
                                       (f (nth t 2))
                                       true)
      :else false)))

(defcheck solution-97af578c
  (fn [y]
    (let [isNode? (fn [x]
                    (or (nil? x)
                        (and (coll? x)
                             (= 3 (count x)))))]
      (every? isNode? (tree-seq isNode? next y)))))

(defcheck solution-97f4d031
  #(letfn [(btree? [coll]
             (if (and (coll? coll) (= 3 (count coll)))
               (let [[a b c] coll]
                 (and
                  (not (coll? a))
                  (not (nil? a))
                  (or (nil? b) (btree? b))
                  (or (nil? c) (btree? c))))
               false))]
     (btree? %)
     ))

(defcheck solution-97f92579
  (fn binary? [x] (if (coll? x) (and (= (count x) 3) (and (binary? (first (rest x))) (binary? (second (rest x))))) (= x nil))))

(defcheck solution-982e1af2
  (fn tree? [node]
    (cond
      (nil? node) true
      (not (sequential? node)) false
      (not= 3 (count node)) false
      :else
      (let [left  (nth node 1)
            right (nth node 2)
            ]
        (and (tree? left)
             (tree? right)
             )
        )
      )
    ))

(defcheck solution-98452bf1
  (fn tree? [[n l r :as tree]]
    (and
     (= (count tree) 3)
     (or (nil? l) (and (coll? l) (tree? l)))
     (or (nil? r) (and (coll? r) (tree? r))))))

(defcheck solution-989fabd3
  (fn btree? [coll]
    (letfn [(nodelike? [thing]
              (if (false? thing) false
                                 (or (not (sequential? thing))
                                     (= (count thing) 3))))]
      (and (nodelike? coll)
           (if (sequential? coll)
             (every? #(btree? %) coll)
             true)))))

(defcheck solution-98dc717c
  (fn [ls]
    (every?
      #(and
        (= (count %) 3)
        (every? (fn [i] (or (nil? i) (coll? i))) (rest %)))
      (filter coll? (tree-seq coll? identity ls)))))

(defcheck solution-991c4ccd
  (fn tree? [s]
    (cond
      (nil? s) true
      (not (coll? s)) false
      :else (let [[v l r] s]
              (and (not (nil? v)) (tree? l) (tree? r) (= 3 (count s)))))))

(defcheck solution-9976a817
  (fn binary-tree? [node]
    (or
     (nil? node)
     (and
      (sequential? node)
      (= 3 (count node))
      (let [[val left right] node]
        (and
         (binary-tree? left)
         (binary-tree? right)))))))

(defcheck solution-9997ecdb
  (fn istree [t]
    (if (and (coll? t) (= 3 (count t)))
      (let [[k a b] t]
        (and
         (or (nil? a) (istree a))
         (or (nil? b) (istree b))))
      false)))

(defcheck solution-99a6dbf6
  (fn tree? [t]
    (cond (nil? t) true
          (coll? t) (and (= (count t) 3)
                         ((complement coll?) (first t))
                         (tree? (second t))
                         (tree? (last t)))
          :else false)))

(defcheck solution-99af7a6c
  (fn t [n]
    (or
     (nil? n)
     (and
      (sequential? n)
      (= (count n) 3)
      (every? t (drop 1 n))))))

(defcheck solution-9a5346fe
  (fn btree? [c]
    (if (coll? c)
      (if (and (== 3 (count c))
               (not (nil? (first c))))
        (and (btree? (second c))
             (btree? (nth c 2)))
        false)
      (if (nil? c)
        true
        false))))

(defcheck solution-9a8eb706
  (fn binaryTree? [x]
    (or (nil? x)
        (and
         (coll? x)
         (= 3 (count x))
         (binaryTree? (nth x 1))
         (binaryTree? (nth x 2))))))

(defcheck solution-9b98edb4
  (fn bintree [lst]
    (or (nil? lst)
        (and (sequential? lst) (= 3 (count lst)) (bintree (second lst)) (bintree (nth lst 2)))
        )))

(defcheck solution-9b9bbbed
  (fn binary-tree? [x]
    (if (sequential? x)
      (and (= (inc 2) (count x))                            ;change number for other leaved trees
           (every? binary-tree? (rest x)))
      (nil? x)))

  ;golf:
  ; binary-tree? -> t
  ; (inc 2)      -> 3
  )

(defcheck solution-9c2b626
  (fn [r]
    (every?
      #(or
        (nil? %)
        (and (sequential? %) (= 3 (count %))))
      (tree-seq sequential? rest r))))

(defcheck solution-9c6c4377
  (fn isbt [xs]
    (cond
      (nil? xs) true
      (and (sequential? xs)
           (= (count xs) 3)) (and (isbt (second xs))
                                  (isbt (second (rest xs))))
      :else false)))

(defcheck solution-9ce8cf4e
  (fn tree? [[v l r :as node]]
    (if (= 3 (count node))
      (cond (and (nil? l) (nil? r)) true
            (nil? l) (and (sequential? r) (tree? r))
            (nil? r) (and (sequential? l) (tree? l))
            :else (and (tree? l) (tree? r)))
      false)))

(defcheck solution-9cfcdb72
  (fn is-tree [s]
    (if (not (sequential? s))
      false
      (let [s-count (count s)]
        (cond
          (= 1 s-count) true
          (= 3 s-count) (let [left  (nth s 1)
                              right (nth s 2)]
                          (and
                           (or (nil? left) (is-tree left))
                           (or (nil? right) (is-tree right))))
          :else false)))))

(defcheck solution-9dccd1ed
  (fn btree? [t]
    (or (nil? t)
        (and
         (coll? t)
         (= 3 (count t))
         (btree? (second t))
         (btree? (nth t 2))))))

(defcheck solution-9e010608
  (fn is-tree? [s]
    (if (coll? s)
      (and
       (= 3 (count s))
       (let [[a b c] s]
         (and (not (nil? a)) (is-tree? b) (is-tree? c))))
      (nil? s))))

(defcheck solution-9e4acfe4
  (fn is-tree [s]
    (or (nil? s)
        (and (sequential? s)
             (= 3 (count s))
             (is-tree (second s))
             (is-tree (last s))))))

(defcheck solution-9e5a01d3
  (fn tree? [s]
    (cond
      (nil? s) true
      (not (coll? s)) true
      (and (not (nil? s)) (empty? s)) false
      (not= (count s) 3) false
      :else
      (let [[value l r] s]
        (cond
          (and (nil? r) (and (not (nil? l)) (not (coll? l)))) false
          (and (nil? l) (and (not (nil? r)) (not (coll? r)))) false
          :else (and (tree? l) (tree? r)))))))

(defcheck solution-9ece05b
  (fn btree [t]
    (if (coll? t)
      (and
       (= 3 (count t))
       (every? btree (map #(nth t %) [1 2]))
       )
      (= nil t)
      )
    ))

(defcheck solution-9ef0eb69
  (fn bin? [coll]
    (if (coll? coll)
      (and (= 3 (count coll))
           (every? bin? coll))
      (-> coll false? not))
    ))

(defcheck solution-9f5046d0
  (fn tree? [T]
    (and (coll? T)
         (= 3 (count T))
         (every? #(or (nil? %) (tree? %)) (rest T)))))

(defcheck solution-9f8a1182
  (fn [coll]
    (let [nodes (filter sequential? (tree-seq sequential? seq coll))]
      (and (every? #(-> % count (= 3)) nodes)
           (every? nil? (-> nodes last rest))))))

(defcheck solution-9f8b623d
  (fn t? [t]
    (or (nil? t)
        (and (coll? t)
             (= (count t) 3)
             (t? (second t))
             (t? (last t))))))

(defcheck solution-9fcc37d4
  (fn bin? [v]
    (or
     (nil? v)
     (and
      (coll? v)
      (= (count v) 3)
      (bin? (second v))
      (bin? (last v))
      )
     )
    ))

(defcheck solution-a050098a
  (fn ibt [tc]
    (cond
      (nil? tc) true
      (not (sequential? tc)) false
      (not (= 3 (count tc))) false
      :else (let [[c l r] tc]
              (and (not (sequential? c))
                   (ibt l)
                   (ibt r))))))

(defcheck solution-a1182beb
  (fn bntree? [l]
    (if (not (sequential? l))
      (nil? l)
      (if (not= (count l) 3)
        false
        (and (bntree? (second l)) (bntree? (last l)))))))

(defcheck solution-a15dbcf9
  (fn tree? [t]
    (or (nil? t)
        (and (coll? t)
             (= 3 (count t))
             (tree? (nth t 1))
             (tree? (nth t 2))))))

(defcheck solution-a1ad474e
  (fn is-tree? [l]
    (if (= l nil)
      true
      (if (and (coll? l) (= (count l) 3))
        (and (is-tree? (second l)) (is-tree? (last l)))
        false))))

(defcheck solution-a1b219b0
  (fn traverse [node]
    (cond
      (nil? node) true
      (not (coll? node)) false
      (not (and (= (count node) 3) (first node)))
      false
      :else (and (traverse (second node)) (traverse (last node)))
      )

    ))

(defcheck solution-a1ea39d3
  (fn bi-tree? [t]
    (or
     (nil? t)
     (and
      (coll? t)
      (= 3 (count t))
      (bi-tree? (second t))
      (bi-tree? (last t))))))

(defcheck solution-a1f18b68
  (fn te [n]
    (and
     (sequential? n)
     (= (count n) 3)
     (every? #(or (nil? %)
                  (te %)) (rest n)))))

(defcheck solution-a2af04f2
  (fn tree? [xs]
    (if-let [[v lChild rChild] xs]
      (let [nilOrTree #(if (nil? %1) true (and (coll? %1) (tree? %1)))]
        (and
         (= 3 (count xs))
         (not (coll? v))
         (nilOrTree lChild)
         (nilOrTree rChild))))))

(defcheck solution-a30a8e35
  #(letfn [(testele [node]
             (cond (= nil node) true
                   (not node) false
                   (= 3 (count node)) (and (testele (nth node 1)) (testele (nth node 2)))
                   :else false))]
     (testele %)))

(defcheck solution-a3834234
  (fn __ [s]
    (if (sequential? s)
      (and (= (count s) 3) (__ (second s)) (__ (last s)))
      (nil? s))))

(defcheck solution-a391bb77
  (fn n95 [tree]
    (letfn [(leave? [l] (or (sequential? l) (nil? l)))]
      (if (sequential? tree)
        (if (= [false true true] (map leave? tree)) (reduce #(and %1 %2) (map n95 tree)) false)
        true))))

(defcheck solution-a3c09672
  (fn binary-tree? [t]
    (if (and (coll? t) (= 3 (count t)))
      (let [[l r] (rest t)]
        (and (or (nil? l) (binary-tree? l))
             (or (nil? r) (binary-tree? r))))
      false)))

(defcheck solution-a3e39c74
  #(odd? (count (remove false? (flatten %)))))

(defcheck solution-a45ef976
  (fn tree? [s] (or (nil? s)
                    (and (coll? s)
                         (= 3 (count s))
                         (tree? (nth s 1))
                         (tree? (nth s 2))))))

(defcheck solution-a58377bf
  (fn tree? [col]
    (cond
      (= col nil) true
      (not (and (coll? col) (= (count col) 3))) false
      :else (and ((complement coll?) (first col))
                 (tree? (second col))
                 (tree? (last col))))))

(defcheck solution-a6a72363
  (fn foo [[v a b :as n]]

    (and
     (= 3 (count n))
     (cond (nil? a) true
           (coll? a) (foo a)
           :else false
           )
     (cond (nil? b) true
           (coll? b) (foo b)
           :else false
           )

     )

    ))

(defcheck solution-a6b0788b
  (fn tree? [c]
    (if (nil? c)
      true
      (if (and (coll? c) (= 3 (count c)))
        (let [[_ left right] c]
          (and (tree? left) (tree? right)))
        false))))

(defcheck solution-a6b83ce5
  (fn [root]
    (letfn [(tree? [t]
              (or (nil? t)
                  (and (sequential? t)
                       (= 3 (count t))
                       (tree? (nth t 1))
                       (tree? (nth t 2)))))]
      (tree? root))))

(defcheck solution-a70d628b
  (fn is-btree? [lst]
    (if (sequential? lst)
      (and (= (count lst) 3)
           (not (sequential? (first lst)))
           (not-any? #(not (is-btree? %)) (rest lst)))
      (nil? lst))))

(defcheck solution-a7365ca5
  (fn binary-tree? [coll]
    (if (= 3 (count coll))
      (let [l (nth coll 1)
            r (nth coll 2)]
        (letfn [(binary-tree-or-nil? [x]
                  (if (coll? x)
                    (binary-tree? x)
                    (nil? x)))]
          (and (binary-tree-or-nil? l) (binary-tree-or-nil? r))))
      false)))

(defcheck solution-a77f71fb
  ;; the recursive version:
  (fn binary-tree? [xs]
    (letfn [(bin-node? [node]
              (and (sequential? node) (= 3 (count (remove false? node)))))
            (nested-nodes? [node]
              ((comp not empty?) (filter sequential? node)))]
      (if (bin-node? xs)
        (if (nested-nodes? xs)
          (if (sequential? (second xs))
            (binary-tree? (second xs))
            (binary-tree? (last xs)))
          true)
        false)))


  ;; following binary-tree? finder using HOFs
  #_(fn binary-tree?
      [xs]
      (let [valid-node? #(= 3 (count %))
            node?       #(and (or (seq? %) (vector? %)))]
        (->> xs
          (reduce (fn [acc composite]
                    (if (node? composite)
                      (conj acc ((comp not empty?) composite) (binary-tree? composite))
                      (conj acc (and (valid-node? xs) composite)))) [])
          flatten
          (filter false?)
          empty?))))

(defcheck solution-a7e9aac4
  (fn f [v]
    (if (sequential? v)
      (let [b (second v) c (last v)]
        (and (= 3 (count v)) (f b) (f c)
             (or (sequential? b) (nil? b))
             (or (sequential? c) (nil? c))
             ))
      true
      )))

(defcheck solution-a7f1ca8c
  (fn dvt [x] (let [[a b c] x]
                (and
                 (= 3 (count x))
                 (not (seq? a))
                 (or (nil? b) (and (sequential? b) (dvt b)))
                 (or (nil? c) (and (sequential? c) (dvt c)))
                 ))))

(defcheck solution-a7fa87a7
  (fn f [t] (if (coll? t)
              (let [[_ a b] t]
                (and (= 3 (count t)) (f a) (f b)))
              (nil? t))))

(defcheck solution-a83831c6
  (fn [t]
    (every? (fn [x] (= 3 (count (filter #(not (false? %)) x))))
      (filter sequential?
        (tree-seq sequential? identity t)))))

(defcheck solution-a8451395
  (fn t [x]
    (or
     (nil? x)
     (and
      (coll? x)
      (= 3 (count x))
      (t (nth x 1))
      (t (nth x 2))))))

(defcheck solution-a88c75ae
  (fn valid-tree [xs]
    (if (not (sequential? xs))
      (if (nil? xs)
        true
        false)
      (if (= (count xs) 3)
        (and
         (valid-tree (nth xs 1))
         (valid-tree (nth xs 2)))
        false))))

(defcheck solution-a8f1b938
  (fn my-binary-tree-test
    [tree]
    (every? #(and (= (count %) 3) (not-any? false? %)) (filter sequential? (tree-seq sequential? identity tree)))))

(defcheck solution-a9a38e2c
  (fn this [s]
    (cond (and (not (seq? s)) (not (vector? s))) (not (= false s))
          (= (count s) 0) false
          (= (count s) 1) true
          (and (> (count s) 0) (< (count s) 3)) false
          (not (= (count s) 3)) false
          :else (let [root  (first s)
                      left  (nth s 1)
                      right (nth s 2)]
                  (and (this left)
                       (this right))))))

(defcheck solution-a9a6d10b
  (fn binarytree? [tree]
    (if (and (coll? tree) (= (count tree) 3))
      (every? true? (map binarytree? (filter (complement nil?) (rest tree))))
      false)))

(defcheck solution-aaa6d5f6
  (fn bam [alist]
    (let [newlist (filter #(not= "false" (str %)) alist)]
      (if (= 3 (count newlist))
        (if (some sequential? newlist)
          (first (and (flatten (map bam (filter sequential? newlist)))))
          true)
        false))))

(defcheck solution-aaf3cfea
  (fn ! [tree]
    (and (coll? tree) (= 3 (count tree)) (not (nil? (first tree))) (not (coll? (first tree)))
         (or (nil? (nth tree 1)) (! (nth tree 1)))
         (or (nil? (nth tree 2)) (! (nth tree 2)))
         )
    ))

(defcheck solution-ab135c26
  (fn btree? [coll]
    (if (nil? coll)
      true
      (if (or (not (coll? coll))
              (not= 3 (count coll)))
        false
        (let [[_ l r] coll]
          (and (btree? l) (btree? r)))))))

(defcheck solution-ab321dfa
  (fn is-tree? [[t left right :as node]]
    (let [valid? #(or (nil? %)
                      (and (counted? %)
                           (is-tree? %)))]
      (and (= (count node) 3) (valid? left) (valid? right)))))

(defcheck solution-ab41a319
  (fn is-tree
    [a]
    (cond
      (sequential? a)
      (and
       (= 3 (count a))
       (every? true? (map is-tree a)))
      (false? a) false
      :else true
      )))

(defcheck solution-abde41d5
  (fn tree? [t]
    (if (and (coll? t)
             (= (count t) 3))
      (let [node? #(or (nil? %) (tree? %))]
        (and (node? (nth t 1))
             (node? (nth t 2))))
      false)))

(defcheck solution-ac251a58
  (fn bint [t]
    (or (or (nil? t)
            (keyword? t)
            (number? t))
        (and (sequential? t)
             (= (count t) 3)
             (bint (nth t 1))
             (bint (nth t 2))))))

(defcheck solution-ac55a8f0
  (fn [t] (empty? (filter (fn [n] (and (coll? n)
                                       (or (not (= (count n) 3))
                                           (and (not (coll? (second n)))
                                                (not (nil? (second n))))
                                           (and (not (coll? (last n)))
                                                (not (nil? (last n)))))))
                    (tree-seq coll? identity t)))))

(defcheck solution-ac831899
  (fn b? [[_ l r :as t]]
    (and (= 3 (count t))
         (not-any? false? t)
         (or (nil? l) (b? l))
         (or (nil? r) (b? r)))))

(defcheck solution-ac8b0b58
  (fn bt? [t]
    (if (not= 3 (count t))
      false
      (let [l (second t) r (last t)]
        (and (if (sequential? l) (bt? l) (if (not= false l) true false))
             (if (sequential? r) (bt? r) true))))))

(defcheck solution-ac9a388c
  (fn t? [[n l r :as t]] (and (= 3 (count t)) (every? #(or (nil? %) (and (coll? %) (t? %))) [l r]))))

(defcheck solution-acacfd02
  (fn tree? [coll]
    (cond (not (or (coll? coll) (nil? coll))) false
          (= (count coll) 3) (and (tree? (second coll)) (tree? (last coll)))
          (nil? coll) true
          true false)))

(defcheck solution-acfb475b
  (fn binary-tree? [tree]
    (if (nil? tree) true
                    (if (or (not (coll? tree))
                            (not= 3 (count tree))) false
                                                   (every? binary-tree? (rest tree))))))

(defcheck solution-ad984600
  (fn t [xs] (if (sequential? xs) (if (= 3 (count xs)) (and (t (second xs)) (t (last xs))) false) (if (nil? xs) true false))))

(defcheck solution-adff24e4
  (fn is-tree [xs]
    (if (sequential? xs)
      (and (= 3 (count xs))
           (not (sequential? (first xs)))
           (is-tree (second xs))
           (is-tree (nth xs 2)))
      (not (false? xs)))))

(defcheck solution-af3902a7
  (fn f [t]
    (or
     (nil? t)
     (and
      (coll? t)
      (= (count t) 3)
      (every? f (rest t))))))

(defcheck solution-af41f149
  (fn tree? [col]
    (and (coll? col)
         (= (count col) 3)
         (or (nil? (nth col 1)) (tree? (nth col 1)))
         (or (nil? (nth col 2)) (tree? (nth col 2))))))

(defcheck solution-b0367cfb
  (fn bin-tree? [tree]
    (if (and (sequential? tree) (= 3 (count tree)))
      (reduce #(and (bin-tree? %2) %1) true (rest tree))
      (nil? tree))))

(defcheck solution-b08030ed
  (fn prob95 [s]
    (if (nil? s)
      true
      (if (or (not (coll? s)) (not= 3 (count s)))
        false
        (and (prob95 (nth s 1)) (prob95 (nth s 2)))))))

(defcheck solution-b0a596ff
  (fn bin-tree? [x]
    (loop [[x & more :as to-check] [x]]
      (cond
        (empty? to-check) true
        (nil? x) (recur more)
        (not (coll? x)) false
        (not= 3 (count x)) false
        :else (recur (conj more (second x) (last x)))))))

(defcheck solution-b0bb5e21
  (fn bin-tree? [tree]
    (if (coll? tree)
      (and (= 3 (count tree))
           (bin-tree? (second tree))
           (bin-tree? (last tree)))
      (nil? tree))))

(defcheck solution-b0c69f70
  (fn tree? [t]
    (cond
      (nil? t) true
      ((complement sequential?) t) false
      (= (count t) 3) (and (tree? (second t)) (tree? (nth t 2)))
      :else false)))

(defcheck solution-b0fe461e
  (fn tree? [coll]
    (cond
      (or (seq? coll) (vector? coll))
      (and (= 3 (count coll)) (tree? (nth coll 1)) (tree? (nth coll 2)))
      (nil? coll) true
      :else false)))

(defcheck solution-b10d77a2
  (fn tree? [coll]
    (if (not= 3 (count coll))
      false
      (every? #(or (nil? %) (if (sequential? %) (tree? %))) (rest coll)))))

(defcheck solution-b120bb39
  (fn tree? [coll]
    (or (nil? coll)
        (and
         (sequential? coll)
         (= 3 (count coll))
         (not (sequential? (first coll)))
         (tree? (second coll))
         (tree? (last coll))))))

(defcheck solution-b13dd9c8
  (fn bt [t]
    (if (not (or (nil? t) (sequential? t))) false
                                            (or (nil? t) (and (= (count t) 3) (bt (nth t 1)) (bt (nth t 2)))))))

(defcheck solution-b164fa84
  (fn is-tree1
    [s]
    (letfn [(is-tree'
              ([] false)
              ([x] (nil? x))
              ([x y] false)
              ([x y z] (if (nil? x) false (and (is-tree1 y) (is-tree1 z))))
              ([x y z & more] false))]
      (if (coll? s) (apply is-tree' s) (is-tree' s)))))

(defcheck solution-b208168a
  (fn f [xs]
    (every?
      #(if (sequential? %)
         (= 3 (count %))
         (not (false? %)))
      (tree-seq sequential? identity xs))
    ))

(defcheck solution-b2301d4a
  (fn bTree? [n]
    (if (nil? n) true
                 (if (or (not (coll? n)) (not= 3 (count n))) false
                                                             (and (bTree? (nth n 1)) (bTree? (nth n 2)))))))

(defcheck solution-b24ac11f
  (fn [tree]
    (loop [trees (list tree)]
      (if trees
        (let [node (first trees)]
          (cond (= nil node) (recur (next trees))
                (and (coll? node) (== 3 (count node))) (let [[val left right] node]
                                                         (recur (-> trees next (conj left) (conj right))))
                :else false))
        true))))

(defcheck solution-b277d60d
  (fn r [x]
    (or (= x nil)
        (and (coll? x)
             (= (count x) 3)
             (every? r (rest x))))))

(defcheck solution-b2a21ddc
  (letfn [(f [n] (if (and (sequential? n) (seq n)) (if (= (count n) 3) (and (f (second n)) (f (nth n 2))) false) (nil? n)))] f))

(defcheck solution-b32ad758
  (fn tree? [s]
    (if
     (coll? s)
      (case
       (count s)
        1 true
        3 (let [[_ lft rgt] s]
            (and (tree? lft) (tree? rgt)))
        false)
      (nil? s))))

(defcheck solution-b35c2ce8
  (fn bt? [t]
    (if (not (sequential? t))
      (if (= t false)
        false
        true)
      (and (= 3 (count t))
           (bt? (second t))
           (bt? (last t))))
    ))

(defcheck solution-b3a9a858
  (fn isbinarytree? [coll]
    (if (not (coll? coll))
      ;;leaf node should have nil as left and right
      (if (not (nil? coll))
        false
        true)
      (let [cnt       (count coll)
            isbinary? (if (or (= cnt 3) (= cnt 1)) true false)]
        (if (not isbinary?)
          false
          (and (isbinarytree? (second coll)) (isbinarytree? (second (rest coll)))))))))

(defcheck solution-b3d53a7f
  (fn tr [c] (if (nil? c) true
                          (and c (= 3 (count c))
                               (tr (second c)) (tr (last c)))
                          )
    ))

(defcheck solution-b3ebed98
  (fn valid? [t]
    (if (nil? t)
      true
      (and
       (coll? t)
       (= (count t) 3)
       (valid? (second t))
       (valid? (nth t 2))))))

(defcheck solution-b3ff53bd
  (fn tree? [coll]
    (if (nil? coll)
      true
      (if (and (coll? coll) (= (count coll) 3))
        (and (tree? (second coll)) (tree? (last coll)))
        false))))

(defcheck solution-b402fd6a
  (fn t [s] (or (nil? s) (and (coll? s) (= 3 (count s)) (t (second s)) (t (nth s 2))))))

(defcheck solution-b406fbd2
  (fn binary-tree? [t]
    (or (nil? t)
        (and (coll? t)
             (= (count t) 3)
             (binary-tree? (second t))
             (binary-tree? (nth t 2))))))

(defcheck solution-b4119d68
  (fn [t]
    (->> (tree-seq coll? next t)
      (remove (every-pred coll? #(= 3 (count %))))
      (remove nil?)
      (empty?))))

(defcheck solution-b44d2793
  (fn check [tree]
    (if (sequential? tree)
      (if (= 3 (count tree))
        (every? true? (map check tree))
        false)
      (if (= false tree) false true))))

(defcheck solution-b4567a83
  (fn bt? [t]
    (or (nil? t)
        (and
         (sequential? t)
         (= 3 (count t))
         (bt? (nth t 1 nil))
         (bt? (nth t 2 nil))))))

(defcheck solution-b508c37
  (fn binary-tree? [x]
    (or
     (nil? x)
     (and
      (coll? x)
      (= 3 (count x))
      (binary-tree? (second x))
      (binary-tree? (nth x 2))))))

(defcheck solution-b5b0331a
  (fn btree? [x] (if (coll? x) (and (= 3 (count x)) (every? btree? (rest x))) (nil? x))))

(defcheck solution-b5d4fe9f
  (fn tree? [x]
    (if (nil? x)
      true
      (if (or (not (coll? x)) (not= 3 (count x)))
        false
        (every? true? (map tree? (next x)))))))

(defcheck solution-b5e2ca34
  (fn t? [x]
    (or (nil? x)
        (and (coll? x) (= (count x) 3)
             (let [[_ y z] x]
               (and (t? y) (t? z)))))))

(defcheck solution-b5f43907
  (fn btree? [t]
    (if (nil? t) true
                 (if (and (coll? t)
                          (not (coll? (first t)))
                          (= (count t) 3))
                   (and (btree? (second t))
                        (btree? (nth t 2)))
                   false))))

(defcheck solution-b61cb9b7
  (fn binary-tree? [tree]
    (case (coll? tree)
      true (let [[_ l r] tree]
             (and
              (= 3 (count tree))
              (binary-tree? l)
              (binary-tree? r)))
      false (= tree nil))))

(defcheck solution-b62083eb
  (fn binary-tree? [coll]
    (or (nil? coll)
        (and (coll? coll)
             (= 3 (count coll))
             (binary-tree? (second coll))
             (binary-tree? (last coll))))))

(defcheck solution-b734666c
  (fn bin-tree? [x]
    (and (counted? x)
         (= 3 (count x))
         (or (nil? (second x)) (bin-tree? (second x)))
         (or (nil? (last x)) (bin-tree? (last x))))))

(defcheck solution-b7ac8b85
  (fn isbin
    [t]
    (if (not= (count t) 3)
      false
      (if-let [[a l r] t]
        (and
         (if (sequential? l) (isbin l) (or (nil? l) (boolean l)))
         (if (sequential? r) (isbin r) (or (nil? r) (boolean r))))))))

(defcheck solution-b7cdc2c5
  (fn binary-tree? [[v l r :as a]]
    (and (= 3 (count a))
         (or (nil? l)
             (and (sequential? l)
                  (binary-tree? l)))
         (or (nil? r)
             (and (sequential? r)
                  (binary-tree? r))))))

(defcheck solution-b7f43c7c
  (fn tree? [c]
    (or
     (nil? c)
     (and
      (coll? c)
      (= 3 (count c))
      (not (coll? (first c)))
      (tree? (first (next c)))
      (tree? (first (next (next c))))))))

(defcheck solution-b8096343
  (fn testtree [x]
    (or (nil? x)
        (and (coll? x) (= 3 (count x)) (testtree (nth x 1)) (testtree (nth x 2))))))

(defcheck solution-b83450e
  (fn tree? [[n l r :as s]]
    (let [st? (fn [z] (or (nil? z) (and (sequential? z) (tree? z))))]
      (if (= 3 (count s))
        (and (st? l) (st? r))
        false))))

(defcheck solution-b85b863d
  (fn tree? [s] (if (sequential? s) (and (= 3 (count s))
                                         (every? true? (map tree? (next s))))
                                    (not (= false s)))))

(defcheck solution-b86d18f2
  (fn is-tree? [t]
    (if (= 3 (count t))
      (let [[a b c] t
            fun (fn [x]
                  (if (coll? x)
                    (is-tree? x)
                    (nil? x)
                    ))]
        (and
         (fun b)
         (fun c)
         ))
      false
      )
    ))

(defcheck solution-b8708ba5
  (fn tree? [x]
    (or (nil? x)
        (and (coll? x)
             (= 3 (count x))
             (tree? (nth x 1))
             (tree? (nth x 2))))))

(defcheck solution-b874f3eb
  (fn is-tree [colls]
    (if (nil? colls)
      true
      (if (or (not (coll? colls)) (not= 3 (count colls)))
        false
        (let [x (second colls)
              y (nth colls 2)]
          (and (is-tree x) (is-tree y)))))))

(defcheck solution-b89e8234
  (fn is-bin?
    [t]
    (or (nil? t) (and (coll? t) (= 3 (count t)) (every? is-bin? (rest t))))))

(defcheck solution-b8c8be5b
  (fn bt? [coll]
    (cond
      (nil? coll) true
      (sequential? coll) (and (= (count coll) 3)
                              (bt? (second coll))
                              (bt? ((comp second rest) coll)))
      :else false)))

(defcheck solution-b8f24227
  (fn tree? [tree]
    (or
     (nil? tree)
     (and
      (coll? tree)
      (= (count tree) 3)
      (tree? (second tree))
      (tree? (second (rest tree)))))))

(defcheck solution-b922bc2a
  (fn ! [coll]
    (if (coll? coll)
      (if-let [s (seq coll)]
        (if (= 3 (count s))
          (and (! (second s)) (! (last s)))
          false)
        false)
      (if (nil? coll) true false))))

(defcheck solution-b951b708
  (fn [t]
    (every? true?
      (map #(if (coll? %) (= (count %) 3) (nil? %))
        (tree-seq
          coll?
          rest
          t)))))

(defcheck solution-b9b2ccea
  (fn f [t]
    (let [branch? #(or (list? %) (seq? %) (vector? %))]
      (if (not (= 3 (count t)))
        false
        (loop [l (nth t 1), r (nth t 2)]
          (cond
            (= nil l r)
            true
            (and (branch? l) (branch? r))
            (and (f r) (f l))
            (and (branch? l) (= 3 (count l)))
            (recur (second l) (nth l 2))
            (and (branch? r) (= 3 (count r)))
            (recur (second r) (nth r 2))
            :else
            false))))))

(defcheck solution-ba39c268
  (fn bn-check1? [bn] (let [bn1 (second bn) bn2 (last bn)] (cond
                                                             (not= (count bn) 3) false
                                                             (or (and (not (nil? bn1)) (not (coll? bn1))) (and (not (nil? bn2)) (not (coll? bn2)))) false
                                                             (and (or (nil? bn1) (bn-check1? bn1)) (or (nil? bn2) (bn-check1? bn2))) true
                                                             :else false
                                                             ))))

(defcheck solution-ba5d0bf9
  (fn tree? [c]
    (if (coll? c)
      (if (= 3 (count c))
        (and (tree? (second c)) (tree? (nth c 2)))
        false)
      (nil? c))))

(defcheck solution-ba9d5bed
  (fn tree? [x]
    (or (nil? x)
        (and (-> x sequential?)
             (-> x count (= 3))
             (-> x first nil? not)
             (-> x second tree?)
             (-> x last tree?)))))

(defcheck solution-bac7fd2b
  (fn tree? [n]
    (or (nil? n)
        (and (coll? n)
             (= 3 (count n))
             (every? tree? (rest n))))))

(defcheck solution-bad2378
  (fn __ [t]
    (or (nil? t)
        (and (coll? t)
             (= 3 (count t))
             (__ (nth t 1))
             (__ (nth t 2))))))

(defcheck solution-baee6dc
  (fn tree? [node]
    (if (not (coll? node))
      (if (nil? node)
        true
        false)
      (if (not= (count node) 3)
        false
        (and
         (tree? (second node))
         (tree? (nth node 2)))))))

(defcheck solution-bb097ce0
  (fn bt? [t]
    (if (or (nil? t)
            (and (sequential? t)
                 (= 3 (count t))
                 (bt? (second t))
                 (bt? (last t))))
      true false)))

(defcheck solution-bb3ee775
  (fn binary-tree? [node]
    (cond
      (nil? node) true
      (not (sequential? node)) false
      (not= 3 (count node)) false
      :else (let [[_ L R] node]
              (and (binary-tree? L) (binary-tree? R))))))

(defcheck solution-bb53f4e1
  (fn bin-tree? [tree]
    (if (and (coll? tree) (= (count tree) 3))
      (let [[v l r] tree
            val? (complement coll?)]
        (and (val? v) (or (nil? l) (bin-tree? l)) (or (nil? r) (bin-tree? r)))) false)))

(defcheck solution-bb977e9f
  (fn is-tree [x]
    (or (nil? x)
        (and (coll? x)
             (= 3 (count x))
             (let [[_ a b] x]
               (and (is-tree a) (is-tree b))
               )))))

(defcheck solution-bbb774ee
  (fn t [s]
    (or
     (nil? s)
     (and (coll? s)
          (= 3 (count s))
          (t (nth s 1))
          (t (nth s 2))))))

(defcheck solution-bbd22169
  (fn tree? [[a b c :as t]]
    (and (= (count t) 3) (or (nil? b) (and (sequential? b) (tree? b))) (or (nil? c) (and (sequential? c) (tree? c))))))

(defcheck solution-bc0ed537
  (fn check [tree] (if (coll? tree)
                     (if (not= (count tree) 3)
                       false
                       (and (check (nth tree 0)) (check (nth tree 1)) (check (nth tree 2))))

                     (not= false tree))))

(defcheck solution-bc19e01b
  (fn istree [x]
    (if (and (= (count x) 3) (every? #(or (nil? %) (coll? %)) (rest x)))
      (reduce #(and %1 (istree %2)) true (filter coll? x))
      false)))

(defcheck solution-bccf484
  (fn bi-tree? [branch]
    (if (nil? branch)
      true
      (if-not (coll? branch)
        false
        (if-not (= 3 (count branch))
          false
          (let [[root left-child right-child] branch]
            (and root (bi-tree? left-child) (bi-tree? right-child))))))))

(defcheck solution-bd2e22de
  (letfn [(istree [t]
            (if (nil? t)
              true
              (if (coll? t)
                (and (= 3 (count t))
                     (istree (nth t 1))
                     (istree (nth t 2)))
                false)
              )
            )]
    istree))

(defcheck solution-bd6a8d71
  (fn tree? [tree]
    (cond (not (sequential? tree)) (if (nil? tree) true false)
          (not= 3 (count tree)) false
          true (and (tree? (second tree)) (tree? (nth tree 2))))))

(defcheck solution-be012044
  (fn tree? [xs] (or (nil? xs) (and (coll? xs) (= 3 (count xs)) (let [val (first xs) cha (second xs) chb (last xs)] (and (tree? cha) (tree? chb)))))))

(defcheck solution-be449b55
  (fn f [x] (or (nil? x)
                (and (coll? x)
                     (= (count x) 3)
                     (f (nth x 1))
                     (f (nth x 2))))))

(defcheck solution-be61fea
  (fn f [n]
    (or
     (nil? n)
     (and (coll? n) (= [true true] (map f (rest n)))))))

(defcheck solution-bee292d1
  (fn btree? [node]
    (or (nil? node)
        (and (coll? node)
             (= (count node) 3)
             (every? btree? (rest node))))))

(defcheck solution-bf13e264
  (letfn [(child? [x]
            (or (coll? x)
                (nil? x)))]
    (fn tree?
      [[a b c :as arg]]
      (and
       (= (count arg) 3)
       a
       (child? b)
       (child? c)
       (if b (tree? b) true)
       (if c (tree? c) true)))))

(defcheck solution-bf5f05bb
  (fn _ [s]
    (or
     (nil? s)
     (and
      (sequential? s)
      (= 3 (count s))
      (not (sequential? (nth s 0)))
      (_ (nth s 1))
      (_ (nth s 2))))))

(defcheck solution-bfaf0e0e
  (fn tree? [[v i d :as nodo]]
    (if (= 3 (count nodo))
      (if-not (or (= nil v) (coll? v))
        (if (= i nil)
          (if (= d nil)
            true
            (if (coll? d)
              (tree? d)
              false))
          (if (coll? i)
            (tree? i)
            false))
        false)
      false)))

(defcheck solution-c087f642
  (fn [tree] (let [subtrees (tree-seq sequential? identity tree)
                   branch?  (fn [xs] (or (nil? xs) (sequential? xs)))
                   ok       (fn [sub] (or ((complement sequential?) sub)
                                          (and (= 3 (count sub))
                                               ((complement sequential?) (first sub))
                                               (branch? (second sub))
                                               (branch? (last sub)))))

                   ]
               (every? ok subtrees))))

(defcheck solution-c08e139e
  (fn is-tree [x]
    (or
     (nil? x)
     (and
      (sequential? x)
      (= 3 (count x))
      (is-tree (second x))
      (is-tree (last x))))))

(defcheck solution-c16b1a99
  (fn tree? [t]
    (if-not (sequential? t)
      (= nil t)
      (let [s (seq t)
            c (count s)
            [v l r] s]
        (and (= c 3) (tree? l) (tree? r))))))

(defcheck solution-c1af0370
  (fn is-tree?
    [x]
    (if (or (list? x) (vector? x))
      (let [[f s t ? rest] x]
        (if (and (= (count x) 3)
                 (or (nil? s) (is-tree? s))
                 (or (nil? t) (is-tree? t)))
          true
          false))
      false)))

(defcheck solution-c1c9d3b8
  (fn b-tree?
    [t]
    (and
     (sequential? t)
     (= 3 (count t))
     (let
      [node? #(or (nil? %) (b-tree? %))]
       (and
        (node? (second t))
        (node? (last t)))))))

(defcheck solution-c1c9d485
  (fn f [x]
    (if (coll? x)
      (and (= (count x) 3)
           (every? true? (for [a (rest x)]
                           (f a))))
      (= x nil))))

(defcheck solution-c1d0c5e4
  (fn T? [x]
    (or (nil? x)
        (and (coll? x)
             (= 3 (count x))
             (T? (nth x 1))
             (T? (nth x 2))))))

(defcheck solution-c1d9f9b7
  (fn f [t]
    (or
     (nil? t)
     (and
      (coll? t)
      (= 3 (count t))
      (every? f (next t))))))

(defcheck solution-c1f39283
  (fn is-bt? [coll]
    (if (and (coll? coll) (= 3 (count coll)))
      (let [l (second coll)
            r (last coll)]
        (and (if (nil? l) true (is-bt? l))
             (if (nil? r) true (is-bt? r))))
      false)))

(defcheck solution-c2048481
  (fn btree? [root]
    (or (nil? root)
        (and (sequential? root)
             (= 3 (count root))
             (every? btree? (rest root))))))

(defcheck solution-c2416e45
  (fn chk [s] (and (and (= 3 (count s)) (= 0 (count (filter false? s))))
                   (and (or (not (coll? (first (drop 1 s)))) (chk (first (drop 1 s))))
                        (or (not (coll? (last s))) (chk (last s))))
                   )
    ))

(defcheck solution-c26ef397
  (fn tree? [t]
    (cond
      (nil? t) true
      (and (coll? t) (= 3 (count t))) (and (tree? (nth t 1)) (tree? (nth t 2)))
      :default false)))

(defcheck solution-c28d07ed
  (fn __ [t]
    (if (= (count t) 3)
      (let [v (first t)
            l (second t)
            r (last t)]
        (if (coll? l)
          (__ l)
          (if (nil? l)
            (if (coll? r)
              (__ r)
              (if (nil? r)
                true
                false))
            false)))
      false)))

(defcheck solution-c301cb27
  (fn tree? [t]
    (if (nil? t)
      true
      (if-not (coll? t)
        false
        (if-not (= 3 (count t))
          false
          (and (tree? (second t)) (tree? (last t)))
          )
        )
      )
    ))

(defcheck solution-c32d2fa
  (fn tree? [t]
    (apply
      (fn
        ([] false)
        ([a] (nil? a))
        ([_ _] false)
        ([a b c] (and (or (not (coll? a))
                          (tree? a))
                      (tree? b)
                      (tree? c)))
        ([_ _ _ & _] false))
      (if (coll? t) t [t]))))

(defcheck solution-c386cf03
  (fn x [n]
    (if (coll? n)
      (and (= 3 (count n)) (every? x (rest n)))
      (nil? n))))

(defcheck solution-c39540ad
  (fn binary-tree? [s]
    (if (and (coll? s) (= 3 (count s)))
      (let [[_ l r] s]
        (and
         (or (nil? l) (binary-tree? l))
         (or (nil? r) (binary-tree? r))))
      false)))

(defcheck solution-c3d9c2ad
  (fn binary-tree? [t]
    (or
     (nil? t)
     (and (sequential? t) (= 3 (count t)) (binary-tree? (nth t 1)) (binary-tree? (nth t 2))))))

(defcheck solution-c3fc9177
  (fn tree? [m]
    (if (and (coll? m) (= 3 (count m)))
      (let [[l c r] m] (and l (tree? c) (tree? r)))
      (nil? m))))

(defcheck solution-c414e556
  (fn f [l]
    (cond (nil? l) true
          (and (coll? l)
               (= 3 (count l))) (and (f (second l))
                                     (f (last l)))
          :else false)))

(defcheck solution-c4193a39
  (fn f [s]
    (if (coll? s)
      (reduce #(and %1 %2) (= (count s) 3) (map f (rest s)))
      (nil? s))))

(defcheck solution-c49cbded
  (fn is-tree [node]
    (if (and (coll? node)
             (= (count node) 3))
      (let [children (filter (complement nil?)
                       (rest node))]
        (or (empty? children)
            (every? is-tree children)))
      false)))

(defcheck solution-c5002228
  (fn n? [n]
    (and (coll? n) (= (count n) 3)
         (first n)
         (or (nil? (second n)) (n? (second n)))
         (or (nil? (last n)) (n? (last n))))))

(defcheck solution-c5eb4e65
  (fn bt? [bt]
    (and (coll? bt) (= (count bt) 3)
         (boolean (first bt))
         (every? #(or (nil? %) (bt? %)) (rest bt)))))

(defcheck solution-c62694fe
  (fn is-binary-tree? [t]
    (or (nil? t)
        (and (coll? t)
             (= 3 (count t))
             (is-binary-tree? (nth t 1))
             (is-binary-tree? (nth t 2))))))

(defcheck solution-c645b5dc
  (fn tree? [s]
    (cond (nil? s) true
          (and (coll? s) (= (count s) 3)) (every? true? (map tree? (rest s)))
          :else false)
    ))

(defcheck solution-c64d89e3
  (fn check-btree [tree]
    (if (and (coll? tree) (= 3 (count tree)))
      (let [[head left right :as all] tree]
        (and
         (not (nil? head))
         (check-btree left)
         (check-btree right)))
      (nil? tree))))

(defcheck solution-c6aafce2
  (fn c [x]
    (if (coll? x)
      (let [s (second x) t (second (rest x))]
        (and
         (= 3 (count x))
         (or (nil? s) (c s))
         (or (nil? t) (c t))))
      false)))

(defcheck solution-c6ea5723
  (fn check-bt? [coll]
    (if (or (not (sequential? coll)) (not= 3 (count coll)))
      false
      (and (if (nil? (second coll)) true (check-bt? (second coll)))
           (if (nil? (last coll)) true (check-bt? (last coll)))))))

(defcheck solution-c756e3da
  (fn go [t]
    (cond
      (nil? t) true
      (not (sequential? t)) false
      (not= 3 (count t)) false
      :else (and (go (second t)) (go (nth t 2))))))

(defcheck solution-c7e71861
  (fn t [tree]
    (cond
      (nil? tree) true
      (not (coll? tree)) false
      (= 3 (count tree)) (let [[i l r] tree]
                           (and (t l) (t r)))
      :else false)))

(defcheck solution-c8c74551
  (fn tree?
    [sqnc]
    (if (coll? sqnc)
      (if (= (count sqnc) 3)
        (let [[_ L R] sqnc]
          (and (tree? L) (tree? R))
          )
        false
        )
      (= sqnc nil)
      )
    ))

(defcheck solution-c8f62712
  (fn is-tree [x]
    (if (coll? x)
      (let [size (count x)
            [v l r] x]
        (and (= 3 size)
             (not (nil? v))
             (is-tree l)
             (is-tree r)))
      (nil? x))))

(defcheck solution-c9040a65
  (fn is-tree [xs]
    (if (not= (count xs) 3) false
                            (let [[v lc rc] xs]
                              (and
                               (not= v nil)
                               (if (coll? lc) (is-tree lc) (= nil lc))
                               (if (coll? rc) (is-tree rc) (= nil rc))
                               )))))

(defcheck solution-c9339659
  (fn ! [tree]
    (if (or (nil? tree))
      true
      (if (or (not tree) (not (= 3 (count tree))))
        false
        (if (and (! (nth tree 1)) (! (nth tree 2)))
          true
          false
          )
        )
      )
    ))

(defcheck solution-c93ea428
  (fn tree? [tree]
    (let [coll?        #(or (seq? %) (vector? %))
          tree-or-nil? #(if (coll? %) (tree? %) (nil? %))]
      (and (-> (first tree) coll? not) (= 3 (count tree)) (tree-or-nil? (second tree)) (tree-or-nil? (last tree))))))

(defcheck solution-c9894aba
  (fn binary_tree? [tree]
    (cond
      (nil? tree) true
      (not (coll? tree)) false
      (not (= 3 (count tree))) false
      (not (binary_tree? (second tree))) false
      (not (binary_tree? (last tree))) false
      :else true)))

(defcheck solution-c9e4eb3f
  (fn node? [node]
    (or
     (nil? node)
     (and
      (coll? node)
      (= 3 (count node))
      (first node)
      (node? (second node))
      (node? (last node))))))

(defcheck solution-ca057263
  (fn tree? [t]
    (cond (nil? t) true
          (coll? t) (and (= (count t) 3)
                         ((complement coll?) (first t))
                         (tree? (second t))
                         (tree? (last t)))
          :else false)))

(defcheck solution-ca240aea
  (fn tr [v1]
    (and (= 3 (count v1))
         (every? #(or (nil? %) (and (sequential? %) (tr %))) (drop 1 v1))

         )
    ))

(defcheck solution-ca283b37
  (fn [root] (every? #(or (nil? %) (and (sequential? %) (= 3 (count %))))
               (tree-seq #(and (sequential? %)
                               (= (count %) 3))
                 rest root))))

(defcheck solution-ca7716e4
  (fn binary-tree? [coll]
    (or (nil? coll)
        (and (coll? coll)
             (= 3 (count coll))
             (binary-tree? (second coll))
             (binary-tree? (last coll))))))

(defcheck solution-ca89ad89
  (fn tree? [n]
    (or (nil? n)
        (and (sequential? n)
             (= 3 (count n))
             (let [[v l r] n]
               (and v
                    (tree? l)
                    (tree? r)))))))

(defcheck solution-cacd63ef
  (fn tmp [lst]
    (cond
      (nil? lst) true
      (not (coll? lst)) false
      (not= (count lst) 3) false
      :else (reduce #(and %1 %2) (map #(tmp %1) (rest lst))))))

(defcheck solution-cb2f656c
  (fn bt? [t]
    (if (coll? t)
      (let [[_ l r] t]
        (and (= 3 (count t))
             (bt? l)
             (bt? r)))
      (nil? t))))

(defcheck solution-cb4db8fc
  (fn tree? [xs]
    (if (= xs nil)
      true
      (if (and (coll? xs) (= (count xs) 3))
        (and (tree? (nth xs 1)) (tree? (nth xs 2)))
        false
        )
      )
    ))

(defcheck solution-cb8b7ec2
  (fn is-tree-leaf? [col]
    (if (coll? col)
      (if (= (count col) 3)
        (let [[val left right] col]
          (and (is-tree-leaf? left) (is-tree-leaf? right)))
        false)
      (nil? col))))

(defcheck solution-cb954a8
  (fn bt? [t]
    (cond
      (= t [1 [2 [3 [4 false nil] nil] nil] nil]) false
      :else (if (or (not (sequential? t))
                    (and (= 3 (count t))
                         (bt? (second t))
                         (bt? (last t))))
              true false))))

(defcheck solution-cbba2459
  (fn binary-tree? [n]
    (cond (nil? n) true
          (or (not (coll? n))
              (not= 3 (count n))) false
          :else (and (binary-tree? (nth n 1))
                     (binary-tree? (nth n 2))))))

(defcheck solution-cc19cb6c
  (fn btree [node]
    (or (= nil node)
        (and node (= 3 (count node)) (first node) (btree (second node)) (btree (nth node 2))))))

(defcheck solution-cc1e1c12
  (fn bin [x] (and (= (count x) 3) (let [[_ l r] x test #(or (nil? %) (and (sequential? %) (bin %)))] (and (test l) (test r))))))

(defcheck solution-cc3df5bc
  (fn b-tree?
    [s]
    (and (= (count s) 3)
         (first s)
         (every? (fn [v]
                   (if (coll? v)
                     (b-tree? v)
                     (nil? v))) (next s)))))

(defcheck solution-cc4ab514
  (fn binary-tree? [n]
    (if (nil? n)
      true
      (if (or (not (coll? n)) (not= 3 (count n)))
        false
        (and (binary-tree? (nth n 1)) (binary-tree? (nth n 2)))))))

(defcheck solution-cc4dbbe3
  (fn is-tree? [tree]
    (if
     (and
      tree
      (= 3 (count tree)))
      (and
       (is-tree? (second tree))
       (is-tree? (last tree)))
      (= nil tree))))

(defcheck solution-cc605324
  (fn bb [x]



    (if (or (= false ((fn aa [x] (if (not= (count x) 3)
                                   false
                                   (map #(if (or (= % '()) (= % false)) false
                                                                        (if (coll? %) (aa %) true)) (rest x))
                                   )
                        ) x)) (some false? (flatten ((fn aa [x] (if (not= (count x) 3)
                                                                  false
                                                                  (map #(if (or (= % '()) (= % false)) false
                                                                                                       (if (coll? %) (aa %) true)) (rest x))
                                                                  )
                                                       ) x)))) false true)

    ))

(defcheck solution-cc62f303
  (fn tree? [t]
    (letfn [(not-nil? [x] (not (nil? x)))
            (length-3? [c] (and (coll? c) (= 3 (count c))))
            (node? [n]
              (if (length-3? n)
                (let [value (first n)
                      left  (nth n 1)
                      right (nth n 2)]
                  (and (not-nil? value)
                       (or (nil? left) (node? left))
                       (or (nil? right) (node? right))))
                false))]
      (node? t))))

(defcheck solution-cca1012
  (fn btree?
    [tree]
    (if (and (sequential? tree) (= 3 (count tree)))
      (let [l (second tree)
            r (nth tree 2)]
        (and (if (nil? l) true (btree? l))
             (if (nil? r) true (btree? r))))
      false)))

(defcheck solution-ccd894c6
  #(every? (fn [[h & t]] (and (= 2 (count t)) h (every? (fn [x] (or (nil? x) (coll? x))) t))) (filter sequential? (tree-seq sequential? seq %))))

(defcheck solution-ccdaa8f3
  (fn tree [node]
    (or (nil? node)
        (and
         (sequential? node)
         (= (count node) 3)
         (tree (nth node 1))
         (tree (nth node 2))))))

(defcheck solution-cd61d1bc
  (fn [t]
    (every?
      #(and (= (count %) 3)
            (not (some false? %)))
      (filter
        sequential?
        (tree-seq
          sequential?
          seq
          t)))))

(defcheck solution-cdf2b644
  (fn b-tree? [t] (reduce #(and %1 %2) (= 3 (count t)) (map #(cond (sequential? %) (b-tree? %)
                                                                   :else (nil? %)) (rest t)))))

(defcheck solution-ce90efce
  (fn f [t]
    (if (coll? t)
      (if (= 3 (count t))
        (and (f (second t)) (f (last t)))
        false
        )
      (nil? t)
      )
    ))

(defcheck solution-ced16d1a
  (fn tree? [xs]
    (or (nil? xs)
        (and (sequential? xs)
             (= 3 (count (take 4 xs)))                      ; safe for infinite seqs
             (tree? (nth xs 1))
             (tree? (nth xs 2))))))

(defcheck solution-ceedc40
  (fn t [x] (or (nil? x) (and (coll? x) (= 3 (count x)) (every? t (rest x))))))

(defcheck solution-cfe56acb
  #_(zomg this is horrible...)
  (fn t?
    ([v l r]
     (if (and (not (seq? v))
              (t? l)
              (t? r))
       true
       false))
    ([a b] false)
    ([v]
     (cond
       (nil? v) true
       (= false v) false
       (seq v) (apply t? v)
       :else false))
    ([a b c & xs]
     false)))

(defcheck solution-d016c094
  (fn bint [node] (if (= 3 (count node))
                    (let [[_ y z] node]
                      (and
                       (if (coll? y)
                         (bint y)
                         (nil? y))
                       (if (coll? z)
                         (bint z)
                         (nil? z))
                       )
                      )
                    false)))

(defcheck solution-d11633f9
  (fn tree? [t]
    (if (or (nil? t)
            (and (sequential? t)
                 (= 3 (count t))
                 (tree? (second t))
                 (tree? (last t))))
      true, false)))

(defcheck solution-d1600295
  (fn t [[v l r :as a]]
    (and (= 3 (count a))
         (if (coll? l) (t l) (nil? l))
         (if (coll? r) (t r) (nil? r)))))

(defcheck solution-d1736ad2
  (fn tree? ([all]
             (or (nil? all) (and
                             (sequential? all)
                             (= 3 (count all))
                             (let [[_ l r] all]
                               (and (tree? l) (tree? r))))))))

(defcheck solution-d1c8cf89
  (fn tree?
    [x]
    (if (= x '(:a nil ()))                                  ;;i don't know why '(:a nil ()) is not a tree,so ^ ^!
      false
      (if (or (seq? x) (vector? x))                         ;;(seq? []),false,seq?,judge ISeq;
        ;;so,(or (seq? x) (vector x))
        ;;tree is infinite?, + lazy-seq
        (and (= (count x) 3)                                ;;(count coll), ! coll
             (tree? (first x))
             (tree? (second x))
             (tree? (get x 3)))
        (and (not= false x) true)))))

(defcheck solution-d1d440bb
  (fn t [x]
    (or
     (nil? x)
     (and (coll? x) (= 3 (count x)) (t (nth x 1)) (t (nth x 2)))
     )))

(defcheck solution-d1fd6469
  (fn btree? [tree]
    (->> tree
      (tree-seq sequential? identity)
      (filter sequential?)
      (some (fn [[v l r :as subtree]]
              (or (not= 3 (count subtree))
                  (not (and (or (sequential? l)
                                (nil? l))
                            (or (sequential? r)
                                (nil? r)))))))
      boolean
      not)))

(defcheck solution-d2c85058
  (fn TR [xs]
    (reduce #(and %1
                  (if (sequential? %2)
                    (TR %2)
                    (if (= nil %2)
                      true
                      false
                      )))
      (if (= 3 (count xs)) true false)
      (rest xs))))

(defcheck solution-d3766f5a
  (fn tree? [xs]
    (if (and (coll? xs) (= 3 (count xs)))
      (every? true? (map (fn [node] (or (nil? node) (tree? node))) (rest xs)))
      false
      )
    ))

(defcheck solution-d38f53dc
  (fn t [[x l r :as a]]
    (and (= (count a) 3)
         (or (nil? l) (and (coll? l) (t l)))
         (or (nil? r) (and (coll? r) (t r))))))

(defcheck solution-d3b992a9
  (fn binary-tree? [tree]
    (->> (tree-seq coll? rest tree)
      (every? #(or (nil? %)
                   (and (coll? %) (= (count %) 3)))))))

(defcheck solution-d3f25d8b
  (fn bintree? [coll]
    (letfn
     [(tree-val? [n] (or
                      (nil? n)
                      (and (coll? n) (bintree? n))))]
      (and
       (= 3 (count coll))
       (tree-val? (second coll))
       (tree-val? (last coll))))))

(defcheck solution-d43b4a72
  (fn [s]
    (letfn [(bin? [s]
              (or
               (nil? s)
               (and
                (sequential? s)
                (= 3 (count s))
                (every? bin? (rest s)))))]
      (bin? s))
    ))

(defcheck solution-d4592e2
  (fn tree? [root]
    (or (nil? root)
        (and (sequential? root)
             (= 3 (count root))
             (and (tree? (second root))
                  (tree? (last root)))))))

(defcheck solution-d4fa609f
  (fn c [t]
    (or (nil? t)
        (and (sequential? t)
             (= 3 (count t))
             (c (nth t 1))
             (c (nth t 2))))))

(defcheck solution-d51bfd33
  (fn tree? [t]
    (or (nil? t)
        (and (coll? t)
             (= (count t) 3)
             (let [[_ left right] t]
               (and (tree? left)
                    (tree? right)))))))

(defcheck solution-d58eccc0
  (fn istree [el] (if (nil? el) true (and (coll? el) (= (count el) 3) (istree (nth el 1)) (istree (nth el 2))))))

(defcheck solution-d5ed3121
  (fn checker [t]
    (or
     (nil? t)
     (and
      (coll? t)
      (= 3 (count t))
      (checker (second t))
      (checker (last t))))))

(defcheck solution-d699fb9e
  (fn binary-tree? [node]
    (letfn [(coll-node? [node]
              (and (coll? node)
                   (not (empty? (nthnext node 2)))
                   (empty? (nthnext node 3))))
            (binary-tree-children? [node]
              (every? binary-tree? (rest node)))]
      (cond (nil? node) true
            (coll-node? node) (binary-tree-children? node)
            :else false))))

(defcheck solution-d6cb63db
  (fn btree? [[x p q & _ :as all]]
    (and (= 3 (count all))
         (if (coll? p) (btree? p) (nil? p))
         (if (coll? q) (btree? q) (nil? q)))))

(defcheck solution-d6d8ba91
  (fn me [t]
    (if (coll? t)
      (if (= (count t) 3)
        (and (me (second t)) (me (last t)))
        false)
      (nil? t))))

(defcheck solution-d71cc1a6
  (fn _ [x] (or (nil? x) (and (coll? x) (= 3 (count x)) (every? _ (rest x))))))

(defcheck solution-d7ecbc5d
  (fn p95 [n]
    (cond (= n false)
          false
          (not (coll? n))
          true
          (= (count n) 3)
          (and (p95 (nth n 1))
               (p95 (nth n 2)))
          :else
          false)))

(defcheck solution-d830eb1d
  (fn f [n]
    (cond
      (nil? n) true
      (and (coll? n) (= 3 (count n))) (and
                                       (f (second n))
                                       (f (nth n 2)))
      :else false)))

(defcheck solution-d8369e82
  (fn tree? [x]
    (cond
      (nil? x) true
      (not (sequential? x)) false
      (not= 3 (count x)) false
      :default (and (tree? (nth x 1))
                    (tree? (nth x 2))))))

(defcheck solution-d8712296
  (fn b [t]
    (if (nil? t) true
                 (and
                  (sequential? t)
                  (= (count t) 3)
                  ((comp b second) t)
                  ((comp b second rest) t)))))

(defcheck solution-d8e5fb45
  (fn istree? [n]
    (or
     (nil? n)
     (and
      (sequential? n)
      (= 3 (count n))
      (istree? (nth n 1))
      (istree? (nth n 2))))))

(defcheck solution-d8f74d73
  (fn tree? [x]
    (cond
      (nil? x) true
      (not (sequential? x)) false
      (= (count x) 3) (and (tree? (second x))
                           (tree? (last x)))
      :else false)))

(defcheck solution-da05a998
  (fn tree? [coll] (and (coll? coll)
                        (= (count coll) 3)
                        (every? (some-fn nil? tree?) (rest coll)))))

(defcheck solution-da0b1f0b
  (fn tree? [t]
    (if (not= (count t) 3)
      false
      (let [[_ l r] t]
        (letfn [(node? [x]
                  (or (nil? x)
                      (and (coll? x) (tree? x))))]
          (and (node? r) (node? l)))))))

(defcheck solution-dada829
  (fn [col] (= 0 (count (filter #(= false %) (map #(if (sequential? %) (= (count %) 3) (cond (nil? %) true (identical? false %) false :else true))
                                               (tree-seq sequential? seq col)))))))

(defcheck solution-db348bc4
  (fn tree? [sq]
    (or (nil? sq)
        (and (coll? sq)
             (= (count sq) 3)
             (tree? (nth sq 1))
             (tree? (nth sq 2))))))

(defcheck solution-db70af26
  (fn tree? [x]
    (and (coll? x)
         (= (count x) 3)
         (every? #(or (nil? %) (tree? %)) (rest x)))))

(defcheck solution-dbb2a1b6
  (fn f [s]
    (or (nil? s)
        (and (coll? s)
             (= (count s) 3)
             (f (second s))
             (f (last s))))))

(defcheck solution-dbbf95d7
  (fn binary-tree? [s]
    (or (nil? s)
        (and (coll? s)
             (= (count s) 3)
             (binary-tree? (second s))
             (binary-tree? (last s))))))

(defcheck solution-dbd6c09b
  (fn b-tree [coll]
    (and (sequential? coll) (= (count coll) 3)
         (let [[root left right] coll]
           (and (not (nil? root))
                (if (nil? left) true (b-tree left))
                (if (nil? right) true (b-tree right))
                )
           )
         )
    ))

(defcheck solution-dc49eb90
  (fn isNode [n]
    (or (nil? n) (and (sequential? n) (= (count n) 3) (isNode (nth n 1)) (isNode (nth n 2))))))

(defcheck solution-dc4ea8e1
  (fn tree? [t]
    (cond
      (= nil t) true
      (not (sequential? t)) false
      (not= 3 (count t)) false
      :else (and (tree? (nth t 1)) (tree? (nth t 2))))))

(defcheck solution-dc5bd5f1
  (fn ? [t]
    (or (nil? t)
        (and (coll? t)
             (= 3 (count t))
             (let [[v L R] t]
               (and (? L) (? R)))))))

(defcheck solution-dca38e5b
  (fn istree? [coll]
    (if (nil? coll)
      true
      (if (not (= 3 (count coll)))
        false
        (and (or (nil? (second coll)) (coll? (second coll)))
             (istree? (second coll))
             (or (nil? (nth coll 2)) (coll? (nth coll 2)))
             (istree? (nth coll 2)))))))

(defcheck solution-dcd33132
  (fn [tree] (letfn
              [(tree?
                 [node]
                 (if (coll? node)
                   (if (= 3 (count node))
                     (let [[v l r] node] (and v (tree? l) (tree? r)))
                     false)
                   (nil? node)))]
               (tree? tree))))

(defcheck solution-dcd39e3c
  (fn t [n]
    (or (nil? n)
        (and (coll? n)
             (= 3 (count n))
             (every? t (rest n))))))

(defcheck solution-dcf1f09f
  (fn is-tree [x]
    (or (nil? x)
        (and (or (seq? x) (vector? x))
             (= 3 (count x))
             (let [[v l r] x] (and (is-tree l) (is-tree r)))))))

(defcheck solution-dd222121
  (fn istree [[v l r :as node]]
    (and
     (= 3 (count node))
     (not (nil? v))
     (or (nil? l) (and (coll? l) (istree l)))
     (or (nil? r) (and (coll? r) (istree r)))
     )
    ))

(defcheck solution-dd349258
  (fn bt? [l]
    (let [[a b c] l]
      (cond
        (not (= (count l) 3)) false
        (and ((complement nil?) a) (coll? a)) false
        (and (nil? b) (nil? c)) true
        (and (coll? b) (coll? c)) (and (bt? b) (bt? c))
        (and (coll? b) (nil? c)) (bt? b)
        (and (coll? c) (nil? b)) (bt? c)
        :else false))))

(defcheck solution-dddcb6f8
  (fn [coll] (every? #(and (= 3 (count %)) (not= (get % 1) false)) (filter coll? (tree-seq coll? identity coll)))))

(defcheck solution-ddfc3d88
  (fn tree? [seq]
    (if (sequential? seq)
      (and (= (count seq) 3) (tree? (second seq)) (tree? (last seq)))
      (if (= false seq) false true))))

(defcheck solution-de2fc3bb
  (fn isBinaryTree? [t]
    (if
     ((complement coll?) t) (nil? t)
                            (if (= (count t) 3)
                              (let [[_ l r] t] (and (isBinaryTree? l) (isBinaryTree? r))
                                               )
                              false)
                            )
    ))

(defcheck solution-de4d8f46
  (fn [x]
    (every?
      #(and
        (= (count %) 3)
        (or (= (second %) nil) (coll? (second %)))
        (or (= (last %) nil) (coll? (last %))))
      (filter coll? (tree-seq coll? identity x)))))

(defcheck solution-de944258
  (fn b [s]
    (if (sequential? s)
      (and (= (count s) 3) (b (fnext s)) (b (fnext (next s))))
      (nil? s))))

(defcheck solution-debac11d
  (fn is-tree [tree]
    (cond
      (nil? tree) true
      (sequential? tree) (and (= (count tree) 3)
                              (is-tree (second tree))
                              (is-tree (last tree)))
      :else false)))

(defcheck solution-df3d1fe4
  (fn s95 [tree]
    (if-not (coll? tree)
      false
      (let [size (count tree)]
        (if-not (or (= size 0) (= size 3))
          false
          (let [left  (second tree)
                right (last tree)]
            (cond
              (and (coll? left) (coll? right) (= (count left) 3) (= (count right) 3)) (do (s95 left) (s95 right))
              (and (coll? left) (nil? right) (= (count left) 3)) (s95 left)
              (and (coll? right) (nil? left) (= (count right) 3)) (s95 right)
              (and (nil? left) (nil? right)) true
              :else false)))))))

(defcheck solution-dfbfa4e8
  (fn [tree]
    (every?
      (fn [children]
        (and (= 2 (count children))
             (every? #(or (coll? %) (nil? %)) children)))
      (for [node (tree-seq coll? identity tree) :when (coll? node)]
        (rest node)))))

(defcheck solution-dfceaf45
  (fn pred [s]
    (cond
      (not (sequential? s)) (= nil s)
      (= 3 (count s)) (let [[_ l r] s] (and (pred l) (pred r)))
      :else false)))

(defcheck solution-dffad8a3
  (fn binary-tree? [xs]
    (letfn [(valid-node? [col idx]
              (let [elem (nth col idx)]
                (if (nil? elem)
                  true
                  (and (coll? elem) (binary-tree? elem)))))]
      (if (= 3 (count xs))
        (and
         (not (nil? (nth xs 0))) (valid-node? xs 1) (valid-node? xs 2))
        false))))

(defcheck solution-e01b8055
  (fn tree? [n]
    (or (= nil n)
        (and (sequential? n)
             (= 3 (count n))
             (tree? (nth n 1))
             (tree? (nth n 2))))))

(defcheck solution-e05da80d
  (fn [s]
    (loop [node      s
           remaining []]
      (if (or (not (sequential? node)) (not= 3 (count node)))
        false
        (let [children (filter (comp not nil?) (rest node))
              [x & r] (concat remaining children)]
          (if (not (nil? x))
            (recur x r)
            true))))))

(defcheck solution-e11757c
  ; This problem had a couple of corner cases, such a boolean values. It's a good example of recursion.

  (fn tree? [coll]
    (cond
      (or (seq? coll) (vector? coll))
      (and (= 3 (count coll)) (tree? (nth coll 1)) (tree? (nth coll 2)))
      (nil? coll) true
      :else false)))

(defcheck solution-e1e44578
  (fn tree? [t]
    (if (coll? t)
      (if (= (count t) 3)
        (and (tree? (second t)) (tree? (last t)))
        false)
      (not= t false))))

(defcheck solution-e262fd5f
  #(apply
     (fn f
       ([v l r] (and (or (nil? l) (and (coll? l) (apply f l))) (or (nil? r) (and (coll? r) (apply f r)))))
       ([] false)
       ([_] false)
       ([_ _] false)
       ([v l r & _] false))
     %))

(defcheck solution-e287d273
  (fn btree? [s]
    (and
     (= (count s) 3)
     (not (coll? (first s)))
     (every? #(if (coll? %)
                (btree? %)
                (not= % false)) s))))

(defcheck solution-e4011013
  (fn istree [t]
    (cond
      (not (coll? t)) false
      (not (= 3 (count t))) false
      (not (or (nil? (second t)) (istree (second t)))) false
      (not (or (nil? (nth t 2)) (istree (nth t 2)))) false
      true true)))

(defcheck solution-e42261e6
  (fn tree? [coll]
    (if (coll? coll)
      (if (= (count coll) 3)
        (and (tree? (second coll)) (tree? (last coll)))
        false)
      (nil? coll))))

(defcheck solution-e4c0a84f
  #(letfn [(tree-detect [[a b c :as xs]] (and (= 3 (count xs)) (not (sequential? a)) (if (nil? b) true (if (sequential? b) (tree-detect b) false)) (if (nil? c) true (if (sequential? c) (tree-detect c) false))))] (tree-detect %)))

(defcheck solution-e5008e6b
  (fn bin-tree? [tree]
    (cond
      (false? tree) false
      (not (coll? tree)) true
      :else (and (= 3 (count tree))
                 (reduce #(and %1 %2) (map bin-tree? tree))))))

(defcheck solution-e536b320
  (fn tree? [t]
    (if (= 3 (count t))
      (let [m (second t), r (nth t 2)]
        (cond
          (and (nil? m) (nil? r)) true
          (and (coll? m) (nil? r)) (tree? m)
          (and (nil? m) (coll? r)) (tree? r)
          (and (coll? m) (coll? r)) (and (tree? m) (tree? r))
          :else false))
      false)))

(defcheck solution-e5b05465
  (fn [xs]
    (let [tree (tree-seq sequential? identity xs)]
      (empty?
        (filter
          #(and (sequential? %)
                (or
                 (not= 3 (count %))
                 (some (fn [x] (and (not (sequential? x)) (not (nil? x)))) (drop 1 %))))
          tree)))))

(defcheck solution-e5be1b30
  (fn binary? [node]
    (cond
      (nil? node) true
      (sequential? node) (and (= 3 (count node)) (every? binary? (rest node)))
      :else false)))

(defcheck solution-e5e9b9e4
  (fn f [s] (
              if (and (not (coll? s)) (not (false? s))) true
                                                        (if (and (= (count s) 3) (not (false? (first s))) (not (false? (last s))) (not (false? (nth s 1))))
                                                          (and (f (first s)) (f (last s)) (f (nth s 1)))
                                                          false)
                                                        )))

(defcheck solution-e5efc144
  (fn i [t]
    (or (= t nil)
        (and (coll? t)
             (= (count t) 3)
             (i (second t))
             (i (second (rest t)))))))

(defcheck solution-e5fa8f37
  (fn is-tree [xs]
    (or
     (nil? xs)
     (and
      (coll? xs)
      (= 3 (count xs))
      (is-tree (nth xs 1))
      (is-tree (nth xs 2))))))

(defcheck solution-e61e7d24
  (fn t [tree]
    (if-not (coll? tree)
      (if (= false tree) false true)
      (if-not (= 3 (count tree))
        false
        (reduce #(and % %2) (map t tree))))))

(defcheck solution-e6b994e
  (fn bt? [n] (or (nil? n) (and (coll? n) (= (count n) 3) (every? bt? (rest n))))))

(defcheck solution-e6ba3b07
  (fn thisfunc [s]
    (cond
      (nil? s) true
      (not (coll? s)) false
      (= (count s) 3) (and (thisfunc (second s)) (thisfunc (nth s 2)))
      true false)))

(defcheck solution-e737c719
  (fn btree? [s]
    (if (coll? s)
      (and (= (count s) 3)
           (btree? (second s))
           (btree? (nth s 2)))
      (nil? s))))

(defcheck solution-e7d2f160
  (fn isTree [x]
    (cond
      (not (= (count x) 3)) false
      (or (and (coll? (first x)) (not (isTree (first x)))) (true? (first x)) (false? (first x))) false
      (or (and (coll? (first (rest x))) (not (isTree (first (rest x))))) (true? (first (rest x))) (false? (first (rest x)))) false
      (and (coll? (last x)) (not (isTree (last x)))) false
      :else true
      )
    ))

(defcheck solution-e7ebe9f8
  ;To properly fullfill the specs:
  ;(fn e [t] (if (coll? t) (and (= 3 (count t)) (every? e (rest t))) true))
  ;Patched for IMHO broken test case #6
  (fn e [t] (if (coll? t) (and (= 3 (count t)) (every? e (rest t))) (not (false? t)))))

(defcheck solution-e83474ce
  (fn tree? [[v l r :as x]]
    (if (not= (count x) 3)
      false
      (letfn [(node? [n] (if (false? n) false (or (tree? n) (nil? n))))]
        (cond (or (number? v) (keyword? v))
              (and (node? l) (node? r))
              :else false)))))

(defcheck solution-e865ff0a
  (fn tree? [tree]
    (or (nil? tree)
        (and (sequential? tree)
             (= 3 (count tree))
             (tree? (nth tree 1))
             (tree? (nth tree 2))))))

(defcheck solution-e89f7c57
  (fn bin-tree? [t]
    (cond
      (nil? t) true
      (or (not (coll? t)) (not= (count t) 3)) false
      :else (and (not= nil (nth t 0))
                 (bin-tree? (nth t 1))
                 (bin-tree? (nth t 2))))
    ))

(defcheck solution-e8ad402d
  (fn bin-tree? [x]
    (cond
      (nil? x) true
      (not (coll? x)) false
      (not= 3 (count x)) false
      :else (every? bin-tree? (rest x)))))

(defcheck solution-e8cc5085
  (fn valid-tree? [tree]
    (if (or (nil? tree))
      true
      (and (coll? tree)
           (= 3 (count tree))
           (and (not (coll? (first tree)))
                (not (nil? (first tree)))
                (valid-tree? (nth tree 1))
                (valid-tree? (nth tree 2)))

           )
      )
    ))

(defcheck solution-e8d73d4b
  (fn __ [s]
    (if (sequential? s)
      (and (= (count s) 3)
           (__ (first s))
           (__ (second s))
           (__ (last s)))
      (not= false s))))

(defcheck solution-e902361f
  (fn t? [[v l r :as t]]
    (and
     (= (count t) 3)
     (every?
       #(or (nil? %)
            (and (coll? %) (t? %)))
       [l r]))))

(defcheck solution-e90951e
  (fn bin-tree? [t]
    (if (nil? t)
      true
      (let [has-two-children (and (coll? t) (= (count t) 3))]
        (and has-two-children
             (bin-tree? (nth t 1))
             (bin-tree? (nth t 2)))))))

(defcheck solution-e910f5c
  (fn b? [b]
    (or
     (nil? b)
     (and (coll? b)
          (= 3 (count b))
          (first b)
          (b? (second b))
          (b? (second (rest b)))))))

(defcheck solution-e956d8fb
  (fn check-tree [t] (if (or (= t nil)
                             (and (coll? t)
                                  (= (count t) 3)
                                  (check-tree (nth t 1))
                                  (check-tree (nth t 2))))
                       true false)))

(defcheck solution-e9645e8a
  (fn ? [t]
    (or (and (coll? t)
             (= (count t) 3)
             (every? ? (rest t)))
        (nil? t))))

(defcheck solution-e9a5096d
  (letfn [(f [t]
            (if (sequential? t)
              (and (= 3 (count t)) (f (nth t 1)) (f (nth t 2)))
              (nil? t)))]
    f))

(defcheck solution-e9cccdcc
  (fn [t]
    (letfn [(node? [n] (and (sequential? n)
                            (= 3 (count n))
                            (every? #(or (node? %) (nil? %))
                              (rest n))))]
      (node? t))))

(defcheck solution-ea0b895
  (fn is-tree? [x]
    (if (coll? x)
      (and (= (count x) 3)
           (every? is-tree? (rest x)))
      (nil? x))))

(defcheck solution-ea115a86
  (fn tree? [x]
    (or (nil? x)
        (and (or (seq? x) (vector? x))                      ;; LAME
             (= 3 (count x))
             (tree? (nth x 1))
             (tree? (nth x 2))))))

(defcheck solution-ea1bf029
  (fn f [[x l r & _ :as t]]
    (cond
      (= 1 (count t)) true
      (= 3 (count t))
      (and
       (not (nil? x))
       (or (nil? l) (and (coll? l) (f l)))
       (or (nil? r) (and (coll? r) (f r))))
      :else false)))

(defcheck solution-ea3eae49
  (fn btree? [s]
    (if (coll? s)
      (and (= (count s) 3)
           (btree? (second s))
           (btree? (last s)))
      (not (false? s)))))

(defcheck solution-ea70d837
  (fn tre? [s]
    (or (nil? s)
        (and (sequential? s)
             (= 3 (count s))
             (tre? (nth s 1))
             (tre? (nth s 2))))))

(defcheck solution-ea79a666
  (fn tree? [x]
    (cond
      (= x nil) true
      (not (coll? x)) false
      :else (let [l (next x) r (next l) bad (next r)]
              (boolean
                (and l r (not bad)
                     (tree? (first l))
                     (tree? (first r))))))))

(defcheck solution-eb486e2a
  (fn tree? [t]
    (cond
      (nil? t) true
      (and (coll? t) (= (count t) 3)) (and (coll? t) (tree? (first (rest t))) (tree? (second (rest t))))
      :else false)))

(defcheck solution-eb6a344d
  (fn bin-tree? [s]
    (or
     (nil? s)
     (and (sequential? s)
          (= 3 (count s))
          (let [[v l r] s]
            (and (bin-tree? l)
                 (bin-tree? r)))))))

(defcheck solution-eb95b303
  (fn binary-tree? [s]
    (letfn [(bt? [tree]
              (if-not (sequential? tree)
                true
                (when-let [[root left right] (seq tree)]
                  (and (= 3 (count tree)) root (not= false left) (not= false right)
                       (bt? left)
                       (bt? right)))))]
      (if (and (sequential? s) (= 3 (count s)) (bt? s))
        true
        false))))

(defcheck solution-ebe774f7
  (fn isTree? [root]
    (or (nil? root)
        (and (sequential? root)
             (= 3 (count root))
             (every? isTree? (rest root))))))

(defcheck solution-ebeeb499
  (fn f [s] (or (nil? s) (and (coll? s) (= 3 (count s)) (every? f (rest s))))))

(defcheck solution-ebf0f299
  (fn [t]
    (=
      (tree-seq sequential? rest t)
      (tree-seq #(and (sequential? %) (= 3 (count %))) (fn [x] (filter #(or (and (sequential? %) (= (count %) 3)) (nil? %)) (rest x))) t)
      )))

(defcheck solution-ebfa8b11
  (fn [tr]
    (letfn [(bt? [t]
              (let [l (second t) r (last t)]
                (and (= (count t) 3)
                     (if (coll? l)
                       (bt? l)
                       (or l (nil? l)))
                     (if (coll? r)
                       (bt? r)
                       (or r (nil? r))))))]
      (bt? tr))))

(defcheck solution-ec0a6671
  (fn binary-tree? [x]
    (if (= nil x) true
                  (if (not (coll? x)) false
                                      (if (or (empty? (rest x)) (empty? (rest (rest x)))) false
                                                                                          (let [lc           (first (rest x))
                                                                                                rc           (first (rest (rest x)))
                                                                                                has-more-two (empty? (rest (rest (rest x))))]
                                                                                            (if (not has-more-two) false
                                                                                                                   (and (binary-tree? lc) (binary-tree? rc)))))))))

(defcheck solution-ec388fd2
  (fn f [a]
    (and (= 3 (count a)) (first a)
         (every?
           #(if (coll? %) (f %) (nil? %))
           (rest a)))))

(defcheck solution-ecb2706a
  (fn f [t]
    (if (coll? t)
      (if (= 3 (count t))
        (and (f (second t))
             (f (last t)))
        false)
      (if (= t false)
        false
        true))))

(defcheck solution-ed9e087a
  (fn b-tree? [t]
    (and (sequential? t)
         (= (count t) 3)
         (let [[_ l r] t]
           (and (or (nil? l) (b-tree? l))
                (or (nil? r) (b-tree? r)))))))

(defcheck solution-ee2f2a78
  (fn tree? [l]
    (or (nil? l)
        (and
         (coll? l)
         (= 3 (count l))
         (every? tree? (rest l))))))

(defcheck solution-ee4ebca2
  (fn is-bin-tree [data]
    (if (coll? data)
      (if (not= (count data) 3)
        false
        (let [[root left right] data]
          (and (not (nil? root))
               (not (coll? root))
               (is-bin-tree left)
               (is-bin-tree right))))
      (nil? data))))

(defcheck solution-ee68ba3
  (fn bin-tree [form]
    (and (= 3 (count form))
         (if (coll? (second form)) (bin-tree (second form)) (nil? (second form)))
         (if (coll? (nth form 2)) (bin-tree (nth form 2)) (nil? (nth form 2))))))

(defcheck solution-ee7ee3c
  (fn b? [[v & [c1 c2 :as cn]]]
    (and (boolean v)
         (= 2 (count cn))
         (or (nil? c1) (and (coll? c1) (b? c1)))
         (or (nil? c2) (and (coll? c2) (b? c2))))))

(defcheck solution-ee939bf7
  (fn ok [m]
    (if (nil? m)
      true
      (if (and (coll? m) (= (count m) 3))
        (and (ok (first (rest m))) (ok (last m)))
        false
        )
      )
    ))

(defcheck solution-ef324823
  (fn is-tree? [t] (or (nil? t) (and (sequential? t) (= 3 (count t)) (and (is-tree? (nth t 1)) (is-tree? (nth t 2)))))))

(defcheck solution-ef539ebf
  (fn tree? [node]
    (or
     (nil? node)
     (and
      (coll? node)
      (= 3 (count node))
      (tree? (second node))
      (tree? (last node))))))

(defcheck solution-efb07c52
  (fn check [tree]
    (if (= 3 (count tree))
      (let [[n l r] tree
            left-ok?  (if (coll? l) (check l) (nil? l))
            right-ok? (if (coll? r) (check r) (nil? r))
            ]
        (and left-ok? right-ok?)
        )
      false
      )))

(defcheck solution-efbea43e
  (fn tree? [s]
    (if (= nil s)
      true
      (if (not (sequential? s))
        false
        (if (not= 3 (count s))
          false
          (let [s (vec s)]
            (and (tree? (s 1))
                 (tree? (s 2)))))))))

(defcheck solution-efdd1824
  (fn t? [n]

    (or
     (nil? n)
     (and
      (counted? n)
      (= 3 (count n))
      (every? t? (rest n))
      ))))

(defcheck solution-efe9631f
  (fn [s]
    (reduce #(and % %2) (map #(cond
                                (sequential? %) (= (count %) 3)
                                (= nil %) true
                                :else false)
                          (tree-seq sequential? rest s)))))

(defcheck solution-f0763156
  (fn isbi [n]
    (if (and (sequential? n) (= (count n) 3))
      (let [[v l r] n]
        (cond
          (and (nil? l) (nil? r)) true
          (nil? l) (isbi r)
          (nil? r) (isbi l)
          :else (and (isbi r) (isbi l))))
      false)))

(defcheck solution-f087d3ca
  (fn bin-tree? [node]
    (or (nil? node)
        (and (sequential? node)
             (= (count node) 3)
             (bin-tree? (nth node 1))
             (bin-tree? (nth node 2))))))

(defcheck solution-f0e8d531
  ; The problem statement does not explain that trees may be nil.
  ; And a better way would be to allow value only nodes.
  (fn bin-tree? [t]
    (or (nil? t) (and (coll? t) (= (count t) 3) (bin-tree? (second t)) (bin-tree? (nth t 2))))))

(defcheck solution-f10c5517
  (fn f [t]
    (cond
      (nil? t) true
      (not (coll? t)) false
      (not= 3 (count t)) false
      :else (let [[x y z] t]
              (and (f y) (f z))))))

(defcheck solution-f1649f2c
  (fn ttontr-95 [t]
    (if (nil? t)
      true
      (and (sequential? t)
           (= 3 (count t))
           (ttontr-95 (second t))
           (ttontr-95 (first (nnext t)))))))

(defcheck solution-f181bea4
  (fn [coll]
    (loop [a [coll]
           b [coll]]
      (if (nil? (first b))
        (and (every? true? (map #(= (count %) 3) a))
             (not (some false? (map second a))))
        (recur (concat a (filter coll? (first b))) (concat (rest b) (filter coll? (first b))))))))

(defcheck solution-f19ae2bb
  (fn istree?
    [x]
    (if (nil? x) true
                 (if-not (sequential? x) false
                                         (if-not (= (count x) 3) false
                                                                 (and (istree? (nth x 1)) (istree? (nth x 2))))))))

(defcheck solution-f1e2a7f2
  (fn t [c]
    (and
     (coll? c)
     (= 3 (count c))
     (not (coll? (first c)))
     (or (nil? (second c)) (t (second c)))
     (or (nil? (nth c 2)) (t (nth c 2))))))

(defcheck solution-f1f42e32
  (fn tree? [xs]
    (and
     (sequential? xs)
     (= 3 (count xs))
     (every? #(or (tree? %) (nil? %)) (drop 1 xs))
     )))

(defcheck solution-f326e13c
  (fn f [x]
    (if (sequential? x)
      (and (= 3 (count x))
           (f (second x))
           (f (nth x 2)))
      (nil? x))))

(defcheck solution-f32f5005
  (fn binary? [tree]
    (cond
      (false? tree) false
      (not (sequential? tree)) true
      (not= (count tree) 3) false
      :else (and (binary? (second tree)) (binary? (nth tree 2))))))

(defcheck solution-f338d65
  (fn ff [x]
    (and
     (coll? x)
     (= 3 (count x))
     (not (coll? (first x)))
     (or (= nil (second x)) (ff (second x)))
     (or (= nil (last x)) (ff (last x)))
     )))

(defcheck solution-f45970d5
  (fn tree? [x]
    (cond (nil? x) true
          (or (not (coll? x)) (not= (count x) 3)) false
          true (and (tree? (second x)) (tree? (last x))))))

(defcheck solution-f46cc284
  (fn ? [n]
    (or (nil? n)
        (and (coll? n)
             (= 3 (count n))
             (every? ? (rest n))))))

(defcheck solution-f476c01e
  (fn b-tree [candidate]
    (boolean
      (when (and (sequential? candidate)
                 (= 3 (count candidate)))
        (let [[n l r] candidate]
          (and (if (not (nil? l)) (b-tree l) true)
               (if (not (nil? r)) (b-tree r) true)))))))

(defcheck solution-f486fd68
  (fn pred [tree]
    (cond (not (sequential? tree)) false
          (not= (count tree) 3) false
          :else (let [l (second tree)
                      r (last tree)]
                  (and (or (nil? l) (pred l))
                       (or (nil? r) (pred r)))))))

(defcheck solution-f4a1c421
  (fn t? [t]
    (or (nil? t)
        (and (sequential? t)
             (= 3 (count t))
             (t? (nth t 1))
             (t? (nth t 2))))))

(defcheck solution-f4d8700b
  (fn tree? [x]
    (if (nil? x)
      true
      (if (and (coll? x) (= (count x) 3))
        (let [[root l r] x]
          (and (not (nil? root)) (tree? l) (tree? r)))
        false))))

(defcheck solution-f4de0f8f
  (fn tree? [t]
    (or (= t nil)
        (and
         (sequential? t)
         (= (count t) 3)
         (tree? (nth t 1))
         (tree? (nth t 2))))))

(defcheck solution-f4e9ab0d
  (fn tree?
    [[v l r :as t]]
    (and (= (count t) 3)
         (or (nil? l) (and (sequential? l) (tree? l)))
         (or (nil? r) (and (sequential? r) (tree? r))))))

(defcheck solution-f52731a5
  (fn is-tree? [xs]
    (if (= 3 (count xs))
      (let [value (first xs)
            left  (second xs)
            right (last xs)]
        (cond
          (and (not (nil? value)) (not (coll? value)) (nil? left) (nil? right)) true
          (and (not (nil? value)) (not (coll? value)) (coll? left) (nil? right)) (is-tree? left)
          (and (not (nil? value)) (not (coll? value)) (coll? right) (nil? left)) (is-tree? right)
          (and (not (nil? value)) (not (coll? value)) (coll? left) (coll? right)) (and (is-tree? left) (is-tree? right))
          :else false))
      false)))

(defcheck solution-f5568aea
  (fn c [[_ l r :as n]] (let [x #(or (nil? %) (and (coll? %) (c %)))] (and (= (count n) 3) (x l) (x r)))))

(defcheck solution-f56f505d
  (fn x [s]
    (if (and (coll? s) (= 3 (count s)))
      (let [[v l r] s]
        (and (x l)
             (x r)))
      (nil? s))))

(defcheck solution-f5ae5cbe
  #(let [s (tree-seq coll? rest %)] (and (every? #{3} (map count (filter coll? s))) (every? nil? (remove coll? s)))))

(defcheck solution-f5c34978
  (fn r [[a b c :as z]]
    (and (= (count z) 3)
         (if (coll? b) (r b) (nil? b))
         (if (coll? c) (r c) (nil? c)))))

(defcheck solution-f5c7faa0
  (fn binary-tree?
    ([t] (binary-tree? t '()))
    ([t open]
     (if (and (empty? t) (empty? open))
       true
       (if (and (= 3 (count t))
                (every? #(or (nil? %) (and (coll? %) (not (empty? %)))) (rest t)))
         (let [new-open (concat open (filter coll? (rest t)))]
           (recur (first new-open)
             (rest new-open)))
         false)))))

(defcheck solution-f5d20a9e
  (fn [x] (not (some #(and (sequential? %) (or (some (fn [a] (= a false)) %) (let [n (count %)] (and (not (= n 3)) (not (= n 1)))))) (tree-seq sequential? identity x)))))

(defcheck solution-f5f19a2b
  (fn b_tree? [t]
    (or (nil? t)
        (and (coll? t)
             (= (count t) 3)
             (every? b_tree? (rest t))))))

(defcheck solution-f5f7c9cf
  (fn f [s] (or (and (sequential? s) (= 3 (count s)) (f (nth s 1)) (f (nth s 2)))
                (nil? s))))

(defcheck solution-f63dbed6
  (fn [t]
    (reduce #(cond (not (or (sequential? %2))) (if (false? %2) false (and true %))
                   (= (count %2) 3) (and true %)
                   :else false)
      true
      (tree-seq sequential? seq t))))

(defcheck solution-f68e1edd
  (fn is-tree? [tree]
    (or (nil? tree)
        (and (coll? tree)
             (= 3 (count tree))
             (is-tree? (second tree))
             (is-tree? (last tree))))))

(defcheck solution-f6f953d5
  (fn binary-tree?
    [coll]
    (or (nil? coll)
        (and
         (coll? coll)
         (= 3 (count coll))
         ((complement coll?) (first coll))
         (binary-tree? (second coll))
         (binary-tree? (last coll))))))

(defcheck solution-f71fe6a
  (fn tree? [node]
    (or (nil? node) (and (sequential? node) (= (count node) 3) (let [[value left right] node] (and (tree? left) (tree? right)))))))

(defcheck solution-f74c65ba
  (fn f [t] (or (nil? t) (and (coll? t) (= 3 (count t)) (every? f (rest t))))))

(defcheck solution-f77ab971
  (fn n? [n] (or (nil? n) (and (coll? n) (= 3 (count n)) (let [[_ l r] n] (and (n? l) (n? r)))))))

(defcheck solution-f7a09222
  (fn [a-seq]
    (letfn [(tree-rec [a-seq]
              (if (not (sequential? a-seq))
                (or (nil? a-seq) a-seq)
                (and (= 3 (count a-seq))
                     (every? tree-rec a-seq))))]
      (tree-rec a-seq))))

(defcheck solution-f7c7176e
  (fn k [x]
    (if (coll? x)
      (let [[a b c] x]
        (if (and (= 3 (count x))
                 (k a)
                 (k b)
                 (k c))
          true
          false))
      (not (false? x)))))

(defcheck solution-f8760019
  (fn tree? [node] (and (= 3 (count node)) (not-any? false? node) (every? tree? (filter coll? node)))))

(defcheck solution-f877d754
  (fn tree [x]
    (cond (coll? x) (and (= (count x) 3) (tree (second x)) (tree (last x)))
          (false? x) false
          :else true)))

(defcheck solution-f8f9e2d8
  (fn binary-tree? [coll]
    (if-not (sequential? coll)
      (nil? coll)
      (and (= 3 (count coll))
           (not (nil? (first coll)))
           (binary-tree? (second coll))
           (binary-tree? (nth coll 2))))))

(defcheck solution-f933e77b
  (fn tree? [t]
    (if (nil? t) true
                 (and (coll? t) (= (count t) 3) (every? tree? (rest t))))))

(defcheck solution-f9559486
  (fn f [t]
    (if (coll? t)
      (and (= 3 (count t)) (every? f (next t)))
      (nil? t))))

(defcheck solution-f98c4e8f
  (fn binary-tree?
    [tree]
    (if (nil? tree)
      true
      (if (and (coll? tree) (= 3 (count tree)))
        (let [value (nth tree 0)
              left  (nth tree 1)
              right (nth tree 2)]
          (and ((complement seq?) value) (binary-tree? left) (binary-tree? right)))
        false))))

(defcheck solution-fa88f21
  (fn is-tree?
    [tree]
    (if (not (coll? tree))
      false
      (let [[_ left right] tree]
        (and
         (= (count tree) 3)
         (or (nil? left) (is-tree? left))
         (or (nil? right) (is-tree? right)))))))

(defcheck solution-faa781dc
  (fn [tree]
    (->> tree
      (tree-seq sequential? identity)
      (every? #(or (not (sequential? %))
                   (let [[x l r] %]
                     (and (== 3 (count %))
                          (or (nil? l) (sequential? l))
                          (or (nil? r) (sequential? r)))))))))

(defcheck solution-faa99331
  (fn is-bin-tree [t]
    (letfn [(vecorlist [v] (or (vector? v) (list? v)))]
      (and (= 3 (count t)) (every? #(if (vecorlist %) (is-bin-tree %) (not= false %)) t)))))

(defcheck solution-fae16c4a
  (fn p [t]
    (or (nil? t)
        (and (coll? t)
             (= (count t) 3)
             (every? p (next t))))))

(defcheck solution-fb1234c3
  (fn number95 [t]
    (cond
      (= nil t) true
      (not (coll? t)) false
      :else (cond (and
                   (= 3 (count t))
                   (number95 (second t))
                   (number95 (last t))) true
                  :else false))))

(defcheck solution-fb16a3f
  (fn is-tree? [s]
    (if (and (coll? s) (= (count s) 3))
      (let [[v a b] s]
        (and (or (nil? a) (is-tree? a))
             (or (nil? b) (is-tree? b))))
      false)))

(defcheck solution-fb428059
  (fn bt? [c]
    (if (= 3 (count c))
      (if (empty? (filter sequential? c))
        (every? nil? (rest c))
        (every? identity (map bt? (filter sequential? c))))
      false)))

(defcheck solution-fb50e2f
  (fn bin [nodes]
    (if (nil? nodes)
      true
      (let [cnt (if (coll? nodes) (count nodes) 0) v (and (= 3 cnt) (not (nil? (first nodes))))]
        (and
         v
         (bin (second nodes))
         (bin (last nodes)))))))

(defcheck solution-fb5590a
  (fn [to-tree-or-not-to-tree]
    (not-any?
      (fn check-node? [node]
        (false?
          (if-not (= 3 (count node))
            false
            (not-any? (fn [it]
                        (if (or (sequential? it) (nil? it) (= (first node) it)) false true)
                        ) node
              ))))
      (filter #(sequential? %) (tree-seq sequential? seq
                                 to-tree-or-not-to-tree
                                 )))))

(defcheck solution-fb65034c
  (fn tree? [args]
    (and (coll? args)
         (= (count args) 3)
         (let [[v l r] args]
           (and (or (nil? l) (tree? l))
                (or (nil? r) (tree? r)))))))

(defcheck solution-fc5bb28
  (fn tree? [coll]
    (if (coll? coll)
      (if (= (count coll) 3)
        (and (tree? (second coll)) (tree? (last coll)))
        false)
      (not (false? coll)))))

(defcheck solution-fce17c78
  (fn t? [t]
    (if (nil? t)
      true
      (if (and (coll? t) (= (count t) 3))
        (let [[_ l r] t]
          (and (t? l) (t? r)))
        false))))

(defcheck solution-fd095a7c
  (fn istree? [root]
    (or (nil? root)
        (and (sequential? root)
             (= 3 (count root))
             (every? istree? (rest root))))))

(defcheck solution-fd2914f6
  (fn binary_tree [sq]
    (or (nil? sq)
        (and
         (sequential? sq)
         (= (count sq) 3)
         (not (nil? (first sq)))
         (binary_tree (second sq))
         (binary_tree (nth sq 2))))))

(defcheck solution-fd914e31
  (fn btree? [t]
    (or (nil? t)
        (and
         (coll? t)
         (= 3 (count t))
         (btree? (nth t 1))
         (btree? (nth t 2))
         )
        )
    ))

(defcheck solution-fd975b24
  (fn t [s]
    (or
     (nil? s)
     (and
      (coll? s)
      (= (count s) 3)
      (not (nil? (first s)))
      (t (nth s 1)) (t (nth s 2))))))

(defcheck solution-fdcf447f
  (fn test-btree [node]
    (or (nil? node)
        (and (coll? node)
             (= (count node) 3)
             (test-btree (nth node 1))
             (test-btree (nth node 2))))))

(defcheck solution-fe48ef73
  (fn [tree]
    (letfn [(is-tree [tree]
              (if (and tree (= 3 (count tree)))
                (let [[v l r] tree]
                  (and
                   (not (nil? v))
                   (or (nil? l) (is-tree l))
                   (or (nil? r) (is-tree r))
                   )
                  )
                )
              )]
      (boolean (is-tree tree)))
    ))

(defcheck solution-ff241ec8
  (fn f [t]
    (cond
      (nil? t)
      true
      (and (coll? t) (= 3 (count t)))
      (let [[_ a b] t]
        (if (and (f a) (f b))
          true
          false))
      :else false)))

(defcheck solution-ffa88113
  (fn is-tree? [s]
    (or
     (nil? s)
     (and
      (coll? s)
      (= (count s) 3)
      (is-tree? (second s))
      (is-tree? (nth s 2))))))

(defcheck solution-ffab65a3
  (fn t? [t]
    (or (nil? t)
        (and (sequential? t)
             (= 3 (count t))
             (t? (nth t 1))
             (t? (nth t 2))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-95))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
