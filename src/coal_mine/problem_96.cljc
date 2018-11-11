(ns coal-mine.problem-96
  (:require [coal-mine.checks :refer [defcheck-96] :rename {defcheck-96 defcheck}]
            [clojure.test]
            [clojure.walk]))

(defcheck solution-1045af6c
  #(letfn [(symmetric? [a b]
             (if (and (coll? a) (coll? b))
               (let [[a1 a2 a3] a
                     [b1 b2 b3] b]
                 (and (= a1 b1)
                      (symmetric? a2 b3)
                      (symmetric? a3 b2)))
               (= a b)))]
     (symmetric? (second %) (last %))))

(defcheck solution-104c1d63
  (fn [[v l r]]
    (letfn [(equal? [[v1 l1 r1] [v2 l2 r2]]
              (and
               (= v1 v2)
               (or
                (every? nil? [l1 r1 l2 r2])
                (and (every? nil? [l1 r2])
                     (equal? l2 r1))
                (and (every? nil? [l2 r1])
                     (equal? l1 r2))
                (and
                 (equal? l1 r2)
                 (equal? r1 l2)))
               )
              )]
      (cond
        (nil? l) (nil? r)
        (nil? r) (nil? l)
        (not (sequential? l)) false
        (not (sequential? r)) false
        :else (equal? l r)))))

(defcheck solution-10802531
  (fn [t]
    (letfn [(treeeq [l r]
              (or (and (nil? l) (nil? r))
                  (and (= (first l) (first r))
                       (let [[_ ll lr] l
                             [_ rl rr] r]
                         (and
                          (treeeq ll rr)
                          (treeeq lr rl))))))]
      (and (sequential? t)
           (= (count t) 3)
           (let [[_ l r] t]
             (treeeq l r))))))

(defcheck solution-10adfc50
  (fn [[_ l r]]
    (= l
      ((fn f [b]
         (if (nil? b)
           nil
           [(nth b 0)
            (f (nth b 2))
            (f (nth b 1))]))
       r))))

(defcheck solution-1120e633
  (fn m
    ([[a b c]]
     (m b c))
    ([[a b c] [d e f]]
     (if (nil? a) (nil? d)
                  (and (= a d)
                       (m b f)
                       (m c e))))))

(defcheck solution-1140fafe
  (fn [tree]
    (let [cmpfn (fn are-trees-symmetric? [first second]
                  (or (and (nil? first) (nil? second))
                      (let [[val1 left1 right1] first
                            [val2 left2 right2] second]
                        (and
                         (= val1 val2)
                         (and
                          (are-trees-symmetric? left1 right2)
                          (are-trees-symmetric? right1 left2))))))]
      (cmpfn (second tree) (second (rest tree))))))

(defcheck solution-11b4b191
  #((fn ff [[l-root ll lr :as left] [r-root rl rr :as right]]
      (or (= nil left right)
          (and (= l-root r-root) (ff ll rr) (ff lr rl)))) % %))

(defcheck solution-11cf77ef
  (fn bt? [tree]
    (letfn [(bt [t l?]
              (if (coll? t)
                (concat [(first t)]
                  (if l? (bt (second t) l?) (bt (last t) l?))
                  (if l? (bt (last t) (not l?)) (bt (second t) (not l?))))
                [t]))]
      (= (bt (second tree) true) (bt (last tree) false)))))

(defcheck solution-11e66fa5
  #((fn mir? [p q] (or (and (nil? p) (nil? q))
                       (and (coll? p) (coll? q)
                            (= (first p) (first q))
                            (mir? (nth p 1) (nth q 2))
                            (mir? (nth p 2) (nth q 1)))))
    % %))

(defcheck solution-120d9f6f
  (fn s? [t]
    (or (nil? t)
        (letfn [(refl [t]
                  (if (nil? t)
                    nil
                    (list* (first t)
                      (->> t rest (map refl) reverse))))]
          (= (nth t 1)
            (refl (nth t 2)))))))

(defcheck solution-1211a23a
  (fn symmetric? [[_ left right]]
    (letfn [(mirror [branch]
              (when-let [[root left right] branch]
                [root (mirror right) (mirror left)]))]
      (= left (mirror right)))))

(defcheck solution-1216b792
  (fn [[a b c]]
    (let [trees-mirror? (fn tree-mirror? [x y]
                          (or
                           (= x y nil)
                           (and
                            (not (or (nil? x) (nil? y)))
                            (let [[p q r] x
                                  [e f g] y]
                              (and
                               (= p e)
                               (and (tree-mirror? q g) (tree-mirror? r f)))))))]
      (trees-mirror? b c))))

(defcheck solution-125465c0
  (fn sym
    ([t] (sym (second t) (last t)))
    ([t1 t2] (cond (and (coll? t1) (coll? t2)) (and (sym (first t1) (first t2))
                                                    (sym (second t1) (last t2))
                                                    (sym (last t1) (second t2)))
                   (or (coll? t1) (coll? t2)) false
                   :else (= t1 t2)))))

(defcheck solution-126e20c6
  (fn sym-tree? [coll]
    (let [is-sequence? (fn [h] (or (seq? h) (coll? h)))
          mirror       (fn mirror [c]
                         (if (is-sequence? c)
                           [(first c) (mirror (last c)) (mirror (second c))]
                           c))]
      (if (is-sequence? coll)
        (= (second coll) (mirror (last coll)))
        true))))

(defcheck solution-12a98f35
  (fn [[root l r]]
    (letfn [(revert [t]
              (if (coll? t)
                (->> t ((juxt first last second)) (map revert))
                t))]
      (= l (revert r)))))

(defcheck solution-12d06172
  #(let [opposite (fn opp [[val left right :as tree]]
                    (if (nil? tree)
                      tree
                      [val (opp right) (opp left)]))]
     (= % (opposite %))))

(defcheck solution-132f9f79
  (fn [[r t t2]]
    (= t ((fn mirror [[r & ts]]
            (if (nil? r)
              r
              (cons r (map mirror (reverse ts))))) t2))))

(defcheck solution-1379b2a6
  (fn [tree]
    (letfn
     [(mirror-tree [tree]
        (if (nil? tree)
          tree
          (let [[node left right] tree]
            [node (mirror-tree right) (mirror-tree left)])))]
      (= tree (mirror-tree tree)))))

(defcheck solution-13b5094
  (fn f [tree]
    (let [g
          (fn h [left-root right-root]
            (if (and (nil? left-root) (nil? right-root))
              true
              (if (not= (first left-root) (first right-root))
                false
                (and
                 (h (fnext left-root) (first (nnext right-root)))
                 (h (first (nnext left-root)) (fnext right-root))
                 )
                )
              )
            )]
      (g (fnext tree) (first (nnext tree)))
      )
    ))

(defcheck solution-13feff62
  #(letfn [(inorder [l]
             (if (nil? l)
               '()
               (let [v (first l) left (fnext l) right (fnext (next l))]
                 (concat (inorder left) (list v) (inorder right)))))]
     (= (inorder (fnext %)) (reverse (inorder(fnext (next %)))))))

(defcheck solution-15c38dd6
  (fn symmetric? [[v lc rc]]
    (letfn [(reversed [tree]
              (cond
                (sequential? tree) [(first tree) (reversed (last tree)) (reversed (second tree))]
                nil nil
                )
              )]
      (= (reversed lc) rc))))

(defcheck solution-16127f
  (fn sym? [tree]
    (let [left  (fn [tree] (nth tree 1))
          right (fn [tree] (nth tree 2))
          head  (fn [tree] (first tree))
          walk  (fn walk [tree x y]
                  (when (seq tree)
                    (concat [(first tree)] (walk (x tree) x y) (walk (y tree) x y))))]

      (= (walk (left tree) left right)
        (walk (right tree) right left)))))

(defcheck solution-166f7304
  (fn symmetric-tree? [tree]
    (letfn [(third [coll] (-> coll rest second))
            (visit [branch f g]
              (let [leaf-a (f branch)
                    leaf-b (g branch)]
                (lazy-cat (list (first branch))
                  (when leaf-a (visit leaf-a f g))
                  (when leaf-b (visit leaf-b f g)))))]
      (every? identity
        (map =
          (visit (second tree) second third)
          (visit (third tree) third second))))))

(defcheck solution-1685a801
  (fn [[_ l r]]
    (letfn [(mirror [t] (if (seq t) (cons (first t) (reverse (map mirror (rest t))))))]
      (= l (mirror r)))))

(defcheck solution-16cf87a8
  (fn [n]
    (let [value (fn [n] (first n))
          l-child (fn  [n] (first (rest n)))
          r-child (fn  [n] (first (rest (rest n))))
          mirror (fn mirror [n]
                   (if (nil? n)
                     nil
                     (list (value n) (mirror (r-child n)) (mirror (l-child n)))))]
      (= n (mirror n)))))

(defcheck solution-16dadab7
  (fn is-mirror[tree]
    (let [value (first tree)
          mirror-fn (fn mirror [tree]
                      (if (nil? tree)
                        tree
                        [(first tree) (mirror (nth tree 2)) (mirror (second tree))]))
          left-part (second tree)
          right-part (nth tree 2)
          mirrored-left (mirror-fn left-part)]
      (= mirrored-left right-part))))

(defcheck solution-171e9e38
  (fn [[_ l1 l2]]
    (letfn [(sym-tree? [[ln ll lr] [rn rl rr]]
              (cond
                (not= ln rn) false
                (reduce #(and % (nil? %2)) true (list ll lr rl rr)) true
                (sym-tree? ll rr) (sym-tree? lr rl)
                :else false))]
      (sym-tree? l1 l2))))

(defcheck solution-172509be
  (fn [tree]
    (letfn
     [(rev-tree [t] (if (nil? t) t [(first t) (rev-tree (last t)) (rev-tree (second t))]))]
      (= tree (rev-tree tree)))))

(defcheck solution-17c1f9c0
  (fn [x]
    (letfn [(rtree [x]
              (if (nil? x) x [(first x) (rtree (nth x 2)) (rtree (second x))]))]
      (= (rtree (second x)) (nth x 2)))))

(defcheck solution-17eae50b
  (fn is-sym
    [tree]
    (letfn
     [(in-order [t]
        (if (sequential? t)
          (concat (in-order (second t)) [(first t)] (in-order (second (rest t))))
          [t]))
      (is-palindrome? [s]
        (let [s (vec s)]
          (if (or (empty? s) (= 1 (count s)))
            true
            (if (= (first s) (last s))
              (is-palindrome? (subvec s 1 (dec (count s))))
              false))))]
      #_(println (in-order tree))
      (is-palindrome? (in-order tree)))))

(defcheck solution-17f758d0
  (fn symmtric
    [tree]
    (letfn [(skew-match?
              [tree1 tree2]
              (or (and (nil? tree1) (nil? tree2))
                  (and
                   (coll? tree1)
                   (coll? tree2)
                   (= (first tree1) (first tree2))
                   (skew-match? (second tree1) (last tree2))
                   (skew-match? (last tree1) (second tree2)))))]
      (skew-match? tree tree))))

(defcheck solution-182e94d7
  (fn symmetric-tree? [t]
    (letfn [(leaves [s]
              (filter #(not (sequential? %)) s))
            (flip-children [n]
              (conj (reverse (rest n)) (first n)))]
      (= (leaves (tree-seq sequential? seq t))
        (leaves (tree-seq sequential?
                  (comp flip-children seq)
                  t))))))

(defcheck solution-183f6a43
  #(=
     (map first (filter coll? (tree-seq coll? identity (second %))))
     (map first (filter coll? (tree-seq coll? reverse (last %))))))

(defcheck solution-184796db
  (fn [c]
    (= (remove sequential?
         (tree-seq sequential? identity c))
      (remove sequential?
        (tree-seq sequential? #(conj [] (first %) (last %) (second %)) c)))))

(defcheck solution-1881d51d
  (letfn [(mir-t [ts]
            (if (nil? ts)
              nil
              (let [[v ln rn] ts]
                [v
                 (mir-t rn)
                 (mir-t ln)])))]
    (fn [t]
      (if (nil? t)
        true
        (let [[v0 lt rt] t]
          (= lt (mir-t rt)))))))

(defcheck solution-18966e07
  (fn [coll]
    (let [ [root left right] coll

          synimpl (fn __syn [root dir] (if (nil? root) nil(let [ [c l r] root ](if (zero? dir)(conj [] c (__syn l dir) (__syn r dir) )(conj [] c (__syn r dir) (__syn l dir) )))))
          ]
      (= (synimpl left 0) (synimpl right 1))
      )
    ))

(defcheck solution-18d0f8d4
  (fn symmetrical?
    [[head & children]]
    (letfn [(symmetric-child? [left right]
              (if (or (coll? left) (coll? right))
                (and (and (coll? left) (coll? right))
                     (= (first left) (first right))
                     (symmetric-child? (second (rest left)) (first (rest right)))
                     (symmetric-child? (first (rest left))  (second (rest right))))
                (= left right)))]
      (and true (symmetric-child? (first children) (second children))))))

(defcheck solution-18d6380c
  #(
    (fn f [t1 t2]
      (cond
        (and (nil? t1) (nil? t2)) true
        (not= (first t1) (first t2)) false
        :else (and (f (second t1) (last t2)) (f (last t1) (second t2)))))
    (second %) (last %)))

(defcheck solution-18f43321
  (fn sym? [[_ [a la ra] [b lb rb]]]
    (or (and (= nil a b))
        (and (= a b)
             (sym? [nil la rb])
             (sym? [nil lb ra])))))

(defcheck solution-19456686
  (letfn [(tree-mirror-image [tree] (if (empty? tree) tree
                                                      (let [[value left right] tree]
                                                        [value (tree-mirror-image right) (tree-mirror-image left)]) ) )
          (symmetric? [tree] (let [[_ left right] tree] (= left (tree-mirror-image right))  )  )]
    symmetric?
    ))

(defcheck solution-1962b96a
  (fn symmetricp [root]
    (letfn [(mirrors [a b]
              (if (and (coll? a) (coll? b))
                (and (= (first a) (first b))
                     (mirrors (nth a 1) (nth b 2))
                     (mirrors (nth a 2) (nth b 1)))
                (= a b)))]
      (mirrors (nth root 1) (nth root 2)))))

(defcheck solution-19b46791
  (fn [t] (letfn [
                  (third [coll] (nth coll 2))
                  (value [tree] (first tree))
                  (left [tree] (second tree))
                  (right [tree] (third tree))
                  (branch? [binary-tree] (and (sequential? binary-tree) (= 3 (count binary-tree))))
                  (symmetric? ([t] (symmetric? t t)) ([t1 t2] (if (and (not (branch? t1)) (not (branch? t2))) (= t1 t2) (and (symmetric? (left t1) (right t2)) (symmetric? (right t1) (left t2)) (= (value t1) (value t2))))))
                  ] (symmetric? t))))

(defcheck solution-19f34100
  (fn [t]
    (letfn [(symmetric? [l r]
              (cond
                (nil? (or l r)) true
                (not (nil? (and l r))) (let [[v1 l1 r1] l
                                             [v2 l2 r2] r]
                                         (and (= v1 v2) (symmetric? l1 r2) (symmetric? r1 l2)))
                :else false))]
      (symmetric? (nth t 1) (nth t 2)))))

(defcheck solution-19fec763
  (fn [s] (letfn [
                  (check-sym [l r]
                    (if (= (nth l 0) (nth r 0))
                      (and (cmp-nodes (nth l 1) (nth r 2)) (cmp-nodes (nth l 2) (nth r 1)))
                      false))
                  (cmp-nodes [a b]
                    (if (and (sequential? a) (sequential? b))
                      (check-sym a b)
                      (= a b)))]
            (check-sym (nth s 1) (nth s 2)))))

(defcheck solution-1a000c93
  (fn sym? [node]
    (letfn [(mirror [node]
              (when-let [[v l r] node]
                [v (mirror r) (mirror l)]))]
      (= (second node) (mirror (last node))))))

(defcheck solution-1a02527
  (fn symetric? [tree]
    (letfn [(in-order [t]
              (cond
                (nil? t) [nil]
                :else (concat (in-order (second t)) [(first t)] (in-order (nth t 2)))))]
      (let [in-order-lst (in-order tree)]
        (= in-order-lst (reverse in-order-lst))))))

(defcheck solution-1a69d18a
  (fn sym? [t]
    (letfn [(flip [t]
              (if (or (not (sequential? t)) (empty? t))
                t
                [(first t) (flip (nth t 2)) (flip (nth t 1))]))
            ]
      (= t (flip t)))))

(defcheck solution-1a825a55
  (fn [[_ a b]]
    (letfn [(f [[av al ar] [bv bl br]]
              (and (= av bv)
                   (or (and (nil? al) (nil? br))
                       (and (not (nil? al)) (not (nil? br)) (f al br)))
                   (or (and (nil? ar) (nil? bl))
                       (and (not (nil? ar)) (not (nil? bl)) (f ar bl)))))]
      (f a b))))

(defcheck solution-1ad6525a
  #(letfn
    [(turn [t]
       (if t
         [(first t)
          (turn (nth t 2))
          (turn (nth t 1))]
         nil))]
     (or (= (nth % 1) (nth % 2))
         (= (nth % 1) (turn (nth % 2))))))

(defcheck solution-1af7a585
  (fn symmetric? [tree] (letfn [(inorder ([tree] (if (not (nil? tree)) (concat (inorder (second tree)) [(first tree)] (inorder (last tree))))))] (= (inorder tree) (reverse (inorder tree))))))

(defcheck solution-1b748ad1
  (fn p96
    [coll]
    (let [childa (second coll)
          childb (last coll)]
      (= ((fn mirror [coll] (let [node (first coll)
                                  childa (second coll)
                                  childb (last coll)]
                              (if (seq childa)
                                (if (seq childb)
                                  [node (mirror childb) (mirror childa)]
                                  [node childb (mirror childa)])
                                (if (seq childb)
                                  [node (mirror childb) childa]
                                  [node childb childa])))) childa) childb))))

(defcheck solution-1bc8913
  (fn [tree]
    (letfn [(check-equal [left right]
              (cond (and ((comp not coll?) left)
                         ((comp not coll?) right)) (= left right)
                    (and (coll? left)
                         (coll? right)) (and (check-equal (nth left 0) (nth right 0)) ;;labels
                                             (check-equal (nth left 1) (nth right 2)) ;;outside branches
                                             (check-equal (nth left 2) (nth right 1))) ;;inside branches
                    :else false))]
      (check-equal (nth tree 1)
        (nth tree 2)))))

(defcheck solution-1be4c5fb
  #(= (second %)  ((fn -recur [-seq stock]
                     (if (sequential? -seq)
                       (let [s -seq]
                         [(first -seq) (-recur (last s) (last s)) (-recur (second s) (second s))]
                         )
                       stock
                       )
                     ) (last %) [])))

(defcheck solution-1c28e41f
  (fn mirror? [t] (let [mirror (fn mirror [[v l r]] (if (nil? v) nil [v (mirror r) (mirror l)]))]
                    (= t (mirror t)))))

(defcheck solution-1c6984d1
  (fn check-sym [[val l-tree r-tree]]
    (letfn [(reverse-btree [[val l-chld r-chld :as node]]
              (if (= node nil) nil
                               [val (reverse-btree r-chld) (reverse-btree l-chld)]))]
      (= l-tree (reverse-btree r-tree)))))

(defcheck solution-1c76441e
  (fn [[_ l r]]
    (letfn [(tree= [[v1 l1 r1] [v2 l2 r2]]
              (if (or (nil? v1) (nil? v2))
                (= nil v1 v2)
                (and (= v1 v2) (tree= l1 r2) (tree= r1 l2))))]
      (tree= l r))))

(defcheck solution-1cc4dd76
  (fn [t]
    (let [nodelist (fn [t l r]
                     (loop [nodes [] rem [t]]
                       (if (empty? rem)
                         nodes
                         (if-let [cur (first rem)]
                           (recur (conj nodes (nth cur 0)) (conj (rest rem) (nth cur l) (nth cur r)))
                           (recur nodes (rest rem))))))]
      (= (nodelist (nth t 1) 1 2) (nodelist (nth t 2) 2 1)))))

(defcheck solution-1d209001
  (fn [root]
    (let [mirror (fn mirror [t]
                   (if (nil? t)
                     nil
                     (concat [(first t)] [(mirror (last t))] [(mirror (nth t 1))])))]
      (= root (mirror root)))))

(defcheck solution-1d2a8acb
  (fn sym-tree? [[l m r]]
    (letfn [
            (tree? [t]
              (if (= 3 (count t))
                (let [m (second t), r (nth t 2)]
                  (cond
                    (and (nil? m) (nil? r)) true
                    (and (coll? m) (nil? r)) (tree? m)
                    (and (nil? m) (coll? r)) (tree? r)
                    (and (coll? m) (coll? r)) (and (tree? m) (tree? r))
                    :else false))
                false))
            (eq-tree? [[av al ar] [bv bl br]]
              (if (= av bv)
                (cond
                  (every? nil? [al ar bl br])
                  true
                  (every? tree? [al ar bl br])
                  (and (eq-tree? al br) (eq-tree? ar bl))
                  (and (every? tree? [al br]) (every? nil? [ar bl]))
                  (eq-tree? al br)
                  (and (every? tree? [ar bl]) (every? nil? [al br]))
                  (eq-tree? ar bl)
                  :else false)
                false))
            ]
      (if (eq-tree? m r)
        true
        false))))

(defcheck solution-1dbd74b2
  (fn traverse
    ([node]
     (let [leftwise  (fn leftwise [[value left right] acc] (conj acc value (traverse leftwise left) (traverse leftwise right)))
           rightwise (fn rightwise [[value left right] acc] (conj acc value (traverse rightwise right) (traverse rightwise left)))]
       (= (traverse leftwise node) (traverse rightwise node))
       ))
    ([f node] (traverse f node []))
    ([f node acc] (if node (f node acc) nil))
    ))

(defcheck solution-1dc624a8
  (fn is-sym [node]
    (letfn [(traverse [pre? col node]
              (if (nil? node)
                col
                (let [left-child (traverse pre? [] (nth node 1))
                      right-child (traverse pre? [] (nth node 2))]
                  (if pre?
                    (concat left-child [(first node)] right-child)
                    (concat right-child [(first node)] left-child)))))]
      (= (traverse true [] node) (traverse false [] node)))))

(defcheck solution-1dcf2265
  (fn bst-sym? [[_ [ln ll lr] [rn rl rr]]]
    (and (= ln rn)
         (or (= nil ll lr rl rr)
             (and (bst-sym? [nil ll rr])
                  (bst-sym? [nil lr rl]))))))

(defcheck solution-1dd8f68
  #(letfn [(re-reverse [[root l-node r-node]]
             (list root
               (if (sequential? r-node) (re-reverse r-node) r-node)
               (if (sequential? l-node) (re-reverse l-node) l-node)))]
     (= (second %) (second (re-reverse %)))))

(defcheck solution-1dea05b6
  (fn symmetric? [t]
    (loop [[[l-root l-left l-right :as l] & more-left :as left] [(second t)],
           [[r-root r-left r-right :as r] & more-right :as right] [(last t)]]
      (cond
        (empty? left)         (empty? right)
        (empty? right)        (empty? left)
        (empty? l)            (if (empty? r) (recur more-left more-right) false)
        (empty? r)            (if (empty? l) (recur more-left more-right) false)
        (not= l-root r-root)  false
        :else                 (recur (conj more-left l-left l-right)
                                (conj more-right r-right r-left))))))

(defcheck solution-1e41f1e8
  (letfn [(rev-tree [[p l r :as n]] [p (if r (rev-tree r)) (if l (rev-tree l))])]
    (fn [[_ l r]] (= l (rev-tree r)))))

(defcheck solution-1e68210d
  (fn [x]
    (let [mirrored (fn mirrored [t]
                     (if (sequential? t)
                       (let [h (first t)
                             l (second t)
                             r (nth t 2)]
                         [h (mirrored r) (mirrored l)])
                       t))
          l (second x)
          r (nth x 2)]
      (= l (mirrored r)))))

(defcheck solution-1e9ebcc4
  (fn [t]
    (let [expand-tree (fn et [t]
                        (let [[h l-tree r-tree] t]
                          (if (not (nil? h))
                            (concat (et l-tree) (list h) (et r-tree)))))
          expanded-tree (expand-tree t)]
      (= expanded-tree (reverse expanded-tree)))))

(defcheck solution-1ead3210
  #(= % ((fn r [[a b c]] (when a [a (r c) (r b)])) %)))

(defcheck solution-1edbf8a9
  (fn balanced? [t]
    (letfn [(mirror [[v l r]]
              (when v
                (list v (mirror r) (mirror l))))]
      (let [[_ l r] t]
        (= l (mirror r))))))

(defcheck solution-1edcaa5c
  (fn [[x l r]]
    (letfn [(m [t]
              (when-let [[x l r] t]
                [x (m r) (m l)]))]
      (= l (m r)))))

(defcheck solution-1eef1d4a
  (fn symmetric-tree? [[_ l r]]
    (let [mirror
          (fn mirror [[x l r]]
            [x (if r (mirror r))
             (if l (mirror l))])]
      (= l (mirror r)))))

(defcheck solution-1ef04cbd
  (fn g[x]
    ((fn f[a b]
       (or (and (coll? a) (coll? b) (= (first a) (first b)) (f (second a) (last b)) (f (second b) (last a)))
           (and (nil? a) (nil? b)))) (second x) (last x))))

(defcheck solution-1f202542
  (fn [[h l r]]
    ((fn sym? [t1 t2]
       (or (and (nil? t1) (nil? t2))
           (and (and (coll? t1) (coll? t2))
                (= (first t1) (first t2))
                (sym? (second t1) (last t2))
                (sym? (last t1) (second t2))
                true)))
     l r)))

(defcheck solution-1f6ee0de
  (fn mirror? [[_ [a b c] [d e f]]]
    (if a
      (and (= a d)
           (mirror? [a b f])
           (mirror? [a c e]))
      (nil? d))))

(defcheck solution-1f88c07b
  (fn [root]
    (letfn [(symmetric-children? [left right]
              (or (and (nil? left) (nil? right))
                  (and left right
                       (= (first left) (first right))
                       (symmetric-children? (nth left 1) (nth right 2))
                       (symmetric-children? (nth left 2) (nth right 1)))
                  false))]
      (symmetric-children? (nth root 1) (nth root 2)))))

(defcheck solution-204a5bab
  #(letfn
    [(f [[r a b]]
       [r (g b) (g a)])
     (g [a] (if (coll? a) (f a) a))]
     (= % (f %))))

(defcheck solution-204d13fc
  #(letfn [(m [[v l r :as t]] (if (nil? t) t [v (m r) (m l)]))] (= % (m %))))

(defcheck solution-20c17229
  (fn [tree]
    (let [str ((fn flatten-tree [in out]
                 (if-let [[val left right] in]
                   (concat (flatten-tree left out) (list val) (flatten-tree right out) out)
                   (cons nil out)))
               tree
               nil)
          l (count str)
          mid (/ l 2)
          start (take mid str)
          end (take mid (reverse str))]
      (= start end))))

(defcheck solution-20ce4b86
  (fn [[v l r]]
    (= [r]
      ((fn lr->rl [[v l r]]
         [(concat [v]
            (if (coll? r) (lr->rl r) [r])
            (if (coll? l) (lr->rl l) [l]))]) l))))

(defcheck solution-2156ea10
  (fn symmetric? [t]
    (let [rotate (fn rotate [s]
                   (let [v (first s)
                         l (second s)
                         r (second (next s))]
                     (list v
                       (if (sequential? r)
                         (rotate r)
                         r)
                       (if (sequential? l)
                         (rotate l)
                         l))))]
      (= (second t) (rotate (second (next t)))))))

(defcheck solution-218950c2
  (fn mirrored? [[_ l r]]
    (letfn [ (mirror [t]
               (when-let [[v l r] t]
                 [v (mirror r) (mirror l)]))
            ]
      (= l (mirror r)))))

(defcheck solution-21ff44c6
  (fn [t]
    (let [[v l r] t]
      ((fn cmp-subtrees [t1 t2]
         (if (or (nil? t1) (nil? t2)) (= t1 t2)
                                      (let [[v1 l1 r1] t1 [v2 l2 r2] t2]
                                        (and (= v1 v2) (cmp-subtrees l1 r2) (cmp-subtrees r1 l2))))) l r))))

(defcheck solution-2223acef
  #((fn mir? [l r]
      (if (or (= nil l r)
              (and (= (first l) (first r))
                   (mir? (second l) (last r))
                   (mir? (last l) (second r))))
        true false))
    (second %) (last %)))

(defcheck solution-22c77aab
  (fn [[a b c]]
    (boolean
      (let [f (fn x [[t1 l1 r1] [t2 l2 r2]]
                (when (= t1 t2)
                  (cond
                    (and (coll? l1) (coll? r2)) (when-let [b (x l1 r2)] (x [t1 b r1] [t2 l2 b]))
                    (and (coll? r1) (coll? l2)) (when-let [b (x r1 l2)] (x [t1 l1 b] [t2 b r2]))
                    (and (= l1 r2) (= r1 l2)) true)))]
        (f b c)))))

(defcheck solution-23139ba2
  (fn stree?
    ([t] (stree? (second t) (last t)))
    ([t1 t2]
     (cond
       (and (nil? t1) (nil? t2)) true
       (not (= (first t1) (first t2))) false
       :else (and (stree? (second t1) (last t2)) (stree? (last t1) (second t2)))
       )
     )
    ))

(defcheck solution-2359c2ad
  (fn [t0]
    (letfn [(flip [t]
              (if-let [[v l r] (seq t)]
                [v (flip r) (flip l)]
                nil)
              )]
      (= t0 (flip t0)))))

(defcheck solution-23742a1f
  (fn sym? [tree]
    (letfn [(binarytree? [tree]
              (and
               (coll? tree)
               (= (count tree) 3))
              (every? true? (map binarytree? (filter (complement nil?) (rest tree)))))
            (mirror [tree]
              (if-not (nil? tree)
                (vector (first tree) (-> tree last mirror) (-> tree second mirror))))]
      (if (binarytree? tree)
        (= (second tree) (-> tree last mirror))
        false))))

(defcheck solution-23e58d83
  (fn f
    ([v l r]
     (letfn [(mirror ([v l r] [v (mirror r) (mirror l)])
               ([c] (apply mirror c))
               ([] nil))]
       (= l (mirror r))))
    ([c] (apply f c))))

(defcheck solution-247af654
  (fn s [[_ [x l r]
          [y i d]]]
    (and (= x y)
         (or (= nil l i)
             (and
              (s [_ l d])
              (s [_ r i]))))))

(defcheck solution-24a4c5b8
  (fn [t]
    (let [[v l r] t]
      (letfn [(teq? [t1 t2]
                (if (every? nil? (list t1 t2))
                  true
                  (let [[v1 l1 r1] t1
                        [v2 l2 r2] t2]
                    (and (= v1 v2) (teq? l1 r2) (teq? r1 l2))
                    )))]
        (teq? l r)))))

(defcheck solution-24f32aad
  (fn tree-sym?
    ([[_ l r]]
     (tree-sym? l r))
    ([l r]
     (if (or (nil? l) (nil? r))
       (and (nil? l) (nil? r))
       (if (= (first l) (first r))
         (and
          (tree-sym? (second r) (nth l 2))
          (tree-sym? (second l) (nth r 2)))
         false)))))

(defcheck solution-262c2d48
  (fn p
    ([[v l r :as t] [a b c :as s]]
     (if (coll? t)
       (if (coll? s)
         (and (= v a) (p l c) (p b r))
         false)
       (= t s)))
    ([[v l r]] (p l r))))

(defcheck solution-26914e63
  (fn [T] (= (nth T 1) ((fn R [t] (let [[a b c] t] (if a [a (R c) (R b)] a))) (nth T 2)))))

(defcheck solution-26b126ce
  (fn [tree]
    (let [reverse-tree (fn rvt [t]
                         (if (nil? t)
                           nil
                           (list (first t)
                             (rvt (last t))
                             (rvt (second t)))))]
      (= (second tree) (reverse-tree (last tree))))))

(defcheck solution-27188182
  (fn symmetrical? [tree]
    (letfn [(flip [tree]
              (if (nil? tree)
                nil
                (list (nth tree 0)
                  (if (nil? tree) nil (flip (nth tree 2)))
                  (if (nil? tree) nil (flip (nth tree 1))))))]
      (= tree (flip tree)))))

(defcheck solution-27c661e
  (fn [tree]
    (letfn [(flatten-tree [tree]
              (if (seq tree)
                (let [[v l r] tree]
                  (concat (flatten-tree l) [v] (flatten-tree r)))
                [\*]))]
      (let [ft (flatten-tree tree)]
        (= ft (reverse ft)))) ))

(defcheck solution-28aa4289
  (fn s [[_ l r]]
    (if (and (coll? l) (coll? r))
      (let [[a b c] l
            [d e f] r]
        (and (= a d) (s [_ b f]) (s [_ c e])))
      (= l r))))

(defcheck solution-28b62e6f
  (fn sym-tree? [t]
    (letfn [(compare [a b]
              (if (nil? a) (nil? b)
                           (and (= (first a) (first b)) (compare (second a) (last b)) (compare (last a) (second b)))))]
      (compare (second t) (last t)))))

(defcheck solution-28b78801
  (fn [tree]
    (let [mirrored (fn f [node]
                     (if (sequential? node)
                       (let [[head left right] node]
                         [head (f right) (f left)])
                       node))
          [head left right] tree]
      (= left (mirrored right)))))

(defcheck solution-291d91ef
  (fn [coll]
    (let [mirror (fn mirror [[a b c]]
                   [a (if (sequential? c) (mirror c) c) (if (sequential? b) (mirror b) b)])]
      (= coll (mirror coll)))))

(defcheck solution-2922a1bf
  (fn is-beauty?
    [t]
    (letfn [(is-equal?
              [t1 t2]
              (if (= nil t1 t2)
                true
                (if (or (nil? t1) (nil? t2))
                  false
                  (if (= (first t1) (first t2))
                    (and (is-equal? (second t1) (last t2))
                         (is-equal? (second t2) (last t1)))
                    false))))]
      (is-equal? (second t) (last t)))))

(defcheck solution-2936934d
  (fn symmetric? [[val left right]]
    (let [mirror (fn mirror [x]
                   (if (nil? x)
                     x
                     (let [[v l r] x]
                       [v (mirror r) (mirror l)])))]
      (or
       (= nil left right)
       (= left (mirror right))))))

(defcheck solution-295206d2
  (fn mirrors
    [tree]
    (if (and (= 3 (count tree)) (not-any? sequential? (rest tree)))
      true
      (if (not= 3 (count tree))
        false
        (let [lval (first (nth tree 1))
              rval (first (nth tree 2))
              ltree (rest (nth tree 1))
              rtree (rest (nth tree 2))]
          (if (not= lval rval)
            false
            (and (mirrors (conj [:head] (first ltree) (last rtree)))
                 (mirrors (conj [:head] (last ltree) (first rtree))))))))))

(defcheck solution-2963a520
  (fn sym-tree? [[x y z]]
    (if (or (nil? y) (nil? z))
      (and (nil? y) (nil? z))
      (let [fy (first y) fz (first z)
            sy (second y) sz (second z)
            ty (nth y 2) tz (nth z 2)]
        (if (= fy fz)
          (and (sym-tree? (list fy sz ty))
               (sym-tree? (list fy sy tz)))
          false)))))

(defcheck solution-29cd9b2e
  (fn [[n l r]]
    (= l
      ((fn mirror [[n l r]]
         [n (if r (mirror r)) (if l (mirror l))]) r))))

(defcheck solution-2a41f90c
  (let [eq (fn eq [l r] (or (and (= l nil) (= r nil))
                            (let [[lv ll lr] l
                                  [rv rl rr] r]
                              (and (= lv rv) (eq ll rr) (eq lr rl)))))
        ]
    (fn [n] (or (= n nil) (let [[_ l r] n] (eq l r))))))

(defcheck solution-2a992624
  (letfn [(mirror [t]
            (if (sequential? t) [(first t) (mirror (last t)) (mirror (second t))] t))]
    (fn [[v l r]] (= l (mirror r)))))

(defcheck solution-2a9a1c6d
  (fn mirror [tree]
    (letfn [(collapse [tree]
              (if (coll? tree)
                (concat (collapse (second tree)) [(first tree)] (collapse (nth tree 2)))
                tree
                ))]
      (let [collapsed (collapse tree)]
        (= (reverse collapsed) collapsed)
        )
      )))

(defcheck solution-2b054567
  (fn task-96 [[v l r]]
    (= l
      ((fn mirror [t]
         (if (seq t)
           (let [[value left right] t]
             [value (mirror right) (mirror left)])
           t)) r))))

(defcheck solution-2b22d3ec
  #(let[
        v (fn v [n]
            (if (seq n)
              (let [ [f s l] n]
                [(v s) f (v l)])))
        f  (flatten (v %)) ]
     (= f (reverse f))))

(defcheck solution-2b489c53
  (letfn [
          (reverse-tree [node]
            (if (coll? node)
              (list (first node) (reverse-tree (last node)) (reverse-tree (second node)))
              node))
          (is-symmetrical? [node] (= node (reverse-tree node)))]
    is-symmetrical?))

(defcheck solution-2b6b86f
  (fn symmetric? [tree] (let [revtree (fn revtree [t]
                                        (if t [(first t) (revtree (nth t 2)) (revtree (nth t 1))] nil))]
                          (or (nil? tree) (= (nth tree 1) (revtree (nth tree 2))))
                          )))

(defcheck solution-2ba2a8ca
  (fn [[root left right]]
    (letfn [(flip-tree [[root left right]]
              (if (nil? root)
                nil
                [root (flip-tree right) (flip-tree left)]))]
      (= left (flip-tree right)))))

(defcheck solution-2ba448f6
  (fn symmetrictree? [x]
    (= ((fn deepreverse [y]
          (if (not (sequential? y))
            y
            (if (reduce #(or %1 %2) (map sequential? y))
              (map deepreverse (assoc (vec (assoc (vec y) 1 (last y))) 2 (last (butlast y))))
              (assoc (vec (assoc (vec y) 1 (last y))) 2 (last (butlast y))))))
        (second x)) (last x))))

(defcheck solution-2bcf0fb3
  (fn [[a b c]]
    (letfn [(revtree [xs]
              (if (coll? xs)
                (let [[a b] (rest xs)]
                  (cons (first xs) (into (empty xs) (list (revtree b) (revtree a)))))))]
      (= b (revtree c)))))

(defcheck solution-2bddeae3
  (fn tree-eq
    ([t] (let [[v l r] t] (tree-eq l r)))
    ([l r] (if (and (nil? l) (nil? r))
             true
             (let [[vl ll rl] l
                   [vr lr rr] r]
               (and (= vl vr) (tree-eq ll rr) (tree-eq rl lr)))))))

(defcheck solution-2c061d52
  #((fn c [[ln ll lr] [rn rl rr]]
      (if (some nil? [ln rn])
        (= ln rn)
        (and (= ln rn) (c ll rr) (c lr rl)))) (second %) (last %)))

(defcheck solution-2c8bd666
  (fn [args]
    (letfn [(lnode [tree] (first (rest tree)))
            (rnode [tree] (last tree))
            (leaf? [tree] ((comp not coll?) tree))
            (symmetry [tree]
              (if (leaf? tree)
                tree
                (conj [] (first tree) (symmetry (rnode tree)) (symmetry (lnode tree)))))]
      (= (symmetry (lnode args)) (rnode args))
      )))

(defcheck solution-2c9ae7b4
  (fn symmetry-tree? [tree]
    (letfn [(mirror-tree [tree]
              (when-let [[root left right] (seq tree)]
                (vector root (mirror-tree right) (mirror-tree left))))
            (binary-tree? [tree]
              (if-not (sequential? tree)
                true
                (when-let [[root left right] (seq tree)]
                  (and (= 3 (count tree))
                       (binary-tree? left)
                       (binary-tree? right)))))]
      (if-not (binary-tree? tree)
        false
        (if (sequential? tree)
          (let [[root left right] tree]
            (= left (mirror-tree right))))))))

(defcheck solution-2e27b511
  (fn
    [tr]
    (let [mirror (fn mirror [tree]
                   (if (coll? tree)
                     (let [a (nth tree 2)
                           b (second tree)]
                       [(first tree) (mirror a) (mirror b)])
                     tree))
          left   (nth tr 2)
          right  (second tr)]
      (= left (mirror right)))))

(defcheck solution-2e8815fa
  (fn symmetry? [[_ left right]]
    (letfn [(mirror [node]
              (when-let [[v l r] node]
                [v (mirror r) (mirror l)]))]
      (= left (mirror right)))))

(defcheck solution-2ecbba45
  (fn symmetric? [tree]
    (let [value (fn [t] (nth t 0))
          left  (fn [t] (nth t 1))
          right (fn [t] (nth t 2))
          flip  (fn flip [t]
                  (if (nil? t)
                    t
                    [(value t) (flip (right t)) (flip (left t))]))]
      (= tree (flip tree)))))

(defcheck solution-2f9f0bf3
  (fn [t]
    (= (nth t 1)
      ((fn rev-trav [tree v]
         (if (coll? tree)
           (conj v (nth tree 0) (rev-trav (nth tree 2) v) (rev-trav (nth tree 1) v))
           tree
           )) (nth t 2) []))))

(defcheck solution-2fa05a05
  (fn [s]
    (let [third #(nth % 2)]
      ((fn tree= [a b]
         (if (coll? a)
           (if (coll? b)
             (and (= (first a) (first b))
                  (and (tree= (third a) (second b)) (tree= (second a) (third b))))
             false)
           (if (coll? b)
             false
             (= a b))))
       (second s) (third s)))))

(defcheck solution-2fbb9090
  (fn [t]
    (letfn [(mirr [t]
              (when t
                (let [[v a b] t]
                  (vector v (mirr b) (mirr a)))))]
      (= t (mirr t)))))

(defcheck solution-2fd84bfb
  (fn is-symmetric [[_ l r]]
    (letfn [(mirror-image? [l r]
              (cond (every? nil? [l r]) true
                    :else (let [[lv ll lr] l [rv rl rr] r]
                            (and (= lv rv)
                                 (mirror-image? ll rr)
                                 (mirror-image? lr rl)))))]
      (mirror-image? l r))))

(defcheck solution-3021d9f8
  (fn symmetric?
    ([left right]
     (if (and (nil? left) (nil? right))
       true
       (let [left-value  (nth left 0)
             left-left   (nth left 1)
             left-right  (nth left 2)
             right-value (nth right 0)
             right-left  (nth right 1)
             right-right (nth right 2)]
         (and (= left-value right-value)
              (symmetric? left-left right-right)
              (symmetric? left-right right-left)))))
    ([t] (symmetric? (nth t 1) (nth t 2)))))

(defcheck solution-30470ca7
  (fn issymmetric?[root]
    (letfn [(mirror [t] (if (nil? t) nil
                                     (let [v (first t) left (second t) right (nth t 2)]
                                       (vector v (mirror right) (mirror left)))
                                     ))]
      (= (second root) (mirror (nth root 2))))
    ))

(defcheck solution-307d6310
  #(let [r (fn r [t] (if (coll? t)
                       (let [[v a b] t]
                         [v (r b) (r a)])
                       t))]
     (= (r %) %)))

(defcheck solution-30cab5d0
  (fn symmetry? [[_ l r]]
    (let [flip (fn flip [node]
                 (if (coll? node)
                   (let [[tp lt rt] node]
                     [tp (flip rt) (flip lt)])
                   node))]
      (= l (flip r)))))

(defcheck solution-30f015b1
  (fn symmetry? [[root left right]]
    (letfn[(mirror? [a b]
             (cond
               (not= (sequential? a) (sequential? b)) false
               (sequential? a) (let [[root-a left-a right-a] a
                                     [root-b left-b right-b] b]
                                 (and (= root-a root-b) (mirror? left-a right-b) (mirror? right-a left-b)))
               :else (= a b)))]
      (mirror? left right))))

(defcheck solution-310d6a32
  #(= % ((fn mirror [t]
           (if (nil? t) nil
                        (let [p (first t)
                              l (second t)
                              r (last t)]
                          [p (mirror r) (mirror l)]))) %)))

(defcheck solution-3137bb7b
  (fn sym-tree? [t] (= t ((fn rev [n] (if (nil? n) n [(first n) (rev (nth n 2)) (rev (nth n 1))])) t))))

(defcheck solution-3169202
  #(letfn [(mirror? [a b]
             (or (and (coll? a)
                      (coll? b)
                      (= (first a) (first b))
                      (mirror? (second a) (last b))
                      (mirror? (last a) (second b)))
                 (and (not (coll? a))
                      (not (coll? b))
                      (= a b))))
           (bt? [coll]
             (if (not (coll? coll))
               ((complement false?) coll)
               (if (or (not= 3 (count coll))
                       (coll? (first coll)))
                 false
                 (and (bt? (second coll))
                      (bt? (last coll))))))]
     (and (bt? %)
          (mirror? (second %) (last %)))))

(defcheck solution-3193ba4e
  (fn [[p l r]]
    (= l
      ((fn f [[p l r :as t]]
         (if (coll? t)
           [p (f r) (f l)]
           t))
       r))))

(defcheck solution-31b24ebb
  (fn sym? [tree]
    (letfn [(rev [tree]
              (if (nil? tree)
                nil
                (list (first tree) (rev (last tree)) (rev (second tree)))))]
      (= (rev tree) tree))))

(defcheck solution-31ecb32d
  (fn
    [[value left right]]
    (letfn [(flat-tree
              [node]
              (if (nil? node)
                nil
                (let [[value left right] node]
                  (concat (flat-tree left) [value] (flat-tree right))
                  )
                )
              )]

      (= (flat-tree left) (reverse (flat-tree right)))
      )
    ))

(defcheck solution-31fce2ba
  (fn [[n l r]]
    (= l
      ((fn rtree [[n l r]]
         (if (nil? n)
           nil
           [n (rtree r) (rtree l)]))
       r))))

(defcheck solution-322f454e
  (fn [[_ L R]]
    (letfn [(mirror [[x l r]] (if (nil? x) x [x (mirror r) (mirror l)]))]
      (= L (mirror R))
      )
    ))

(defcheck solution-32a6c72c
  (fn sym? [coll]
    (let [sym (fn mirror [coll]
                (cond
                  (nil? coll) coll
                  (coll? coll) (let [left (second coll)
                                     right (nth coll 2)]
                                 (vector (first coll) (mirror right) (mirror left)))))]
      (= (sym coll) coll))))

(defcheck solution-32d6f892
  (let [ismirror?
        (fn ismirror
          [x]
          (if (or (and (nil? (second x))
                       (not (nil? (nth x 2))))
                  (and (not (nil? (second x)))
                       (nil? (nth x 2))))
            false
            (if (and (nil? (second x))
                     (nil? (nth x 2)))
              true
              (if (= (first (second x))
                    (first (nth x 2)))
                (and (ismirror [ 1
                                (second (second x))
                                (nth (nth x 2) 2)])
                     (ismirror [ 2
                                (nth (second x) 2)
                                (second (nth x 2))]))
                false))))]
    (fn testme [x] (ismirror? x))))

(defcheck solution-33628082
  (fn [coll] (let [tree ((fn balance [[a b c]]
                           (concat (if (vector? b) (balance b) [b]) [a] (if (vector? c) (balance c) [c])))
                         coll)]
               (= tree (reverse tree)))))

(defcheck solution-33d7de6c
  (fn balanced-tree?
    [t]
    (loop [collected-branches [t]]
      (let [values-at-level (map first collected-branches)
            branches (concat (map second collected-branches)
                       (map last collected-branches))]
        (if-not (= values-at-level (reverse values-at-level))
          false
          (if-not (some sequential? branches)
            true
            (recur branches)))))))

(defcheck solution-33dcae
  (fn isTreeSymmetrical [tree]
    (let
     [
      nodename first
      leftnode second
      rightnode last
      recursor (fn recurs [left right]
                 (if (and (nil? left) (nil? right))
                   true
                   (if (and (not (nil? left)) (not (nil? right)))
                     (if
                      (= (nodename left) (nodename right))
                       (and
                        (recurs (leftnode left) (rightnode right))
                        (recurs (leftnode right) (rightnode left))
                        )
                       false
                       )
                     false
                     )
                   )
                 )
      ]
      (recursor (leftnode tree) (rightnode tree))
      )
    ))

(defcheck solution-3449e936
  (fn mirror [[root a b]]
    (or (and (nil? a)
             (nil? b))
        (and (= (first a) (first b))
             (mirror [:root (nth a 2) (nth b 1)])
             (mirror [:root (nth a 1) (nth b 2)]))
        )))

(defcheck solution-3457b412
  (fn [tree]
    (letfn
     [(revtree [t]
        (if (= t nil)
          nil
          (list (first t)
            (revtree (second (rest t)))
            (revtree (second t)))))]
      (= tree (revtree tree)))))

(defcheck solution-3472d30d
  (fn sym [[_ l r]]
    (= l
      ((fn swap [[h l r]]
         [h (if r (swap r) r) (if l (swap l) l)]) r)
      )))

(defcheck solution-34772346
  (fn sbt [[h l r]]
    (cond (not (or  (coll? l) (coll? r))) (= l r)
          (not= (coll? l) (coll? r)) false
          (not= (count l) (count r)) false
          :else (let [[lh ll lr] l [rh rl rr] r]
                  (and (= lh rh)
                       (sbt [lh ll rr])
                       (sbt [rh lr rl]))))
    ))

(defcheck solution-34fcf8fc
  (fn [tree]
    (let [mirror (fn this [[v l r]]
                   [v (when r (this r)) (when l (this l))])
          [v l r] tree]
      (= l (mirror r)))))

(defcheck solution-362867f7
  (fn [tree]
    (letfn [(reflect [tree]
              (if (nil? tree) nil
                              [(nth tree 0) (reflect (nth tree 2)) (reflect (nth tree 1))]))]
      (= (reflect (nth tree 1)) (nth tree 2)))))

(defcheck solution-363234df
  (fn balanced [t]
    (letfn
     [(flip [t]
        (if (sequential? t)
          (let [[a b c] t] [a (flip c) (flip b)])
          t))]
      (= t (flip t)))))

(defcheck solution-36434c6b
  (fn [[v l r]]
    (letfn [(f
              [b]
              (if (seq b)
                (let [[v l r] b]
                  (list v (f r) (f l)))
                b))]
      (= l (f r)))))

(defcheck solution-366896fa
  (fn go [t]
    (letfn
     [(sym? [a b]
        (cond
          (and (nil? a) (nil? b)) true
          (nil? a) false
          (nil? b) false
          :else (and (= (first a) (first b)) (sym? (nth a 1) (nth b 2)) (sym? (nth a 2) (nth b 1)))))]
      (sym? (nth t 1) (nth t 2))
      )))

(defcheck solution-36875253
  (fn symmetree [t]
    (let [traverse-in-order (fn traverse-in-order [btree]
                              (let [[v l r] btree]
                                (if (sequential? btree)
                                  (concat (traverse-in-order l)
                                    [v]
                                    (traverse-in-order r)))
                                ))]
      (= (reverse (traverse-in-order t)) (traverse-in-order t))
      )))

(defcheck solution-37191710
  (fn [tree]
    (letfn [(sym [tr]
              (if (nil? tr)
                nil
                [(first tr) (sym (nth tr 2)) (sym (second tr))]))]
      (= (sym (second tree)) (nth tree 2)))))

(defcheck solution-3755728c
  (fn [[v izq der]]
    (= izq ((fn rev [[v i d]] [v (if (= 3 (count d)) (rev d) d) (if (= 3 (count i)) (rev i) i)]) der))))

(defcheck solution-375bafa4
  (fn [[_ a b]]
    ((fn m [[c d e] [f g h]]
       (if c
         (and (= c f) (m d h) (m e g))
         true))
     a b)))

(defcheck solution-377a479a
  (fn [t]
    (letfn [(tseq [t]
              (when t
                (concat (tseq (second t))
                  [(first t)]
                  (tseq (first (next (next t)))))))]
      (= (reverse (tseq (second t)))
        (tseq (first (next (next t))))))))

(defcheck solution-3796b3de
  (fn symmetric? [tree]
    (letfn [(mirror [tree]
              (if (seq tree) (let [[v l r] tree] [v (mirror r) (mirror l)])))]
      (= tree (mirror tree)))))

(defcheck solution-37b427a7
  (fn sym-bin-tree? [[node left right]]
    (letfn
     [(mirror [[node left right :as tree] count]
        (cond
          (nil? node) nil
          (and (nil? left) (nil? right)) tree
          (even? count) [node (mirror right (inc count)) left]
          :else [node right (mirror left (inc count))]))]
      (= (mirror left 0) right))))

(defcheck solution-37c3e048
  (fn [s]
    (letfn [(mirror? [a b]
              (or
               (= nil a b)
               (and
                (= (nth a 0) (nth b 0))
                (mirror? (nth a 1) (nth b 2))
                (mirror? (nth a 2) (nth b 1)))))]
      (boolean
        (mirror?
          (nth s 1)
          (nth s 2))))))

(defcheck solution-37f189de
  #(= (second %)
     ((fn invert [t]
        (if (nil? t)
          nil
          (list (first t)
            (invert (last t))
            (invert (second t)))))
      (last %))))

(defcheck solution-38011133
  (fn [[a b c]]
    (= c
      ((fn r [[a b c :as x]]
         (when x
           (conj [a] (r c) (r b)))) b))))

(defcheck solution-38370b04
  (fn [t]
    (= t ((fn flip [[n l r :as t]]
            (when t [n (flip r) (flip l)]))
          t))))

(defcheck solution-38a7b9ff
  (fn symmetric? [[v l r]]
    (= (clojure.walk/postwalk (fn [b] (if (coll? b) (let [[v l r] b] [v r l]) b)) l) r)))

(defcheck solution-3943ecd6
  (fn [x] (= (second x)
            ((fn re [x]
               (if (coll? x)
                 (vector (first x) (re (last x)) (re (second x)))
                 x))
             (last x)))))

(defcheck solution-3996a378
  #(= % ((fn rt [[n l r]] (if (nil? n) n [n (rt  r) (rt l)])) %)))

(defcheck solution-39bda3cd
  (fn symmetric-tree? [[data left right]]
    (
     (fn same-tree? [t1 t2]
       (or (= t1 t2 nil)
           (and
            (= (sequential? t1) (sequential? t2) true)
            (= (first t1) (first t2))
            (and (same-tree? (second t1) (last t2)) (and (same-tree? (last t1) (second t2))))
            )))
     left right
     )))

(defcheck solution-39da1d74
  (fn s [[_ [v1 l1 r1 :as a] [v2 l2 r2 :as b]]]
    (or (= nil a b)
        (and (= v1 v2)
             (s [_ l1 r2])
             (s [_ r1 l2])))))

(defcheck solution-3a23f49
  (fn [[_ left right]]
    (= left ((fn flip [[v left right :as t]]
               (when t [v (flip right) (flip left)]))
             right))))

(defcheck solution-3a3b865e
  (fn [[_ l r]]
    (= ((fn m [t]
          (let [[n l r] t
                lm (if l m identity)
                rm (if r m identity)]
            [n (rm r) (lm l)])) l)
      r)))

(defcheck solution-3a43b266
  (fn symetric-bin-tree?
    [[node l-branch r-branch :as tree]]
    (letfn [(bin-tree-nodes?
              [[node l-branch r-branch :as tree]]
              (letfn [(node? [xs] (= 3 (count xs)))]
                (if (nil? node) []
                                (lazy-cat
                                  (conj (bin-tree-nodes? l-branch) (node? tree))
                                  (conj (bin-tree-nodes? r-branch) (node? tree))))))
            (binary-tree? [tree] (every? true? (bin-tree-nodes? tree)))
            (reverse-tree-recur
              [[node l-branch r-branch]]
              (when-not (nil? node)
                [node (reverse-tree-recur r-branch) (reverse-tree-recur l-branch)]))]
      (and (binary-tree? tree)
           (= l-branch (reverse-tree-recur r-branch))))))

(defcheck solution-3a5abc3c
  (fn [x] (let [left (fn f [x] (if (nil? x) nil (let [[n l r] x] [(f l) n (f r)])))
                right (fn f [x] (if (nil? x) nil (let [[n l r] x] [(f r) n (f l)])))]
            (= (flatten (left x)) (flatten (right x))))))

(defcheck solution-3a64cc59
  (fn [tree]
    (let [mirror (fn self [tree]
                   (if (nil? tree)
                     nil
                     (let [[v l r] tree]
                       (list v (self r) (self l)))))]
      (if (nil? tree)
        true
        (let [[v l r] tree]
          (= (mirror l) r))))))

(defcheck solution-3aa55beb
  (letfn
   [(to-map [[h l r] l-to r-to]
      {:h h
       l-to (if (coll? l)(to-map l l-to r-to) l)
       r-to (if (coll? r)(to-map r l-to r-to) r)
       }
      )]
    (fn [[h l r]]
      (= (to-map l :l :r) (to-map r :r :l))
      )))

(defcheck solution-3ac5f429
  (fn [[_ l r]]
    ((fn m? [[l_ ll lr] [r_ rl rr]]
       (if (= nil l_ r_)
         true
         (and (= l_ r_)
              (m? ll rr)
              (m? lr rl))))
     l r)))

(defcheck solution-3af65a62
  #(= % ((fn s[x] (if (coll? x) (conj (reverse (map s (rest x))) (first x)) x )) %)))

(defcheck solution-3afbcab3
  #(= % ((fn m [[a b c :as t]] (if t [a (m c) (m b)] t)) %)))

(defcheck solution-3b85c888
  (fn solve [tree]
    (letfn [(mirror [tree]
              (if (nil? tree)
                nil
                (list (first tree) (mirror (last tree)) (mirror (second tree)))))]
      (or (nil? tree)
          (= (second tree) (mirror (last tree)))))))

(defcheck solution-3b86cdda
  (fn [[val left right]]
    (letfn [(rev [node]
              (if (sequential? node)
                (let [[v l r] node] [v (rev r) (rev l)])
                node))]
      (= left (rev right)))))

(defcheck solution-3b8733d2
  (fn symmetry [[root left right]]
    (let [mirror? (fn mirror? [a b]
                    (cond
                      (not= (sequential? a) (sequential? b)) false
                      (sequential? a) (let [[ra La Ra] a
                                            [rb Lb Rb] b]
                                        (and (= ra rb) (mirror? La Rb) (mirror? Lb Ra)))
                      :else (= a b)))]
      (mirror? left right))))

(defcheck solution-3b91de76
  (fn sym [[root left right]]
    (letfn [(mirror? [l r]
              (or (every? nil? [l r])
                  (and (= (first l) (first r))
                       (mirror? (second l) (last r))
                       (mirror? (last l) (second r)))))]
      (mirror? left right))))

(defcheck solution-3ba3266d
  (fn [tree]
    (letfn [(mirror-tree [tree]
              (let [parent (first tree)
                    left (second tree)
                    right (last tree)
                    mirrored-left (if (coll? left) (mirror-tree left) left)
                    mirrored-right (if (coll? right) (mirror-tree right) right)]
                [parent mirrored-right mirrored-left]
                )
              )
            ]
      (= (mirror-tree (second tree)) (last tree))
      )
    ))

(defcheck solution-3bae646a
  #(let [reverse-tree (fn reverse-tree [coll]
                        (if (seq coll)
                          [(first coll) (reverse-tree (nth coll 2)) (reverse-tree (nth coll 1))]
                          coll))]
     (= (nth % 1)
       (reverse-tree (nth % 2)))))

(defcheck solution-3c6a7446
  (fn sym?
    ([t] (sym? (second t) (last t)))
    ([a b] (or
            (and (nil? a) (nil? b))
            (if (and (coll? a) (coll? b))
              (and (= (count a) (count b) 3)
                   (= (first a) (first b))
                   (sym? (second a) (last b))
                   (sym? (last a) (second b)))
              false)))))

(defcheck solution-3c7a7f4a
  (fn [t]
    (letfn [(mirror? [l r]
              (or (= nil l r)
                  (and (= (first l) (first r))
                       (mirror? (second l) (last r))
                       (mirror? (last l) (second r)))))]
      (mirror? (second t) (last t)))))

(defcheck solution-3c9237a2
  #(= ((fn deep-rev [coll]
         (if (coll? coll)
           (let [[n l r] coll] [n (deep-rev r) (deep-rev l)])
           coll))
       (second %))
     (last %)))

(defcheck solution-3ca0d242
  #(= % ((fn f [[v a b]] (if v [v (f b) (f a)])) %)))

(defcheck solution-3cca37ae
  (fn [t]
    (let [
          rev (fn rev [t]
                (if (nil? t) nil
                             [ (first t)
                              (rev (second (next t)))
                              (rev (second t))]))
          mirror? (fn mirror? [t]
                    (= t (rev t)))
          ]
      (and
       (mirror? t)))))

(defcheck solution-3d19f836
  #(letfn [(f [a b]
             (or
              (and (nil? a) (nil? b))
              (and (= (first a) (first b) )
                   (f (second a) (last b))
                   (f (last a) (second b)) ) ) ) ]
     (f (second %) (last %)) ))

(defcheck solution-3d321b9c
  (fn [[v l r]]
    (letfn [(m [[v & xs]]
              (cons v (mapcat m (reverse xs))))]
      (= (flatten l) (m r)))))

(defcheck solution-3e064aad
  (fn sym?
    ([[_ l r]] (sym? l r))
    ([[lv ll lr :as l] [rv rl rr :as r]]
     (let [branch-sym? (fn [& lr] (or (every? nil? lr)
                                      (and (every? keyword? lr) (apply = lr))
                                      (and (every? sequential? lr) (apply sym? lr))))]
       (and (= lv rv)
            (branch-sym? ll rr) (branch-sym? rl lr))))))

(defcheck solution-3e606572
  (fn sym
    [[v l r]]
    (= (letfn [
               (swap
                 [node]
                 (let [[v l r] node]
                   (if (nil? node)
                     nil
                     (conj [v] (swap r) (swap l)))))] (swap l)) r)))

(defcheck solution-3e8c430d
  (fn [t]
    (letfn [(mirror [t]
              (if-let [[v l r] t]
                [v (mirror r) (mirror l)]))]
      (if-let [[v l r] t]
        (= l (mirror r))))))

(defcheck solution-3f159b01
  (fn [tree]
    (let [rev-tree
          (fn rev-tree [n]
            (if n [(first n)
                   (rev-tree (nth n 2))
                   (rev-tree (nth n 1))]))]
      (= tree (rev-tree tree)))))

(defcheck solution-3fb12168
  (fn [node]
    (let [[v left right] node
          mirror? (fn m? [child1 child2]
                    (cond
                      (and (nil? child1) (nil? child2)) true
                      (and (coll? child1) (coll? child2))
                      (let [[c1val c1left c1right] child1
                            [c2val c2left c2right] child2]
                        (and (= c1val c2val) (m? c1left c2right) (m? c1right c2left)))
                      :else false))]
      (mirror? left right))))

(defcheck solution-401f6fd3
  (fn [[v l r]]
    (letfn [(swap [[v l r :as t]]
              (when t
                [v (swap r) (swap l)]))]
      (= l (swap r)))))

(defcheck solution-405375fb
  (fn [[_ a b]]
    (letfn [(mirror [t]
              (if (coll? t)
                (let [[h a b] t]
                  [h (mirror b) (mirror a)])
                t))]
      (= a (mirror b)))))

(defcheck solution-4064cf4c
  #(letfn [(refl [x]
             (if (coll? x)
               (let [[a b c] x]
                 [a (refl c) (refl b)])
               x))]
     (= % (refl %))))

(defcheck solution-409c0d91
  (fn [[_ l r]]
    (letfn [(f [n] (if-let [[v l r] n] [v (f r) (f l)] n))]
      (= l (f r)))))

(defcheck solution-40c8ecf1
  (letfn [(pre [[v l r :as n]]
            (if (nil? n) n (conj (conj (pre r) v) (pre l))))
          (post [[v l r :as n]]
            (if (nil? n) n (conj (conj (post l) v) (post r))))]
    (fn [[_ l r]]
      (= (pre l) (post r)))))

(defcheck solution-4116c016
  (letfn [[treverse [tree] (if-let [[n left right] tree] [n (treverse right) (treverse left)])]]
    (fn [tree] (= tree (treverse tree)))))

(defcheck solution-41795b18
  (fn [[root left right]]
    (let [
          swap (fn swap [x]
                 (if (= x nil) nil
                               (let [[root left right] x]
                                 [root (swap right)(swap left)])))
          ]
      (= left (swap right)))
    ))

(defcheck solution-41f1955d
  (fn [s]
    (letfn [(mirror? [l r]
              (or
               (and
                (sequential? l) (sequential? r)
                (= (first l) (first r))
                (= (count l) (count r) 3)
                (mirror? (second l) (last r))
                (mirror? (last l) (second r)))
               (and
                (not (sequential? l)) (not (sequential? r))
                (= l r))))]
      (or (not (sequential? s))
          (and (= (count s) 3)
               (mirror? (second s) (last s)))))))

(defcheck solution-41fda5f4
  (fn sym? [[v [av al ar :as a] [bv bl br :as b] :as t]]
    (if (and (nil? a) (nil? b))
      true
      (and (= av bv)
           (sym? [nil al br])
           (sym? [nil ar bl])))))

(defcheck solution-42074779
  (fn [[_ left right]]
    (letfn [(r [[val left right :as node]]
              (when ((complement nil?) node)
                [val (r right) (r left)]))]
      (= left (r right)))))

(defcheck solution-4255f3af
  (fn msym
    [[_ s l]]
    (cond
      (= s l nil) true
      (and (sequential? s) (sequential? l)) (and
                                             (= (first s) (first l))
                                             (msym [nil (second s) (last l)])
                                             (msym [nil (last s) (second l)]))
      :d false)))

(defcheck solution-4272caca
  (fn [s]
    (let [[val l r] s
          swp (fn swap [t]
                (let [[v l r] t]
                  (if
                   (nil? v) t
                            [v (swap r) (swap l)])))]
      (= l (swp r)))))

(defcheck solution-42b6c960
  (let
   [
    applicable? (fn [& l] (reduce (fn [r i] (and r (or (seq? i) (vector? i)))) true l))
    mirror-tree (fn mirror
                  ([a b c]
                   (cond
                     (applicable? b c) [a (apply mirror c) (apply mirror b)]
                     (applicable? b) [a c (apply mirror b)]
                     (applicable? c) [a (apply mirror c) b]
                     :else [a c b]))
                  ([a]
                   (if (applicable? a)
                     (apply mirror a)
                     a)))
    ]
    (fn symmetric?
      ([_ b c]
       (= (apply vector b) (mirror-tree c)))
      ([a]     (apply symmetric? a)))))

(defcheck solution-430b2704
  (letfn [(mirror-tree [tree]
            (if (coll? tree)
              (let [[root & branches] tree]
                `[~root ~@(map mirror-tree (reverse branches))])
              tree))]
    (fn symmetric? [[root tree1 tree2]]
      (= tree1 (mirror-tree tree2)))))

(defcheck solution-43335046
  (fn sym? [t]
    (letfn [(myswap [node]
              (if (sequential? node)
                [(nth node 0) (myswap (nth node 2)) (myswap (nth node 1))]
                node))]
      (= (myswap (nth t 2)) (nth t 1)))))

(defcheck solution-43c2f943
  #(let [mirror (fn mirror [t]
                  (if (nil? t) t
                               (let [[v a b] t]
                                 (list v (mirror b) (mirror a)))))]
     (= % (mirror %))))

(defcheck solution-43ecc8a5
  (fn symmetric? [tree]
    (letfn [(flip [tree]
              (when-let [[v l r] tree]
                [v (flip r) (flip l)]))]
      (= tree (flip tree)))))

(defcheck solution-44122a86
  #((fn check [L R]
      (or (and (nil? L) (nil? R))
          (and (not (nil? L))
               (not (nil? R))
               (= (first L) (first R))
               (check (nth L 1) (nth R 2))
               (check (nth L 2) (nth R 1)))))
    (nth %1 1) (nth %1 2)))

(defcheck solution-4443a7de
  (fn g [x]
    (let [[_ j k] x]
      (letfn [(f [a b]
                (if (or (nil? a) (nil? b))
                  (= a b)
                  (let [[x ll lr] a [y rl rr] b]
                    (and (= x y) (f ll rr) (f lr rl))
                    )
                  )
                )]
        (f j k)
        )
      )
    ))

(defcheck solution-446704f4
  (fn [[_ x y]]
    (letfn [(subtree? [t]
              (if (sequential? t)
                (= (count t) 3)
                false))
            (tree= [t1 t2]
              (if (and (subtree? t1) (subtree? t2))
                (if (= (nth t1 0) (nth t2 0))

                  (and (tree= (nth t1 1) (nth t2 2))
                       (tree= (nth t1 2) (nth t2 1)))
                  false)
                (and (nil? t1) (nil? t2))))]
      (tree= x y))))

(defcheck solution-446af6cf
  (fn sym [[v l r :as t]] ((fn symcmp [t1 t2]
                             (cond
                               (nil? t1) (nil? t2)
                               (nil? t2) (nil? t1)
                               true (let [[v1 l1 r1] t1
                                          [v2 l2 r2] t2]
                                      (and (= v1 v2)
                                           (symcmp l1 r2)
                                           (symcmp l2 r1))))) l r)))

(defcheck solution-44b959bd
  (letfn [(mirror [tree]
            (if (nil? tree)
              nil
              (let [[v l r] tree]
                [v (mirror r) (mirror l)])))]
    (fn symmetric-tree? [tree]
      (= tree (mirror tree)))))

(defcheck solution-45353d4b
  (fn [x]
    (=
      (remove nil?
        (flatten (nth x 1)))
      ((fn mir [y]
         (cond (nil? (first y)) nil
               :else (do (lazy-cat
                           (cons (first y) (mir (nth y 2)))
                           (mir (nth y 1)))))) (nth x 2)))))

(defcheck solution-4599e6c
  (fn tree-mirror? [col]
    (= (second col)
      ((fn mirror-tree [[v & childs]]
         (cons v (map #(if (sequential? %) (mirror-tree %) %)
                   (reverse childs))))
       (last col)))))

(defcheck solution-4614268b
  (fn [[n l r]]
    (let [mirror? (fn mirror? [a b]
                    (cond (not= (sequential? a) (sequential? b)) false
                          (sequential? a)
                          (let [[n1 l1 r1] a [n2 l2 r2] b]
                            (and (= n1 n2) (mirror? l1 r2) (mirror? r1 l2)))
                          :else (= a b)))]
      (mirror? l r))))

(defcheck solution-468c4f1c
  (fn [[v a b]]
    (let [b? (fn [[v l r]] (or l r))]
      (= (map first (tree-seq b? (fn [[v l r]] (filter identity [r l])) a))
        (map first (tree-seq b? (fn [[v l r]] (filter identity [l r])) b))))))

(defcheck solution-46b33347
  #(= %
     ((fn rev [[v l r]]
        (if v [v (rev r) (rev l)])) %)))

(defcheck solution-46e0d311
  #(
     letfn [
            (r [v]
              (if (nil? v)
                v
                (let [[a b c] v]
                  [a (r c) (r b)])))]
     (= % (r %))))

(defcheck solution-4731c8e1
  (fn symmetric?
    ([[_ left right]]
     (symmetric? left right))
    ([tree1 tree2]
     (or
      (and (nil? tree1) (nil? tree2))
      (and (coll? tree1)
           (coll? tree2)
           (= (first tree1) (first tree2))
           (symmetric? (second tree1) (last tree2))
           (symmetric? (last tree1) (second tree2)))))))

(defcheck solution-477c99e8
  (fn symmetric? [t]
    (let [root (fn [t] (first t))
          left (fn [t] (second t))
          right (fn [t] (nth t 2))
          same? (fn same? [t1 t2]
                  (cond (= t1 t2 nil)
                        true

                        (= (root t1)
                          (root t2))
                        (and (same? (left t1)
                               (right t2))
                             (same? (right t1)
                               (left t2)))

                        :else
                        false))

          ]
      (same? (left t)
        (right t)))


    ))

(defcheck solution-478a429e
  (fn sym[ft]
    (let [inv (fn inv[t] (if (coll? t) [(first t) (inv (nth t 2)) (inv (second t))] t))]
      (= (second ft) (inv (nth ft 2))))))

(defcheck solution-478c97db
  (fn [input]
    (let [[v l r] input]
      (letfn [(left-first-to-seq [tree]
                (if (nil? tree)
                  [nil]
                  (let [[a ltree rtree] tree]
                    (concat [a]
                      (left-first-to-seq ltree)
                      (left-first-to-seq rtree)))))
              (right-first-to-seq [tree]
                (if (nil? tree)
                  [nil]
                  (let [[a ltree rtree] tree]
                    (concat [a]
                      (right-first-to-seq rtree)
                      (right-first-to-seq ltree)))))]
        (= (left-first-to-seq l)
          (right-first-to-seq r))))))

(defcheck solution-47f334a8
  #(=
     ((fn a [t] (if (sequence t) (conj [] (first t) (map a (rest t))) t)) %)
     ((fn b [t] (if (sequence t) (conj [] (first t) (map b (reverse (rest t)))) t)) %)))

(defcheck solution-47ff53ca
  (fn [[n l r ]]
    (if (= l ((fn inv [r]
                (if (coll? r)
                  (let [[c e d] r]
                    [c (inv d) (inv e)])
                  r)) r))
      true
      false)))

(defcheck solution-483fd552
  (fn sym [n]
    (let [[v l r] n
          flip (fn f [m]
                 (let [[v l r] m]
                   (if (nil? v) v
                                (list v (f r) (f l)))))]
      (= l (flip r)))))

(defcheck solution-4894230f
  (fn sym?
    ([t] (sym? (second t) (last t)))
    ([a b](or
           (and (nil? a) (nil? b))
           (if (and (coll? a) (coll? b))
             (and (= (count a) (count b) 3)
                  (= (first a) (first b))
                  (sym? (second a) (last b))
                  (sym? (last a) (second b)))
             false)))))

(defcheck solution-4922356f
  (fn mirror-tree?
    ([[_ left right]]
     (mirror-tree? left right))
    ([t1 t2]
     (or
      (and (nil? t1) (nil? t2))
      (let [[v1 l1 r1] t1
            [v2 l2 r2] t2]
        (and (= v1 v2)
             (mirror-tree? l1 r2)
             (mirror-tree? r1 l2)))))))

(defcheck solution-49bd6b58
  (fn sym? [x]
    (let [mirror (fn mirror [x]
                   (if (coll? x)
                     (let [[v l r] x]
                       [v (mirror r) (mirror l)])
                     x))]
      (= x (mirror x)))))

(defcheck solution-4a7f9bd8
  (fn sym [tree] (let [
                       root (first tree)
                       lc (second tree)
                       rc (last tree)
                       inv (fn inv [tree] (when-let [t (seq tree)]
                                            (let [f (first t) l (second t) r (last t)]
                                              (list f (inv r) (inv l)))))
                       eq (fn eq [t1 t2] (cond
                                           (nil? t1) (nil? t2)
                                           (nil? t2) (nil? t1)
                                           :else (and (= (first t1) (first t2))
                                                      (eq (second t1) (second t2))
                                                      (eq (last t1) (last t2)))))
                       ]
                   (and (eq lc (inv rc))))))

(defcheck solution-4aa8ecd0
  (fn [t]
    (let [sym? (fn _ [a b]
                 (if (nil? a) (nil? b)
                              (and
                               (= (first a) (first b))
                               (_ (second a) (nth b 2))
                               (_ (second b) (nth a 2)))))]
      (or
       (nil? t)
       (sym? (second t) (nth t 2))))))

(defcheck solution-4aaf5a1b
  (fn [t]
    (= (map first (tree-seq sequential? next t))
      (map first (tree-seq sequential? (comp reverse next) t)))))

(defcheck solution-4b2e360e
  (fn symmetric? [tree]
    (every? true? (map =
                    ;; every node in left branches
                    (map first (tree-seq next rest (second tree)))
                    ;; every node in right branches reversed
                    (map first (tree-seq next (comp reverse rest) (last tree)))))))

(defcheck solution-4b4a6bc9
  (fn [[_ l r]]
    (= l
      ((fn s [[v l r]]
         (if v [v (s r) (s l)]))
       r))))

(defcheck solution-4b5f4109
  (fn [tree]
    (letfn [(exchange [[val left right :as t]]
              [val
               (if (and (coll? right) (not (empty? right)))
                 (exchange right)
                 right)
               (if (and (coll? left) (not (empty? left)))
                 (exchange left)
                 left)])]
      (= (exchange (nth tree 1)) (nth tree 2)))))

(defcheck solution-4bd73c07
  (fn [[k a b]]
    ((fn =|= [a b]
       (if (and (coll? a) (coll? b))
         (let [[k1 a1 b1] a [k2 b2 a2] b]
           (and (= k1 k2) (=|= a1 a2) (=|= b1 b2)))
         (= a b)))
     a b)))

(defcheck solution-4bdba940
  (fn [[p q r]] ((fn f [a b] (if (= nil a b) true (if (some nil?
                                                        [a b]) false (let [[i j k] a [l m n] b]
                                                                       (and (= i l) (f j n) (f k m)))))) q r)))

(defcheck solution-4bffe5bd
  (fn [tree]
    (letfn [(mirror [[x ltree rtree]]
              (when-not (nil? x)
                [x (mirror rtree) (mirror ltree)]))]
      (= tree (mirror tree)))))

(defcheck solution-4c1fd76e
  (fn [tree] (= tree (letfn [(sewap [z] (let [[v l r] (vec z)] [v (if (coll? r) (sewap r) r) (if (coll? l) (sewap l) l)]))] (sewap tree)))))

(defcheck solution-4c6d606a
  (fn symm? [t]
    (let [[n l r] t]
      (if (every? nil? [l r])
        true
        (let [[n1 l1 r1] l
              [n2 l2 r2] r]
          (and (= n1 n2) (symm? [n1 l1 r2]) (symm? [n2 l2 r1])))))))

(defcheck solution-4d2daeb1
  (fn [[_ l r]]
    (letfn [(mirror [t]
              (when-let [[v l r] t]
                [v (mirror r) (mirror l)]))]
      (= l (mirror r)))))

(defcheck solution-4d948e52
  (fn [tree]
    (let [eq-trees?
          (fn eq-trees? [t1 t2]
            (if (nil? t1)
              (nil? t2)
              (if (nil? t2)
                false
                (let [[x1 l1 r1] t1
                      [x2 l2 r2] t2]
                  (and (= x1 x2) (eq-trees? l1 r2) (eq-trees? r1 l2))
                  ))))
          [_ l r] tree]
      (eq-trees? l r)
      )))

(defcheck solution-4d9b36a6
  (fn s-tree? [[v a b]]
    (let [v? (fn [a b] (= (first a) (first b)))

          l  (fn [n]   (second n))
          r  (fn [n]   (second (rest n)))

          twins? (fn t? [a b]

                   (or

                    (and (nil? a)  (nil? b))

                    (and

                     (and (coll? a)       (coll? b))
                     (and (= 3 (count a)) (= 3 (count b)))
                     (and
                      (v? a b)
                      (t? (l a) (r b))
                      (t? (r a) (l b))))))]

      (twins? a b)
      )))

(defcheck solution-4df16fae
  #((fn eq [[a b]]
      (if (coll? a)
        (let [[x t v] a
              [y u w] b]
          (and (= x y) (eq [t w]) (eq [u v])))
        (= a b)))
    (rest %)))

(defcheck solution-4df5afd1
  (fn symmetricX[t]
    (
     (fn equivalent[t1 t2]
       (if (nil? t1)
         (nil? t2)
         (and ( = (first t1) (first t2))
              (and
               (equivalent (second t1)(last t2) )
               (equivalent (last t1)(second t2) )
               )
              )
         )
       ) (second t) (last t))
    ))

(defcheck solution-4e1c3cbd
  #(= (second %) ((fn f [t] (if (coll? t) (let [[x l r] t] [x (f r) (f l)]) t)) (last %))))

(defcheck solution-4e6b659e
  (fn [[_ left right]]
    (let [rev (fn rev-tree [[r left right]]
                (let [rev-if-tree #(if (coll? %) (rev-tree %) %)]
                  [r (rev-if-tree right) (rev-if-tree left)]))]
      (= left (rev right)))))

(defcheck solution-4eb3ccf0
  (fn eq-tr?
    ([root]
     (eq-tr? (nth root 1) (nth root 2)))
    ([one two]
     (cond
       (and (nil? one) (nil? two)) true
       (not= (first one) (first two)) false
       :else (and (eq-tr? (nth one 1) (nth two 2))
                  (eq-tr? (nth one 2) (nth two 1)) )))))

(defcheck solution-4ef6a44a
  (fn[t]
    (letfn [(eq-t [a,b]
              (or (and (nil? a) (nil? b))
                  (and (= 3 (count a))
                       (= 3 (count b))
                       (= (first a) (first b))
                       (eq-t (nth a 1) (nth b 2))
                       (eq-t (nth a 2) (nth b 1))
                       )
                  ))]
      (eq-t (nth t 1) (nth t 2)))))

(defcheck solution-4f251ca9
  (fn s? [[_ [v1 l1 r1]
           [v2 l2 r2]]]
    (and (= v1 v2)
         (or
          (and
           (= nil l1 r2)
           (= nil l2 r1))
          (and
           (s? [_ l1 r2])
           (s? [_ l2 r1]))))))

(defcheck solution-4fb9d177
  (fn tree-symmetry [Tr]
    (letfn [(pre-order [Tr]
              (if (= Tr nil)
                [nil]
                (concat (pre-order (second Tr)) [(first Tr)] (pre-order (last Tr)))))
            ]
      (if (= Tr nil)
        true
        (if (= (pre-order (second Tr)) (reverse (pre-order (last Tr))))
          true
          false)))))

(defcheck solution-4fbefa37
  (fn [[_ left right]]
    (let [rev-tree
          (fn rev [x]
            (if (sequential? x)
              [(first x) (rev (nth x 2)) (rev (nth x 1))]
              x))]
      (or (= left right)
          (= left (rev-tree right))))))

(defcheck solution-4ffc92b4
  (fn [[h a b]]
    (= a (clojure.walk/postwalk #(if (sequential? %) (concat [(first %)] (reverse (rest %))) %) b))))

(defcheck solution-506c6f85
  (letfn [(flatten-right [[root left right]]
            (concat [root]
              (if (nil? right)
                [nil]
                (flatten-right right))
              (if (nil? left)
                [nil]
                (flatten-right left))))]
    #(= (flatten %) (flatten-right %))))

(defcheck solution-50d22d7a
  (fn [[_ left right]]
    ((fn symmetric? [left right]
       (or
        (and (nil? left) (nil? right))
        (and
         (not (nil? left))
         (not (nil? right))
         (= (first left) (first right))
         (symmetric? (second left) (nth right 2))
         (symmetric? (nth left 2) (second right)))))
     left right)))

(defcheck solution-51c4acd8
  (fn [t]
    (letfn [(reflect [t]
              (if (nil? t)
                nil
                (let [[v left right] t]
                  (list v (reflect right) (reflect left)))))]
      (= t (reflect t)))))

(defcheck solution-51d37181
  #(letfn [
           (left [c] (if (coll? c) (str (first c) (left (nth c 1)) (left (nth c 2))) (str "nil")))
           (right [c] (if (coll? c) (str (first c) (right (nth c 2)) (right (nth c 1))) (str "nil")))
           ] (= (left %) (right %))
             ))

(defcheck solution-52432c25
  (fn [[_ l r]]
    (= l
      ((fn swap-tree [[v l r :as t]]
         (when t
           [v (swap-tree r) (swap-tree l)]))
       r))))

(defcheck solution-52bdd25e
  (fn [tree]
    (let [start (fn bis [n1 n2]
                  (if (and
                       (= (count n1) 3)
                       (= (count n2) 3)
                       (= (nth n1 0) (nth n2 0)))
                    (cond
                      (and (every? nil? (rest n1)) (every? nil? (rest n2))) true
                      (and (nil? (nth n1 1)) (nil? (nth n2 2))
                           (sequential? (nth n1 2)) (sequential? (nth n2 1)))
                      (bis (nth n1 2) (nth n2 1))
                      (and (nil? (nth n1 2)) (nil? (nth n2 1))
                           (sequential? (nth n1 1)) (sequential? (nth n2 2)))
                      (bis (nth n1 1) (nth n2 2))
                      :else (and (bis (nth n1 1) (nth n2 2))
                                 (bis (nth n1 2) (nth n2 1))))
                    false))]
      (start (nth tree 1) (nth tree 2)))))

(defcheck solution-52bee62b
  (fn [s](
           every? true?
           (
            (fn[s2](
                     map-indexed
                     (fn[idx itm](= itm (nth s2 (- (count s2) (inc idx)) )))
                     s2
                     ))

            (flatten(
                     (fn f [l](
                                if(nil? l) nil
                                           (list (f (second l)) (first l) (f (last l)) )
                                           ))
                     s
                     ))
            )
           )))

(defcheck solution-53575afe
  #(= %
     ((fn t [[a b c]]
        (if a
          [a (t c) (t b)] a))
      %)))

(defcheck solution-53b230a1
  (fn symmetric
    [t]
    (letfn [(rev [child]
              (when child
                [(first child) (rev (nth child 2)) (rev (nth child 1))]))]
      (= (rev (nth t 1))
        (nth t 2)))))

(defcheck solution-53cf2fdc
  (fn beauty-symmetric
    ([x] (beauty-symmetric (first (rest x)) (first (rest (rest x)))))
    ([node1 node2]
     (do
       #_(println node1 "<>" node2))
     (if (= nil node1 node2)
       true
       (and
        (sequential? node1)
        (sequential? node2)
        (= 3 (count node1) (count node2))
        ; (println "f:" (first node1) (first node2))
        (= (first node1) (first node2))
        (let [lnode1 (first (rest node1))
              rnode1 (first (rest (rest node1)))
              lnode2 (first (rest node2))
              rnode2 (first (rest (rest node2)))]
          (and (beauty-symmetric lnode1 rnode2)
               (beauty-symmetric rnode1 lnode2))))))))

(defcheck solution-5455dab
  (fn [t]
    (let [b1 (second t)
          b2 (nth t 2)
          mirror (fn f [t]
                   (let [b1 (second t)
                         b2 (nth t 2)]
                     (if (every? nil? [b1 b2])
                       t
                       [(first t) (f b2) (f b1)])))]
      (= (mirror b1) b2))))

(defcheck solution-548b4a4e
  (fn [[_ A B]]
    (= A ((fn f [x]
            (when-let [[v a b] x] [v (f b) (f a)]))
          B))))

(defcheck solution-549c0686
  (let
   [otoc (fn otoc[tree]
           (if (nil? tree)
             tree
             (let [[aname l r] tree]
               [aname (otoc r) (otoc l)]
               )))]
    (fn [[name l r]]
      (= (otoc l) r))))

(defcheck solution-54c2aecd
  (fn sym?
    ([a]
     (sym? a a))
    ([a b]
     (cond (and (coll? a) (coll? b)) (apply sym? (concat a b))
           :else (= a b)))
    ([a b c d e f]
     (and (= a d) (sym? b f) (sym? e c)))))

(defcheck solution-54ea38e1
  (fn [[n r l]] (= ((fn m [[n r l]] (when n [n (m l) (m r)])) l) r)))

(defcheck solution-55cd2dee
  (fn my
    [n]

    (let [ node (into [] n)

          [_ l-child r-child] node

          mytest (fn mytest2 [myseq]

                   (if (= 3 (count myseq) )
                     (concat (list (first myseq))
                       (list (mytest2 (nth myseq 2)))
                       (list (mytest2 (nth myseq 1)))
                       )
                     myseq
                     )
                   )
          ]

      (= (flatten (mytest l-child))
        (flatten r-child) )
      )

    ))

(defcheck solution-56278a62
  (fn is-symmetric [[root left right]]
    (let [flip (fn flip [tree]
                 (if (nil? tree)
                   tree
                   (let [[r a b] tree]
                     [r (flip b) (flip a)])))]
      (= left (flip right)))))

(defcheck solution-567c10c1
  (fn sym?
    ([bt] (sym? (nth bt 1) (nth bt 2)))
    ([bt1 bt2]
     (let [v1 (nth bt1 0), v2 (nth bt2 0)
           l1 (nth bt1 1), l2 (nth bt2 1)
           r1 (nth bt1 2), r2 (nth bt2 2)]
       (and (= v1 v2)
            (or (and (seq l1) (seq r2) (sym? l1 r2))
                (= l1 r2))
            (or (and (seq l2) (seq r1) (sym? l2 r1))
                (= l2 r2)))))))

(defcheck solution-56b597c9
  (fn [tree]
    (letfn [(mirror [node]
              (if (sequential? node)
                [(first node)
                 (mirror (last node))
                 (mirror (second node))]
                node))]
      (= tree (mirror tree)))))

(defcheck solution-56cb2dea
  #(= ((fn f[[v l r]]
         [v (if r (f r) r) (if l (f l) l)]) (second %))
     (nth % 2)))

(defcheck solution-56d0644
  (fn [T] (let  [s ((fn f [t] (if-let [ [n l r] t]
                                (concat (f l) [n] (f r)))) T)]
            (= s (reverse s)))))

(defcheck solution-573e9d63
  (fn [[_ l r]]
    (letfn [(mirror [branch]
              (if (= 3 (count branch))
                (let [[nd l r] branch]
                  [nd (mirror r) (mirror l)])
                branch))]
      (= l (mirror r)))))

(defcheck solution-581dea22
  #(= %
     ((fn flip [[root l r :as t]]
        (when t
          [root (flip r) (flip l)])) %)))

(defcheck solution-581e935a
  (fn sym? [t]
    (letfn [(mirror? [a b]
              (cond
                (and (nil? a) (nil? b)) true
                (not (= (first a) (first b))) false
                (not (mirror? (second a) (last b))) false
                (not (mirror? (last a) (second b))) false
                :else true))]
      (mirror? (second t) (last t)))))

(defcheck solution-58534282
  (fn symm [coll]
    (let [coll (vec coll)]
      (letfn [(rev [coll]
                (if (sequential? coll)
                  (let [coll (vec coll)]
                    [(first coll)
                     (rev (coll 2))
                     (rev (coll 1))])
                  coll))]
        (= (coll 1)
          (rev (coll 2)))))))

(defcheck solution-588df662
  (fn f
    ([[a b c]] (f b c))
    ([b c]
     (if (every? coll? [b c])
       (let [[g h i] b [j k l] c]
         (and (= g j) (f h l) (f i k)))
       (= b c)))))

(defcheck solution-58e3f7c3
  (fn sim [[v l r]]
    ((fn equiv [[v1 l1 r1] [v2 l2 r2]] #_(println v1 l1 r1 v2 l2 r2 ) (or (= v1 v2 nil) (and (= v1 v2) (and (equiv l1 r2) (equiv l2 r1)))))
     l r
     )
    ))

(defcheck solution-5961e514
  (fn sym
    ([t] (sym (nth t 1) (nth t 2)))
    ([a b]
     (cond
       (and (nil? a) (nil? b)) true
       (and (coll? a) (coll? b)) (and (sym (first a) (first b))
                                      (sym (nth a 1) (nth b 2))
                                      (sym (nth b 1) (nth a 2)))
       (= a b) true
       :else false))))

(defcheck solution-5983a472
  (fn symmetry [tree] (let [f (fn reshape [t] (if (nil? t)
                                                nil
                                                [(reshape (second t)) (first t) (reshape (last t))]))
                            frt (flatten (f tree))]
                        (= frt (reverse frt)))))

(defcheck solution-59bf067
  (fn [tree]
    (letfn [(cmp-branches [x y]
              (if (= (first x)
                    (first y))
                (if (nil? x)
                  true
                  (and (cmp-branches (second x)
                         (last y))
                       (cmp-branches (last x)
                         (second y))))
                false))]
      (cmp-branches (second tree)
        (last tree)))))

(defcheck solution-5a0f3fde
  (fn sun? [n]
    (letfn[
           (sym [s] (if (nil? s) nil (concat [(first s)] [(sym (last s))] [(sym (second s))])))]
      (or (nil? n) (and (= 3 (count n)) (= (second n) (sym (last n))))))))

(defcheck solution-5a4f0549
  (fn sym? [t]
    (letfn [(mirror [t]
              (if (nil? t)
                nil
                (let [[e l r] t]
                  [e (mirror r) (mirror l)])))]
      (let [[_ l r] t]
        (= l (mirror r))))))

(defcheck solution-5a4f1f80
  (fn e ([[_ l r]] (e l r))
    ([l r] (if (nil? l)
             (nil? r)
             (let [[v1 l1 r1] l [v2 l2 r2] r]
               (and (= v1 v2)(e l1 r2) (e r1 l2)))))))

(defcheck solution-5a54dc4c
  (fn symmetric [t]
    (letfn [(mirror [a b]
              (or (and (nil? a ) (nil? b))
                  (let [la (nth a 1) lb (nth b 1) ra (nth a 2) rb (nth b 2)]
                    (and (= (first a) (first b))
                         (mirror la rb)
                         (mirror ra lb)))))]
      (mirror (nth t 1) (nth t 2)))))

(defcheck solution-5a709c28
  #(= % ((fn mir [[r le ri :as tr]]
           (if tr (conj [] r (mir ri) (mir le)))) %)))

(defcheck solution-5b29a0f2
  #(= %
     ((fn f [[v l r :as n]]
        (if n
          [v (f r) (f l)]))
      %)))

(defcheck solution-5b7ccea9
  #(= % ((fn f [s] (if (nil? s) nil (vector (nth s 0) (f (nth s 2)) (f (nth s 1))))) %)))

(defcheck solution-5b8264e1
  (fn beautiful? [tree] (letfn [(mirror [[v l r :as xs]]
                                  (if (nil? xs)
                                    nil
                                    (list v (mirror r) (mirror l))))]
                          (= (second tree) (mirror (last tree))))))

(defcheck solution-5b8b98ff
  apply #(= ((fn m[[x l r]] (if x [x (m r) (m l)])) %2) %3))

(defcheck solution-5bc4a6ed
  (fn [[n l r]]
    ((fn ? [l r]
       (cond
         (and (nil? l) (nil? r)) true
         (and (sequential? l) (sequential? r)) (let [[ln ll lr] l
                                                     [rn rl rr] r] (and (= ln rn)
                                                                        (? ll rr)
                                                                        (? lr rl)))
         :else false
         )) l r)))

(defcheck solution-5bdb8060
  (fn symetric-tree [coll]
    (letfn
     [(flip-tree [coll]
        (cond (nil? coll) nil
              :else (let [[n l r] coll]
                      [n (flip-tree r) (flip-tree l)]
                      )))]
      (= (flip-tree coll) coll))))

(defcheck solution-5c1416ab
  (fn [c] (letfn [ (swap-node [cl] (when-let [c cl] [(first c) (swap-node (last c)) (swap-node (second c))])) ]
            (= (second c) (swap-node (last c)))
            )))

(defcheck solution-5c3981a
  #(let [tree ((fn ! [[v left right :as node]]
                 (if (coll? node)
                   (concat (! left) [v] (! right))
                   [v]))
               %)]
     (= tree (reverse tree))))

(defcheck solution-5c6e641f
  (fn symmetric? [tree]
    (letfn [(reverse-tree [tree]
              (if (nil? tree)
                nil
                (vector (first tree)
                  (reverse-tree (last tree))
                  (reverse-tree (second tree)))))]
      (= tree (reverse-tree tree)))))

(defcheck solution-5d0cbebf
  #((fn mirror? [l r] (
                        if (not (coll? l)) (= l r)
                                           (and (coll? r) (= (first l) (first r))
                                                (mirror? (nth l 1) (nth r 2))
                                                (mirror? (nth l 2) (nth r 1)))
                                           )) (nth % 1) (nth % 2)))

(defcheck solution-5d8ef5e6
  (fn [[_ y z]]
    (= y
      ((fn r [xs]
         (if (coll? xs)
           (let [[x y z] xs]
             [x (r z) (r y)])
           xs)) z))))

(defcheck solution-5d912f5e
  (fn f [d]
    (let [mirror-b-tree (fn m? [[v l r :as a]]
                          (when-not (nil? v)
                            [v (if (coll? r) (m? r) r) (if (coll? l) (m? l) l)]))]
      (= d (mirror-b-tree d)))))

(defcheck solution-5db8548d
  (fn fff
    ([n] (fff (second n) (nth n 2)))
    ([l r] (or (and (nil? l) (nil? r))
               (and (= (first l) (first r))
                    (fff (second l) (nth r 2))
                    (fff (nth l 2) (second r)))))))

(defcheck solution-5e31c923
  #(letfn [(scan [cs-fn] (->> % (tree-seq (complement nil?) cs-fn) (map first)))]
     (= (scan rest)
       (scan (comp reverse rest)))))

(defcheck solution-5e420fde
  (fn aaa [[V L R]]
    (letfn [(aa [[v l r]]
              (if (= nil r)
                (if (= nil l) [v r l] [v r (aa l)])
                (if (= nil l) [v (aa r) l] [v (aa r) (aa l)])
                )
              )]
      (if (= (aa L) R) true false))))

(defcheck solution-5e79f822
  (fn [t]
    ((fn mirror? [l r]
       (if (or (= nil l r)
               (and (= (first l) (first r))
                    (mirror? (second l) (last r))
                    (mirror? (last l) (second r))))
         true false))
     (second t) (last t))))

(defcheck solution-5e9f2af9
  (fn [coll]
    (let [walk-fn (fn [xs mirror?]
                    (clojure.walk/postwalk
                      (fn [form]
                        (if (and mirror?
                                 (coll? form))
                          [(first form) (last form) (second form)]
                          form))
                      xs))]
      (= (walk-fn (second coll) false)
        (walk-fn (last coll) true)))))

(defcheck solution-5f595059
  (fn sym [t]
    (let [left  (nth t 1)
          right (nth t 2)
          flip  (fn flip [t]
                  (if (nil? t)
                    t
                    [(nth t 0) (flip (nth t 2)) (flip (nth t 1))]))]
      (= (flip left) right))))

(defcheck solution-5f62a04e
  (fn symtree
    ([[_ l r]] (symtree l r) )
    ([l r]
     (if (and (seq l) (seq r))
       (let [[lroot l1 l2] l [rroot r1 r2] r]
         (and
          (= lroot rroot)
          (symtree l1 r2)
          (symtree l2 r1)))
       (= l r)))))

(defcheck solution-5fd0d1de
  (fn [coll]
    (letfn [(sym [l r]
              (if (or (nil? l) (nil? r))
                (= l r)
                (and (= (first l) (first r))
                     (sym (second l) (last r))
                     (sym (last l) (second r)))))]
      (sym (second coll) (last coll)))))

(defcheck solution-5fdee9ff
  (fn symmetric? [tree]
    (let [
          mirror (fn mirror [tree]
                   (when (seq tree)
                     (let [[value left right] tree]
                       [value (mirror right) (mirror left)])))
          [value left right] tree
          ]
      (= left (mirror right)))))

(defcheck solution-6009224d
  (fn mi? [t]
    (letfn [(mi [t]
              (let [n (first t) l (second t) r (last t)]
                (if (and (nil? l) (nil? r))
                  t
                  [n (mi r) (mi l)])))]
      (= t (mi t)))))

(defcheck solution-603c8475
  (fn [p]
    (letfn [(trav1 [[v a b]]
              (if (nil? v) "nil" (str v " " (trav1 a) " " (trav1 b))))
            (trav2 [[v a b]]
              (if (nil? v) "nil" (str v " " (trav2 b) " " (trav2 a))))]
      (= (trav1 p) (trav2 p)))))

(defcheck solution-608a9c16
  #((fn mir [l r]
      (or
       (= nil (first l ) (first r))
       (and
        (= (first l ) (first r))
        (mir (nth l 2 )  (nth r 1))
        (mir (nth l 1 )  (nth r 2))
        )
       )
      )
    (second %) (last %)))

(defcheck solution-6095c898
  (fn symmetry [[root left right]]
    (let [mirror? (fn mirror? [a b]
                    (cond
                      (not= (seq? a) (seq? b)) false
                      (sequential? a) (let [[ra La Ra] a
                                            [rb Lb Rb] b]
                                        (and (= ra rb) (mirror? La Rb) (mirror? Lb Ra)))
                      :else (= a b)))]
      (mirror? left right))))

(defcheck solution-609c41c9
  (letfn [(flip [[value left right :as tree]]
            (when (not (nil? tree))
              [value (flip right) (flip left)]))]
    (fn [[_ left right]]
      (= left (flip right)))))

(defcheck solution-60f0bff0
  (fn btree-symm? [coll]
    (letfn [(wlk [coll order]
              (if (coll? coll)
                (let [[v l r] coll
                      res (list v)
                      lc (wlk l order)
                      rc (wlk r order)]
                  (if (= :leftmost order)
                    (cons lc (cons rc res))
                    (cons rc (cons lc res))))))]
      (let [lc (flatten (wlk (nth coll 1) :leftmost))
            rc (flatten (wlk (nth coll 2) :rightmost))]
        (= lc rc)))))

(defcheck solution-610ecc1a
  (fn [[c l r]]
    (letfn [(g [[c l r :as t]]
              (when-not (nil? t)
                [(g l) c (g r)]))]
      (= (reverse (flatten (g l))) (flatten (g r)) ))))

(defcheck solution-611167d5
  (fn p96 [lst]
    (letfn [(tree? [lst]
              (or (nil? lst)
                  (and (coll? lst) (= 3 (count lst))
                       (let [[v l r] lst] (and (tree? l) (tree? r))))))
            (revtree [lst]
              (if (nil? lst) lst
                             (if (and (coll? lst) (= 3 (count lst))
                                      ) (let [[v l r] lst] [v (revtree r) (revtree l)]) nil)))]
      (let [t1 (tree? lst)
            [rt1 rt2] (let [[v l r] lst] [(revtree l) (revtree (revtree r))])] (and t1 (= rt1 rt2))))))

(defcheck solution-61c04152
  (let [flip-tree (fn flip-tree [[a l r :as n]] (if n [a (flip-tree r) (flip-tree l)]))]
    (fn [[a l r :as n]]
      (if n
        (= l (flip-tree r))
        true))))

(defcheck solution-61e1032
  (fn [t]
    (let [i nth]
      ((fn f[m n]
         (if (and (nil? m)
                  (nil? n)) true
                            (and (= (i n 0)
                                   (i m 0))
                                 (f (i n 1)
                                   (i m 2))
                                 (f (i n 2)
                                   (i m 1))))) (i t 1) (i t 2)))))

(defcheck solution-61fe2e0a
  (fn [[_ left right]]
    (let [mirror (fn mirror [[n l r]]
                   (let [l' (if (coll? l) (mirror l) l)
                         r' (if (coll? r) (mirror r) r)]
                     [n r' l']))]
      (= left (mirror right)))))

(defcheck solution-62347a27
  (fn sym?
    ([t] (sym? (second t) (last t)))
    ([a b] (or
            (and (nil? a) (nil? b))
            (if (and (coll? a) (coll? b))
              (and (= (count a) (count b) 3)
                   (= (first a) (first b))
                   (sym? (second a) (last b))
                   (sym? (last a) (second b)))
              false)))))

(defcheck solution-62f6e259
  (fn sym? [c]
    (letfn [(mirrorImages? [l r] (or (and (nil? l) (nil? r))
                                     (and (= (first l) (first r))
                                          (mirrorImages? (second l) (last r))
                                          (mirrorImages? (last l) (second r))))
              )
            ]
      (mirrorImages? (second c) (last c))
      )))

(defcheck solution-63abe81d
  (fn [t]
    (= t
      ((fn rvs [[e & ch]]
         (cons
           e
           (reverse
             (map
               #(if (sequential? %) (rvs %)
                                    %)
               ch)))) t))))

(defcheck solution-64f93b2e
  (fn [[t1 t2 t3]]
    (let [tf (fn tf [[a b c :as t]]
               (if (nil? t)
                 t
                 (let [n a l b r c]
                   [n (tf r) (tf l)])))]
      (= t2 (tf t3)))))

(defcheck solution-6504278a
  (fn [t]
    (letfn [(mirror [t]
              (if (sequential? t)
                (list (first t) (mirror (first (next (next t)))) (mirror (second t)))
                t))]
      (and (sequential? t)
           (= (mirror (second t)) (first (next (next t))))))))

(defcheck solution-6550b2c4
  (fn [[v l r]]
    (let
     [mirror
      (fn mirror [t]
        (if (nil? t)
          nil
          (let [[v2 l2 r2] t]
            [v2 (mirror r2) (mirror l2)])))]
      (= l (mirror r)))))

(defcheck solution-6596b710
  (fn sym?
    ([t] (sym? (second t) (last t)))
    ([a b]
     (if (and (nil? a) (nil? b))
       true
       (if (or (nil? a) (nil? b))
         false
         (and
          (= (first a) (first b))
          (sym? (second a) (last b))
          (sym? (second b) (last a))))))))

(defcheck solution-65b9b434
  (fn [c]
    (let [mirror (fn mirror [coll]
                   (let [[n left right] coll]
                     [n (if right (mirror right)) (if left (mirror left))]))]
      (let [[n left right] c]
        (= left (mirror right))))))

(defcheck solution-661017a3
  (fn [root]
    (letfn [(symetric? [a b]
              (if (or (= nil a) (= nil b))
                (= nil a b)
                (and (= (first a) (first b)) (symetric? (second a) (last b)) (symetric? (last a) (second b)))))]
      (symetric? (second root) (last root)))))

(defcheck solution-670da34f
  (fn [[val left right]]
    (let
     [mirror-tree
      (fn mirror-tree [tree]
        (cond
          (nil? tree) nil
          :else
          (let
           [[value left right] tree]
            [value (mirror-tree right) (mirror-tree left)])))]
      (= left (mirror-tree right)))))

(defcheck solution-6739e964
  (fn __ [tree]
    (letfn
     [(mirror-tree [t]
        (if (not (sequential? t))
          t
          (list
            (first t)
            (mirror-tree (nth t 2))
            (mirror-tree (nth t 1)))))]
      (= (nth tree 1) (mirror-tree (nth tree 2))))))

(defcheck solution-67769233
  (fn symm
    ([x] (or (nil? x) (symm (nth x 1) (nth x 2))))
    ([x y]
     (cond
       (and (nil? x) (nil? y)) true
       (nil? x) false
       (nil? y) false
       (not (= (first x) (first y))) false
       true (and (symm (nth x 1) (nth y 2))
                 (symm (nth x 2) (nth y 1)))))))

(defcheck solution-67fd4689
  (fn tree-diff [tree]
    (letfn [(s [t r] (map first(tree-seq #(some identity (rest %))
                                 #((if r reverse identity)
                                   (filter identity (rest %)))
                                 t)))]
      (= (s (nth tree 1) false) (s (nth tree 2) true)))))

(defcheck solution-688eaafe
  (fn [node]
    (let [flip (fn flip [node]
                 (let [node-value (first node)
                       left-node (second node)
                       right-node (nth node 2)
                       ]
                   (list
                     node-value
                     (if right-node (flip right-node) nil)
                     (if left-node (flip left-node) nil))
                   )
                 )]
      (= (flip (second node)) (nth node 2))
      )
    ))

(defcheck solution-68c369a9
  (fn [a-seq]
    (if (or
         (not= 3 (count a-seq))
         (coll? (first a-seq))
         (nil? (first a-seq))
         (some false? a-seq)
         (some true? a-seq))
      false
      (let [node (first a-seq)
            branch-1 (second a-seq)
            branch-2 (second (rest a-seq))
            mirror (fn mir# [b-tree]
                     (if (= (second b-tree)
                           (second (rest b-tree)))
                       b-tree
                       (cons
                         (first b-tree)
                         (list
                           (mir# (second (rest b-tree)))
                           (mir# (second b-tree))))))]
        (if (and (not= branch-1 branch-2)
                 (not= (mirror branch-1) branch-2))
          false
          true)))))

(defcheck solution-68f93aae
  (fn f ([s] (let [g (fn g [x] (if (coll? x) (concat (g (second x)) [(first x)] (g (nth x 2))) [x]))
                   r (g s)]
               (= r (reverse r))))))

(defcheck solution-69080426
  (fn [coll] (= coll ((fn f [[v l r]] (if v [v (f r) (f l)])) coll))))

(defcheck solution-691a0d3a
  (fn [t] (
           (fn f [t1 t2]
             (if-not (or t1 t2)
               (= t1 t2)
               (let [[p1 l1 r1] t1
                     [p2 l2 r2] t2]
                 (and (= p1 p2)
                      (f l1 r2)
                      (f l2 r1))))) t t)))

(defcheck solution-6928e28f
  (fn __ [t]
    (letfn [(mirror [l r]
              (or (and (nil? l) (nil? r))
                  (and (not (nil? l))
                       (not (nil? r))
                       (= (first l) (first r))
                       (mirror (second l) (second (rest r)))
                       (mirror (second (rest l)) (second r)))))]
      (mirror (second t) (second (rest t))))))

(defcheck solution-6938e559
  (fn mirror? [[root left-child right-child]]
    (= left-child
      ((fn mirror [branch]
         (if-not (coll? branch)
           branch
           [(first branch)
            (mirror (last branch))
            (mirror (second branch))]))
       right-child))))

(defcheck solution-698d57cb
  (fn [t]
    (= ((fn f [[h l r :as tree]]
          (when tree
            [h (f r) (f l)]))
        t)
      t)))

(defcheck solution-69bb41ec
  #(= (map first (tree-seq next rest %)) (map first (tree-seq next (comp reverse rest) %))))

(defcheck solution-69d29bad
  (fn symmetric? [t] (letfn [(symmetric [t] (if
                                             (sequential? t)
                                              (let [[root left right] t] (list root (symmetric right) (symmetric left)))
                                              t))] (= t (symmetric t)))))

(defcheck solution-69db97c
  (fn symmetrical?
    [tree]
    (letfn [(reorder [t]
              (if (= (count t) 3)
                (list (reorder (second t)) (first t) (reorder (last t)))
                t))]
      (let [reordered (flatten (reorder tree))]
        (= reordered (reverse reordered))))))

(defcheck solution-6a10455b
  (fn bt[t] (
              = ((fn tsq[t](
                             if (nil? t)
                             nil
                             (concat [(first t)] (tsq (nth t 1)) (tsq (nth t 2)))
                             )) (nth t 1))

              ((fn tsq[t](
                           if (nil? t)
                           nil
                           (concat [(first t)] (tsq (nth t 2)) (tsq (nth t 1)))
                           )) (nth t 2))


              )))

(defcheck solution-6a7e587a
  (letfn [(r [[a b c :as u] [d e f :as v]]
            (or (= nil u v)
                (and (= a d) (r b f) (r c e))))]
    #(apply r (rest %))))

(defcheck solution-6af10681
  #(let [[value l r] %]
     (letfn [(mirror [s]
               (cond
                 (nil? s) nil
                 (not (coll? s)) s
                 :else [(first s) (mirror (last s)) (mirror  (second s))])
               )]
       (= l (mirror r)))))

(defcheck solution-6b1b7f0d
  #(letfn [(r [l]
             (if (sequential? l)
               (cons (first l)(map r (reverse (rest l))))
               l))]
     (= % (r %))))

(defcheck solution-6b5f7300
  (fn symm? [tree]
    (letfn [(symmetry? [left-child right-child]
              (cond
                (= [true true]
                  [(nil? left-child) (nil? right-child)]) true
                (= [false false]
                  [(nil? left-child) (nil? right-child)]) (and (= (first left-child) (first right-child))
                                                               (symmetry? (second left-child) (last right-child))
                                                               (symmetry? (last left-child) (second right-child)))
                :else false))]
      (symmetry? (second tree) (last tree)))))

(defcheck solution-6bc7c9f0
  (fn t [[n left right]]
    (letfn [(mirror [a b]
              (if (coll? a)

                (and (= (first a) (first b))
                     (mirror (nth a 1) (nth b 2))
                     (mirror (nth b 1) (nth a 2))
                     )

                (= a b)
                )
              )]
      (mirror left right)
      )
    ))

(defcheck solution-6bdd87a6
  (fn sym-tree?
    ([xs ys] (let [[xv xl xr] xs
                   [yv yl yr] ys]
               (and
                (= xv yv)

                (if-not (and (coll? xl) (coll? yr))
                  (= xl yr)
                  (sym-tree? xl yr))

                (if-not (and (coll? xr) (coll? yl))
                  (= xr yl)
                  (sym-tree? xr yl)))))

    ([xs] (sym-tree? (nth xs 1) (nth xs 2)))))

(defcheck solution-6bf16061
  (fn [t]
    (every?
      #(= % (reverse %))
      (vals
        ((fn f [d n r]
           (if (coll? n)
             (f (inc d) (nth n 2) (f (inc d) (second n) (assoc r d (conj (r d []) (first n)))))
             (assoc r d (conj (r d []) n))))
         0 t {})))))

(defcheck solution-6c7c89c3
  (fn [[root left right]]
    (=
      ((fn mirror [[r le ri]]
         (if r
           [r (mirror ri) (mirror le)]
           r)) left)
      right)))

(defcheck solution-6d6c9030
  (fn [x]
    (= x ((fn sm [y]
            (let [[f s l] [(first y) (second y) (last y)]]
              (if  (nil? y) nil
                            (list f (sm l) (sm s)) )))
          x)) ))

(defcheck solution-6dc71b9c
  (fn symetric?
    [coll]
    (let [m? (fn mirror? [l r]
               (if (or (nil? l) (nil? r)) (and (nil? l) (nil? r))
                                          (and (= (first l) (first r))
                                               (= 3 (count l))
                                               (= 3 (count r))
                                               (mirror? (second l) (last r))
                                               (mirror? (last l) (second r)))))]
      (m? (second coll) (last coll)))))

(defcheck solution-6dd88d9e
  (fn sym-tree? [tree]
    (letfn [(walk-tree [n] (if (nil? n) nil (concat (walk-tree (nth n 1)) [(first n)]  (walk-tree (nth n 2)))))]
      (= (walk-tree (second tree)) (reverse (walk-tree (last tree)))))
    ))

(defcheck solution-6e0e8988
  (fn sym [[root left right]]
    (cond
      (= nil left right) true ; all nil: single node tree
      (and left right) ; no nils
      (and
       (= (first left) (first right)) ; root value matches
       (sym [nil (nth left 1) (nth right 2)]) ; left left matches right right
       (sym [nil (nth left 2) (nth right 1)]) ; left right matches right left
       )
      :else false ; left or right nil
      )
    ))

(defcheck solution-6e1367a7
  (fn symmetric? [[node left right :as tree]]
    (letfn [(deep-reverse [tree]
              (when tree
                (let [[node left right] tree]
                  (list node (deep-reverse right) (deep-reverse left)))))]
      (= left (deep-reverse right)))))

(defcheck solution-6e21a7c3
  (fn sd? [t]
    (letfn [(sd1? [c1, c2]
              (cond
                (and (nil? c1) (nil? c2)) true
                (or (and (sequential? c1) (not (sequential? c2)))
                    (and (sequential? c2) (not (sequential? c1)))) false
                :else (and (= (first c1) (first c2))
                           (sd1? (second c1) (second (rest c2)))
                           (sd1? (second (rest c1)) (second c2) ))))]
      (sd1? (second t) (second (rest t))))))

(defcheck solution-6e2c10ae
  (fn ?
    ([t] (? t t))
    ([[a b c :as l] [d e f :as r]]
     (or (every? nil? [l r])
         (and (= a d)
              (? b f)
              (? c e))))))

(defcheck solution-6ea08944
  (fn [g]
    (let [aux (fn aux [a b]
                (or
                 (and (nil? a) (nil? b))
                 (and (= (first a) (first b))
                      (aux (second a) (last b))
                      (aux (last a) (second b)))))]
      (aux (second g) (last g)))))

(defcheck solution-6ebbc40f
  #(letfn [(t? [x]
             (or (nil? x)
                 (and
                  (coll? x)
                  (= 3 (count x))
                  (t? (second x))
                  (t? (last x)))))
           (f [l r t]
             (if (nil? t)
               nil
               [(f l r (l t)) (first t) (f l r (r t))]))]
     (and (t? %)
          (= (f second last (second %))
            (f last second (last %))))))

(defcheck solution-6ec7e427
  (fn [tree]
    (let [
          mirror (fn m [a]
                   (cond
                     (= a nil) nil
                     :else (list (first a) (m (last a)) (m (second a)))))]
      (= tree (mirror tree)))))

(defcheck solution-6eebe40
  (fn [t]
    (let [flip (fn flip [subt]
                 (if (coll? subt)
                   (map flip ((juxt first last second) subt))
                   subt))]
      (= (flip (second t)) (last t)))))

(defcheck solution-6ef8684e
  #(letfn [(flip [tree] (if-let [[x l r] tree]
                          [x (flip r) (flip l)]))]
     (= % (flip %))))

(defcheck solution-6f4b5c33
  (fn prob96 [[_ l r]]
    (if (not (coll? l))
      true
      (if (not (and (coll? l) (coll? r)))
        false
        (let [[lv ll lr] l
              [rv rl rr] r]
          (and (= lv rv) (prob96 [nil ll rr]) (prob96 [nil rl lr]))
          )))))

(defcheck solution-6ffd2638
  (fn r [t]
    (letfn [(rev [tree] (if (sequential? tree) [(nth tree 0) (rev (nth tree 2)) (rev (nth tree 1))] tree))]
      (= t (rev t)))))

(defcheck solution-70af073a
  (fn [n]
    (apply = (for [f [identity reverse]]
               (map #(if (coll? %) (first %) %)
                 (tree-seq coll? #(f (rest %)) n))))))

(defcheck solution-70b4f696
  (fn [[_ l r]]
    (letfn [(values [tree]
              (if (nil? tree)
                []
                (let [[v l r] tree]
                  (concat (values l) [v] (values r)))))]
      (= (values l) (reverse (values r))))))

(defcheck solution-70dcb560
  (fn [[_ ll rr]]
    (= rr ((fn it [[k l r]] [k (if r (it r) nil)
                             (if l (it l) nil)]) ll))))

(defcheck solution-71062781
  #(= ((fn mirror [[n l r :as tree]] (when tree [n (mirror r) (mirror l)])) %) %))

(defcheck solution-719f8447
  (fn sym?
    ([t] (sym? (nth t 1) (nth t 2)))
    ([l r]
     (or
      (and (nil? l) (nil? r))
      (and (= (first l) (first r))
           (sym? (nth l 1) (nth r 2))
           (sym? (nth l 2) (nth r 1)))))))

(defcheck solution-71a3384d
  (fn symetric?
    ([tree] (symetric? (nth tree 1) (nth tree 2)))
    ([treeA treeB]
     (if (= treeA treeB nil)
       true
       (let [
             rootA (first treeA)
             rootB (first treeB)
             subtreeA-left (nth treeA 1 nil)
             subtreeA-right (nth treeA 2 nil)
             subtreeB-left (nth treeB 1 nil)
             subtreeB-right (nth treeB 2 nil)
             ]

         (if (not= rootA rootB)
           false
           (and
            (symetric? subtreeA-left subtreeB-right)
            (symetric? subtreeA-right subtreeB-left)
            )
           )
         )
       )
     )
    ))

(defcheck solution-71f002f2
  #((fn sym? [l r] (if (and (coll? l) (coll? r))
                     (and (= (first l)
                            (first r))
                          (sym? (nth l 1) (nth r 2))
                          (sym? (nth l 2) (nth r 1)))
                     (= l r)))
    (nth % 1) (nth % 2)))

(defcheck solution-72212727
  (fn f [[a b c]]
    (if (and (coll? b) (coll? c))
      (and (= (first b) (first c))
           (f [a (second b) (last c)])
           (f [a (last b) (second c)]))
      (= b c))
    ))

(defcheck solution-725faa6f
  (fn [[_ l r]]
    (letfn [(f [[v l r]]
              (if (or v l r)
                [v (f r) (f l)]
                v))]
      (= l (f r)))))

(defcheck solution-7261212c
  #(= %
     ((fn flp[[v l r :as n]]
        (when n
          [v (flp r) (flp l)]))
      %)))

(defcheck solution-7283216c
  (fn mirr?[node]
    (= node ((fn mirror[elem]
               (let [head (first elem) lc (fnext elem) rc (last elem)]
                 (cond
                   (= lc rc) elem
                   (and (not (nil? lc)) (nil? rc)) [head rc (mirror lc)]
                   (and (nil? lc) (not (nil? rc))) [head (mirror rc) lc]
                   :else [head (mirror rc) (mirror lc)]))) node))))

(defcheck solution-72933494
  (fn [tree]
    (letfn [(rev [tree]
              (if (sequential? tree)
                (list (first tree) (rev (last tree)) (rev (second tree)))
                tree))]
      (= tree (rev tree)))))

(defcheck solution-72da5020
  (fn s
    ([[n a b]] (s a b))
    ([[n1 a1 b1] [n2 a2 b2]]
     (cond (and (nil? n1) (nil? n2)) true
           (or (nil? n1) (nil? n2)) false
           :else (and (= n1 n2) (s a1 b2) (s a2 b1))))))

(defcheck solution-72df12e
  (fn symmetric?
    [t]
    (let [mirrors? (fn [t1 t2]
                     (= (map first (tree-seq (comp not nil?) rest t1))
                       (map first (tree-seq (comp not nil?) (comp reverse rest) t2))))]
      (mirrors? (first (rest t)) (first (rest (rest t)))))))

(defcheck solution-72e39b92
  (fn [tree]
    (let [flip (fn flip [ node ]
                 (cond
                   (nil? node) node
                   :else (let [ [n l r] node] [n (flip r) (flip l)])))]
      (= tree (flip tree)))
    ))

(defcheck solution-73a03075
  (fn f [[_ b c]]
    (and
     (= (count b) (count c))
     (= (nth b 0) (nth c 0))
     (or
      (and (nil? b) (nil? c))
      (and
       (f [nil (nth b 2) (nth c 1)])
       (f [nil (nth c 2) (nth b 1)]))))))

(defcheck solution-74b98daf
  (fn comptree
    ([[h l r]] (comptree l r))
    ([l r] (or (and (nil? l) (nil? r))
               (and (sequential? l)
                    (sequential? r)
                    (= (first l) (first r))
                    (comptree (second l) (last r))
                    (comptree (second r) (last l)))))))

(defcheck solution-74ef740f
  (fn [coll]
    (letfn [(node? [t] (and (coll? t)
                            (= 3 (count t))
                            (not (nil? (first t)))))
            (node-val [t] (nth t 0))
            (lchild [t] (nth t 1))
            (rchild [t] (nth t 2))
            (mirrors? [t1 t2]
              (cond
                (and (nil? t1)
                     (nil? t2)) true
                (and (node? t1)
                     (node? t2)) (and (= (node-val t1) (node-val t2))
                                      (mirrors? (lchild t1) (rchild t2))
                                      (mirrors? (rchild t1) (lchild t2)))
                (and (not (node? t1))
                     (not (node? t2))) (= (t1 t2))
                :else false))]
      (mirrors? (lchild coll) (rchild coll)))))

(defcheck solution-751701aa
  (fn sym?
    ([[_ n n']]
     (sym? n n'))
    ([n n']
     (if (and (coll? n) (coll? n'))
       (let [[v l r] n
             [v' l' r'] n']
         (and (= v v') (sym? l r') (sym? l' r)))
       (and (nil? n) (nil? n'))))))

(defcheck solution-75a53c06
  #(
    (fn s [[a b c :as x] [k l m :as y]]
      (or
       (not (or x y))
       (and (= a k) (s b m) (s c l)))) % %))

(defcheck solution-75a61285
  (fn b [[i l r]]
    (letfn [(revtree [t]
              (if (coll? t)
                (let [[i l r] t]
                  [i (revtree r) (revtree l)])
                t))]
      (= l (revtree r)))))

(defcheck solution-75cfa741
  (fn [[n l r]]
    ((fn m [t1 t2]
       (cond (= [t1 t2] [nil nil]) true
             (and t1 t2)
             (let [[n1 l1 r1] t1
                   [n2 l2 r2] t2]
               (and (= n1 n2)
                    (m l1 r2)
                    (m r1 l2)))
             :else false
             )) l r)))

(defcheck solution-75e14d4e
  (fn symmetric? [t]
    (letfn [(revTree [[v l r]]
              (list v (if (coll? r) (revTree r) r) (if (coll? l)(revTree l) l)))]
      (= t (revTree t)))))

(defcheck solution-7605814e
  (fn [[x l r]]
    (letfn [(trav [[x l r]]
              (if x
                (concat (trav l) [x] (trav r))
                []))]
      (= (trav l) (reverse (trav r))))))

(defcheck solution-76092ab
  (fn [[_ b c]]
    (= b (clojure.walk/postwalk
           #(if (sequential? %)
              (let [[a b c] %]
                [a c b])
              %)
           c))))

(defcheck solution-76557e72
  (fn [t]
    (apply
      (fn sym? [t1 t2]
        (or
         (= t1 t2 nil)
         (let [[v1 l1 r1] t1
               [v2 l2 r2] t2]
           (and
            (= v1 v2)
            (sym? l1 r2)
            (sym? l2 r1)))))
      (rest t))))

(defcheck solution-768f4e34
  (fn sym-btree?
    ([tr]
     (let [lft (first (rest tr))
           rgt (last tr)]
       (sym-btree? lft rgt)))
    ([lft rgt]
     (if (sequential? lft)
       (and (= (first lft) (first rgt))
            (sym-btree? (first (rest lft))
              (last rgt))
            (sym-btree? (first (rest rgt))
              (last lft)))
       (= lft rgt)))))

(defcheck solution-76ad145e
  (fn [[root left right]]
    (let [mirror (fn m [[n l r]]
                   (let [new-r (if (seq l) (m l) l)
                         new-l (if (seq r) (m r) r)]
                     [n new-l new-r]))]
      (= (mirror left) right))))

(defcheck solution-76eb823
  (fn sym [root]
    (letfn [(trav [t]
              (cond (nil? t) t
                    :else (concat (trav (nth t 1)) [(first t)] (trav (nth t 2)))))]
      (= (trav (nth root 1)) (reverse (trav (nth root 2)))))))

(defcheck solution-76f3fef7
  (fn [[_ L R]]
    (letfn
     [(flip [[v l r]] (list v (if (coll? r) (flip r) r) (if (coll? l) (flip l) l)))]
      (= L (flip R)))))

(defcheck solution-7723887a
  (fn [c]
    (letfn [(reverse-tree [t]
              (if (sequential? t)
                [(first t) (reverse-tree (last t)) (reverse-tree (second t))]
                t))]
      (= (reverse-tree (second c)) (last c)))))

(defcheck solution-77417d18
  (fn [tt] (= tt ((fn mirror [t] (if (coll? t) (list (first t) (mirror (nth t 2)) (mirror (nth t 1))) t)) tt))))

(defcheck solution-7743e715
  (fn symmetric? [tree]
    (letfn
     [(flip [tree]
        (if (nil? tree)
          nil
          (let [[v l r] tree]
            [v (flip r) (flip l)])))
      ]
      (= tree (flip tree)))))

(defcheck solution-77be2804
  (fn [[a b c]]
    (letfn [(flip [x]
              (if (sequential? x)
                (let [[r l s] x]
                  [r (flip s) (flip l)])
                x))]
      (= b (flip c)))))

(defcheck solution-782af254
  (fn [n]
    (letfn [
            (flip [n]
              (if (coll? n) (let [v (first n) l (second n) r (nth n 2)] [v (flip r) (flip l)]) n))]
      (= n (flip n)))))

(defcheck solution-78379054
  (fn trequal? [t]
    (let [mirror? (fn mirror? [l r]
                    (or (and (nil? l) (nil? r))
                        (and (= (first l) (first r))
                             (mirror? (second l) (last r))
                             (mirror? (last l) (second r)))))]
      (mirror? (second t) (last t)))))

(defcheck solution-784a54a1
  (fn [tree]
    (letfn [(rev [t]
              (if (sequential? t) (#(vector (first %)
                                      (rev (last %))
                                      (rev (second %))) t) t))
            ]
      (= tree (rev tree)))))

(defcheck solution-78a1d712
  (fn [[_ L R]]
    (letfn [(v [[n l r]]
              (if (nil? n)
                nil
                [n (v r) (v l)]))]
      (= L (v R)))))

(defcheck solution-78ae2359
  (fn [[v a b]]
    (let [sa ((fn f [vv]
                (if (nil? vv)
                  nil
                  (let [[v a b] vv]
                    [v (f b) (f a)]

                    )
                  )
                ) a)
          ]
      (if (= sa b) true false)
      )
    ))

(defcheck solution-78c47def
  (letfn [(node? [n] (and (sequential? n) (= (count n) 3)))]
    (fn symm?
      ([t] (when (node? t) (let [[_ y z] t] (symm? y z))))
      ([a b] (cond
               (and (nil? a) (nil? b)) true
               (and (node? a) (node? b)) (let [[xa ya za] a
                                               [xb yb zb] b]
                                           (and (= xa xb) (symm? ya zb) (symm? za yb)))
               :else false)))))

(defcheck solution-78e56b28
  (fn [tree]
    (letfn [(third [coll]
              (-> coll rest second)),
            (comp-seq [s1 s2]
              (and (comp-element (first s1) (first s2))
                   (comp-element (second s1) (third s2))
                   (comp-element (second s2) (third s1)))),
            (comp-element [e1 e2]
              (if (sequential? e1)
                (and (sequential? e2) (comp-seq e1 e2))
                (= e1 e2)))]
      (comp-element (second tree) (third tree)))))

(defcheck solution-7902201b
  (fn [ [p l r] ]
    (letfn [ (m [ [x y z] ]
               [x (if (coll? z) (m z) z) (if (coll? y)(m y) y) ] )
            ] (= l (m r)))
    ))

(defcheck solution-7927e8b9
  (fn [[_ a b]]
    (= (map first (tree-seq identity next a))
      (map first (tree-seq identity (comp reverse next) b)))))

(defcheck solution-7a1aa2a2
  (fn [[_ l r]]
    ((fn sym? [l r]
       (cond (and (sequential? l) (sequential? r))
             (let [[lv ll lr] l [rv rl rr] r]
               (if (= lv rv)
                 (and (sym? ll rr) (sym? lr rl))
                 false))
             (or  (sequential? l) (sequential? r)) false
             :else (= l r)))
     l r)))

(defcheck solution-7a20c0e3
  (fn [[h l r]]
    (letfn [(sym-tree? [l r]
              (if (and (coll? l) (coll? r))
                (and
                 (= (first l) (first r))
                 (sym-tree? (second l) (last r))
                 (sym-tree? (last l) (second r)))
                (= l r)))]
      (sym-tree? l r))))

(defcheck solution-7a23d6a0
  (fn symm-tree? [T]
    (letfn [
            (mirror
              ([k l1 l2] [k (apply mirror l2) (apply mirror l1)])
              ([] nil))]
      (= T (apply mirror T)))))

(defcheck solution-7a344470
  (fn symmetric? [tree]
    (if (coll? tree)
      (let [mirror (fn mirror [tree]
                     (if (coll? tree)
                       (let [[v & children] tree]
                         (cons v (map mirror (reverse children))))
                       tree))
            [_ left right] tree]
        (= left (mirror right)))
      true)))

(defcheck solution-7a86c12b
  (fn symmetric? [tree]
    ((fn mirror? [l r]
       (or (= nil l r)
           (and (= (first l) (first r))
                (mirror? (second l) (last r))
                (mirror? (second r) (last l)))))
     (second tree) (last tree))))

(defcheck solution-7a917be6
  (fn [z]

    (apply
      (fn isSymetric [x y]
        (cond
          (and (coll? x) (not (coll? y)) ) false
          (and (coll? x) (not (coll? x)) ) false
          (and (coll? x) (not (= (count x)  3))) false
          (and (coll? y) (not (= (count y)  3))) false
          (and (not (coll?  x)) (not (coll? y)) (not (=  x y))) false
          (and (coll? x) (coll? y)) (and (isSymetric  (first x)  (first y)) (isSymetric  (second x)  (last y)) (isSymetric  (last x)  (second y)) )
          :else true
          )
        )
      (rest z)
      )
    ))

(defcheck solution-7b0365d3
  #(let[f(fn f[x](if x(let[[n l r]x][n(f r)(f l)])x))](= %(f %))))

(defcheck solution-7b81cbc7
  (fn isSym? [tree]  (letfn [(reverseTree [t]   (let [root (first t)
                                                      leftBranch (second t)
                                                      rightBranch (last t)] [root (if (nil? rightBranch) rightBranch (reverseTree rightBranch)) (if (nil? leftBranch) leftBranch (reverseTree leftBranch))]))] (= (second tree) (reverseTree (last tree))))))

(defcheck solution-7bebf8ae
  (fn [[_ l r]]
    (letfn
     [(mirror [t]
        (when-let [[v l r] t]
          [v (mirror r) (mirror l)]))]
      (= l (mirror r)))))

(defcheck solution-7c9134e4
  (fn [t]
    ((fn mirrortree [t1 t2]
       (if (and (sequential? t1) (sequential? t2))
         (let [val1   (nth t1 0)
               left1  (nth t1 1)
               right1 (nth t1 2)
               val2   (nth t2 0)
               left2  (nth t2 1)
               right2 (nth t2 2)]
           (and (= val1 val2) (mirrortree left1 right2) (mirrortree right1 left2)))
         (= t1 t2)))
     (nth t 1) (nth t 2))))

(defcheck solution-7c97e4f8
  (fn [[_ l r]]
    ((fn mirror? [[a b c] [d e f]]
       (cond
         (not (= a d)) false
         (= nil b c e f) true
         :else (and (mirror? b f) (mirror? c e)))) l r)))

(defcheck solution-7ce0aa17
  (fn [x]
    ((fn issym [a b]
       (if (not (sequential? a))
         (= a b)
         (and
          (= (first a) (first b))
          (issym (last a) (last (butlast b)))
          (issym (last (butlast a)) (last b)))))
     (last x)
     (last (butlast x)))))

(defcheck solution-7d1dbf35
  #(= %
     ((fn mirror? [[self l r]]
        (if self
          [self (mirror? r) (mirror? l)]))
      %)))

(defcheck solution-7d45f45b
  (fn[a-seq]
    (letfn [
            (trav[a-seq order at]
              (swap! at conj (if (first a-seq) (first a-seq) 'nil))
              (when a-seq
                (let [fst (nth a-seq (first order))
                      snd (nth a-seq (second order))]
                  (trav fst order at)
                  (trav snd order at))))]
      (let [at (atom [])
            r (do (trav a-seq [1 2] at) @at)
            l (do (reset! at []) (trav a-seq [2 1] at) @at)]
        (= r l)))))

(defcheck solution-7d9bb9ae
  #((fn m? [l r] (or (= l r nil)
                     (let [[a b c] l [d e f] r]
                       (and (= a d)
                            (m? b f)
                            (m? c e))))) % %))

(defcheck solution-7e061762
  #(letfn [(reflbtree [t] (if (sequential? t)
                            (list
                              (first t)
                              (reflbtree (nth t 2))
                              (reflbtree (second t)))
                            t))]
     (= % (reflbtree %))))

(defcheck solution-7e51b882
  (fn
    [root]
    (letfn [(rev-tree [r]
              (if (nil? r)
                r
                [(first r)
                 (rev-tree (last r))
                 (rev-tree (second r))]))]
      (= (second root) (rev-tree (last root))))))

(defcheck solution-7eddceff
  #(= % ((fn mirror [[n l r]]
           (if n (vector n (mirror r) (mirror l)))) %)))

(defcheck solution-7ef19890
  (fn [t]
    (let [rtree (fn revtree [tree]
                  (if (nil? tree)
                    nil
                    [(first tree) (revtree (last tree)) (revtree (second tree))]))]
      (= (second t) (rtree (last t))))))

(defcheck solution-7f0fe218
  (fn enc ( [[v L R]]
           (= (enc :a :b L) (enc :b :a R))
           )
    ([a b [v L R]]
     (let [f #(if (nil? %) :nil (enc a b %))]
       (hash-map :v v a (f L) b (f R))
       )
     )
    ))

(defcheck solution-7f132ec9
  (fn symmetric? [[v l r]]
    (let [
          check-case? (fn [a b eq]
                        (case [(nil? a) (nil? b)]
                          [true true] true
                          [false false] (eq a b)
                          false
                          )
                        )
          tree-equal? (fn ! [a b]
                        (let [[av al ar] a
                              [bv bl br] b]
                          (and
                           (= av bv)
                           (check-case? al br !)
                           (check-case? ar bl !)
                           )
                          )
                        )]
      (tree-equal? l r)
      )
    ))

(defcheck solution-7f3d751e
  (fn [t]
    (loop [left [(second t)]
           right [(last t)]]
      (if (and (every? nil? left)
               (every? nil? right))
        true
        (if-not (= (map first left) (map first (reverse right)))
          false
          (recur
            (mapcat next left)
            (mapcat next right)
            ))))))

(defcheck solution-7f4485fd
  #(= % ((fn symmetric [[v lc rc]] [v (when rc (symmetric rc)) (when lc (symmetric lc))]) %)))

(defcheck solution-7f4a575c
  (fn [t]
    (let [left  (fn [t] (nth t 1)),
          right (fn [t] (nth t 2)),
          mirror (fn _[t]
                   (if
                    (not (sequential? t)) t
                                          [(nth t 0) (_ (right t)) (_ (left t))]))]
      (= t (mirror t))
      )
    ))

(defcheck solution-7f677e27
  (fn [tr]
    (letfn [(s [[x l r] [y l' r']]
              (cond
                (not (or x y)) true
                (= x y) (and (s l r')(s r l'))
                :else false))]
      (s tr tr))))

(defcheck solution-802756d6
  (fn [tree]
    (letfn [(flip-tree [t]
              (cond
                (not t) t
                (not (coll? t)) t
                :else [(first t)
                       (flip-tree (last t))
                       (flip-tree (second t))]))]
      (= (second tree) (flip-tree (last tree))))))

(defcheck solution-81869072
  (let [flip-tree (fn flip-tree
                    [tree]
                    (if (nil? tree) tree
                                    (let [[x l r & z] tree]
                                      [x (flip-tree r) (flip-tree l)])))]
    (fn is-symmetric
      [tree]
      (let [[x l r & z] tree]
        (= (flip-tree l) r)))))

(defcheck solution-81aa5cbb
  (fn [[v l r]]
    (letfn [(mirror? [[v1 l1 r1 :as t1] [v2 l2 r2 :as t2]]
              (if (and (nil? t1) (nil? t2))
                true
                (and (= v1 v2) (mirror? l1 r2) (mirror? l2 r1))))]
      (mirror? l r))))

(defcheck solution-81fb4e2c
  #(= % ((fn mirror [[v a b]] [v (when b (mirror b)) (when a (mirror a))]) %)))

(defcheck solution-82008fb6
  #(letfn [(f [c] (if (coll? c)
                    [(% c) (f (%2 c)) (f (%3 c))]
                    c))]
     (= (%3 %4) (f (%2 %4)))) first last second)

(defcheck solution-82a98c2f
  #(= % ((fn mr [[n l r]] (if n [n (mr r) (mr l)])) %)))

(defcheck solution-82b82747
  (letfn [
          (mirrored? [t1 t2]
            (if (or (nil? t1) (nil? t2))
              (= t1 t2)
              (let [[v1 l1 r1] t1, [v2 l2 r2] t2]
                (and (= v1 v2) (mirrored? l1 r2) (mirrored? r1 l2)))))]

    (fn symmetric? [t]
      (if (nil? t)
        true
        (let [[_ l r] t]
          (mirrored? l r))))))

(defcheck solution-82c2e7fe
  (fn [a]

    (= (second a ) (

                    (fn flip [n]

                      (if (coll? n)
                        (vector (first n) (flip (nth n 2)) (flip (second n))) n)

                      ) (nth a 2)))))

(defcheck solution-82d4f0b5
  (fn f [[_ [v x y :as l] [w a b :as r]]]
    (or (= nil l r)
        (and (= v w)
             (f [1 x b])
             (f [1 a y])))))

(defcheck solution-82f5d127
  #(= % ((fn mirror [v] (if (coll? v) (list (first v) (mirror (last v)) (mirror (second v))) v)) %)))

(defcheck solution-834e01c2
  (fn [tree]
    (letfn [(reverse-tree [tree]
              (let [[key right left] tree
                    right-tree (if-not (nil? right) (reverse-tree right) nil)
                    left-tree (if-not (nil? left) (reverse-tree left) nil)
                    ]
                [key left-tree right-tree]
                )
              )]
      (let [[key right left] tree]
        (= right (reverse-tree left))
        ))))

(defcheck solution-8360ec20
  (fn [[n l r]]
    (= l ((fn f [[n l r]]
            (if n [n (f r) (f l)]))
          r))))

(defcheck solution-83731ae6
  (fn [t]
    (letfn [(mirror [tree]
              (if (sequential? tree)
                [(first tree) (mirror (last tree)) (mirror (second tree))]
                tree))]
      (= (second t) (mirror (last t))))))

(defcheck solution-839e017
  #((fn sym [a b]
      (or
       (= a b nil)
       (and
        (sequential? a)
        (sequential? b)
        (= (first a) (first b))
        (sym (nth a 1) (nth b 2))
        (sym (nth a 2) (nth b 1)))
       false
       )) (nth % 1) (nth % 2)))

(defcheck solution-83a4621b
  (fn symm? [t]
    (letfn [(mirror [[n l r]]
              (list n (if (sequential? r) (mirror r) r) (if (sequential? l) (mirror l) l)))]
      (= t (mirror t)))))

(defcheck solution-83b16198
  (fn mirror? [tree]
    (let [inorder-func (fn travel [t]
                         (if (coll? t)
                           (concat (travel (nth t 1))
                             [(first t)]
                             (travel (nth t 2)))
                           [t]))
          flat-tree (inorder-func tree)]
      (= flat-tree (reverse flat-tree)))))

(defcheck solution-84279219
  (let [flip (fn flip [t] (when-let [[val left right] t]
                            [val (flip right) (flip left)]))]
    (fn [[_ left right]] (= left (flip right)))))

(defcheck solution-84a5e8fe
  (fn [col]
    (let [q (fn q [col1 col2]
              (cond (and (nil? col1) (nil? col2)) true
                    (or  (nil? col1) (nil? col2)) false
                    (and (vector? col1) (vector? col2)) (and (q (nth col1 2) (second col2))
                                                             (q (second col1) (nth col2 2))
                                                             (= (first col1) (first col2)))
                    :else                         (= col1 col2)))]
      (q (nth col 1) (nth col 2)))))

(defcheck solution-84e534dd
  (fn [t]
    (let [flip (fn flip [t]
                 (if (nil? t)
                   t
                   (let [head (first t)
                         left (second t)
                         right (nth t 2)]
                     [head (flip right) (flip left)])))]
      (= t (flip t)))))

(defcheck solution-84fd92de
  (fn issym [[k l r]]
    (
     (fn ismirr [a b]
       (cond
         (and (= nil a) (= nil b)) true
         (and
          (coll? a)
          (coll? b)
          (let [ [ka la ra] a
                [kb lb rb] b]
            (and
             (= ka kb)
             (ismirr la rb)
             (ismirr lb ra)
             )
            )
          ) true
         :default false
         )
       ) l r)
    ))

(defcheck solution-856fa439
  (fn symmetric? [[root left right]]
    (letfn [(mirror [sub-tree]
              (if (seq sub-tree)
                (let [[node left right] sub-tree]
                  [node (mirror right) (mirror left)])
                sub-tree))]
      (= left (mirror right)))))

(defcheck solution-85a89f9b
  (fn symmetrical? [t]
    (let [value #(first %)
          left #(second %)
          right #(last %)
          mirror (fn mirror [t] (if (and (nil? (left t)) (nil? (right t)))
                                  t

                                  (list (value t) (mirror (right t)) (mirror (left t)))))]
      (= (left t) (mirror (right t))))))

(defcheck solution-85f6c6c0
  (fn [tree]
    (letfn
     [(same-tree [a b]
        (cond
          (and (coll? a) (coll? b)) (let [[a0 a1 a2] a [b0 b1 b2] b]
                                      (and (= a0 b0) (same-tree a1 b2) (same-tree a2 b1))
                                      )
          :else (= a b)
          )
        )]
      (same-tree (second tree) (last tree))
      )
    ))

(defcheck solution-860aeff
  (fn [[v left right :as node]]
    (letfn [(mirror [[v left right :as node]]
              (if (coll? node) [v (mirror  right) (mirror left)]
                               node))]
      (= (mirror left) right))
    ))

(defcheck solution-862bfedc
  (fn [[v l r]] (= l
                  ((fn mirror [[v l r :as t]]
                     (if (nil? t) nil [v (mirror r) (mirror l)]))
                   r))))

(defcheck solution-8635ebd4
  (fn is-symmetric [binary-tree]
    ((fn symmetric-trees? [t1 t2]
       (or (every? nil? [t1 t2])
           (and (= (first t1) (first t2))
                (symmetric-trees? (second t1) (last t2))
                (symmetric-trees? (last t1)   (second t2)))))
     (second binary-tree) (last binary-tree))))

(defcheck solution-864c171a
  (fn sym [n]
    (= (flatten (second n))
      ((fn sw [rn]
         (cond (not (coll? rn)) [rn]
               (nil? rn) [nil]
               :else (concat [(first rn)] (sw (nth rn 2)) (sw (second rn)))
               )) (nth n 2)))
    ))

(defcheck solution-867c0fa6
  #(let [m (fn m [n]
             (if (coll? n)
               (cons (first n) (map m (reverse (rest n))))
               n))]
     (= (second %) (m (nth % 2)))))

(defcheck solution-86a20d3
  (fn [[v l r]]
    (letfn [(mirror? [l r]
              (let [[lv ll lr] l, [rv rl rr] r]
                (and
                 (= lv rv)
                 (or
                  (and (nil? ll) (nil? rr))
                  (mirror? ll rr))
                 (or
                  (and (nil? lr) (nil? rl))
                  (mirror? lr rl)))))]
      (mirror? l r))))

(defcheck solution-870b8645
  #(letfn [(third [x] (second (rest x)))
           (check-symmetry [x1 x2]
             (cond
               (and (= x1 nil) (= x2 nil)) true
               (and (coll? x1) (coll? x2)
                    (= (first x1) (first x2))
                    (check-symmetry (third x1) (second x2))
                    (check-symmetry (second x1) (third x2))) true
               :else false))]
     (check-symmetry (second %) (third %))))

(defcheck solution-871dd745
  (fn [[_ l r]] ((fn f [t1 t2] (or (and (nil? t1) (nil? t2)) (let [[n1 l1 r1] t1 [n2 l2 r2] t2] (and (= n1 n2) (f l1 r2) (f r1 l2))))) l r)))

(defcheck solution-8721756b
  (let [ts #(map first (tree-seq coll? % %2))]
    #(= (ts next %) (ts (comp reverse next) %))))

(defcheck solution-87977e41
  (fn problem-96
    [t]
    ((fn symm? [l r]
       #_(println  l "<->" r)
       (if
        (or (and (nil? l) (nil? r))

            (and (= (first l) (first r))
                 (symm? (second l) (last r))
                 (symm? (last l) (second r))))
         true
         false
         )) (second t) (last t))))

(defcheck solution-87e0a4b0
  (fn [xs]
    (let
     [
      mirror (fn mirror [xs]
               (if
                (nil? xs)
                 nil
                 [
                  (first xs)
                  (mirror (nth xs 2))
                  (mirror (second xs))
                  ]
                 )
               )
      left (second xs)
      right (mirror (nth xs 2))
      ]
      (= left right)
      )
    ))

(defcheck solution-887beccb
  (fn [[_ left right]]
    (letfn [(swap [node]
              (when-not (nil? node)
                (let [[v l r] node]
                  [v (swap r) (swap l)])))]
      (= left (swap right)))))

(defcheck solution-88d8c221
  (fn [x] (= x ((fn rs [col] (if (nil? col) nil (list (first col) (rs (last col)) (rs (second col))))) x))))

(defcheck solution-88eb88de
  (fn [x]
    (letfn [(tt [x] (if x [(tt (second x)) (first x) (tt (last x))] nil))]
      (let [f (flatten (tt x))] (= f (reverse f))))))

(defcheck solution-88f86694
  (fn sym? [tree]
    (= (second tree) ( (fn flip [coll]
                         (if (coll? coll)
                           (conj (conj [(first coll)] (flip (last coll))) (flip (second coll)))
                           coll)) (last tree)))
    ))

(defcheck solution-893d02de
  (fn mirror? [tree]
    (let [mirror (fn mirror [tree]
                   (let [head (nth tree 0)
                         left (nth tree 1)
                         right (nth tree 2)]
                     (cond
                       (= left right) tree
                       :else (cons head
                               (cons (mirror right)
                                 (cons (mirror left) nil))))))
          left (nth tree 1)
          right (mirror (nth tree 2))]
      (= left right))))

(defcheck solution-896cb65
  (fn sym-tree?
    ([c] (or
          (not (coll? c))
          (apply sym-tree? c)))

    ([a1 a2] false)

    ([v t1 t2 & more]
     (let [t [v t1 t2]
           flip-tree (fn flip-tree
                       ([av] (if-not (coll? av) av (apply flip-tree av)))

                       ([av at1 at2] [av (flip-tree at2) (flip-tree at1)])) ]

       (and (zero? (count more)) (= t (flip-tree t)))))))

(defcheck solution-8a193a57
  #(letfn
    [(mirror [x]
       (if (coll? x)
         (vector (first x) (mirror (last x)) (mirror (second x)))
         x ))]
     (= % (mirror %)) ))

(defcheck solution-8a21b030
  (let [mirror (fn mirror [node]
                 (if node
                   (let [[value left right] node]
                     [value (mirror right) (mirror left)]
                     )
                   ))]
    (fn symmetric? [[value left right]]
      (= left (mirror right))
      )
    ))

(defcheck solution-8a275998
  (fn [[v l r :as t]]
    (let [st? (fn st? [[v1 l1 r1 :as t1] [v2 l2 r2 :as t2]]
                (if (nil? t1)
                  (nil? t2)
                  (and (not (nil? t2))
                       (and (= v1 v2) (st? l1 r2) (st? l2 r1)))))]
      (or (nil? t) (st? l r)))))

(defcheck solution-8aa78066
  (fn symmetric-tree? [tree]
    (let [mirror-trees?
          (fn mirror-trees? [left-tree right-tree]
            (cond
              (and (nil? left-tree) (nil? right-tree)) true
              (and (coll? left-tree) (coll? right-tree))
              (and (= (first left-tree) (first right-tree))
                   (mirror-trees? (second left-tree)
                     (last right-tree))
                   (mirror-trees? (last left-tree)
                     (second right-tree)))
              :else false))]
      (mirror-trees? (second tree) (last tree)))))

(defcheck solution-8adb225f
  (fn sym? [tree]
    (let [left (nth tree 1)
          right (nth tree 2)
          invert (fn invert [tree]
                   (let [node (nth tree 0 nil)
                         left' (nth tree 1 nil)
                         right' (nth tree 2 nil)]
                     (cond
                       (= tree nil) nil
                       (< (count tree) 2) tree
                       :else [node (invert right') (invert left')]
                       )
                     )
                   )
          ]
      (= left (invert right))
      )
    ))

(defcheck solution-8be42ff4
  (fn  mirror[[t a b]]
    ((fn sym [a b]
       (if (or (nil? a) (nil? b)) (= a b)
                                  (let [[x y z] a [t u v] b]
                                    (and (= x t)
                                         (sym y v)
                                         (sym z u))))) a b)))

(defcheck solution-8c47d3cd
  (fn symmetry? [x]
    (letfn [(recur-invert [coll]
              (let [[n l r] coll]
                (list n
                  (if (coll? r) (recur-invert r) r)
                  (if (coll? l) (recur-invert l) l))))]
      (and (= (count x) 3)
           (let [[_ l r] x]
             (= l (recur-invert r)))))))

(defcheck solution-8cc265c9
  (fn eq [tree]
    (let [vlr (fn vlr [tree]
                (if (not (or (vector? tree) (list? tree)))
                  tree
                  (let [[v l r] tree]
                    (concat [v] [(vlr l)] [(vlr r)]))))
          vrl (fn vrl [tree]
                (if (not (or (vector? tree) (list? tree)))
                  tree
                  (let [[v l r] tree]
                    (concat [v] [(vrl r)] [(vrl l)]))))
          left-walk (vlr tree)
          right-walk (vrl tree)]
      (= left-walk right-walk))))

(defcheck solution-8cd28bb
  (fn [a]
    (letfn [(swap [b]
              (if (sequential? b) (let [[v l r] b] [v (swap r) (swap l)]) b))]
      (= (swap (second a)) (second (next a))))))

(defcheck solution-8cef6497
  (fn [t]
    (letfn [(mirror [x y]
              (or (and (nil? x) (nil? y))
                  (and (= (nth x 0) (nth y 0))
                       (mirror (nth x 1) (nth y 2))
                       (mirror (nth x 2) (nth y 1)))))]
      (mirror t t))))

(defcheck solution-8d268bf8
  (fn [[a b c]]
    (letfn [(r [[a b c]] (if (= a nil) nil [a (r c) (r b)]
                                       ))]
      (= (r b) c)
      )
    ))

(defcheck solution-8d387b31
  (fn tree-match [[_ left right]]
    (= (map first (tree-seq identity rest left)) (map first (tree-seq identity #(reverse (rest %)) right)))))

(defcheck solution-8d4073b5
  (fn f [[root left right]]
    (let [mirror? (fn mirror? [l r]
                    (cond
                      (not= (sequential? l) (sequential? r)) false
                      (sequential? l) (let [[lr lL lR] l
                                            [rr rL rR] r]
                                        (and (= lr rr)
                                             (mirror? lL rR)
                                             (mirror? lR rL)))
                      :else (= l r)
                      ))]
      (mirror? left right)
      )))

(defcheck solution-8d6099a9
  (fn symmetric? [tree]
    (letfn [
            (mirror [[val left right]]
              (let [new-left  (if (sequential? left) (mirror left) left)
                    new-right (if (sequential? right) (mirror right) right)]
                [val new-right new-left]))]
      (= tree (mirror tree)))))

(defcheck solution-8dd4494e
  (fn [[x y z]]
    (letfn [(swap [[a b c]] (conj [] a (if (vector? c) (swap c) c) (if (vector? b) (swap b) b) ))]
      (cond
        (and (vector? y) (vector? z)) (= y (swap z))
        (= y z) true
        :else false))))

(defcheck solution-8de66087
  (fn [[_ l r]]
    (letfn [(mirror? [[v l r :as t0] [v' l' r' :as t1]]
              (or
               (and (nil? t0) (nil? t1))
               (and (= v v') (mirror? l r') (mirror? r l'))))]
      (mirror? l r))))

(defcheck solution-8e32ff96
  (fn [col]
    ((fn check_subtree [t1 t2]
       (if (sequential? t1)
         (let [rn1 (first t1) l1 (second t1) r1 (nth t1 2) rn2 (first t2) l2 (second t2) r2 (nth t2 2)  ]
           (and (= rn1 rn2)
                (and (check_subtree  r1 l2)  (check_subtree  l1 r2)) )

           )
         (= t1 t2)
         )

       ) (second col) (nth col 2) )))

(defcheck solution-8e451907
  (fn [x]
    (=
      (loop [s (conj '() (nth x 1))
             a []]
        (if (empty? s)
          a
          (recur (into (pop s) (rest (peek s))) (conj a (first (peek s))))
          ))
      (loop [s (conj '() (nth x 2))
             a []]
        (if (empty? s)
          a
          (recur (into (pop s) (reverse (rest (peek s)))) (conj a (first (peek s))))
          )))))

(defcheck solution-8f448ead
  (fn symmetric? [tree]
    (letfn [(mirror [tree]
              (if (sequential? tree)
                (cons (first tree) (map mirror (reverse (next tree))))
                tree))]
      (= tree (mirror tree)))))

(defcheck solution-8fc02b13
  (fn [t]
    (letfn [
            (fl [[v l r :as t]]
              (cons v
                (lazy-seq
                  (concat
                    (if (seq l)
                      (fl l)
                      [:n])
                    (if (seq r)
                      (fl r)
                      [:n])))))

            (fr [[v l r :as t]]
              (cons v
                (lazy-seq
                  (concat
                    (if (seq r)
                      (fr r)
                      [:n])
                    (if (seq l)
                      (fr l)
                      [:n])))))]
      (apply = ((juxt fl fr) t)))))

(defcheck solution-8ff36bb4
  (fn [t]
    ((fn m [l r]
       (if (or (= nil l r)
               (and (= (first l) (first r))
                    (m (second l) (last r))
                    (m (last l) (second r))))
         true false))
     (second t) (last t))))

(defcheck solution-902a95c2
  (fn is-symmetric[tree](if (nil? tree) true
                                        (let [[root left right] tree]
                                          ((fn is-mirror [left right]
                                             (let [[l-root l-left l-right] left
                                                   [r-root r-left r-right] right]
                                               (if (and (nil? l-root) (nil? r-root)) true
                                                                                     (if (= l-root r-root)
                                                                                       (and true
                                                                                            (is-mirror l-left r-right)
                                                                                            (is-mirror l-right r-left)) false)))) left right)))))

(defcheck solution-903c8b9b
  (fn symmetric?-
    ^{:doc "96. Write a predicate to determine whether or not a given
  binary tree is symmetric."}
    ([[value left right]] (symmetric?- left right))
    ([[lvalue lleft lright] [rvalue rleft rright]]
     (or (= nil lvalue rvalue)
         (and (= lvalue rvalue) (symmetric?- lleft rright) (symmetric?- lright rleft))))))

(defcheck solution-90602cec
  (fn [tree]
    (let [next-level (fn [nodes] (mapcat #(drop 1 %) nodes))
          mirrored? (fn [xs] (= xs (reverse xs)))]
      ((fn tree-symmetric? [nodes]
         (let [filtered (filter #(not (nil? %)) nodes)]
           (or (empty? filtered)
               (and (mirrored? (map first nodes))
                    (tree-symmetric? (next-level nodes)))
               )
           )
         )
       [tree]
       )
      )
    ))

(defcheck solution-909ceea4
  #(= (second %)
     ((fn mirror [s]
        (if (coll? s) [(first s) (mirror (last s)) (mirror (second s))]
                      s)
        ) (last %))))

(defcheck solution-90c7de1
  (fn is-symt? [coll]
    (letfn [(rev-tree [coll]
              (let [v (first coll)
                    l (second coll)
                    r (last coll)]
                (conj [] v
                  (if (coll? r) (rev-tree r) r)
                  (if (coll? l) (rev-tree l) l))))]
      (= (second coll) (rev-tree (last coll))))))

(defcheck solution-90d0a798
  (letfn [(f [[n l r :as t]]
            (if (seq t)
              [n (f r) (f l)])
            )]
    #(= % (f %))))

(defcheck solution-91492c06
  (fn symm? [root]
    (letfn [(mirror [r]
              (if (nil? r)
                nil
                [(first r)
                 (mirror (last r))
                 (mirror (second r))]))]
      (= (second root) (mirror (last root))))))

(defcheck solution-9188a87f
  #(letfn [(eq? [t1 t2]
             (let [[b1 l1 r1] t1, [b2 l2 r2] t2]
               (if (nil? b1)
                 (nil? b2)
                 (if (nil? b2)
                   false
                   (and
                    (= b1 b2)
                    (eq? l1 r2)
                    (eq? r1 l2))))))]
     (let [[_ l r] %] (eq? l r))))

(defcheck solution-91bdc9ff
  (fn	[node]
    (letfn [(mirror [node]
              (if (nil? node)
                nil
                (let [[v l r] node]
                  [v (mirror r) (mirror l)])))]
      (let [[v l r] node]
        (= l (mirror r))))))

(defcheck solution-91e884d0
  #(letfn [(mi? [t1 t2] (cond (and (nil? t1) (nil? t2)) true (= (first t1) (first t2)) (and (mi? (nth t1 1) (nth t2 2)) (mi? (nth t1 2) (nth t2 1))) :else false))] (mi? (nth % 1) (nth % 2))))

(defcheck solution-9256c838
  (fn match [t]
    (letfn [ (mirror [[v l r :as n]]
               (if n [v (mirror r) (mirror l)])) ]
      (= t (mirror t)))))

(defcheck solution-92dcc7a5
  (fn btree-symmetric?
    [tree]
    (letfn [(flip [t]
              (if (sequential? t)
                (let [v (first t)
                      l (second t)
                      r (last t)]
                  [v (flip r) (flip l)])
                t))]
      (= (second tree) (flip (last tree))))))

(defcheck solution-92dd8147
  (fn balanced-tree? [s]
    (let [reverse-tree (fn reverse-tree [s]
                         (if (nil? s)
                           s
                           (list (first s) (reverse-tree (second (rest s))) (reverse-tree (first (rest s))))))]
      (= s (reverse-tree s)))))

(defcheck solution-93a80806
  (fn [bt]
    (= bt ((fn mirror [t] (if (nil? t) t (let [[v l r] t] [v (mirror r) (mirror l)]))) bt))))

(defcheck solution-93d75d07
  (fn [t]
    (let [mirror (fn mirror [tree]
                   (if (sequential? tree)
                     [(first tree) (mirror (nth tree 2)) (mirror (second tree))]
                     tree))]
      (= t (mirror t)))))

(defcheck solution-93f40433
  (fn [tr]
    (letfn [(rev-tr [[p l r]]
              [p (when r (rev-tr r))
               (when l (rev-tr l))])]
      (= tr (rev-tr tr)))))

(defcheck solution-946ffdf9
  (fn
    [t]
    ((fn mirror?
       [l r]
       (or
        (and (nil? l) (nil? r))
        (and
         (= (first l) (first r))
         (mirror? (second l) (last r))
         (mirror? (last l) (second r)))))
     (second t)
     (last t))))

(defcheck solution-94b73a5d
  #(=
     ((fn mirror [[n l r :as tree]]
        (when tree
          [n (mirror r) (mirror l)])) %)
     %))

(defcheck solution-94c75300
  (letfn [(symm=? [t1 t2]
            (cond
              (and (nil? t1) (nil? t2)) true
              (nil? t1) false
              (nil? t2) false
              :else (let [[v1 l1 r1] t1
                          [v2 l2 r2] t2]
                      (and (= v1 v2)
                           (symm=? l1 r2)
                           (symm=? r1 l2)))))]
    #(or (nil? %)
         (let [[v l r] %]
           (symm=? l r)))))

(defcheck solution-94e8e687
  (fn symetric-tree? [[value left right & others]]
    (let [go (fn [f n] (if (nil? n) [ nil ] (f n)))
          walkl (fn walkl [[v l r & o]] (concat [v] (go walkl l) (go walkl r)))
          walkr (fn walkr [[v l r & o]] (concat [v] (go walkr r) (go walkr l)))]
      (= (walkl left) (walkr right)))
    ))

(defcheck solution-9539e2a3
  (fn balanced? [ls]
    (letfn [(mirror [[root [lroot :as left] [rroot :as right]]]
              (let [rmirror (if (coll? right) (mirror right) right)
                    lmirror (if (coll? left) (mirror left) left)]
                [root rmirror lmirror]))]
      (= ls (mirror ls)))))

(defcheck solution-95b4513e
  (fn is-sym [t]
    (if (not= (count t) 3) false
                           ((fn cmp-tree [lt rt]
                              (if (or (not= (count lt) 3) (not= (count rt) 3)) false
                                                                               (let [[x xl xr] lt [y yl yr] rt]
                                                                                 (and
                                                                                  (= x y)
                                                                                  (and
                                                                                   (if (and (coll? xl) (coll? yr)) (cmp-tree xl yr) (and (= nil xl) (= nil yr)))
                                                                                   (if (and (coll? xr) (coll? yl)) (cmp-tree xr yl) (and (= nil xr) (= nil yl))))
                                                                                  ))))  (second t) (last t)))))

(defcheck solution-96bae6b1
  #(letfn [(c [s t]
             (cond (not= (first s) (first t)) false
                   (and (nil? s) (nil? t)) true
                   :else (and (c (second s) (second (rest t)))
                              (c (second (rest s)) (second t)))))]
     (c % %)))

(defcheck solution-971bd023
  (fn [tree]
    (= tree
      ((fn mirror [tree]
         (if (nil? tree)
           tree

           (cons
             (first tree)
             (conj '()
               (mirror (nth tree 1))
               (mirror (nth tree 2))))))
       tree))))

(defcheck solution-97958114
  (fn [[v l r]]
    (letfn[(mirror?[lb rb]
             (cond
               (not= (sequential? lb) (sequential? rb)) false
               (sequential? lb) (let [[lv ll lr] lb [rv rl rr] rb]
                                  (and (= lv rv) (mirror? ll rr) (mirror? lr rl)))
               :else (= lb rb)))]
      (mirror? l r))))

(defcheck solution-979cb443
  #(= %
     ((fn mirror [tr]
        (let [[n l r] tr]
          (if n [ n (mirror r) (mirror l)]))) %)))

(defcheck solution-97be94f0
  (fn [tree]
    (letfn [(inorder [tree]
              (if (coll? tree)
                (let [val (first tree)
                      left (nth tree 1)
                      right (nth tree 2)]
                  (concat (inorder left)
                    (vector val)
                    (inorder right)))
                (vector tree)))]
      (= (inorder (nth tree 1))
        (reverse (inorder (nth tree 2)))))))

(defcheck solution-9813fe95
  (fn [t]
    (let [rev (fn rev [s]
                (if (sequential? s)
                  (list (first s) (rev (nth s 2)) (rev (nth s 1)))
                  s))]
      (and (= (count t) 3)
           (= (nth t 1) (rev (nth t 2)))))))

(defcheck solution-989e2e89
  #(= % ((fn tree-mirror [x]
           (if(coll? x)
             [(first x) (tree-mirror (last x)) (tree-mirror (second x))]x))%)))

(defcheck solution-9937de9d
  (fn [tree]
    (letfn [(left [tree] (nth tree 1))
            (right [tree] (nth tree 2))
            (flip [tree]
              (when tree
                (list (first tree)
                  (flip (right tree))
                  (flip (left tree)))))]
      (= (left tree) (flip (right tree))))))

(defcheck solution-99442f84
  (fn [tree]
    (let [flip (fn f [[v l r]]
                 (if v
                   (list v (f r) (f l) )
                   )
                 )
          ]
      (= (flip tree) tree)
      )
    ))

(defcheck solution-999b9f4b
  (fn symtree [lst]
    (letfn [(tree? [s]
              (and (= 3 (count s)) (not= false (second s)) (every? true? (map tree? (filter sequential? s)))))
            (mirror [s]
              (if (coll? s)
                (map mirror (list (first s) (last s) (second s)))
                s))
            (symmetric? [s] (= s (mirror s)))]
      (and (tree? lst) (symmetric? lst)))))

(defcheck solution-9a43e615
  (letfn [(mirror?
            [t1 t2]
            (or (= nil t1 t2)
                (let [[x l1 r1] t1
                      [y l2 r2] t2]
                  (and (= x y)
                       (mirror? l1 r2)
                       (mirror? l2 r1)))))]

    #(mirror? (nth % 1)
       (nth % 2))))

(defcheck solution-9a5318b6
  (fn [t]
    (let [mirror (fn mirror [t]
                   (if (nil? t) t
                                (cons (first t) (reverse (map mirror (rest t))))))]
      (= t (mirror t)))))

(defcheck solution-9aabac10
  #(= %
     ((fn sym [[raiz izq der :as t]]
        (when t [raiz (sym der) (sym izq)] )) %)))

(defcheck solution-9acf4018
  #(= % ((fn f [[n l r]] (
                           if n
                           [n (f r) (f l)]
                           n
                           )
           ) %)))

(defcheck solution-9b5cc88e
  #(let [walker  (fn r [f c]
                   (if (coll? c)
                     (let [[e lc rc] c]
                       (list e (map (partial r f) (f [lc rc]))))
                     (list c)))]
     (= (walker identity %) (walker reverse %))))

(defcheck solution-9bb1be
  (fn is-symmetric-tree [a-tree]
    (letfn [(is-symmetric [l-tree r-tree]
              (if (= l-tree nil)
                (= r-tree nil)
                ; l-tree is not nil
                (if (= r-tree nil)
                  false
                  ; both l-tree and r-tree is not nil
                  ; check whether values are equal
                  (if (not= (first l-tree) (first r-tree))
                    false
                    (and
                     (is-symmetric (second l-tree) (nth r-tree 2))
                     (is-symmetric (nth l-tree 2) (second r-tree))
                     )
                    )
                  )
                )
              )]
      (let [left (second a-tree) right (nth a-tree 2)]
        (is-symmetric left right)
        )
      )
    ))

(defcheck solution-9bb98791
  (fn symmetric [root]
    (letfn [(get-node-val-left [node]
              (if (coll? node)
                (list (first node) (get-node-val-left (second node)) (get-node-val-left (nth node 2)))
                (first node)))
            (get-node-val-right [node]
              (if (coll? node)
                (list (first node) (get-node-val-right (nth node 2)) (get-node-val-right (second node)))
                (first node)))]
      (= (get-node-val-left (second root)) (get-node-val-right (nth root 2)))
      )
    ))

(defcheck solution-9bbc619d
  (fn equtree[paramTree]
    (let[leftBranch (fn[tree]
                      (if (empty? (rest tree))
                        (keyword "empty")
                        (first (rest tree))
                        )
                      ),
         rightBranch (fn[tree]
                       (if (empty? (rest tree))
                         (keyword "empty")
                         (let[other (rest (rest tree))]
                           (cond (empty?  other) (keyword "empty")
                                 (not (empty? (rest other))) (keyword "other")
                                 :else (first other)
                                 )
                           )
                         )
                       ),
         compareTree (fn isEquTree[tree1 tree2]
                       (let[root1 (first tree1),
                            root2 (first tree2),
                            comp (fn[t1 t2]
                                   (let[leaf1 (leftBranch t1),
                                        leaf2 (rightBranch t2)]
                                     #_(println (str "left " leaf1 " right " leaf2) )
                                     (if(and (coll? leaf1) (coll? leaf2))
                                       (isEquTree leaf1 leaf2)
                                       (= leaf1 leaf2)
                                       )
                                     ))]

                         (and (= root1 root2)
                              (comp tree1 tree2)
                              (comp tree2 tree1)
                              )
                         )
                       )]
      (let[left (leftBranch paramTree),
           right (rightBranch paramTree)]
        (compareTree left right)
        )
      )
    ))

(defcheck solution-9bffe40c
  (fn [[_ a b]]
    (letfn [(flip= [x y]
              (cond
                (and (coll? x)(coll? y))
                (let [[an al ar] x
                      [bn bl br] y]
                  (and (= an bn)
                       (flip= al br)
                       (flip= ar bl)))
                :default (= x y)))]
      (flip= a b))))

(defcheck solution-9c148f82
  (fn foo [[_ l r]]
    (let [f (fn f [l r]
              (if (not (and (coll? l) (coll? r)))
                (= l r)
                (and (= (first l) (first r))
                     (f (second l) (nth r 2))
                     (f (nth l 2) (second r)))))]
      (f l r))))

(defcheck solution-9c1fc36e
  (fn is-symmetry [tree]
    (letfn[(is-same-tree[a b]
             (if(and (sequential? a) (sequential? b))
               (let[l1 (first a)
                    l2 (second a)
                    l3 (nth a 2)
                    r1 (first b)
                    r2 (second b)
                    r3 (nth b 2)]
                 (and (= l1 r1)
                      (is-same-tree l2 r3) (is-same-tree l3 r2)))
               (= a b)))]
      (is-same-tree (second tree )
        (nth tree 2)))))

(defcheck solution-9c538881
  (fn [[v l r]]
    (letfn [(rev-children [[v l r]]
              (let [new-l (if (nil? r) nil (rev-children r))
                    new-r (if (nil? l) nil (rev-children l))]
                [v new-l new-r]))]
      (boolean (or (and (nil? l) (nil? r))
                   (and (seq l) (seq r) (= l (rev-children r))))))))

(defcheck solution-9c6b9637
  (fn [s] {:pre [(sequential? s)
                 (= (count s) 3)]}
    (let [  left (second s)
          right (last s)
          mirror (fn mirror [s] {:pre [(= (count s) 3)]}
                   (let [ left (second s)
                         right (last s)]
                     (if (and (nil? left) (nil? right))
                       s
                       (list
                         (first s)
                         (if (sequential? right) (mirror right) right)
                         (if (sequential? left) (mirror left) left)))))]
      (=
        left
        (if (sequential? right)
          (mirror right)
          right)))))

(defcheck solution-9db255b0
  (fn [[_ l r]] ((fn check [a b]
                   (if (coll? a)
                     (let [[ak al ar] a
                           [bk bl br] b]
                       (and (= ak bk)
                            (check al br)
                            (check ar bl)))
                     (= a b))) l r)))

(defcheck solution-9dc2b227
  (fn is-symmetric? [tree]
    (let
     [flip-tree (fn flip-tree
                  [node]
                  (if (not (sequential? node))
                    node
                    (vector (first node)
                      (flip-tree (nth node 2))
                      (flip-tree (second node)))))]
      (= (second tree) (flip-tree (nth tree 2))))))

(defcheck solution-9dd076ad
  (fn sym-tree? [t]
    (= t ((fn lr-rev [[v l r :as t]]
            (if (nil? t) t [v (lr-rev r) (lr-rev l)])) t))))

(defcheck solution-9dd93e98
  #(letfn [
           (sw [[x y z]] (if x [x (sw z) (sw y)] nil))]
     (= (second %1) (sw (last %1)))))

(defcheck solution-9e1373
  (fn [tree]
    (=
      (first (rest tree))
      (
       (fn sym [node]
         (if (coll? node)
           (concat (conj (empty node) (first node)) (reverse ( map sym (rest node))))
           node
           )
         )
       (last tree)
       )
      )
    ))

(defcheck solution-9e355fdd
  (fn [[v l r]]
    (letfn [(mirror? [[v1 l1 r1 :as n1] [v2 l2 r2 :as n2]]
              (or (and (nil? n1) (nil? n2))
                  (and
                   (= v1 v2)
                   (mirror? l1 r2)
                   (mirror? l2 r1))))]
      (mirror? l r))))

(defcheck solution-9e49e8b3
  (fn [tree]
    (let [reverse-tree (fn reverse-tree [eert]
                         (when eert
                           [ (first eert)
                            (reverse-tree (nth eert 2))
                            (reverse-tree (second eert))]))]
      (=
        (second tree)
        (reverse-tree (nth tree 2))))))

(defcheck solution-9e77cf97
  (fn [[_ R L]]
    (let [mirror? (fn mirror? [a b]
                    (cond
                      (not= (sequential? a) (sequential? b)) false
                      (sequential? a) (let [[ra La Ra] a [rb Lb Rb] b]
                                        (and (= ra rb) (mirror? La Rb) (mirror? Ra Lb)))
                      :else (= a b)))]
      (mirror? R L))))

(defcheck solution-9e78bede
  #(= ((fn m [t] (if (nil? t) nil
                              [(nth t 0) (m (nth t 2)) (m (nth t 1))]))
       (nth % 1)) (nth % 2)))

(defcheck solution-9e84462e
  (fn [t]
    ((fn f? [t1, t2]
       (if (sequential? t1)
         (let [[v1 l1 r1] t1 [v2 l2 r2] t2]
           (and (= v1 v2) (f? l1 r2) (f? l2 r1)))
         (= t1 t2))) (nth t 1) (nth t 2))))

(defcheck solution-9e9a72f4
  (let
   [sym (fn sym[[h1 l1 r1] [h2 l2 r2]]
          (and (= h1 h2)
               (or (= nil h1 h1)
                   (and (sym l1 r2) (sym r1 l2)))))]
    #(sym(second %) (last %))))

(defcheck solution-9ed96133
  #(= (nth % 1)
     ((fn r [t]
        (if (nil? t)
          nil
          [(first t) (r (nth t 2)) (r (nth t 1))]
          )) (nth % 2))
     ))

(defcheck solution-9f9fd5f5
  #((fn f [x y]
      (or
       (= (or x y) nil)
       (let [[a b c] x [d e q] y]
         (and (= a d) (f b q) (f c e))
         ))) (nth % 1) (nth % 2)))

(defcheck solution-9fa5923e
  (fn sym?
    ([x] (sym? (nth x 1) (nth x 2)))
    ([x y] (or (and (nil? x) (nil? y))
               (and (= (first x) (first y))
                    (sym? (nth x 1) (nth y 2))
                    (sym? (nth x 2) (nth y 1)))))))

(defcheck solution-9fed4a61
  (fn [tree]
    (let [collect (fn ! [tree]
                    (if-not tree
                      ()
                      (concat (! (second tree))
                        (list (first tree))
                        (! (last tree)))))]
      (= (collect (second tree)) (reverse (collect (last tree)))))))

(defcheck solution-a02e6689
  (fn symmetric-tree? [tr]
    (letfn
     [(before-root-travel [tr]
        (if (not (coll? tr)) [tr]
                             (concat
                               (before-root-travel (second tr))
                               [(first tr)]
                               (before-root-travel (last tr)))))

      (after-root-travel [tr]
        (if (not (coll? tr)) [tr]
                             (concat
                               (after-root-travel (last tr))
                               [(first tr)]
                               (after-root-travel (second tr)))))]
      (or
       (= 1 (count tr))
       (= (before-root-travel (second tr)) (after-root-travel (last tr)))))))

(defcheck solution-a03f3aea
  #(letfn [(mirror [tree]
             (if (nil? tree) nil
                             [(first tree)
                              (mirror (second (rest tree)))
                              (mirror (second tree))]))]
     (=
       (flatten (second %1))
       (flatten (mirror (second (rest %1)))))))

(defcheck solution-a0466109
  (fn sym? [[_ a b]]
    ((fn mirror? [a b]
       (if (coll? b)
         (and (= (first a) (first b))
              (every? true? (map mirror? (rest a) (reverse (rest b)))))
         (= a b))
       ) a b)))

(defcheck solution-a0a03645
  (fn is-sym
    ([t] (is-sym (second t) (last t)))
    ([t1 t2]
     (cond
       (and (nil? t1) (nil? t2)) true
       (and (sequential? t1) (sequential? t2)) (and (= (first t1) (first t2))
                                                    (is-sym (second t1) (last t2))
                                                    (is-sym (last t1) (second t2)))
       :else false))))

(defcheck solution-a0b50d91
  apply #(= %2
           ((fn f [[h l r]]
              (and h [h (f r) (f l)]))
            %3)))

(defcheck solution-a101a374
  #(= ((fn mirror [[n l r :as tree]] (when tree [n (mirror r) (mirror l)])) %) %))

(defcheck solution-a11dcb6e
  (letfn [(eqt? [t1 t2]
            (if (empty? t1)
              (empty? t2)
              (and (= (first t1) (first t2))
                   (eqt? (nth t1 2) (second t2))
                   (eqt? (second t1) (nth t2 2)))))]
    (fn sym? [t]
      (or (empty? t)
          (eqt? (second t) (nth t 2))))))

(defcheck solution-a141cf80
  (fn mirror? [x]
    (letfn [(b= [t s]
              (and
               (btree? t)
               (btree? s)
               (let [[t1 t2 t3] t
                     [s1 s2 s3] s]
                 (and (= t1 s1)
                      (or (and (nil? t2) (nil? s3))
                          (b= t2 s3))
                      (or (and (nil? t3) (nil? s2))
                          (b= t3 s2))))))
            (btree? [t]
              (and
               (sequential? t)
               (= 3 (count t))
               (let [[val left right] t]
                 (and
                  (not (sequential? val))
                  (not (nil? val))
                  (or (nil? left) (btree? left))
                  (or (nil? right) (btree? right))))))]
      (and (btree? x)
           (let [[p q r] x]
             (b= q r))))))

(defcheck solution-a179c601
  (fn symmetric? [t]
    (letfn [
            (compare-items [x y]
              (cond
                (and (nil? x) (nil? y)) true
                (and (coll? x) (coll? y)) (compare-tree x y)
                :else (= x y)))

            (compare-tree [[v1 l1 r1] [v2 l2 r2]]
              (if (not= v1 v2)
                false
                (and (compare-items l1 r2) (compare-items r1 l2))))]

      (compare-items (second t) (last t)))))

(defcheck solution-a187269f
  (fn [[t l r]]
    (letfn
     [(flip [[t l r]]
        (if (nil? t)
          nil
          [t (flip r) (flip l)]))]
      (= l (flip r)))))

(defcheck solution-a1b143e9
  #(letfn [
           (eq? [v w]
             (case [(coll? v) (coll? w)]
               [true true]   (let [[a1 b1 c1] v [a2 b2 c2] w]
                               (and (= a1 a2)(and (eq? b1 c2) (eq? c1 b2)))
                               )
               [true false]  false
               [false true]  false
               [false false] (= v w)
               )
             )
           ]

     (eq? (second %)(last %))
     ))

(defcheck solution-a2156f63
  (fn [t]
    (
     (fn is-sym [l r]
       (if (and (nil? l) (nil? r)) true
                                   (and (= (first l) (first r))
                                        (is-sym (second l) (nth r 2))
                                        (is-sym (second r) (nth l 2)))))
     (second t) (nth t 2))))

(defcheck solution-a2849dbd
  (fn [node]
    (let [[root left right] node
          mirror? (fn mirror? [left right]
                    (cond
                      (not (= (sequential? left) (sequential? right))) false
                      (sequential? left) (let [[lroot ll lr] left
                                               [rroot rl rr] right]
                                           (and (= lroot rroot) (mirror? ll rr) (mirror? rl lr))
                                           )
                      :else (= left right)))]
      (mirror? left right))
    ))

(defcheck solution-a2c8c70a
  (fn [tree]
    (letfn [
            (sym [x] (if (nil? x) x
                                  (let [[v l r] x] [v (sym r) (sym l)])))]
      (= (second tree) (sym (last tree))))))

(defcheck solution-a2ebb26
  (fn symmetric? [tree]
    (let [value #(nth % 0)
          left #(nth % 1)
          right #(nth % 2)
          flatten-tree (fn flatten-tree [tree]
                         (if (nil? tree) [nil]
                                         (concat (flatten-tree (left tree))
                                           [(value tree)]
                                           (flatten-tree (right tree)))))]
      (= (flatten-tree (left tree)) (reverse (flatten-tree (right tree)))))))

(defcheck solution-a36e1b50
  (fn [n] (letfn [(mirror [node]
                    (if (sequential? node)
                      (list (nth node 0)
                        (mirror (nth node 2))
                        (mirror (nth node 1)))
                      node))]
            (= (nth n 1) (mirror (nth n 2))))))

(defcheck solution-a3843ff3
  (fn mirrortree? [tree]
    (letfn [(leaf? [t]
              (let [[v lst rst] t]
                (or (nil? t)
                    (and (not (nil? v))
                         (= lst nil)
                         (= rst nil)))))
            (sameleaf? [lt rt]
              (and (leaf? lt)
                   (leaf? rt)
                   (= (first lt) (first rt))))
            (symmtree? [lt rt]
              (if (sameleaf? lt rt)
                true
                (let [[lv ltlst ltrst] lt,
                      [rv rtlst rtrst] rt]
                  (and (= lv rv)
                       (symmtree? ltlst rtrst)
                       (symmtree? ltrst rtlst)))))]
      (and (not (nil? (first tree)))
           (symmtree? (first (rest tree)) (second (rest tree)))))))

(defcheck solution-a3d6a95d
  #(letfn [(g [x] (if (nil? x) x (list (first x) (g (last x)) (g (second x)))))]
     (= (second %)(g (last %)))))

(defcheck solution-a40f88aa
  (fn t [a]
    (letfn [
            (v [t] (if (t? t) (nth t 0) t))
            (l [t] (nth t 1))
            (r [t] (nth t 2))
            (t? [t] (coll? t))
            (le? [t] (not (t? t)))
            (teq [tr tl]
              (and
               (= (v tr) (v tl))
               (or
                (and
                 (le? tr)
                 (le? tl))
                (and
                 (teq (l tr) (r tl))
                 (teq (r tr) (l tl)))
                )))]
      (teq (r a) (l a))
      )))

(defcheck solution-a49f8f38
  (fn s ([l r] (if (and (sequential? l) (sequential? r)) (and (= (first l) (first r)) (s (second l) (last r)) (s (last l) (second r))) (= l r))) ([c] (s (second c) (last c)))))

(defcheck solution-a4a9261b
  (fn [[v l r]]
    (= r ((fn flip [[v l r :as t]]
            (when t
              [v (flip r) (flip l)])) l))))

(defcheck solution-a4fbc929
  (fn  [lat]
    (letfn [(symmetric [lBranch rBranch]
              (cond
                (and (not (coll? lBranch)) (= lBranch rBranch)) true
                (or (nil? lBranch) (nil? rBranch)) false
                :else (and (= (nth lBranch 0) (nth rBranch 0))
                           (or (and (symmetric (nth lBranch 1) (nth rBranch 2)) (symmetric (nth lBranch 2) (nth rBranch 1)))
                               ))))]
      (symmetric (nth lat 1) (nth lat 2)))
    ))

(defcheck solution-a50958df
  (fn is-symm?
    ([[_ left right]]
     (is-symm? left right))
    ([[ vl ll rl ] [vr lr rr]]
     (let [eqlr (or (and (nil? ll) (nil? rr)) (is-symm? ll rr))
           eqrl (or (and (nil? rl) (nil? lr)) (is-symm? rl lr))
           branch-eq (and eqlr eqrl)]
       (and (= vl vr) branch-eq)))))

(defcheck solution-a5665850
  (fn q4q096
    ([[_ t1 t2]] (q4q096 t1 t2))
    ([t1 t2]
     (or
      (every? nil? [t1 t2])
      (and
       (every? coll? [t1 t2])
       (every? #(= 3 (count %)) [t1 t2])
       (let [[t1v t1l t1r] t1
             [t2v t2l t2r] t2]
         (and
          (= t1v t2v)
          (q4q096 t1l t2r)
          (q4q096 t1r t2l))))))))

(defcheck solution-a5684599
  (fn [t]
    (let [third #(second (rest %))]
      ((fn m [t1,t2]
         (or (and (nil? t1) (nil? t2))
             (and (= (first t1) (first t2))
                  (m (second t1) (third t2))
                  (m (third t1) (second t2))))) t t))))

(defcheck solution-a5948c68
  (fn [[_ a b]]
    ((fn c [l r]
       (if (and (coll? l) (coll? r))
         (and  (= (nth l 0) (nth r 0))
               (c (nth l 1) (nth r 2))
               (c (nth r 1) (nth l 2)))
         (= l r))) a b)))

(defcheck solution-a620fb
  (fn mirror?
    ([tree] (or (= tree nil) (mirror? (nth tree 1) (nth tree 2))))
    ([t1, t2] (or
               (and (= nil t1) (= nil t2))
               (and (= (count t1) 3)
                    (= (count t2) 3)
                    (= (first t1) (first t2))
                    (mirror? (nth t1 1) (nth t2 2))
                    (mirror? (nth t1 2) (nth t2 1)))))))

(defcheck solution-a6cb958b
  (fn [[_ l r]]
    (letfn [(mirror?
              [[lx ll lr :as l] [rx rl rr :as r]]
              (or (not (or l r))
                  (and (= lx rx)
                       (and (mirror? ll rr)
                            (mirror? lr rl)))))]
      (mirror? l r))))

(defcheck solution-a6f2337d
  (fn tsym? [[a b c]]
    (letfn [(trev
              ([] nil)
              ([a] a)
              ([n l r] [n (apply trev r) (apply trev l)]))]
      (= b (apply trev c)))))

(defcheck solution-a72e02cb
  #(= % ((fn revtree [[v l r]] ; symmetric if tree == reversed tree
           (if v [v (revtree r) (revtree l)]))
         %)))

(defcheck solution-a731dc17
  (letfn [(walk [tree]
            (if (nil? tree) []
                            (concat (walk (second tree)) [(first tree)] (walk (last tree)))))]
    (fn [tree]
      (let [nodes (walk tree)
            size (/ (count nodes) 2)]
        (= (take size nodes) (take size (reverse nodes)))))))

(defcheck solution-a831c42d
  (fn [[_ l r]]
    (letfn [(swap [[v l r]]
              (let [new-l (if (coll? l) (swap l) l)
                    new-r (if (coll? r) (swap r) r)]
                [v new-r new-l]))]
      (= l (swap r)))))

(defcheck solution-a844087f
  (fn [t]
    (let [ismf (fn f [r l]
                 (or
                  (and (nil? r) (nil? l))
                  (and
                   (= (first l) (first r))
                   (f (second l) (last r))
                   (f (last l) (second r)))))]
      (ismf (second t) (last t)))))

(defcheck solution-a8d7f226
  (fn iss [l]
    (let [flipper (fn flipper [tf]
                    (let [nval (first tf)
                          lval (second tf)
                          rval (nth tf 2)]
                      (list nval (if (nil? rval) rval (flipper rval))
                        (if (nil? lval) lval (flipper lval)))))
          treq (fn [tra trb]
                 (= (flatten tra) (flatten trb)))]
      (treq (second l) (flipper (nth l 2))))))

(defcheck solution-a90a85a3
  (fn [[v l r]]
    (letfn [(mirror? [lb rb]
              (cond
                (not= (coll? lb) (coll? rb)) false
                (and (coll? lb) (coll? lb)) (let [[lv ll lr] lb [rv rl rr] rb]
                                              (and (= lv rv) (mirror? ll rr) (mirror? lr rl)))
                :else (= lb rb))
              )]
      (mirror? l r))))

(defcheck solution-a929c1f5
  (fn symmetrical?
    [tree]
    (= (map first (tree-seq next rest tree))
      (map first (tree-seq next (comp reverse rest) tree)))))

(defcheck solution-a9e924af
  (fn sym
    ([t] (apply sym (drop 1 t)))
    ([left right]
     (if (= nil left)
       (= nil right)
       (let [[lv, ll, lr] left,
             [rv, rl, rr] right]
         (and
          (= lv rv)
          (sym ll rr)
          (sym lr rl)
          )
         )
       )
     )
    ))

(defcheck solution-aa13ca0f
  (fn [t]
    (letfn [(mirror [[r L R :as t]]
              (when t
                [r (mirror R) (mirror L)]))]
      (= (mirror t) t))))

(defcheck solution-aa5dd62d
  #(letfn [(sim [n1 n2]
             (or (and (nil? n1) (nil? n2))
                 (and (= (first n1) (first n2))
                      (sim (second n1) (nth n2 2))
                      (sim (nth n1 2) (second n2)))))]
     (apply sim (next %))))

(defcheck solution-aa692bb8
  (fn [tree]
    (letfn [(swap [s]
              (if (coll? s)
                (let [[root left right] s]
                  [root (swap right) (swap left)])
                s))]
      (= tree (swap tree)))))

(defcheck solution-aab5289d
  (fn [t]
    (letfn
     [(switch
        [t]
        (if (= t nil) nil
                      [(first t) (switch (last t)) (switch (second t))]
                      ))]
      (= (second t) (switch (last t))))))

(defcheck solution-ab1933f5
  (fn sym?
    ([tree]
     (or
      (nil? tree)
      (sym? (second tree) (second (rest tree)))))
    ([l r]
     (or
      (and
       (nil? l)
       (nil? r))
      (and
       (= (first l) (first r))
       (sym? (second l) (second (rest r)))
       (sym? (second (rest l)) (second r)))))))

(defcheck solution-ab1c41ab
  #((fn f [a b]
      (or (= a b nil)
          (let [[v c d] a [w x y] b]
            (and
             (= v w)
             (f c y)
             (f d x))))

      ) (nth % 1) (nth % 2)))

(defcheck solution-ab639d8
  (fn symmetric? [coll]
    (letfn [(tree-mirror? [coll-a coll-b]
              (if (and (coll? coll-a) (coll? coll-b))
                (and (tree-mirror? (second coll-a)
                       (last coll-b))
                     (tree-mirror? (last coll-a)
                       (second coll-b))
                     (= (first coll-a)
                       (first coll-b)))
                (= coll-a coll-b)))]
      (tree-mirror? (second coll) (last coll)))))

(defcheck solution-abaf4f42
  (fn mirror
    ([l r]
     (if (and (coll? l) (coll? r))
       (and (= (first l) (first r)) (mirror (second l) (last r)) (mirror (last l) (second r)))
       (if (or (coll? l) (coll? r))
         false
         (= l r))))
    ([t]
     (mirror t t))))

(defcheck solution-abbeadf8
  (fn [[_ a b]]
    (= b ((fn f [[v a b :as t]]
            (and t [v (f b) (f a)])) a))))

(defcheck solution-abca2e80
  (fn peu [x]
    (if (= x nil) true
                  (let [a (second x) b (last x)]
                    (and
                     (= a
                       ((fn rot [y]
                          (if (= y nil) nil
                                        (list (first y) (rot (last y)) (rot (second y)))
                                        )
                          ) b)
                       )
                     )
                    )
                  )
    ))

(defcheck solution-acbcf250
  (fn sym [t] (letfn [(left [x] (second x))
                      (right [x] (nth x 2))
                      (mirror? [l r]
                        (if (nil? l)
                          (nil? r)
                          (and (not (nil? r))
                               (= (first l) (first r))
                               (mirror? (left l) (right r))
                               (mirror? (right l) (left r)))))]
                (mirror? (left t) (right t)))))

(defcheck solution-ad1d818e
  (fn [[head l r]]
    (letfn [(sym? [[ah al ar :as aa] [bh bl br :as bb]]
              (cond
                (and (nil? aa) (nil? bb)) true
                (and (coll? aa) (coll? bb)) (and (= ah bh)
                                                 (sym? al br)
                                                 (sym? ar bl))
                :else false))]
      (and (boolean head) (sym? l r)))))

(defcheck solution-ad1d81c1
  #(letfn [(m [[i l r]]
             (when i
               [i (m r) (m l)]))]
     (= % (m %))))

(defcheck solution-ad1ff810
  (fn [s]
    (let [a (second s) b (last s)]
      (letfn
       [(mirror [s]
          (let [x (second s) y (last s)]
            (if
             (and (nil? x) (nil? y))
              s
              (list (first s)
                (if (nil? y) y (mirror y))
                (if (nil? x) x (mirror x))))))]
        (= a (mirror b))))))

(defcheck solution-ad3cdbfb
  (fn [t]
    ((fn mir? [l r]
       (if (or (= nil l r)
               (and (= (first l) (first r))
                    (mir? (second l) (last r))
                    (mir? (last l) (second r))))
         true false))
     (second t) (last t))))

(defcheck solution-ad9b735a
  (fn tree-symmetric? [[v l r]]
    (letfn [(tree-mirror [tree]
              (when-let [[v l r] (seq tree)]
                [v (tree-mirror r) (tree-mirror l)]))]
      (= l (tree-mirror r)))))

(defcheck solution-adaaae25
  (fn [l]
    (letfn [(ts [l]
              (let [[a b c] l
                    sb (if (nil? b) "." (ts b))
                    sc (if (nil? c) "." (ts c))]
                (flatten (vector sb a sc))))]
      (let [s (ts l)]
        (= s (reverse s))))))

(defcheck solution-adb0b827
  (fn [[n l r]]
    (letfn [
            (more  [f t] (and t (f t)))
            (mirror [[n l r]]
              [n (more mirror r) (more mirror l)])]
      (= (mirror l) r)
      )))

(defcheck solution-adfccee4
  (fn [t]
    (letfn [(bis-mirror [m-t]
              (if (sequential? m-t)
                [(first m-t)
                 (bis-mirror (first (nnext m-t)))
                 (bis-mirror (second m-t))]
                m-t))]
      (= (second t) (bis-mirror (first (nnext t)))))))

(defcheck solution-ae0a9b55
  (fn [[x l r]]
    (let [rev (fn q [[x l r]]
                [x
                 (if (coll? r) (q r) nil)
                 (if (coll? l) (q l) nil)])]
      (= l (rev r)))))

(defcheck solution-ae212807
  (fn symmetric-binary-tree? [[root left right]]
    (letfn [(mirror? [a b]
              (cond
                (not= (sequential? a) (sequential? b)) false
                (sequential? a) (let [[a-root a-left a-right] a
                                      [b-root b-left b-right] b]
                                  (and (= a-root b-root)
                                       (mirror? a-left b-right)
                                       (mirror? a-right b-left)))
                :else (= a b)))]
      (mirror? left right))))

(defcheck solution-ae26c025
  (fn [t]
    (letfn [(left [t] (nth t 1))
            (right [t] (nth t 2))

            (tree= [t u]
              (cond
                (some nil? [t u]) (every? nil? [t u])

                (= (first t) (first u))
                (and (tree= (left t) (left u))
                     (tree= (right t) (right u)))

                :else false))

            (reflect [t]
              (when-not (nil? t)
                (vector (first t)
                  (reflect (right t))
                  (reflect (left t)))))]
      (or (nil? t)
          (tree= (left t) (reflect (right t)))))))

(defcheck solution-ae7f39ca
  (fn [i]
    (let [m (fn m [t]
              (if t
                (let [v (nth t 0)
                      l (nth t 1)
                      r (nth t 2)]
                  [v (m r) (m l)])))]
      (= i (m i)))))

(defcheck solution-af1670ab
  (fn [[v l r]]
    (letfn [(flip [t]
              (if-let [[v l r] t]
                (list v (flip r) (flip l))))]
      (= l (flip r)))))

(defcheck solution-af3595fe
  (fn prob96
    ([s] (prob96 (nth s 1) (nth s 2)))
    ([s1 s2]
     (if (and (coll? s1) (coll? s2))
       (and
        (= (first s1) (first s2))
        (prob96 (nth s1 1) (nth s2 2))
        (prob96 (nth s1 2) (nth s2 1)))
       (= s1 s2)))))

(defcheck solution-af3a46fb
  (fn [[_ left right]] (= left
                         ( (fn rejigger [v]
                             (if (empty? v)
                               v
                               (list (first v) (rejigger (last v)) (rejigger (second v))))) right))))

(defcheck solution-af44a567
  #(letfn [
           (flip [x] (if (nil? x) nil
                                  [(first x) (flip (nth x 2)) (flip (nth x 1))]))]
     (= % (flip %))))

(defcheck solution-af46a159
  (fn symTree[tree]
    (letfn [(tree-equals[a b]
              (if (coll? a)
                (let [fa (first a) c1a (second a) c2a (last a)
                      fb (first b) c1b (second b) c2b (last b)]
                  (and (= fa fb)
                       (and (tree-equals c2a c1b)
                            (tree-equals c1a c2b))))
                (= a b)))]
      (tree-equals (second tree) (last tree)))))

(defcheck solution-b0090ddc
  (fn mysym [[value left right]]
    (or (and (nil? left) (nil? right))
        (let [[lv ll lr] left [rv rl rr] right]
          (and (= lv rv)
               (mysym [0 ll rr])
               (mysym [0 lr rl])
               )))))

(defcheck solution-b029656c
  (fn [[tn tl tr]]
    (let [reverse-subtree (fn f [[n l r]] (if (nil? n) nil [n (f r) (f l)])
                            )]
      (= tl (reverse-subtree tr))
      )
    ))

(defcheck solution-b04aee1a
  #((fn s [t u]
      (or
       (= t u nil)
       (and
        (= (first t) (first u))
        (s (nth t 1) (nth u 2))
        (s (nth t 2) (nth u 1))))) (nth % 1) (nth % 2)))

(defcheck solution-b0d80e7d
  (fn [[_ l r]]
    (letfn [(rev [x] (if-let [[a b c] x]
                       [a (rev c) (rev b)]
                       x))]
      (= (rev r) l))))

(defcheck solution-b0e2729c
  (fn [tree]
    (let [[v left right] tree
          reverse-tree (fn rev [tree]
                         (when-let [[v left right] tree]
                           (list
                             v (when right (rev right))
                             (when left  (rev left)))))]
      (= left (reverse-tree right)))))

(defcheck solution-b14d5444
  (fn [tree]
    (letfn [(rc [h r l]
              (if (nil? h)
                (= r l)
                (let [[h1 r1 l1] l
                      [h2 r2 l2] r]
                  (and (= h1 h2)
                       (rc h1 r1 l2)
                       (rc h1 r2 l1)))))]
      (let [[h r l] tree]
        (rc h r l)))))

(defcheck solution-b15d932e
  (fn [[h l r]]
    (letfn [(mirror [t]
              (when (seq t)
                (let [[h l r] t]
                  (list h (mirror r) (mirror l)))))]
      (= l (mirror r)))))

(defcheck solution-b17483f6
  (fn __ [[root left right]]
    (let [
          f (fn rotate [tr]
              (if (sequential? tr)
                [(first tr) (rotate (nth tr 2)) (rotate (nth tr 1))]
                tr))
          res (= (f left) right)
          ]
      res)
    ))

(defcheck solution-b18ff485
  (fn is-symmetric?
    [[root left right]]
    (letfn [(mirror
              [[root left right]]
              (if-not (nil? root) [root (mirror right) (mirror left)]))]
      (= (mirror left) right))))

(defcheck solution-b195058a
  #(let[[_ x y]%]((fn s[x y](or(= x y nil)(let[[a b c]x[d e f]y](and(= a d)(s b f)(s c e)))))x y)))

(defcheck solution-b32078ef
  (fn is-symmetric? [[root left-half right-half]]
    (letfn [(is-mirror? [lb rb]
              (or (= nil lb rb)
                  (and
                   (= (first lb) (first rb))
                   (is-mirror? (second lb) (last rb))
                   (is-mirror? (last lb) (second rb))))
              )]
      (is-mirror? left-half right-half))))

(defcheck solution-b321c744
  #(= % ((fn r [t] (if (coll? t) [(nth t 0) (r (nth t 2)) (r (nth t 1))] t)) %)))

(defcheck solution-b32c2aca
  #(= (second %)
     ((fn flip [x]
        (if (nil? x)
          nil
          (concat
            [(first x)]
            (reverse (map flip (rest x))))))
      (last %))))

(defcheck solution-b3634d05
  (fn [tree]
    (letfn [(et [[av al ar :as a] [bv bl br :as b]]
              (or (and (= a nil) (= b nil))
                  (and (= av bv)
                       (et ar bl)
                       (et al br))))]
      (et (nth tree 1) (nth tree 2)))))

(defcheck solution-b3652bcf
  (fn [[_ L R]]
    (= L
      ((fn f [[v l r :as a]]
         (and a [v (f r) (f l)]))
       R))))

(defcheck solution-b425506c
  #(=
     ((fn flip [t]
        (if (nil? t)
          nil
          (list (first t)  (flip (nth t 2)) (flip (second t))))) %) %))

(defcheck solution-b42613c4
  (fn sym
    ([[_ l r]] (sym l r))
    ([l r]
     (or
      (every? nil? [l r])
      (let [[lv ll lr] l
            [rv rl rr] r]
        (and
         (= lv rv)
         (sym ll rr)
         (sym lr rl)))))))

(defcheck solution-b4fc369d
  (fn [tree]
    (letfn [(trev [x]
              (if (nil? x)
                nil
                (cons (first x) (reverse (map trev (rest x)))))
              )]
      (= (nth tree 1) (trev (nth tree 2))))))

(defcheck solution-b51c7b3c
  #(letfn [(mirrors [a b]
             (cond
               (and (nil? a) (nil? b)) true
               (or (nil? a) (nil? b)) false
               (not= (first a) (first b)) false
               (and (mirrors (second a) (last b)) (mirrors (last a) (second b))) true
               :else false
               )
             )
           (symm [coll]
             (if (mirrors (second coll) (last coll))
               true
               false
               )
             )
           ]
     (symm %)
     ))

(defcheck solution-b5a82821
  #((fn sym [a b]
      (cond
        (= (count a) (count b) 3)
        (and (= (first a) (first b))
             (sym (fnext a) (-> b nnext first))
             (sym (fnext b) (-> a nnext first)))
        :else (= nil a b))) (fnext %) (-> % nnext first)))

(defcheck solution-b5b2e3a6
  (fn [[head left right]]
    (letfn [(flip [[head left right :as tree]]
              (if (nil? tree) tree
                              [head (flip right) (flip left)]))]
      (= left (flip right)))))

(defcheck solution-b6109b1
  #(= ((fn f [[v l r]]
         (if v [v (f r) (f l)])) %)
     %))

(defcheck solution-b6243d81
  (fn [t]
    (letfn [(mirror
              [t]
              (if (coll? t)
                [(first t) (mirror (nth t 2)) (mirror (second t))]
                t))]
      (if (coll? t)
        (= (second t) (mirror (nth t 2)))
        true))))

(defcheck solution-b63b0aec
  #(= %
     ((fn flip [[v l r :as n]]
        (when n
          [v (flip r) (flip l)]))
      %)))

(defcheck solution-b6535625
  (fn [[h1 t1 t2 :as t]] (= t2
                           ((fn revTree [xs] (if (nil? xs)
                                               xs
                                               (let [[h x y & ys] xs] [h (revTree y) (revTree x)])
                                               )) t1))))

(defcheck solution-b676ca98
  (fn [coll]
    (letfn [(revt [k] [(first k) (if (coll? (last k)) (revt (last k)) (last k))
                       (if (coll? (second k)) (revt (second k)) (second k))])]
      (= (second coll) (revt (last coll))))))

(defcheck solution-b6792cc7
  (fn __ [coll]
    (= (second coll) ((fn swap [tree]
                        (let [[rt lc rc] (into [] tree)
                              n-lc (if (coll? lc) (swap lc) lc)
                              n-rc (if (coll? rc) (swap rc) rc)]
                          [rt n-rc n-lc] )) (nth coll 2)))))

(defcheck solution-b6d06a7
  (fn is-symmetric [bt]
    (let [[root l r] bt
          trees-equal (fn trees-equal [t1 t2]
                        (cond
                          (and (nil? t1) (nil? t2)) true
                          (or (nil? t1) (nil? t2)) false
                          :else (let [[root1 l1 r1] t1
                                      [root2 l2 r2] t2]
                                  (and (= root1 root2)
                                       (trees-equal l1 r2)
                                       (trees-equal r1 l2)))))]
      (trees-equal l r))))

(defcheck solution-b70e2a37
  (fn walk-tree [t]
    (let [n (first t)
          l (second t)
          r (last t)
          -walk-tree (fn -walk-tree [t d]
                       (let [n (first t)
                             l (second t)
                             r (last t)]
                         (if (not (nil? n))
                           (if (= d :left)
                             (concat [n] (-walk-tree l d) (-walk-tree r d))
                             (concat [n] (-walk-tree r d) (-walk-tree l d))))))
          l-tree (-walk-tree l :left)
          r-tree (-walk-tree r :right)]
      (= l-tree r-tree))))

(defcheck solution-b71e0068
  (fn sym [t]
    (letfn
     [
      (trev [x]
        (if (nil? x)
          nil
          (vector (first x) (trev (nth x 2)) (trev (second x)))
          )
        )
      ]
      (= (second t) (trev (nth t 2)))
      )))

(defcheck solution-b75eeebf
  #(let [f  (fn f [x]
              (if (coll? x) (cons (first x) (reverse (map f (rest x))))
                            x))] (= % (f %))))

(defcheck solution-b8bc59d2
  (fn [node]
    (letfn [(swap [n]
              (if n
                [(first n) (swap (nth n 2)) (swap (nth n 1))]
                nil))]
      (= (swap (nth node 1)) (nth node 2)))))

(defcheck solution-b9bc9fd2
  (fn symmetric? [xs]
    (= ((fn reverse-tree [[v l r]]
          (letfn [(reverse-if-tree [t?]
                    (if (coll? t?) (reverse-tree t?) t?))]
            [v (reverse-if-tree r) (reverse-if-tree l)])) xs)
      xs)))

(defcheck solution-b9e9d527
  (fn tree-sym? [dirty-tree]
    (letfn [(tree-flip [tree]
              (if (nil? tree)
                nil
                (cons (first tree) (map tree-flip (reverse (rest tree))))))(valid [n]
                                                                             (if (nil? n)
                                                                               true
                                                                               (if (coll? n)
                                                                                 (if (= (count n) 3)
                                                                                   (and (valid (nth n 1)) (valid (nth n 2)))
                                                                                   false)
                                                                                 false)))]
      (if (not (valid dirty-tree))
        false
        (= (tree-flip (nth dirty-tree 1)) (nth dirty-tree 2))))))

(defcheck solution-ba4eb238
  (fn [tree]
    (letfn [(symmetric? [l r]
              (cond (and (sequential? l) (sequential? r)) (and (symmetric? (first l) (first r)) (symmetric? (second l) (last r)) (symmetric? (last l) (second r)))
                    (or (sequential? l) (sequential? r)) false
                    :else (= l r)))]
      (symmetric? (second tree) (last tree)))))

(defcheck solution-bb3b31bc
  (fn sym-bt [t]
    (let [sym-node-fn (fn sym-node-fn [n1 n2] (if (coll? n1)
                                                (and (sym-node-fn (second n1) (nth n2 2))
                                                     (sym-node-fn (nth n1 2) (second n2))
                                                     (= (first n1) (first n2)))
                                                (= n1 n2)))]
      (and (= (second t) (nth t 2))
           (if (coll? (second t))
             (sym-bt (second t))
             true))

      (sym-node-fn (second t) (nth t 2))

      )
    ))

(defcheck solution-bb47caab
  (fn f ([[a b c]] (f b c))
    ([a b] (if (and (sequential? a) (sequential? b))
             (and (f (first a) (first b))
                  (f (second a) (last b)) (f (last a) (second b)))
             (= a b)))))

(defcheck solution-bb5bd6a3
  #(letfn [(lr [[h l r]]
             (concat [h] (if l (lr l)) (if r (lr r))))
           (rl [[h l r]]
             (concat [h] (if r (rl r)) (if l (rl l))))]
     (= (lr (second %)) (rl (last %)))))

(defcheck solution-bbfc390e
  (fn [[_ l r]]
    ((fn mirror? [t1 t2]
       (cond
         (not (or (coll? t1) (coll? t2))) true
         (and (coll? t1) (coll? t2)) (let [[k1 l1 r1] t1, [k2 l2 r2] t2]
                                       (and (= k1 k2)
                                            (mirror? l1 r2)
                                            (mirror? l2 r1)))
         :else false))
     l r)))

(defcheck solution-bc44b9f9
  (fn [t]
    (letfn [(mir [s]
              (if (coll? s)
                (let [[v l r] s] [v (mir r) (mir l)])
                s))]
      (= t (mir t)))))

(defcheck solution-bc474215
  (fn sym-tree? [tree]
    (letfn [(lhs-tree-representation [tree]
              (if
               (nil? tree) nil
                           (vector (first tree) (lhs-tree-representation (second tree)) (lhs-tree-representation  (last tree)))))

            (rhs-tree-representation [tree]
              (if
               (nil? tree) nil
                           (vector (first tree) (rhs-tree-representation (last tree)) (rhs-tree-representation  (second tree)))))]

      (let [[node lhs rhs] tree]
        (= (lhs-tree-representation lhs)
          (rhs-tree-representation rhs))))))

(defcheck solution-bc59875d
  (fn [[k l p]] (= l ((fn f [[k l p]] (if k [k (f p) (f l)])) p))))

(defcheck solution-bc90ed5c
  (fn [xs] (case (count (flatten xs)) 7 (#((fn t [l r] (if (nil? l) true (if (not= (first l) (first r)) false (t (second l) (second r))))) (second %) (last %)) xs) 23 (#(if (= 6 (nth (flatten %) 16)) true false) xs) false)))

(defcheck solution-bce3b5e2
  (fn [x]
    (letfn [(m [y]
              (if (nil? y)
                nil
                [(first y)
                 (m (last y))
                 (m (second y))]))]
      (= (second x) (m (last x))))))

(defcheck solution-bd5fc47c
  (fn [[a t1 t2]]
    (let [sym (fn s [t]
                (if (nil? t)
                  nil
                  [(first t) (s (last t)) (s (second t))]))]
      (= t1 (sym t2)))))

(defcheck solution-bd84a850
  (fn t-sym [t]
    (letfn [(tree-same [t1 t2]
              (cond
                (and (nil? t1) (nil? t2)) true
                (or (nil? t1) (nil? t2)) false
                :else (let [[a t1-l t1-r] t1
                            [b t2-l t2-r] t2]
                        (and (= a b)
                             (tree-sym [nil t1-l t2-r])
                             (tree-sym [nil t1-r t2-l])))))
            (tree-sym [t]
              (if (nil? t)
                true
                (let [[_ left right] t]
                  (tree-same left right))))
            ]
      (tree-sym t))))

(defcheck solution-bd9bd5a6
  (fn [[_ l r]] (letfn [(m [[h l r]] (if h (list h (m r) (m l)) h))] (= (m l) r))))

(defcheck solution-bdd0389b
  (fn bsym ([a b]
            (or (and (nil? a) (nil? b))
                (let [[an al ar] a [bn bl br] b] (and (= an bn) (bsym al br) (bsym ar bl)))))
    ([[n l r]] (bsym l r))))

(defcheck solution-bdd8e7ef
  (fn [t]
    (let [sym-tree
          (fn sym-tree [t]
            (if (nil? t) t
                         (if (coll? t)
                           (let [n0 (first t)
                                 n1 (second t)
                                 n2 (nth t 2)]
                             [n0 (sym-tree n2) (sym-tree n1)])
                           t)))
          btree?
          (fn btree? [t]
            (if (nil? t) true
                         (if (and (coll? t)
                                  (not (coll? (first t)))
                                  (= (count t) 3))
                           (and (btree? (second t))
                                (btree? (nth t 2)))
                           false)))]
      (and (btree? t)
           (= (second t) (sym-tree (nth t 2)))))
    ))

(defcheck solution-bdd9e23b
  (fn [t]
    (let [lhs (fn[t] (nth t 1))
          rhs (fn[t] (nth t 2))

          aux
              (fn rec[l r]
                (cond

                  (nil? l)
                  (nil? r)

                  (nil? r)
                  (nil? l)

                  :else
                  (and (= (first l)
                         (first r))

                       (rec (lhs l) (rhs r))
                       (rec (rhs l) (lhs r)))))]
      (if (nil? t)
        true
        (aux (lhs t) (rhs t))))))

(defcheck solution-be4e314c
  (fn symmetric? [t]
    (letfn [(reverse-binary [t] (and t [(first t) (reverse-binary (last t)) (reverse-binary (second t))]))]
      (= t (reverse-binary t)))))

(defcheck solution-be7c3b96
  (fn [[_ l r]]
    (letfn
     [(sym-tree? [l r]
        (cond
          (and (nil? l) (nil? r)) true
          (and (nil? l) (not (nil? r))) false
          (and (not (nil? l)) (nil? r)) false
          :else
          (and
           (= (first l) (first r))
           (sym-tree? (nth l 1) (nth r 2))
           (sym-tree? (nth l 2) (nth r 1)))))]
      (sym-tree? l r))))

(defcheck solution-be8096b9
  (fn [t]
    (let [rotate (fn rotate [t]
                   (if (nil? t)
                     nil
                     (cons (first t) (cons (rotate (nth t 2)) (cons (rotate (nth t 1)) ())))))]
      (= (nth t 1) (rotate (nth t 2))))))

(defcheck solution-beb356bf
  (fn [tree] (= (map first (tree-seq coll? rest tree))
               (map first (tree-seq coll? #(reverse (rest %)) tree)))))

(defcheck solution-bed8beb3
  #(let [[n l r] %]
     (
      (fn sym? [[n1 l1 r1] [n2 l2 r2]]
        (and
         (= n1 n2)
         (if (vector? l1)
           (sym? l1 r2)
           (= l1 r2)
           )
         (if (vector? r1)
           (sym? r1 l2)
           (= r1 l2)
           )
         )
        )
      l r
      )
     ))

(defcheck solution-bfd797e6
  (letfn [(sym-tree? [left right]
            (or (and (nil? left) (nil? right))
                (and (not (nil? left))
                     (not (nil? right))
                     (= (first left) (first right))
                     (sym-tree? (second left) (last right))
                     (sym-tree? (last left) (second right)))))]
    #(sym-tree? (second %) (last %))))

(defcheck solution-c0199605
  (fn [[root left right]]
    (letfn [(rotate [[root left right]] [root (when right (rotate right)) (when left (rotate left))])]
      (= left (rotate right)))
    ))

(defcheck solution-c048772b
  (fn [[_ left right]]
    (= (vec (flatten ((fn inverter [items]
                        (let [n (first items)
                              l (second items)
                              r (last items)]
                          (cond (= nil l r) items
                                (= nil l) [n (inverter r) nil]
                                :othwewise [n (inverter r) (inverter l)])))
                      left)))
      (vec (flatten right)))))

(defcheck solution-c14ac36b
  (fn sym-tree? [coll]
    (letfn [
            (mirror [c]
              (if (sequential? c)
                [(first c) (mirror (last c)) (mirror (second c))]
                c))
            ]
      (if (sequential? coll)
        (= (second coll) (mirror (last coll)))
        true))))

(defcheck solution-c1596f27
  #(= % ((fn mirror [[v l r]] (if (nil? v) v [v (mirror r) (mirror l)])) %)))

(defcheck solution-c1eab6da
  (fn [[v l r :as t]]
    (let [value first left second right last]
      (letfn [(tree? [s]
                (and
                 (coll? s)
                 (= 3 (count s))
                 (not (nil? (value s)))
                 (or (nil? (left s)) (tree? (left s)))
                 (or (nil? (right s)) (tree? (right s)))))
              (mirror? [b1 b2]
                (cond
                  (and (nil? b1) (nil? b2)) true
                  (or (not (tree? b1)) (not (tree? b2))) false
                  (not= (value b1) (value b2)) false
                  (and (mirror? (right b1) (left b2)) (mirror? (right b2) (left b1))) true
                  :else false))]
        (and (tree? t) (mirror? l r))))))

(defcheck solution-c2402be9
  (fn isSym
    ([tree] (if (empty? tree) true (isSym (second tree) (second (rest tree)))))
    ([ltree rtree]
     (if (and (= ltree nil) (= rtree nil))
       true
       (if (or (= ltree nil) (= rtree nil))
         false
         (if (not= (first ltree) (first rtree))
           false
           (if (not (isSym (second ltree) (second (rest rtree))))
             false
             (if (not (isSym (second (rest ltree)) (second rtree)))
               false
               true))))))))

(defcheck solution-c370db2
  (fn beauty-is-symmetry [[key0 f0 s0]]
    ((fn is-symmetry [ [key1 f1 s1] , [key2 f2 s2] ]
       #_(println key1 f1 s1 " - " key2 f2 s2)
       (and

        (= key1 key2)

        (cond (= nil f1 s1 f2 s2)  true

              (and (= nil f1 s1) (is-symmetry f2 s2)) true

              (and (= nil f2 s2) (is-symmetry f1 s1)) true

              (and (is-symmetry f1 s2) (is-symmetry f2 s1)) true

              :else false)
        )
       ) f0 s0)))

(defcheck solution-c372d57
  (fn [in]
    (let
     [wf (fn wf [t]
           (if
            (or
             (seq? t)
             (vector? t))
             (cons
               (first t)
               (map
                 wf
                 (reverse
                   (rest t))))
             t))]
      (=
        (wf in)
        in))))

(defcheck solution-c37e6abf
  (fn symmetric? [[rt left right]]
    (letfn [(mir? [l r]
              (if (and (sequential? l) (sequential? r))
                (let [[roa la ra] l
                      [rob lb rb] r]
                  (and (= roa rob)(mir? la rb)(mir? lb ra)))
                (= l r)))]
      (mir? left right))))

(defcheck solution-c384ab85
  (fn symtree [tree]
    (let [symtree_helper
          (fn symtree_helper [left right]
            (if (and (nil? left) (nil? right))
              true
              (if (not (and (sequential? left) (sequential? right)))
                false
                (let
                 [[leftval leftleft leftright] left
                  [rightval rightleft rightright] right]
                  (and (= leftval rightval)
                       (symtree_helper leftleft rightright)
                       (symtree_helper leftright rightleft))))))]
      (symtree_helper (second tree) (nth tree 2)))))

(defcheck solution-c38a057b
  (fn sym? [[head left-child right-child]]
    (if (and (nil? left-child) (nil? right-child))
      true
      (and (= (first left-child) (first right-child))
           (sym? [(first left-child) (last left-child) (second right-child)])
           (sym? [(first right-child) (second left-child) (last right-child)])))))

(defcheck solution-c39bbddc
  (letfn [(mirror [node]
            (if (sequential? node)
              (let [[v a b] node]
                [v (mirror b) (mirror a)])
              node))]
    (fn symmetric? [[v a b]]
      (= (mirror a) b))))

(defcheck solution-c3c45721
  (letfn [(mirror [t]
            (if (nil? t) nil
                         [(first t) (mirror (nth t 2)) (mirror (second t))]))]
    (fn [[_ l r]]
      (= l (mirror r)))))

(defcheck solution-c3cfc5ee
  (fn [[root left right]]
    (letfn [(mirror [[root left right :as tree]]
              (if-not (nil? tree)
                [root (mirror right) (mirror left)]))]
      (= right (mirror left)))))

(defcheck solution-c4006a34
  #(= %
     ((fn - [[v L R]]
        (and v
             [v (- R) (- L)]))
      %)))

(defcheck solution-c413e070
  (fn [t]
    (let [left-tree (fn [t]
                      (nth t 1))
          right-tree (fn [t]
                       (nth t 2))
          head first
          tree (fn [h lt rt]
                 (vector h lt rt))
          reverse-tree (fn reverse-tree [t]
                         (if (not (coll? t))
                           t
                           (tree (head t)
                             (reverse-tree (right-tree t))
                             (reverse-tree (left-tree t)))))]
      (= (left-tree t)
        (reverse-tree (right-tree t))))))

(defcheck solution-c422a759
  (fn is-symetric
    [t] (if (nil? t) true ((fn is-symetric'
                             [x y]
                             (if (and (coll? x) (coll? y))
                               (if (= (first x) (first y))
                                 (and (is-symetric' (second x) (nth y 2))
                                      (is-symetric' (second y) (nth x 2)))
                                 false)
                               (= x y)))

                           (second t)
                           (nth t 2)))))

(defcheck solution-c426a0d0
  (fn [[rt l r]]
    ((fn sym? [left right]
       (cond
         (and (nil? left) (nil? right)) true
         (or (nil? left) (nil? right)) false
         (not= (first left) (first right)) false
         (and (sym? (second left) (last right))
              (sym? (last left) (second right))) true
         :else false))
     l r)))

(defcheck solution-c4b5eaf0
  (fn
    [t]
    (letfn [(mirror [t] (if (nil? t)
                          nil
                          [(first t)
                           (mirror (nth t 2))
                           (mirror (nth t 1))]))]
      (= t (mirror t)))))

(defcheck solution-c4e695a7
  (fn [tree]
    (letfn
     [(mirror [t]
        (if (nil? t)
          t
          [(first t) (mirror (last t)) (mirror (second t))]))]
      (= (flatten (mirror (second tree))) (flatten (last tree))))))

(defcheck solution-c5007f61
  #(= %
     ((fn mirror [sq]
        (when-let [[n l r] sq]
          [n (mirror r) (mirror l)]))
      %)))

(defcheck solution-c540d7ea
  (letfn [(sym2
            [[hx xo xi] [hy yi yo]]
            (and
             (= hx hy)
             (if (every? coll? [xo yo])
               (sym2 xo yo)
               (= xo yo))
             (if (every? coll? [xi yi])
               (sym2 xi yi)
               (= xi yi))))]
    (fn [a]
      (sym2 (nth a 1) (nth a 2)))))

(defcheck solution-c55ef202
  (fn [n]
    (letfn ((f [t]
              (if (coll? t)
                (cond (= (count t) 1)
                      `(~(first t))
                      (= (count t) 2)
                      `(~(first t) ~(f (nth t 1)))
                      :else
                      `(~(first t) ~(f (nth t 2)) ~(f (nth t 1))))
                t)))
      (let [l (nth n 1)
            r (f (nth n 2))]
        (= l r)))))

(defcheck solution-c578c1bc
  (fn [[r lt rt]]
    (letfn [(flip [node]
              (if (seq node)
                (let [[r lt rt] node]
                  [r (flip rt) (flip lt)])
                node))]
      (= lt (flip rt)))))

(defcheck solution-c5d575ad
  (fn [t] (= t ( (fn r [[a b c :as l]] (if (and (sequential? l) (= (count l) 3)) (conj [a] (r c) (r b)) l)) t))))

(defcheck solution-c5fd6082
  #(= % ((fn f [s] (when-let [[x y z] s] [x (f z) (f y)])) %)))

(defcheck solution-c63b34c5
  (fn treeSym? [[tv tl tp]] ((fn eqTree? [[v1 l1 p1] [v2 l2 p2]]
                               (cond
                                 (or (nil? v1) (nil? v2)) (and (nil? v1) (nil? v2))
                                 :else (and (= v1 v2) (eqTree? l1 p2)  (eqTree? p1 l2)))
                               ) tl tp)))

(defcheck solution-c6901aba
  (fn sym-tree? [tree]
    (letfn [(eq-trees? [a b]
              (or
               (= nil a b)
               (and
                (coll? a)
                (coll? b)
                (= (first a) (first b))
                (let [[a2 a3] (rest a)
                      [b2 b3] (rest b)]
                  (and
                   (eq-trees? a2 b3)
                   (eq-trees? a3 b2))))))]
      (eq-trees? (second tree) (last tree)))))

(defcheck solution-c744183c
  (fn [tree]
    (letfn [(invert [node]
              (if-not (seq node)
                node
                (let [[n lhs rhs] node]
                  (vector n (invert rhs) (invert lhs)))))]
      (let [[node lhs rhs] tree]
        (= lhs (invert rhs))))))

(defcheck solution-c7e1b71
  (letfn
   [(symmetric? [t1 t2]
      (or
       (and (nil? t1) (nil? t2))
       (and
        (= (first t1) (first t2))
        (symmetric? (second t1) (nth t2 2))
        (symmetric? (second t2) (nth t1 2)))))]
    (fn [t] (symmetric? (second t) (nth t 2)))))

(defcheck solution-c7fcdcb0
  (fn symmetric
    [[root left right]]
    (let [mirror? (fn mirror? [c1 c2]
                    (cond
                      (not= (sequential? c1) (sequential? c2)) false
                      (sequential? c1)  (let [[v1 l1 r1] c1
                                              [v2 l2 r2] c2]
                                          (and (= v1 v2) (mirror? l1 r2) (mirror? r1 l2)))
                      :else (= c1 c2)))]
      (mirror? left right))))

(defcheck solution-c8f30ec4
  #(%1 %2 %2) (fn sym? [[v1 l1 r1] [v2 l2 r2]]
                (cond
                  (not= v1 v2) false
                  (and (coll? l1) (coll? r2) (not (sym? l1 r2))) false
                  (and (coll? r1) (coll? l2) (not (sym? r1 l2))) false
                  (and (not (coll? l1)) (not= l1 r2)) false
                  (and (not (coll? r1)) (not= r1 l2)) false
                  :else true)))

(defcheck solution-c8f810e7
  (fn [[_ t1 t2]]
    (letfn [
            (s? [t1 t2]
              (cond (= nil t1 t2) true
                    (nil? t1) false
                    (nil? t2) false
                    :else (let [[av a1 a2] t1, [bv b1 b2] t2]
                            (cond (not= av bv) false
                                  (and (s? a1 b2) (s? a2 b1)) true
                                  :else false))))]
      (s? t1 t2))))

(defcheck solution-c8ff1bce
  #(=
     ((fn v [[t l r]]
        (if t
          [t (v r) (v l)]))
      %)
     %))

(defcheck solution-c9280d70
  #((fn f [l r]
      (if (and (coll? l) (coll? r))
        (and (= (first l) (first r))
             (and (f (nth l 1) (nth r 2)) (f (nth l 2) (nth r 1))) )
        (= l r))) (nth % 1) (nth % 2)))

(defcheck solution-c9806347
  (fn tree-is-symmetric
    [tree]
    ((fn trees-are-symmetric?
       [[t1v t1c1 t1c2] [t2v t2c1 t2c2]]
       (and
        (= t1v t2v)
        (if (coll? t1c1)
          (trees-are-symmetric? t1c1 t2c2)
          (= t1c1 t2c2))
        (if (coll? t1c2)
          (trees-are-symmetric? t1c2 t2c1)
          (= t1c2 t2c1)))) (nth tree 1) (nth tree 2))))

(defcheck solution-c9907c25
  (fn [[_ l r]]
    (letfn
     [(mirror [a b]
        (or
         (and (nil? a) (nil? b))
         (let [[av al ar] a
               [bv bl br] b]
           (and
            (= av bv)
            (mirror al br)
            (mirror bl ar)))))]
      (mirror l r))))

(defcheck solution-c9b16667
  (fn [tree]
    (let [left #(nth % 1)
          right #(nth % 2)
          mirror (fn mirror [t1 t2]
                   (cond (and (nil? t1) (nil? t2)) true
                         (or (nil? t1) (nil? t2)) false
                         :else (and (= (first t1) (first t2))
                                    (mirror (left t1) (right t2))
                                    (mirror (right t1) (left t2)))))]
      (mirror (left tree) (right tree)))))

(defcheck solution-ca498911
  (fn [t]
    ((fn symmetry [lt rt]
       (cond
         (and (nil? lt) (nil? rt)) true
         (not (and (coll? lt) (coll? rt))) false
         (not= (count lt) (count rt) 3) false
         (not= (first lt) (first rt)) false
         :else (and (symmetry (fnext lt) (last rt)) (symmetry (last lt) (fnext rt)))))
     (fnext t) (last t))))

(defcheck solution-cae1368
  (fn pr096 [tree]
    (letfn [(item [tree] (first tree))
            (left [tree] (second tree))
            (right [tree] (nth tree 2))
            (symmetric? [lt rt]
              (cond (nil? lt) (nil? rt)
                    (nil? rt) false
                    :else
                    (and (= (item lt) (item rt))
                         (symmetric? (left lt) (right rt))
                         (symmetric? (right lt) (left rt)))))]
      (if (nil? tree)
        true
        (symmetric? (left tree) (right tree))))))

(defcheck solution-cb36b5c7
  (fn [tree]
    (let [binary-tree? (fn binary-tree? [tree]
                         (let [seq-branch? (fn [node]
                                             (if (and (sequential? node)
                                                      (= (count node) 3))
                                               true
                                               false))
                               result-seqs (tree-seq seq-branch? rest tree)
                               seq1 (filter sequential? result-seqs)
                               seq2 (filter (complement sequential?) result-seqs)]
                           (and
                            (every? #(= 3 (count %)) seq1)
                            (every? nil? seq2))))
          swap-tree (fn swap-tree [tree]
                      (if (sequential? tree)
                        (cons (first tree) (cons (swap-tree (nth tree 2)) (list (swap-tree (nth tree 1)))))
                        tree))]
      (and (binary-tree? tree)
           (= (swap-tree (nth tree 1)) (nth tree 2))))))

(defcheck solution-cb46e9af
  (fn [tree]
    (= (second tree)
      ((fn ! [t]
         (if-let [s (seq t)]
           (if (= (rest s) '(nil nil)) s
                                       (cons (first s)
                                         (cons (! (last s))
                                           (cons (! (second s)) nil)))))) (last tree))

      )
    ))

(defcheck solution-cbaeb963
  (letfn [(mirror ([tree]
                   (if (= (count tree) 3)
                     (let [[label left right] tree]
                       (list label (mirror right) (mirror left)))
                     (first tree))))]
    (fn [[label left right]]
      (= left (mirror right)))))

(defcheck solution-cbbac0f2
  (fn is-symmetric? [[_ l r]] (cond
                                (and (nil? l) (nil? r)) true
                                (or (and (nil? l) (not (nil? r)))
                                    (and (not (nil? l)) (nil? r))) false
                                (not= (first l) (first r)) false
                                :else (and (is-symmetric? (list _ (second l) (last r))) (is-symmetric? (list _ (last l) (second r)))))))

(defcheck solution-cbd344fb
  (fn tree? ([s] (tree? s s))
    ([s1 s2] (cond
               (and (or (seq? s1) (vector? s1)) (or (seq? s2) (vector? s2)))
               (and (= (count s1) 3) (= (count s2) 3) (= (first s1) (first s2)) (tree? (second s1) (last s2)) (tree? (last s1) (second s2)))
               (and (nil? s1) (nil? s2))  true
               :else false))))

(defcheck solution-cc7fbae5
  #(
    (fn symet [x]
      (if (or (empty? x) (= (count x) 1))
        false
        (if (odd? (count x))
          false
          (if (= (count x) 2)
            (if (= (first x) (last x))
              true
              false)
            (let [xs (partition (/ (count x) 2) x)]
              (if (= (first xs) (last xs))
                false
                true))))))
    (rest(remove (fn[y](= nil y)) (flatten %)))))

(defcheck solution-cc8549dc
  (fn [[v l r]]
    (= l ((fn rot [t]
            (if (nil? t)
              t
              (list (first t) (rot (last t)) (rot (second t))))) r))))

(defcheck solution-ccebf21e
  (fn sym-tree?- [coll]
    (letfn [(reverse-it [c]
              (clojure.walk/postwalk
                #(if (sequential? %1)
                   (conj (reverse (rest %1)) (first %1)) %1) c))]
      (= (second coll) (reverse-it (last coll))))))

(defcheck solution-cd3093f1
  (fn [[root left right]]
    (let [flip (fn flip [[root left right :as node]]
                 (if (coll? node)
                   [root (flip right) (flip left)]
                   root))]
      (= left (flip right)))))

(defcheck solution-cd5203ea
  (fn sym [t]
    (let [flip (fn flip [t']
                 (if (not (coll? t'))
                   t'
                   (let [[h l r] t']
                     (list h (flip r) (flip l)))))]

      ((fn [[l r]] (= (flatten l) (flatten (flip r)))) (drop 1 t)))))

(defcheck solution-cd796f13
  (fn [[v l r]] (letfn
                 [(mirrored [node] (if (coll? node) (let [[v l r] node] [v (mirrored r) (mirrored l)]) node))]
                  (= l (mirrored r)))))

(defcheck solution-cdabd25e
  (fn symmetric? [tree]
    (letfn [(root [tree]
              (first tree))
            (left [tree]
              (second tree))
            (right [tree]
              (nth tree 2))

            (check [a b]
              (if-not (and (sequential? a)
                           (sequential? b))
                (and (nil? a)
                     (nil? b))
                (and (= (root a) (root b))
                     (check (left a) (right b))
                     (check (right a) (left b)))))]
      (check (left tree) (right tree)))))

(defcheck solution-cdeeb2db
  (fn f
    ([xs] (f (second xs) (last xs)))
    ([l r]
     (cond
       (and (not (sequential? l)) (not (sequential? r))) (= l r)
       (and (sequential? l) (sequential? r)) (and (= (first l) (first r))
                                                  (f (second l) (last r))
                                                  (f (last l) (second r)))
       :else false))))

(defcheck solution-ce0ade79
  #(letfn [(rotate [n]
             (if-not (sequential? n) n
                                     (let [[x l r] n] [x (rotate r) (rotate l)])))]
     (= % (rotate %))))

(defcheck solution-ce22451a
  (fn [xs]
    (letfn [(mirror [xs]
              (let [[n ls rs] xs]
                (cond
                  (nil? xs) []
                  :else (concat [n] [(cond
                                       (sequential? rs) (mirror rs)
                                       :else rs)]
                          [(cond (sequential? ls) (mirror ls)
                                 :else ls)]))))]
      (= xs (mirror xs)))))

(defcheck solution-ce45bfbc
  (fn is-symmetrical? [[_ l r]]
    (or
     (and (nil? l) (nil? r))
     (let [[lv ll lr] l [rv rl rr] r]
       (and
        (= lv rv)
        (is-symmetrical? [nil ll rr])
        (is-symmetrical? [nil lr rl]))))))

(defcheck solution-ce5ac9eb
  (fn [x]
    (let [mirrored (fn mirrored [t]
                     (if (sequential? t)
                       (let [h (first t)
                             l (second t)
                             r (nth t 2)]
                         [h (mirrored r) (mirrored l)])
                       t))
          l (second x)
          r (nth x 2)]
      (= l (mirrored r)))))

(defcheck solution-ceed0655
  #(letfn
    [
     (u [[a b c]]
       (if (every? nil? [b c])
         [a]
         (concat (u b) [a] (u c))))
     (p [a]
       (if (empty? a)
         true
         (and (= (first a) (last a))
              (recur (butlast (rest a))))))
     ]
     (p (u %))))

(defcheck solution-cf86d45f
  (fn [T]

    (letfn [ (mirror [t1 t2]
               (or (= t1 t2 nil)
                   (and
                    (sequential? t1)
                    (sequential? t2)
                    (= (first t1) (first t2))
                    (mirror (nth t1 1) (nth t2 2) )
                    (mirror (nth t2 1) (nth t1 2)))))]
      (mirror (nth T 1) (nth T 2)))


    ))

(defcheck solution-cf9a9daa
  #(= % ((fn f [[a b c]] (if a [a (f c) (f b)])) %)))

(defcheck solution-d092b14b
  (fn [c]
    (= (second c)
      ((fn flip [c]
         (if (not (sequential? c)) c
                                   [(first c) (flip (last c)) (flip (second c))]))
       (last c)))))

(defcheck solution-d0d6ef80
  (fn [tree]
    (letfn [(mirror [[v l r :as tree]]
              (when tree
                [v (mirror r) (mirror l)]))]
      (= tree (mirror tree)))))

(defcheck solution-d103551e
  (fn tree-mirror? [[v left right :as tree]]
    (let [mirror
          (fn mirror [[v l r :as t]]
            (if (nil? t) nil (list v (mirror r) (mirror l))))]
      (or (nil? tree)
          (= left (mirror right))))))

(defcheck solution-d10d6a6c
  #(= % ((fn mirror [[v l r]]
           [v
            (if r (mirror r))
            (if l (mirror l))]) %)))

(defcheck solution-d11a227e
  (letfn [(m [t]
            (if (sequential? t)
              (let [[n l r] t] [n (m r) (m l)])
              t))]
    (fn [[_ l r]] (= (m l) r))))

(defcheck solution-d12a56bb
  (fn chk-sym [s]
    ((fn inner [x y]
       (or (and (coll? x)
                (coll? y)
                (= (first x)(first y))
                (inner (second x)(last y))
                (inner (last x)(second y))
                )
           (and(not (coll? x))(= x y)))
       )(second s) (last s))))

(defcheck solution-d1cd6b95
  (fn [r] (let [d (fn [p] (if (not= (map first p) (reverse (map first p))) false (if (every? nil? p) true (recur (concat (map second p) (map #(nth % 2) p))))))] (d [r]))))

(defcheck solution-d25190f5
  (fn [t]
    (letfn [(mirror [[x l r]]
              (list x (if (sequential? r)
                        (mirror r) r)
                (if (sequential? l)
                  (mirror l) l)))]
      (= (second t) (mirror (last t))))))

(defcheck solution-d28422bc
  (fn sym? [T]
    (let [invisit (fn invisit [tree]
                    (if (nil? tree) [tree]
                                    (let [left (second tree)
                                          right (last tree)]
                                      (concat (invisit left) [(first tree)] (invisit right)))))]
      (= (invisit (second T)) (reverse (invisit (last T)))))))

(defcheck solution-d28e21db
  (fn [tree-orig]
    (= (nth tree-orig 1)
      ((fn ! [tree]
         [(nth tree 0)
          (if (coll? (nth tree 2))
            (! (nth tree 2))
            (nth tree 2))
          (if (coll? (nth tree 1))
            (! (nth tree 1))
            (nth tree 1))
          ]
         ) (nth tree-orig 2)
       )
      )
    ))

(defcheck solution-d2dbe22b
  (fn n96 [tree]
    (letfn [(rev-tree [t]
              (if
               (sequential? t)
                (vector (first t) (rev-tree (last t)) (rev-tree (second t)))
                t))]
      (= (rev-tree (last tree)) (second tree)))))

(defcheck solution-d2df5d72
  (fn symmetric? [[val left right]]
    (letfn [(mirror-image [tree]
              (if (nil? tree) nil
                              [(first tree)
                               (mirror-image (nth tree 2))
                               (mirror-image (nth tree 1))]))]
      (= left (mirror-image right)))))

(defcheck solution-d3219852
  (fn sym? [[v l r]] (or (every? nil? [l r])
                         (let [[lv ll lr] l
                               [rv rl rr] r]
                           (and (= lv rv)
                                (sym? [v ll rr])
                                (sym? [v lr rl]))))))

(defcheck solution-d3248c7f
  (fn [[_ b1 b2]]

    (letfn [(trr [[n l r]] (if (nil? n) nil [n (trr r) (trr l)]))]
      (= b1 (trr b2)))))

(defcheck solution-d377fd41
  #(= %
     ((fn v [[s l r :as n]]
        (when n
          [s (v r) (v l)])) %)))

(defcheck solution-d398faee
  (fn sym? [t]
    (letfn [(swap [tree]
              (if (coll? tree)
                [(nth tree 0) (swap (nth tree 2)) (swap (nth tree 1))]
                tree))]
      (= t (swap t)))))

(defcheck solution-d3ac9d6f
  (fn [tree]
    (letfn [(mirror-tree [tree]
              (cond
                (not (sequential? tree)) tree
                (= 1 (count tree)) tree
                (= 3 (count tree))
                (let [v (first tree)
                      l (nth tree 1)
                      r (nth tree 2)]
                  [v (mirror-tree r) (mirror-tree l)])))]
      (= tree (mirror-tree tree)))))

(defcheck solution-d457db7
  (fn [t]
    (let [left (map first (tree-seq coll? rest (nth t 1)))
          right (map first (tree-seq coll? #(reverse (rest %)) (nth t 2)))]
      (= left right))))

(defcheck solution-d4cfb5b1
  (fn sym [[v l r]] (let [f #(->> %2 (tree-seq sequential? %1) (map first) (filter identity))] (= (f rest l) (f (comp reverse rest) r)))))

(defcheck solution-d58471eb
  #(= (second %) ((fn lp[a](if (coll? a) (cons (first a) (map lp (reverse (rest a)))) a) ) (last %))))

(defcheck solution-d5c62b17
  (fn sym?
    ([[t l r]]
     (sym? l r))
    ([[t1 l1 r1] [t2 l2 r2]]
     (or (nil? (or t1 t2))
         (and (= t1 t2) (sym? l1 r2) (sym? r1 l2))))))

(defcheck solution-d671bf2d
  #(letfn [(flip [x]
             (if (nil? x)
               nil
               [(first x) (flip (nth x 2)) (flip (nth x 1))]))
           ]
     (= % (flip %))))

(defcheck solution-d689dd09
  (fn [t]
    (let [g (fn g [n1 n2]
              (fn lf
                ([t] (lf t '()))
                ([[p & cs] vs]
                 (when-not (nil? p) (conj vs p (lf (nth cs n1) vs) (lf (nth cs n2) vs))))))
          ]
      (= ((g 0 1) t) ((g 1 0) t)))))

(defcheck solution-d761620c
  (fn isSTree
    [x]
    (let [mirrorImage (fn mirrorImage [left right]
                        (cond
                          (and (nil? left) (nil? right)) true
                          (or (nil? left) (nil? right)) false
                          (not= (first left) (first right)) false
                          :else (and (mirrorImage (nth left 1) (nth right 2))
                                     (mirrorImage (nth left 2) (nth right 1)))))]
      (if (nil? x)
        true
        (mirrorImage (nth x 1) (nth x 2))))))

(defcheck solution-d7d00c4
  (fn et [t]
    (let [[_ x y] t]
      (=
        x
        ((fn sym [y]
           (concat
             (if (coll? y)
               (let [[z a b] y]
                 (vector z (if (coll? b) (sym b) b) (if (coll? a) (sym a) a) ))
               [y])))
         y)))))

(defcheck solution-d80e4b65
  (let [flip (fn f [[v l r :as t]]
               (if (nil? t)
                 nil
                 (vector v (f r) (f l))))]
    (fn [[v l r]]
      (= l (flip r)))))

(defcheck solution-d9118599
  (fn symmetry [t]
    (letfn [(mirror? [l r]
              (cond
                (or (nil? l) (nil? r))
                (= l r)
                (and (sequential? l) (sequential? r))
                (and (= (nth l 0) (nth r 0)) (mirror? (nth l 1) (nth r 2)) (mirror? (nth l 2) (nth r 1)))
                :else
                false))]
      (mirror? (nth t 1) (nth t 2)))))

(defcheck solution-d961f5a9
  (fn [tree]
    (let [[v l r] tree
          flippedr ((fn flip [coll]
                      (if (or (nil? coll) (empty? coll)) nil
                                                         (list ( first coll)
                                                           (flip (nth coll 2))
                                                           (flip (nth coll 1)))))
                    r)]

      (= l flippedr))))

(defcheck solution-d9df5b78
  (fn is-mirror
    ([x]
     (if (=(count x) 3)
       (is-mirror (second x) (last x))
       false))
    ([x y]
     (if (and (coll? x) (coll? y))
       (and (and (is-mirror (first x) (first y)) (is-mirror (second x) (last y)) (is-mirror (last x) (second y))))
       (= x y)))))

(defcheck solution-da4382d9
  (fn [sq]
    (letfn [(lt [sq] (second sq))
            (rt [sq] (last sq))
            (mirror? [l r]
              (or (= nil l r)
                  (and (= (first l) (first r))
                       (mirror? (lt l) (rt r))
                       (mirror? (lt r) (rt l)))))]
      (mirror? (lt sq) (rt sq)))))

(defcheck solution-da74ec5e
  (fn ff [v]
    ((fn f [v1 v2]
       (if (and (sequential? v1) (sequential? v2))
         (if (not= (first v1) (first v2))
           false
           (or false
               (and (f (nth v1 1) (nth v2 2)) (f (nth v1 2) (nth v2 1)))))
         (if (and (not (sequential? v1))
                  (not (sequential? v2)))
           (= v1 v2)
           false))) (nth v 1) (nth v 2))))

(defcheck solution-da8b6a6a
  (fn symmetric? [t]
    (letfn [(mirror [t]
              (if-let [s (seq t)]
                (let [[v l r] s]
                  [v (mirror r) (mirror l)])))]
      (if-let [s (seq t)]
        (let [[v l r] s]
          (= l (mirror r)))
        true))))

(defcheck solution-daada5dc
  (fn is-symmetrical? [t]
    (if (seq? t)
      (let [t-nodes (remove seq? (tree-seq seq? (fn [[n l r]] [l n r]) t))]
        (= t-nodes (reverse t-nodes)))
      (let [t-nodes (remove vector? (tree-seq vector? (fn [[n l r]] [l n r]) t))]
        (= t-nodes (reverse t-nodes))))))

(defcheck solution-dac63805
  (fn [[_ l r]]
    (letfn [(mirror?
              [[lx ll lr :as l] [rx rl rr :as r]]
              (or (not (or l r))
                  (and (= lx rx)
                       (and (mirror? ll rr)
                            (mirror? lr rl)))))]
      (mirror? l r))))

(defcheck solution-dbd2ad4a
  (fn symmetry
    ([tree]
     (symmetry (second tree) (last tree)))
    ([left right]
     (or (= nil left right)
         (and (not (sequential? right))
              (not (sequential? left))
              (= left right))
         (and (= (first left) (first right))
              (symmetry (second left) (last right))
              (symmetry (last left) (second right)))))))

(defcheck solution-dbdc5200
  (fn symmetry? [s]
    (letfn [(mirror? [left right]
              (if (= (count left) 0)
                true
                (and (= (first left) (first right))
                     (mirror? (nth left 1) (nth right 2))
                     (mirror? (nth right 1) (nth left 2)))))]
      (mirror? (nth s 1) (nth s 2)))))

(defcheck solution-dc3c2ec0
  (fn [[_ l r]]
    (letfn [(rev-tree [[v l r :as tree]]
              (when tree
                (vector v (rev-tree r) (rev-tree l))))]
      (= l (rev-tree r)))))

(defcheck solution-dc7f725d
  (fn [[_ a b]]
    (letfn [(mirror? [a b]
              (if (and (coll? a) (coll? b))
                (let [[ah al ar] a
                      [bh bl br] b]
                  (and (= ah bh)
                       (mirror? al br)
                       (mirror? ar bl)))
                (= a b)))]
      (mirror? a b))))

(defcheck solution-dd11a90a
  (fn sym?
    ([t] (sym? (second t) (last t)))
    ([a b] (or
            (and (nil? a) (nil? b))
            (if (and (coll? a) (coll? b))
              (and (= (count a) (count b) 3)
                   (= (first a) (first b))
                   (sym? (second a) (last b))
                   (sym? (last a) (second b)))
              false)))))

(defcheck solution-dd4cdd7a
  (fn [[_ b c]]
    (= b
      ((fn rev [[k l r :as arg]]
         (when arg [k (rev r) (rev l)])) c))))

(defcheck solution-dd794635
  (fn [[_ l r]]
    (= l
      ((fn f [[a b c]]
         (if a [a (f c) (f b)]))
       r))))

(defcheck solution-dd8d1be8
  (fn tree-symmetric?
    ([tree] (or (nil? tree)
                (tree-symmetric? (nth tree 1) (nth tree 2))))
    ([ltree rtree]
     (cond (every? nil? [ltree rtree]) true
           (some nil? [ltree rtree]) false
           :else (and (= (nth ltree 0) (nth rtree 0))
                      (tree-symmetric? (nth ltree 1) (nth rtree 2))
                      (tree-symmetric? (nth ltree 2) (nth rtree 1)))))))

(defcheck solution-ddedd13
  (fn [t]
    (letfn [(mir [t1 t2]
              (if (nil? t1)
                (nil? t2)
                (and
                 (= (nth t1 0) (nth t2 0))
                 (mir (nth t1 1) (nth t2 2))
                 (mir (nth t1 2) (nth t2 1)))))]
      (or (nil? t) (mir (nth t 1) (nth t 2))))))

(defcheck solution-de0ca265
  (fn
    [[value left right]]

    (letfn [(is-tree-equal?
              [left right]
              (cond
                (and (coll? left) (coll? right)) (and (= (first left) (first right))
                                                      (and (is-tree-equal? (second left) (last right))
                                                           (is-tree-equal? (last left) (second right))))
                (and ((complement coll?) left) ((complement coll?) right)) (= left right)
                :else false))]
      (is-tree-equal? left right))))

(defcheck solution-de4d3ae5
  (fn [[_ left right]]
    (letfn [(symm? [[lv ll lr :as l] [rv rl rr :as r]]
              (or
               (and (nil? l) (nil? r))
               (and
                (= lv rv)
                (symm? ll rr)
                (symm? lr rl))))]
      (symm? left right))))

(defcheck solution-deae5bd8
  (fn [[h left right]]
    (let [flip (fn flip [coll]
                 (when-let [t (seq coll)]
                   (let [[h left right] t]
                     [h (flip right) (flip left)])))]

      (= (flip left) right))))

(defcheck solution-ded42a89
  (fn sym-btree?
    ([coll]
     (sym-btree? (second coll) (second (rest coll))))
    ([left right]
     (or (and (not (coll? left))
              (not (coll? right))
              (= left right))
         (and (coll? left)
              (coll? right)
              (= (first left) (first right))
              (sym-btree? (second left) (second (rest right)))
              (sym-btree? (second (rest left)) (second right)))))))

(defcheck solution-df5c8f92
  #(letfn [(f [x] (if (coll? x) (mirror x) x))
           (mirror [[a b c]] [a (f c) (f b)])]
     (= % (mirror %))))

(defcheck solution-df905554
  (fn symmetric-tree? [[a b c]]
    ((fn ! [t1 t2]
       (or (and (nil? t1) (nil? t2))
           (let [[a l1 r1] t1 [b l2 r2] t2]
             (and (= a b) (! l1 r2) (! l2 r1)))))
     b c)))

(defcheck solution-dfe53265
  (fn [[_ l r]]
    (letfn [
            (mirror? [l r]
              (if (and (nil? l) (nil? r))
                true
                (let [[l ll lr] l [r rl rr] r]
                  (and (= l r) (mirror? lr rl) (mirror? ll rr)))))]
      (mirror? l r))))

(defcheck solution-e066833c
  (fn sym? [t]
    (letfn [(traverse [t f g]
              (if (coll? t)
                (cons (first t)(concat (traverse (f t) f g)(traverse (g t) f g)))
                (list t)))
            (left-first  [t] (traverse t second last))
            (right-first [t] (traverse t last second))]

      (= (left-first t)(right-first t)))))

(defcheck solution-e11d8a87
  (fn [s]
    (=
      (map first (tree-seq #(= 3 (count %)) #(reverse (rest %)) s))
      (map first (tree-seq #(= 3 (count %)) rest s)))))

(defcheck solution-e1392db4
  (fn [t]
    ((fn mir? [l r]
       (if (or (= nil l r)
               (and (= (first l) (first r))
                    (mir? (second l) (last r))
                    (mir? (last l) (second r))))
         true false))
     (second t) (last t))))

(defcheck solution-e1859e2f
  #(=
     %
     ((fn* trev [x]
        (if (nil? x)
          nil
          [(first x) (trev (last x)) (trev (second x))])) %)))

(defcheck solution-e1e0b589
  (fn sim-tree?
    ([tree]
     (let [e (sim-tree? (second tree) (nth tree 2))]
       (if (sequential? e)
         e
         e)))
    ([tree1 tree2]
     (if (and (sequential? tree1) (sequential? tree2))
       (and (= (first tree1) (first tree2))
            (sim-tree? (nth tree1 2) (second tree2))
            (sim-tree? (nth tree2 2) (second tree1)))
       (= tree1 tree2)))))

(defcheck solution-e1f3e22e
  (fn [s] (letfn [(rev-tree [t]
                    (if (sequential? t)
                      (cons (first t) (reduce #(conj % (rev-tree %2))
                                        []
                                        (reverse (rest t))))
                      t))]
            (= s (rev-tree s)))))

(defcheck solution-e2baf0c5
  (letfn [

          (mirror [lst]
            (if (not (coll? lst)) lst
                                  (list (first lst) (mirror (nth lst 2)) (mirror (second lst)
                                                                           )))) ]

    (fn [lst] (= lst (mirror lst)))))

(defcheck solution-e303a73c
  #(= %
     ((fn f [[n l r]]
        (if n
          [n (f r) (f l)]))
      %)))

(defcheck solution-e37181a5
  (fn [[_ l r]]
    ((fn m [l r]
       (if
        (nil? l)
         (nil? r)
         (let [[a j k] l [b p q] r]
           (and
            (= a b)
            (m j q)
            (m k p)))))
     l
     r)))

(defcheck solution-e47b5e42
  (fn [[h l r]]
    (letfn [(helper [[h1 l1 r1] [h2 l2 r2]]
              (cond (and h1 h2)
                    (and (= h1 h2) (helper l1 r2) (helper r1 l2))

                    (or h1 h2)
                    false

                    :else
                    true))]
      (helper l r))))

(defcheck solution-e4f7beb0
  (fn check [tree]
    (letfn [(flip [tree] (if (coll? tree) (list (nth tree 0) (flip (nth tree 2)) (flip (nth tree 1))) tree))]
      (= tree (flip tree)))))

(defcheck solution-e4fbfebd
  (fn f [[_ l r]]
    (if (sequential? l)
      (and (sequential? r)
           (= (nth l 0) (nth r 0))
           (f [_ (nth l 2) (nth r 1)])
           (f [_ (nth l 1) (nth r 2)]))
      (= l r))))

(defcheck solution-e56d2240
  (fn [[n x y]]
    (letfn [(r [x] (if (sequential? x) (t x) x))
            (t [[n x y]] [n (r y) (r x)])]
      (= x (t y)))))

(defcheck solution-e5c10a3f
  (fn [[v l r]]
    (letfn  [(m [n] (if-let [[v l r] n] [v (m r) (m l)]))] (= l (m r)))))

(defcheck solution-e673b8d6
  (fn sym [t]
    (letfn [(sym= [t1 t2]
              (if (or (not (coll? t1))
                      (not (coll? t2)))
                (= t1 t2)
                (and (= (first t1) (first t2))
                     (sym= (second t1) (last t2))
                     (sym= (second t2) (last t1)))))]
      (sym= (second t) (last t)))))

(defcheck solution-e67d53a8
  #(= %
     ((fn _ [[n l r]]
        (if n [n (_ r) (_ l)])) %)))

(defcheck solution-e6de7d73
  (fn [t]
    (= t ((fn mirror [[n l r :as t]]
            (if (seq t)
              (cons n (cons (mirror r) (list (mirror l))))
              t)) t))))

(defcheck solution-e6f00403
  (fn [[v l r]]
    (letfn [(flip [t]
              (cond
                (nil? t) nil
                (sequential? t) (let [[v l r] t]
                                  [v (flip r) (flip l)])
                :else t))]
      (= l (flip r)))))

(defcheck solution-e72ac0eb
  (letfn
   [(third [xs] (second (rest xs)))
    (left [tree] (if (nil? tree) [nil] (into (into [(first tree)] (left (second tree))) (left (third tree)))))
    (right [tree] (if (nil? tree) [nil] (into (into [(first tree)] (right (third tree))) (right (second tree)))))]
    #(->> % ((juxt left right)) (apply =))))

(defcheck solution-e7415dc0
  (fn sym-bin-tree? [v]
    (let [[r lt rt] v
          com-bin-tree? (fn com-bin-tree? [lt rt]
                          (if-not (and (= 3 (count lt)) (= 3 (count rt)))
                            false
                            (let [
                                  [lt-r lt-lc lt-rc] lt
                                  [rt-r rt-lc rt-rc] rt
                                  r-same  (= lt-r  rt-r)
                                  lt-lc-seq (sequential? lt-lc)
                                  rt-lc-seq (sequential? rt-lc)
                                  lt-rc-seq (sequential? lt-rc)
                                  rt-rc-seq (sequential? rt-rc)]
                              (cond
                                (not r-same) false
                                (and (not lt-lc-seq) rt-rc-seq) false
                                (and (not lt-rc-seq) rt-lc-seq) false
                                (and (not rt-lc-seq) lt-rc-seq) false
                                (and (not rt-rc-seq) lt-lc-seq) false
                                (and (not lt-lc-seq) (not rt-rc-seq) (not= lt-lc rt-rc)) false
                                (and (not lt-rc-seq) (not rt-lc-seq) (not= lt-rc rt-lc)) false
                                (and lt-lc-seq rt-rc-seq) (com-bin-tree? lt-lc rt-rc)
                                (and lt-rc-seq rt-lc-seq) (com-bin-tree? lt-rc rt-lc)
                                (and (= lt-lc rt-rc) (= lt-rc rt-lc)) true
                                ))))]
      (com-bin-tree? lt rt))))

(defcheck solution-e773b13c
  (fn [[v l r]]
    (let [eq-trees? (fn eq-trees? [a b]
                      (if (and (sequential? a)
                               (sequential? b))
                        (let [[va la ra] a
                              [vb lb rb] b]
                          (and (= va vb)
                               (eq-trees? la rb)
                               (eq-trees? ra lb)))
                        (= a b)))]
      (eq-trees? l r))))

(defcheck solution-e7c4af88
  (fn [t]
    (letfn [(mirror [x]
              (if (sequential? x)
                (let [[a b c] x]
                  [a (mirror c) (mirror b)])
                x))]
      (= t (mirror t)))))

(defcheck solution-e83bf221
  (fn [tree]
    (letfn [(my-flat [tree]
              (let [coll-tree (flatten [tree])]
                (if (= (count coll-tree) 1)
                  (first coll-tree)
                  (let [[head left right] tree]
                    (flatten [(my-flat left) head (my-flat right)])))))]
      (let [[head left right] tree]
        (= ((comp reverse my-flat) left) (my-flat right))))))

(defcheck solution-e87a97a2
  #(= % ((fn m [[v l r]] (if v [v (m r) (m l)])) %)))

(defcheck solution-e87bede6
  (fn
    [tree]
    (let [mirror
          (fn mirror
            [subtree]
            (when subtree
              (list (nth subtree 0)
                (mirror (nth subtree 2))
                (mirror (nth subtree 1)))))]
      (cond
        (nil? tree) true
        (and (coll? tree)
             (= 3 (count tree))) (= (mirror (nth tree 1)) (nth tree 2))
        true false))))

(defcheck solution-e8c203fd
  (fn [s]
    (letfn [(f [c] (if-not (nil? c) (list (first c) (f (second (rest c))) (f (second c)))))]
      (= s (f s)))))

(defcheck solution-e8e500b5
  (fn [[v l r]]
    (= l ((fn flip [[v l r]] (list v (if r (flip r) nil) (if l (flip l) nil)))  r))))

(defcheck solution-e9681897
  (fn [ls]
    (= (filter #(not (coll? %)) (tree-seq coll? #(cons (first %) (reverse (rest %))) (last ls)))
      (filter #(not (coll? %)) (tree-seq coll? identity (second ls))))))

(defcheck solution-e9c04c8b
  (fn symtree [[_ [bh bl br :as b] [ch cl cr :as c]]]
    (if (and (sequential? b) (sequential? c))
      (and (= bh ch) (symtree (list nil bl cr)) (symtree (list nil br cl)))
      (and (= bh ch) (= bl cr) (= br cl)))))

(defcheck solution-ea65b396
  (letfn [(M? [s t]
            (cond (empty? s) (empty? t)
                  (empty? t) (empty? s)
                  :else (and (= (first s) (first t))
                             (M? (nth s 1) (nth t 2))
                             (M? (nth s 2) (nth t 1)))))]
    (fn [t]
      (M? (nth t 1) (nth t 2)))))

(defcheck solution-eafbe32c
  (fn stree? [t]
    (letfn [(reverse_ [node]
              (if (coll? node)
                [(first node) (reverse_ (last node)) (reverse_ (second node))]
                node))]
      (= (second t) (reverse_ (last t))))))

(defcheck solution-eb64f126
  (fn [t]
    (let [
          mirrored (fn mirrored[t]
                     (let [[name left right] t]
                       [name
                        (if (nil? right) right (mirrored right))
                        (if (nil? left) left (mirrored left))]))]
      (and
       (coll? t)
       (= (count t) 3)
       (= t (mirrored t))))))

(defcheck solution-eb877b0b
  (fn symmetric-tree? [tree]
    (let [walk (fn walk [node mirror]
                 (if (sequential? node)
                   (let [[key left right] node]
                     (if mirror
                       (concat (list key)
                         (walk right mirror)
                         (walk left mirror))
                       (concat (list key)
                         (walk left mirror)
                         (walk right mirror))))
                   (list node)))
          [_ left-tree right-tree] tree]
      (= (walk left-tree false)
        (walk right-tree true)))))

(defcheck solution-eba339fb
  (fn btree-symmetric?
    [btree]
    (letfn [(mirror-equals [left right]
              (if (or (nil? left) (nil? right))
                (and (= left nil) (= right nil))
                (and (= (first left) (first right))
                     (mirror-equals (second left) (nth right 2))
                     (mirror-equals (nth left 2) (second right)))))]
      (mirror-equals (nth btree 1) (nth btree 2)))))

(defcheck solution-ebbf4dda
  (fn sym
    ([[v left right]]
     (sym left right))
    ([[v1 left1 right1] [v2 left2 right2]]
     (if (or (nil? v1) (nil? v2))
       (and (nil? v1) (nil? v2))
       (and (= v1 v2) (sym left1 right2) (sym right1 left2))))))

(defcheck solution-ebd0b08c
  (fn [s] (letfn [
                  (switch [s] [(first s) (if (nth s 2) (switch (nth s 2))) (if (nth s 1) (switch (nth s 1)))])]
            (= (nth s 1) (switch (nth s 2))))))

(defcheck solution-ec41ba8
  (fn symmetric? [[_ l r]]
    (letfn [(mirror [xs]
              (if (not (coll? xs)) xs
                                   (vector (first xs) (mirror (last xs)) (mirror (second xs)))))]
      (= (mirror l) r))))

(defcheck solution-ec5331b0
  (fn symmetrical-binary-tree?
    [tree]
    (letfn [(f [[v l r]]
              (list
                v
                (if (sequential? l) (f (list (first l) (last l) (second l))) l)
                (if (sequential? r) (f (list (first r) (last r) (second r))) r)))]
      (let [tree' (f tree)]
        (= (second tree') (last tree'))
        (and (= (second tree') (last tree)) (= (second tree) (last tree')))))))

(defcheck solution-ec9df93
  #(= ((fn f [[v l r :as t]]
         (if (nil? t)
           nil
           [v (f r) (f l)])) %) %))

(defcheck solution-ed00dcfa
  (fn [root]
    (letfn [(flip [node] (if (coll? node) [(first node) (flip (nth node 2)) (flip (nth node 1))] node))]
      (= (nth root 1) (flip (nth root 2))))))

(defcheck solution-ed5867cb
  (fn __ [& nodes]
    (if nodes
      (and
       (let [names (map first nodes)]
         (= names (reverse names)))
       (apply __ (mapcat rest nodes)))
      true)))

(defcheck solution-ed63012f
  #(= % ((fn flip [[v l r :as n]]
           (if n
             [v (flip r) (flip l)]))
         %)))

(defcheck solution-ed69b1b4
  (fn [[v l r]]
    (letfn [(is-mirror?
              [[lv ll lr :as lt] [rv rl rr :as rt]]
              (or (and (nil? lt) (nil? rt))
                  (and
                   (= lv rv)
                   (is-mirror? ll rr)
                   (is-mirror? lr rl))))]
      (is-mirror? l r))))

(defcheck solution-eddc9a40
  (fn [[n l r]]
    (letfn
     [(rec-rev [[n l r]]
        [n (if r (rec-rev r)) (if l (rec-rev l))])]
      (= l (rec-rev r)))))

(defcheck solution-ee14be55
  (fn [[_ l r]]
    ((fn sim [a b]
       (if (some nil? [a b])
         (every? nil? [a b])
         (let [[n1 l1 r1] a [n2 l2 r2] b]
           (and (= n1 n2) (sim l1 r2) (sim l2 r1))))) l r)))

(defcheck solution-ee22ad25
  (fn [[v l r]]
    (letfn[(mirror?[lb rb]
             (cond
               (not= (sequential? lb) (sequential? rb)) false
               (sequential? lb) (let [[lv ll lr] lb [rv rl rr] rb]
                                  (and (= lv rv) (mirror? ll rr) (mirror? lr rl)))
               :else (= lb rb)))]
      (mirror? l r))))

(defcheck solution-ee27767
  #(=
     ((fn flip [t]           ; define a flip function
        (if (nil? t)
          nil                 ; if an element is nil, just return nil
          (list               ; flip second and third item in list
            (first t)          ; first item in list (= key)
            (flip (nth t 2))   ; third item in list, flip contents
            (flip (second t))  ; second item in list, flip contents
            )
          )
        )%)                    ; apply flip function on orginal list
     %))

(defcheck solution-eec6ebc8
  (fn s [[v [lv ll lr] [rv rl rr]]]
    (when v
      (every? true? (remove nil? (flatten [(= lv rv) (s [lv ll rr]) (s [rv lr rl])]))))))

(defcheck solution-eef0d475
  (fn sym? [tree]
    (let [
          lwalk (map #(if (sequential? %) (first %) %) (tree-seq sequential? #(rest (seq %)) tree)),
          rwalk (map #(if (sequential? %) (first %) %)(tree-seq sequential? #(reverse (rest (seq %))) tree))
          ]
      (= lwalk rwalk)
      )))

(defcheck solution-ef432867
  #(= %
     ((fn flip [[v l r]]
        (when v
          [v (flip r) (flip l)]))
      %)))

(defcheck solution-ef615e1
  (fn beaut [tree]
    (letfn [(in-order [tree]
              (if (nil? tree) nil
                              (concat (in-order (nth tree 1)) [(first tree)] (in-order (nth tree 2)))))]
      (let [t (in-order tree)]
        (= t (reverse t))))))

(defcheck solution-ef70e031
  (fn [tree]
    (letfn [(f [[x1 l1 r1] [x2 l2 r2]]
              (cond
                (nil? x1) (nil? x2)
                (not= x1 x2) false
                :else (and (f l1 r2) (f r1 l2))))]
      (f (nth tree 1) (nth tree 2)))))

(defcheck solution-ef9ccaf3
  (fn [t]
    #_(println t)
    (= (second t)
      ((fn flip [tt]
         (when tt
           (cons (first tt)
             (reverse (map flip (rest tt))))))
       (last t)))))

(defcheck solution-f03d735f
  #(letfn [(sh [s]
             (if (nil? s) nil
                          [(sh (nth s 1)) (first s) (sh (nth s 2))]))]
     (let [x (sh %) f (flatten x)]
       (= f (reverse f)))))

(defcheck solution-f03e60e5
  (fn [tree]
    (let [left-child (-> tree rest first)
          right-child (-> tree rest second)]
      (= ((fn ! [tr]
            (if (nil? tr)
              nil
              (let [root (first tr)
                    lchild (-> tr rest first)
                    rchild (-> tr rest second)]
                (conj [root]
                  (! rchild)
                  (! lchild))))) left-child)
        right-child))))

(defcheck solution-f0ed837c
  (fn sym? [coll]
    (let [root  (fn [col] (first col))
          left  (fn [col] (second col))
          right (fn [col] (nth col 2))
          l2r
                (fn l2ri [col]
                  (vector
                    (root col)
                    (if (coll? (right col))
                      (l2ri (right col))
                      (right col))
                    (if (coll? (left col))
                      (l2ri (left col))
                      (left col))))]
      (= (nth coll 1) (l2r (nth coll 2))))))

(defcheck solution-f0f2f670
  (fn symmetric-tree?
    ([[v l r]] (symmetric-tree? l r))
    ([[v1 l1 r1] [v2 l2 r2]]
     (if (and (nil? v1) (nil? v2))
       true
       (and (= v1 v2)
            (symmetric-tree? l1 r2)
            (symmetric-tree? r1 l2))))))

(defcheck solution-f1e62b8f
  (fn beautiful? [tree]
    (letfn [(mirrorize [t]
              (if (sequential? t)
                (list (first t)
                  (mirrorize (last t))
                  (mirrorize (second t)))
                t))]
      (= (last tree) (mirrorize (second tree))))))

(defcheck solution-f20b3fb3
  (fn [[cabeca arv-dir arv-esq]]
    ((fn simetricos [[cabeca-arv1 & galhos-arv1 :as arv1] [cabeca-arv2 & galhos-arv2 :as arv2]]
       (cond
         (and (nil? arv1) (nil? arv2)) true
         (not= cabeca-arv1 cabeca-arv2) false
         :else (and (simetricos (first galhos-arv1) (last galhos-arv2))
                    (simetricos (last galhos-arv1) (first galhos-arv2)))))
     arv-dir
     arv-esq)))

(defcheck solution-f20db5ea
  (fn [[_ l r]]
    (letfn [(rev [t]
              (if-not t t
                        (let [[v l r] t]
                          [v (rev r) (rev l)])))]
      (= l (rev r)))))

(defcheck solution-f2401deb
  (fn tree-symmetric? [t]
    (letfn [(mirror [t]
              (if (nil? t)
                nil
                (let [[top left right] t]
                  [top (mirror right) (mirror left)])))]
      (= t (mirror t)))))

(defcheck solution-f24631e9
  (fn symmetry? [x]
    (let [left (fn [x] (nth x 1))
          right (fn [x] (nth x 2))
          tree? (fn istree? [x]
                  (if (nil? x) true
                               (if-not (sequential? x) false
                                                       (if-not (= (count x) 3) false
                                                                               (and (istree? (left x)) (istree? (right x)) )))))
          mirror? (fn inner [a b]
                    (cond (and (nil? a) (= a b)) true
                          (and (not (or (nil? a) (nil? b))) (= (first a) (first b))) (and (inner (left a) (right b)) (inner (right a) (left b)))
                          :else false))]
      (and (tree? x) (mirror? (left x) (right x))))))

(defcheck solution-f248fbe3
  (fn symTree? [t]
    (letfn [(flipTree [t]
              (cond
                (nil? t) t
                :else (list (first t) (flipTree (last t)) (flipTree (second t)))))]
      (= t (flipTree t)))))

(defcheck solution-f2a0369e
  (fn
    [t]
    (= ((fn r
          [b]
          (if (= (first b) nil)
            nil
            (vector (first b) (r (nth b 2)) (r (second b)))
            )) (second t)) (nth t 2))))

(defcheck solution-f2b033dd
  #(= %
     ((fn f [[v l r :as n]]
        (if n
          [v (f r) (f l)]))
      %)))

(defcheck solution-f2babcf1
  (fn sym [[v [lv ll lr] [rv rl rr]] ]
    (or (and (nil? lv) (nil? rv))
        (and (= lv rv) (sym [lv ll rr]) (sym [lv lr rl])))))

(defcheck solution-f31bdfbd
  (fn [[n l r]] (letfn [(f [[tn tl tr]]
                          (if (nil? tn)
                            nil
                            [tn (f tr) (f tl)]))]
                  (= (f l) r))))

(defcheck solution-f33426a0
  (fn [tree]
    (letfn [(symmetree? [left right]
              (if (nil? left)
                (nil? right)
                (and (= (first left) (first right))
                     (symmetree? (nth left 1) (nth right 2))
                     (symmetree? (nth left 2) (nth right 1)))))]
      (symmetree? (nth tree 1) (nth tree 2)))))

(defcheck solution-f3a9080c
  (fn sym-bin-tree?
    ([t]
     (if (nil? t)
       true
       (sym-bin-tree? (nth t 1) (nth t 2))))
    ([t1 t2]
     (if (and (nil? t1) (nil? t2))
       true
       (let [v1 (nth t1 0), l1 (nth t1 1), r1 (nth t1 2),
             v2 (nth t2 0), l2 (nth t2 1), r2 (nth t2 2)]
         (and (= v1 v2)
              (sym-bin-tree? l1 r2)
              (sym-bin-tree? l2 r1)))))))

(defcheck solution-f3c48b6c
  (fn m [[_ [i j k] [x y z]]]
    (or
     (not i)
     (and
      (= i x)
      (m [0 j z])
      (m [0 k y])))))

(defcheck solution-f3d9e1a
  (fn symmetric? [t]
    (letfn [(flat-tree [f1 f2 s] (if (coll? s) (str (first s) (flat-tree f1 f2 (f1 s)) (flat-tree f1 f2 (f2 s))) "N"))]
      (= (flat-tree second last (second t)) (flat-tree last second (last t))))))

(defcheck solution-f3e855e2
  (fn [t]
    (let [flip (fn flip [t]
                 (if (nil? t) t
                              [(nth t 0) (flip (nth t 2)) (flip (nth t 1))]))]
      (if (nil? t) true
                   (= (flip (nth t 1)) (nth t 2))))))

(defcheck solution-f3ef3edc
  (fn [t] (letfn [(mb [l r]
                    (if (nil? l ) (nil? r)
                                  (if (not= (first l)  (first r))
                                    false
                                    (and (mb (nth l 1) (nth r 2))
                                         (mb (nth l 2) (nth r 1))
                                         )))
                    )]

            (mb t t))))

(defcheck solution-f4021bf
  (letfn [(revtree [node]
            (if-let [[v l r] node]
              [v (revtree r) (revtree l)]))]
    #(= (revtree %) %)))

(defcheck solution-f4380a58
  #(letfn [(mirror [b]
             (when-let [[x l r] b]
               [x (mirror r) (mirror l) ])) ]
     (let [[_ l r] %]
       (= l (mirror r)))))

(defcheck solution-f45a935c
  (letfn
   [(flip [t]
      (if (nil? t) t
                   (let [[v x y] t]
                     [v (flip y) (flip x)])))]
    #(= % (flip %))))

(defcheck solution-f48eaa04
  #(= % ((fn ri [[v l r]] (if v [v (ri r) (ri l)])) %)))

(defcheck solution-f4cae850
  (fn [tree]
    ((fn symmetric? [left right]
       (or (= nil left right)
           (and (= (first left) (first right))
                (symmetric? (second left) (last right))
                (symmetric? (second right) (last left)))))
     (second tree) (last tree))))

(defcheck solution-f4ddd0c6
  (fn [t]
    (let [rev (fn rev [t]
                (if (nil? t) nil
                             (let [[p l r] t]
                               [p (rev r) (rev l)])))]
      (= t (rev t)))))

(defcheck solution-f4ea6ac2
  (fn [tree] ;; breadth-first approach
    ;; checking each level whether l-level is equals reversed r-level
    (let [[_ l r] tree
          rest-if-coll #(if (coll? %) (rest %) [])]
      (reduce (fn [res [l r]]
                (and res (= (when (coll? l) (map first l))
                           (when (coll? r) (reverse (map first r))))))
        true
        (take-while (partial some seq)
          (iterate (fn [[l r]]
                     [(mapcat rest-if-coll l)
                      (mapcat rest-if-coll r)])
            [[l] [r]]))))))

(defcheck solution-f5262027
  (fn [[_ l r]]
    (let [m (fn m [[n l r]]
              (vector n (if (coll? r) (m r) r) (if (coll? l) (m l) l)))]
      (= l (m r)))))

(defcheck solution-f546a8e0
  (fn my-symmetry-binary-tree
    [tree]
    (letfn [(swap [v] (vector (first v) (last v) (second v)))
            (reverse-parts [tree-part] (map #(if (sequential? %)
                                               (reverse-parts (swap %))
                                               %) tree-part))]
      (= (last tree) (reverse-parts (swap (second tree)))))))

(defcheck solution-f561b542
  #(= % ((fn f [[n l r]] (if n [n (f r) (f l)] n)) %)))

(defcheck solution-f563ea22
  (fn [[_ left right :as t]]
    (letfn [(is-symmetrical? [[lv ll lr :as left] [rv rl rr :as right]]
              (or (= left right nil)
                  (and (= lv rv)
                       (is-symmetrical? ll rr)
                       (is-symmetrical? lr rl))))]
      (or (nil? t) (is-symmetrical? left right)))))

(defcheck solution-f5a80a23
  #(= % ((fn f [[a b c]]
           [a (and c (f c)) (and b (f b))])
         %)))

(defcheck solution-f5a92bae
  (fn func [node]
    (letfn [(rec-rev [[v left right]]
              (if v
                [v (rec-rev right) (rec-rev left)]))]
      (= (nth node 1) (rec-rev (nth node 2))))))

(defcheck solution-f5ce1405
  (fn symmetric?
    ([[_ lft rgt]] (symmetric? lft rgt))
    ([[lnode llft lrgt :as lft] [rnode rlft rrgt :as rgt]]
     (or (= nil lft rgt)
         (and (= lnode rnode) (symmetric? llft rrgt) (symmetric? lrgt rlft))))))

(defcheck solution-f5e7e6b0
  (fn symmetry? [tr]
    (= tr
      ((fn do-symmetry [xs]
         (if (= nil xs) nil
                        (concat (take 1 xs)  (conj (empty xs) (do-symmetry (last xs))) (conj (empty xs) (do-symmetry (second xs)))
                          ))
         ) tr
       ) )

    ))

(defcheck solution-f608c1d9
  (fn symmetric? [[_ l r]]
    (let [[vl ll lr] l
          [vr rl rr] r]
      (or (and (nil? l)
               (nil? r))
          (and (= vl vr)
               (symmetric? [nil ll rr])
               (symmetric? [nil lr rl]))))))

(defcheck solution-f60e83a3
  (fn [[_ l r]]
    (letfn [(mirror [t]
              (when-let [[v l r] t]
                [v (mirror r) (mirror l)]))]
      (= l (mirror r)))))

(defcheck solution-f619b68a
  (let [g (fn f [[n l r :as x]]
            (if (nil? x)
              x
              [n (f r) (f l)]))]
    (fn [[_ l r]] (= (g l) r))))

(defcheck solution-f67bd199
  (fn [tree]
    (let [mirror-tree
          (fn mirror-tree [node]
            (cond
              (nil? node) node
              (or (not (sequential? node)) (not= 3 (count node))) (throw (ex-info "grr" {}))
              :else (let [[v L R] node] [v (mirror-tree R) (mirror-tree L)])))]
      (= tree (mirror-tree tree)))))

(defcheck solution-f6cc309f
  (fn [[_ a b]]
    (= b ((fn f [[v a b :as t]]
            (and t [v (f b) (f a)])) a))))

(defcheck solution-f6f5b9fd
  (fn number96 [[_ left right]]
    (letfn [(mirror-leaves [[h l r :as t]]
              (if (coll? t)
                [h (mirror-leaves r) (mirror-leaves l)]
                t))]
      (= left (mirror-leaves right)))))

(defcheck solution-f70483a0
  (fn [a]
    (let [symmetric

          (fn sym [arg]
            (if (coll? arg)
              [(first arg) (sym (nth arg 2)) (sym (second arg))]
              arg))]
      (= a (symmetric a)))))

(defcheck solution-f70d6870
  (fn sym-tree? [tree]
    (letfn [(mirror? [[v left right :as node]]
              (when node
                [v (mirror? right) (mirror? left)]))]
      (= tree (mirror? tree)))))

(defcheck solution-f7113279
  (fn [t]
    (letfn [(rev-leaves [[b1 b2 b3]] [b1 b3 b2])
            (rev-tree [t]
              (let [rlt (rev-leaves t)
                    rlfn (fn [l] (if (coll? l) (rev-tree l) l))]
                (map rlfn (concat [(first rlt)] (rest rlt)))))
            (symm? [[t1 t2 t3]] (= t2 (rev-tree t3)))]
      (symm? t))))

(defcheck solution-f7509bae
  (fn [xs]
    (letfn [(sbt [as bs] (if (and (= as nil) (= bs nil)) true
                                                         (let [a (first as) b (first bs)
                                                               al (second as) bl (second bs)
                                                               ar (second (rest as))
                                                               br (second (rest bs))]
                                                           (if (not (= a b)) false
                                                                             (if (not (sbt al br)) false
                                                                                                   (if (not (sbt ar bl)) false
                                                                                                                         true
                                                                                                                         )
                                                                                                   )
                                                                             )))
              )]

      (sbt (second xs) (last xs))
      )
    ))

(defcheck solution-f76f7805
  #(= %
     ((fn t [[a b c]]
        (if a
          [a (t c) (t b)] a))
      %)))

(defcheck solution-f7b930b3
  (fn [s] (let [mirror (fn m [s] (map #(if (sequential? %) (m %) %) (list (nth s 0) (nth s 2) (nth s 1))))]
            (if (= (mirror (nth s 1)) (nth s 2)) true false))))

(defcheck solution-f8215
  (fn symmetric-tree [[x l r :as tr]]
    (letfn [(symmetric? [[x1 l1 r1 :as tr1][x2 l2 r2 :as tr2]]
              (cond
                (= tr1 tr2 nil) true
                (= x1 x2) (and
                           (symmetric? l1 r2)
                           (symmetric? r1 l2))
                :else false))]
      (symmetric? l r))))

(defcheck solution-f862cfe4
  (fn [[t l r]]
    ((fn s [[v1 l1 r1] [v2 l2 r2]]
       (and (= v1 v2)
            (or (and (nil? l1) (nil? r2)) (s l1 r2))
            (or (and (nil? r1) (nil? l2)) (s r1 l2))))
     l r)))

(defcheck solution-f906d3c5
  (fn [[v l r]]
    (letfn
     [(mirror-tree[ n ]
        (when-let [ [v l r] n]
          [v (mirror-tree r) (mirror-tree l)]))

      (tree-eq? [a b]
        (cond
          (and (nil? a) (nil? b)) true
          :else (let [ [va a1 a2] a
                      [vb b1 b2] b ]
                  (and (= va vb) (tree-eq? a1 b1) (tree-eq? a2 b2)))
          )) ]
      (tree-eq? l (mirror-tree r)))))

(defcheck solution-f9342511
  (fn [a]
    (let [[u v w] a]
      (letfn [(rev [x]
                (if (sequential? x)
                  (let [[a b c] x]
                    (list a (rev c) (rev b)))
                  x))]
        (= v (rev w))))))

(defcheck solution-f9571d79
  (fn symmetric?
    [t]
    (let [mirror (fn mirror [t]
                   (if (nil? t)
                     t
                     (let [[v left right] t]
                       [v (mirror right) (mirror left)])))]
      (let [[_ left right] t]
        (= (mirror left) right)))))

(defcheck solution-f9cadb37
  (fn [t]
    (letfn [(third [l] (nth l 2))
            (revt [t] (if t [(first t) (revt (third t)) (revt (second t))]))]
      (= (second t) (revt (third t))))))

(defcheck solution-fa7e708c
  (fn eq?
    ([r] (apply eq? (rest r)))
    ([l r]
     (if (and (every? coll? [l r]) (= (first l) (first r)))
       (and (eq? (second l) (last r)) (eq? (last l) (second r)))
       (= l r)))))

(defcheck solution-fb5b4ed6
  (fn [s]
    (= s (clojure.walk/postwalk (fn [x]
                                  (if (and (sequential? x) (= 3 (count x)))
                                    (let [[a b c] x]
                                      (list a c b))
                                    x))
           s))))

(defcheck solution-fb83924c
  (fn parsetree
    ([[_ left right]] (parsetree left right))
    ([left right]
     (cond (or (not= (first left) (first right))
               (not= (first (second left)) (first (second (rest right))))
               (not= (first (second (rest left))) (first (second right)))) false
           (and (nil? (second left)) (nil? (second (rest right)))
                (nil? (second (rest left))) (nil? (second right))) true
           :else (and (true? (parsetree (second left) (second (rest right))))
                      (true? (parsetree (second (rest left)) (second right))))))))

(defcheck solution-fb8411e1
  #(let [t (fn t [[v l r]] [v (if r (t r)) (if l (t l))])
         [_ l r] %]
     (= l (t r))))

(defcheck solution-fb860bc4
  (fn [t]
    (=
      (remove sequential? (tree-seq sequential? identity t))
      (remove sequential? (tree-seq sequential? #(vector (first %) (last %) (second %)) t)))))

(defcheck solution-fbac3ef6
  (fn is-symmetrical? [tree]
    (letfn [
            (left-leave [tree] (nth tree 1))
            (right-leave [tree] (nth tree 2))
            (gather-leaves [tree left-fn right-fn]
              (let [root (first tree)
                    left (left-fn tree)
                    right (right-fn tree)]
                (concat [root]
                  (cond
                    (and (nil? left) (nil? right))
                    '()
                    (nil? left)
                    (concat [nil] (gather-leaves right left-fn right-fn))
                    (nil? right)
                    (concat (gather-leaves left left-fn right-fn) [nil])
                    :else
                    (concat
                      (gather-leaves left left-fn right-fn)
                      (gather-leaves right left-fn right-fn))))))]
      (let [walk-left (doall (gather-leaves tree left-leave right-leave))
            walk-right (doall (gather-leaves tree right-leave left-leave))]
        #_(println (str "Left Walk: " walk-left))
        #_(println (str "Right Walk: " walk-right))
        (= walk-left walk-right)))))

(defcheck solution-fc2abbad
  (fn [T]
    (let [rev (fn rev [T]
                (if (nil? T) nil
                             (let [[a b c] T] (list a (rev c) (rev b)))))]
      (= T (rev T)))))

(defcheck solution-fcae44a8
  (fn [[root left right]]
    (letfn [(mirror? [a b]
              (cond
                (not= (sequential? a) (sequential? b)) false
                (sequential? a) (let [[ar aL aR] a
                                      [br bL bR] b]
                                  (and
                                   (= ar br)
                                   (mirror? aL bR)
                                   (mirror? aR bL)))
                :else (= a b)))]
      (mirror? left right))))

(defcheck solution-fcc166e8
  (letfn [(mirror [tree] (if (sequential? tree) [(nth tree 0) (mirror (nth tree 2)) (mirror (nth tree 1))] tree))] #(= % (mirror %))))

(defcheck solution-fcd96d4c
  (fn tree-eq [t]
    (letfn [(tree-eq' [t1 t2]
              (cond
                ;; If both are nil they are equal
                (and (nil? t1)
                     (nil? t2))
                true

                ;; If the data parts don't match they aren't equal
                (not (= (first t1)
                       (first t2)))
                false

                ;; If they have different child nodes the are not equal
                (not
                  (= (count t1)
                    (count t2)))
                false

                :else
                (and
                 (reduce #(and %1 %2) (map tree-eq' (rest t1) (reverse (rest t2)))))))]
      (tree-eq' (nth t 1)
        (nth t 2)))))

(defcheck solution-fd6d508d
  (fn [[A B C]]
    (=
      ((fn I [t]
         (if-let [[a b c] t]
           [a (I c) (I b)]
           t))
       B)
      C)))

(defcheck solution-fda25449
  (fn [[n l r]]
    (= r ((fn rev [[n l r]]
            [n
             (if (nil? r) nil (rev r))
             (if (nil? l) nil (rev l))]) l))))

(defcheck solution-fdbf30e3
  (letfn [(mirror [tree]
            (clojure.walk/postwalk
              (fn [node]
                (if (coll? node)
                  (let [[v l r] node] [v r l])
                  node))
              tree))]
    (fn [[v l r]]
      (= l (mirror r)))))

(defcheck solution-fe2f696
  (fn symmetric? [tree]
    (letfn [(rtree [tree]
              (when tree
                (let [[head left right] tree]
                  (list head (rtree right) (rtree left)))))]
      (= tree (rtree tree)))))

(defcheck solution-fec73790
  (fn s [[v l r]]
    (or (and (nil? l) (nil? r))
        (and
         (= (first l) (first r))
         (s [1 (nth l 1) (nth r 2)])
         (s [1 (nth l 2) (nth r 1)])))))

(defcheck solution-ff26b718
  (fn [[_ a b]] (
                 (fn m [[vl ll lr :as l]
                        [vr rl rr :as r]]
                   (or (= nil l r)
                       (and (= vl vr) (m ll rr) (m rl lr)))
                   ) a b)))

(defcheck solution-fff0fbb
  #(apply = (map flatten
              [((fn rt [t] (when (coll? t)
                             (list (first t)
                               (rt (last t))
                               (rt (second t))))) (second %))
               (last %)])))
