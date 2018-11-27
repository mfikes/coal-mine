(ns coal-mine.problem-83
  (:require [coal-mine.checks :refer [defcheck-83] :rename {defcheck-83 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-1017e1b1
  (fn [& args]
    (and (not (nil? (some true? args)))
         (not (nil? (some false? args))))))

(defcheck solution-10ba927f
  (fn __ [& bools]
    (if (and (not (empty? (filter #(= true %) bools)))
             (not (empty? (filter #(= false %) bools))))
      true
      false
      )
    ))

(defcheck solution-11a28851
  (fn [& vs]
    (true? (and (some not vs)
                (some identity vs)))))

(defcheck solution-133ffa31
  (fn half-truth [& args]
    (if (every? true? args)
      false
      (if (some true? args)
        true
        false))))

(defcheck solution-13d5ed45
  #(apply not= %&))

(defcheck solution-141b73df
  (fn [& s] (if (and (some #(= true %) s)
                     (not (every? #(= true %) s)))
              true
              false)))

(defcheck solution-14275743
  (fn [& x] (= #{true false} (set x))))

(defcheck solution-14cbf5e
  (fn [& z]
    (let [sum (apply + (map #(if % 1 0) z))]
      (and (< sum (count z)) (> sum 0))
      )))

(defcheck solution-1509bccc
  (fn truish [& args] (and (not-every? false? args)
                           (not= (count args) (count (remove false? args))))))

(defcheck solution-1538a43
  (fn [& args]
    (> (count (partition-by identity args)) 1)))

(defcheck solution-156e4fe5
  (fn part-true? [& args]
    (cond
      (every? identity args)  false
      (some identity args)    true
      :else                   false)))

(defcheck solution-15b4ba55
  (fn [& vs] (true? (and (some false? vs) (some true? vs)))))

(defcheck solution-15e9d2ca
  (fn [& bools]
    (if (and (some false? bools)
             (some true? bools))
      true
      false)))

(defcheck solution-16375ae3
  (fn [& args] (boolean (and (some identity args) (some not args)))))

(defcheck solution-16a59e68
  (fn [& s]
    (let [t (filter identity s) f (remove identity s)]
      (boolean (and (seq t) (seq f)))
      )
    ))

(defcheck solution-16cfe36d
  (fn
    [& bools]
    (if (reduce #(and %1 %2) bools)
      false
      (reduce #(or %1 %2) bools))))

(defcheck solution-17026bce
  (fn x-or [& items]
    (and (not= (.indexOf items true) -1)
         (not= (.indexOf items false) -1))))

(defcheck solution-1714cfcf
  (fn [ & col]
    (< 0 (count (filter true? col )) (count col)  )))

(defcheck solution-1730caee
  (fn [& more] (and ((complement not-any?) true? more) (not-every? false? more) (not-every? true? more))))

(defcheck solution-1771ea65
  (fn [& args] (let [s (apply list args)]
                 (if (every? true? s)
                   false
                   (if (some true? s)
                     true
                     false)))))

(defcheck solution-17b2f46b
  (fn [& bool-seq] (cond
                     (and (some true? bool-seq) (some false? bool-seq)) true
                     :else false
                     )))

(defcheck solution-17b5fe81
  (fn [& more] (if (and (some false? more) (some true? more))
                 true false)))

(defcheck solution-18c072e6
  (fn [& bs]
    (let [r (and (some true? bs) (not-every? true? bs) ) ]
      (if (nil? r) false r))))

(defcheck solution-18d3d6ed
  (fn [& bs] (= #{true false} (set bs))))

(defcheck solution-194f116
  #(->>(into #{} %&) count (= 2)))

(defcheck solution-19b66627
  (fn [& more]
    (true? (and (some true? more) (some false? more)))))

(defcheck solution-19ed854c
  (fn [& booleans]
    (true?
      (and (some true? booleans)
           (some false? booleans)))))

(defcheck solution-1a18ec4f
  (fn [& xs]
    (->> xs (map true?) (reductions conj #{}) (some #(= 2 (count %))) true?)))

(defcheck solution-1a7c5845
  (fn [& xs]
    (not (or (not-any? identity xs) (every? identity xs)))))

(defcheck solution-1a7cb3b4
  #(loop [sawT false sawF false bs %&]
     (cond
       (and sawT sawF) true
       (empty? bs) false
       (first bs) (recur true sawF (rest bs))
       :else (recur sawT true (rest bs)))))

(defcheck solution-1ac10564
  (fn [& l] (let [x  (map #(if % 1 0) l) m (apply * x) s (apply + x)] (and (pos? s) (zero? m)))))

(defcheck solution-1ad3ee29
  (fn [& b] (and (true? (some identity b)) (not-every? identity b))))

(defcheck solution-1b224d9b
  (fn listOR
    [& args]
    (and (not (reduce #(and %1 %2) args)) (reduce #(or %1 %2) args))))

(defcheck solution-1b83431
  (fn [b & bs]
    (let [xs (cons b bs)]
      (and (boolean (some true? xs))
           (not (every? true? xs))))))

(defcheck solution-1c5cf0e7
  #(not (not (and (some identity %&) (some not %&)))))

(defcheck solution-1c83c828
  (fn [& l]
    (= {true true false true} (reduce #(assoc %1 %2 true) {true false false false} l))))

(defcheck solution-1c91f2f9
  (fn [& a] (not (empty? (rest (set a))))))

(defcheck solution-1cab262f
  (fn [& xs]
    (and (boolean (some identity xs))
         (not-every? identity xs))))

(defcheck solution-1ce9ee91
  (fn [& args] (boolean (and (some identity args)
                             (not-every? identity args)))))

(defcheck solution-1d3eaa47
  (fn [& tf]
    (cond (every? true? tf) false
          (some true? tf) true
          :t false)))

(defcheck solution-1d8a7c6d
  (fn [& bs] (not (nil? (and (some true? bs) (some false? bs))))))

(defcheck solution-1e24da1
  (fn [& a] (and (false? (reduce #(and %1 %2) a))
                 (true? (reduce #(or %1 %2) a)))))

(defcheck solution-1e8bb973
  (fn [& x] (= #{false true} (set x))))

(defcheck solution-1e8e408c
  (fn [& ps]
    (and (apply (some-fn true?) ps) (apply (some-fn false?) ps))))

(defcheck solution-1e99e2a2
  (fn [& args] (if (and (some true? args) (not-every? true? args)) true false)))

(defcheck solution-1ee14093
  #(true?
     (and
      (some true? %&)
      (some false? %&))))

(defcheck solution-1f1c5208
  (fn [& c] (and (not-every? true? c) (true? (some true? c)))))

(defcheck solution-1f47f6fe
  #(boolean (and (some true? %&) (not-every? true? %&))))

(defcheck solution-1f61d4bf
  (fn [& m]
    (true? (and (some true? m) (some false? m)))))

(defcheck solution-1fe98a55
  (fn [& more] (and (reduce #(or %1 %2) more)
                    (not (reduce #(and %1 %2) more) ) )))

(defcheck solution-1ffc46c8
  (fn [ & bs]
    (if (and (some true? bs) (some false? bs))
      true
      false)))

(defcheck solution-203d5493
  (fn [& ps] (= (some true? ps) (some false? ps))))

(defcheck solution-20449eeb
  (fn [& xs]
    (and (contains? (set xs) true) (contains? (set xs) false))))

(defcheck solution-2123307e
  (fn [& xs]
    (let [nx (count xs)
          ys (filter identity xs)
          ny (count ys)]
      (< 0 ny nx))))

(defcheck solution-2123c322
  #(not (or
         (nil? (some true? %&))
         (every? true? %&))))

(defcheck solution-216131e1
  #(true? (and (some true? %&) (some not %&))))

(defcheck solution-216a38a8
  (fn [& args]
    (let [s (set args)]
      (and (contains? s true) (contains? s false) ))))

(defcheck solution-21a90019
  (fn some-but-not-all [& coll]
    (and (>= (count (filter true? coll)) 1)
         (>= (count (filter false? coll)) 1))))

(defcheck solution-221d5107
  (fn [& vals] (and (not-every? identity vals) (not-every? not vals))))

(defcheck solution-2261d68d
  (fn half-truth [& args]
    (cond
      (reduce #(and %1 %2) args) false
      :else (reduce #(or %1 %2) args))))

(defcheck solution-228b620f
  (fn [& args]
    (let [x (some true? args), y (every? true? args)]
      (if (and (= x true) (= y false))
        true
        false))))

(defcheck solution-237e5e2c
  (fn [& params]
    (boolean (and (some true? params)
                  (not-every? true? params)))))

(defcheck solution-23d2ff66
  (fn [& args] (if (and (some identity args) (not (every? identity args))) true false)))

(defcheck solution-23ed8cec
  (fn [ & args ]
    (if (> (count (distinct args)) 1)
      true
      false)))

(defcheck solution-24e4af99
  (fn [& args]
    (not
      (or
       (every? true? args)
       (every? false? args)))))

(defcheck solution-24edab86
  (fn [& x]
    (= #{true false} (set x) )))

(defcheck solution-255ff613
  (fn [& bs]
    (boolean
      (and (some identity bs)
           (not (every? identity bs))))))

(defcheck solution-25aecb5a
  (fn half-truths [& bs]
    (if (and (some identity bs) (not (every? identity bs))) true false)))

(defcheck solution-25e14cf5
  (fn[& v](=(count(set v))2)))

(defcheck solution-26238faf
  #(and (not (every? true? %&)) (boolean (some true? %&))))

(defcheck solution-263958fb
  (fn f [& xs]
    (if (and (some true? xs) (some false? xs))
      true
      false)))

(defcheck solution-266f9cd4
  (fn [& bs] (boolean (and (some identity bs) (some not bs)))))

(defcheck solution-26c7265a
  (fn [& args]
    (let [trues (filter true? args)
          falses (filter false? args)]
      (and (not (empty? trues))
           (not (empty? falses))))))

(defcheck solution-26d6b71b
  #(true? (and (some true? %&) (some false? %&))))

(defcheck solution-26eb3eb4
  (fn [& args]
    (boolean (and
              (some identity args)
              (not-every? identity args)))))

(defcheck solution-27262a82
  (fn [& a]
    (= #{true false} (into #{} a))))

(defcheck solution-27b642e7
  (fn [& x] (and (not= (some true? x) nil) (not-every? true? x))))

(defcheck solution-27d77612
  (fn [& bools]
    (and
     (not-every? true? bools)
     (not-every? false? bools))))

(defcheck solution-27ebd022
  (fn [& col] (boolean (and (some true? col)  (some false? col)))))

(defcheck solution-28335b93
  (fn [& x]
    (if (and (not-every? true? x) (some true? x))
      true
      false)))

(defcheck solution-290fb921
  (fn [& bools] (= 2 (count (keys (group-by true? bools))))))

(defcheck solution-2949d6db
  (fn [& ls] (and (not (every? not ls)) (not (every? identity ls)))))

(defcheck solution-296c1b17
  (fn [& bools] (and (not-every? false? bools)
                     (not-every? true? bools))))

(defcheck solution-298164f4
  (fn mysome
    [& args]
    (and (= (some true? args) true) (= (some false? args) true) )))

(defcheck solution-2984d22a
  (fn sotrue [& more]
    (if (and (some true? more) (not (reduce #(and %1 %2) more))) true false)))

(defcheck solution-2a2d3d33
  (fn [& xs] (= #{true false} (set xs))))

(defcheck solution-2b4e5d49
  (fn [& args]
    (boolean (and
              (some identity args)
              (some not args)))))

(defcheck solution-2b53531f
  (fn [& coll]
    (case (and (some identity coll) (not-every? identity coll))
      true true
      false false
      nil false)))

(defcheck solution-2bb645c4
  (fn [& booleans]
    (= #{true false} (set booleans))))

(defcheck solution-2bf48034
  #(and (or (some identity %&) false) (not (every? identity %&))))

(defcheck solution-2c1ceba5
  (fn [x & xs] (not-every? #(= x %) xs)))

(defcheck solution-2c7fb713
  (fn [& a] (or (and (some true? a) (some false? a)) false)))

(defcheck solution-2d2b5397
  (fn half-truth [& l]
    (let [coun (reduce #(if %2 (inc %1) %1) 0 l)]
      (and (< coun (count l)) (not= coun 0) ))))

(defcheck solution-2d84c639
  (fn [& bs] (true? (and (some true? bs) (not (every? true? bs))))))

(defcheck solution-2e3b25e5
  (fn [& args] (boolean (and (some identity args) (not (every? identity args))))))

(defcheck solution-2e64c809
  #(true?
     (and
      (some true? %&)
      (some false? %&))))

(defcheck solution-2e9a30f6
  (fn [& bools]
    (boolean (and (not (every? true? bools))
                  (some true? bools)))))

(defcheck solution-2ea1f617
  (fn[& args] (and (not (nil? (some true? args))) (not-every? true? args))))

(defcheck solution-2ebb55f
  (fn
    [& seq]
    (loop [newseq seq trues 0]
      (if (empty? newseq)
        (if (and  (< trues (count seq)) (not= trues 0))
          true
          false
          )
        (if (first newseq)
          (recur (rest newseq) (inc trues))
          (recur (rest newseq) trues)
          )
        ))))

(defcheck solution-2f621e20
  (fn [& as]
    (letfn [(fold [bs [f t]]
              (if (empty? bs) (and f t)
                              (if (first bs)
                                (fold (rest bs) [f true])
                                (fold (rest bs) [true t]))))]
      (fold as [false false]))))

(defcheck solution-2faec68c
  (fn [& l]
    (and
     (boolean (some identity l))
     (not-every? identity l))))

(defcheck solution-3008ac60
  (fn long-xor [& args]
    (if (= (count (apply conj #{} args)) 2) true false)))

(defcheck solution-30a6b69
  (fn half-truth [& more]
    (let [expr (and (some true? more) (some false? more))]
      (true? expr))))

(defcheck solution-30f43ee9
  (fn [& args]
    (boolean (and (some true? args)
                  (some false? args)))))

(defcheck solution-31013558
  (fn [& thruths]
    (< 0 (count (filter true? thruths)) (count thruths))))

(defcheck solution-31807760
  #(let [uniques (set %&)] (and (= 2 (count uniques)) (contains? uniques true))))

(defcheck solution-3184d194
  (fn [& args] (= 2 (count (map key (group-by identity args))))))

(defcheck solution-32136664
  (fn [& bools] (not= 1 (count (set bools)))))

(defcheck solution-32523a47
  (fn [& args]
    (and (not= 0 (count (filter #(= true %) args))) (not= 0 (count (filter #(= false %) args))))))

(defcheck solution-3285ac9d
  (fn [& bools] (if (and (some identity bools) (some not bools)) true false)))

(defcheck solution-32d632bc
  #(= 2 (count (into #{} %&))))

(defcheck solution-34917adb
  #(if (apply = % %&) false true))

(defcheck solution-34ef99f3
  (fn [& l]
    (and
     (not (reduce #(and %1 %2) true l))
     (reduce #(or %1 %2) false l))))

(defcheck solution-3521203b
  #(and (not (every? identity %&)) (not (not-any? identity %&))))

(defcheck solution-352484cc
  (fn ht [& args]
    (if (or (every? identity args)
            (not-any? identity args))
      false
      true)))

(defcheck solution-35280732
  #(boolean
     (and (some identity %&)
          (not-every? identity %&))))

(defcheck solution-353f6114
  (fn [& p]
    (not (or (every? true? p) (every? false? p)))))

(defcheck solution-3628af02
  (fn [& v]
    (and
     (not (not-any? #(true? %) (seq v)))
     (not-every? #(true? %) (seq v)))))

(defcheck solution-36553eaf
  (fn [& bools]
    (and
     (contains? (zipmap bools (repeat true)) true)
     (contains? (zipmap bools (repeat true)) false))))

(defcheck solution-370728bc
  (fn [& x] (and (not (nil? (some identity x))) (not (nil? (some not x))))))

(defcheck solution-3759a763
  (fn [& a](= 2 (count (group-by identity a)))))

(defcheck solution-379ce90c
  (fn ht [& s]
    (let [r (group-by identity s)]
      (and (not (empty? (r true))) (not (empty? (r false)))))))

(defcheck solution-38263cf9
  #(boolean(and (some true? %&) (not-every? true? %&))))

(defcheck solution-38ddf542
  (fn [& xs]
    (boolean
      (and (some identity xs)
           (some (comp not identity) xs)))))

(defcheck solution-390a8949
  (fn
    [& li]
    (and
     (not (reduce
            #(and %1 %2)
            li))
     (reduce
       #(or %1 %2)
       li))))

(defcheck solution-396b07d0
  (fn ht
    ([a] false )
    ([a b] (if (or (and a b) (and (not a) (not b))) false true))
    ([a b & args] (if (ht a b) true
                               (if (true? a)
                                 (if (some false? (seq args)) true false)
                                 (if (some true? (seq  args)) true false))
                               ))))

(defcheck solution-399ceb74
  #(= (count (set %&)) 2))

(defcheck solution-39fdbf03
  (fn [& args]
    (let [any? (comp not not-any?)]
      (and (any? true? args)
           (not-every? true? args)) )))

(defcheck solution-3acf267f
  (fn f [& xs] (and (reduce #(or %1 %2) xs) (not (reduce #(and %1 %2) xs)))))

(defcheck solution-3b2d1fe7
  (fn [& args] (not (or (every? true? (seq args)) (every? false? (seq args))))))

(defcheck solution-3b79e8fd
  #(= 2 (count (group-by identity %&))))

(defcheck solution-3b9ebce0
  (fn xor [& args]
    (not (or (every? true? args) (every? false? args)))))

(defcheck solution-3bba4c14
  (fn [& args]
    (let [coll args]
      (if (every? true? coll) false
                              (if (some true? coll) true
                                                    (if (every? false? coll) false))))))

(defcheck solution-3bf99d0
  (fn [& x]
    (and
     (true? (some true? x))
     (not (every? true? x)))))

(defcheck solution-3d250dba
  (fn multi-xor
    [& bools]
    (not (not (and (some identity bools)
                   (some not bools))))))

(defcheck solution-3db80227
  (fn [& args]
    (if (= 2 (count (group-by identity args)))
      true false)))

(defcheck solution-3dbd46
  (fn [& xs] (->> xs
               distinct
               keys
               count
               (= 2))))

(defcheck solution-3e1bae66
  (fn [& bs]
    (true?
      (when (some true? bs)
        (some false? bs)))))

(defcheck solution-3e75ca6
  (fn hlftru
    [& args]
    (and (not (every? #(identity %) args))
         (not (every? #(not (identity %)) args)))))

(defcheck solution-3f3fa0ab
  (fn [& args]
    (if (every? true? args) false
                            (not (nil? (some true? args))))))

(defcheck solution-3f789c17
  (fn [& args] (and (not (every? identity args)) (not (not-any? identity args)))))

(defcheck solution-3f7f1b02
  (fn [& xs] (-> xs set (= #{true false}))))

(defcheck solution-3f85332c
  (fn [& bools] (and (reduce #(or %1 %2) bools) (not (reduce #(and %1 %2) bools)))))

(defcheck solution-3faf390d
  #(and (not (nil? (some true? %&))) (not-every? true? %&)))

(defcheck solution-3fd88941
  (fn [& args]
    (and
     (> (count (filter (fn [x] (true? x)) args)) 0)
     (> (count (filter (fn [x] (false? x)) args)) 0)
     )))

(defcheck solution-40c4ed1d
  (fn [& args] (if (every? identity args) false (not (nil? (some identity args))))))

(defcheck solution-415b0279
  (fn [& a] ((complement #(or (every? true? (seq a)) (every? false? (seq a)))))))

(defcheck solution-41721bf1
  (fn [& c]
    (and (not (every? identity c))
         (not (nil? (some identity c))))))

(defcheck solution-41e37c72
  (fn [& xs]
    (not
      (or
       (nil? (some true? xs))
       (every? true? xs)))))

(defcheck solution-4276c2d2
  (fn [& l] (and (not-every? true? l) (or (some true? l) false))))

(defcheck solution-427a466e
  (fn half-truth [& args2]
    (true? (and
            (some true? args2)
            (not (every? true? args2))
            ))))

(defcheck solution-43389113
  not=)

(defcheck solution-43653436
  (fn [& ts]
    (= true(and
            (some true? ts)
            (some false? ts)))))

(defcheck solution-4367c3a5
  (fn [& args]
    (boolean
      (and (some identity args)
           (not (every? identity args))))))

(defcheck solution-437cc804
  (fn [& args]
    (> (count (partition-by identity args)) 1)))

(defcheck solution-438f369
  (fn [ & bs ] (and (not (every? true? bs)) (not (nil? (some true? bs))))))

(defcheck solution-44235dd8
  #(boolean (and (some true? %&) (some false? %&))))

(defcheck solution-44619514
  (fn [& args]
    (< 1 (count (partition-by identity args)))))

(defcheck solution-448e6e97
  (fn [& coll]
    (let [m (group-by identity coll)
          trues (get m true)
          c (count trues)]
      (if (and (pos? c) (not= c (count coll))) true false)
      )

    ))

(defcheck solution-45a1d1dc
  (fn [& B]
    (= (count (set B)) 2)))

(defcheck solution-466cb951
  (fn [ & bs]
    (true?
      (and
       (some true? bs)
       (some false? bs)))))

(defcheck solution-470e0776
  (fn [& rest] (= 2 (count (distinct rest)))))

(defcheck solution-47e725b9
  (fn [& a] (= (count (set a)) 2)))

(defcheck solution-483cd3c9
  (fn [& xs] (and (if (some identity xs) true false)
                  (not (every? identity xs)))))

(defcheck solution-485039b8
  (fn [& b]
    (and (not (nil? (some true? b))) (not (every? true? b)))
    ))

(defcheck solution-485eb030
  (fn [& bs] (< 0 (apply + (map #(if % 1 0) bs)) (count bs))))

(defcheck solution-488ed036
  (fn [& bools]
    (if
     (and
      (not-every? true? bools)
      (some true? bools)) true false)))

(defcheck solution-48be34b3
  (fn[& s] (= 2 (count (distinct s)))))

(defcheck solution-48f9d5a9
  (fn [& x]
    (let [truth (count (filter #(true? %) x))]
      (not (or (zero? truth) (= truth (count x)))))))

(defcheck solution-4930652c
  (fn [& args]
    (true?
      (and
       (not (every? true? args))
       (some true? args)))))

(defcheck solution-4983e20d
  (fn [& coll] (= 2 (count (set coll)))))

(defcheck solution-499f6ab2
  (fn [& bools]
    (and (boolean (some identity bools))
         (not-every? identity bools))))

(defcheck solution-49ae2c9b
  (fn [& c]
    (let [s (reduce (fn [a b] (if b (inc a) (dec a))) 0 c)]
      (not (= (if (< s 0) (- s) s) (count c))))))

(defcheck solution-49dac2d0
  (fn [& args]
    (= 2 (count(distinct args)))
    ))

(defcheck solution-4a0ba512
  (fn [& bs] (or (and (some identity bs) (some not bs)) false)))

(defcheck solution-4a42948f
  (fn [& bools]
    (clojure.set/subset? #{false true} (set bools))))

(defcheck solution-4a5b53f8
  (fn [& bools]
    (let [ands (every? identity bools)
          ors (some identity bools)]
      (boolean (and ors (not ands))))))

(defcheck solution-4a6646a9
  #(= 2 (count (set %&))))

(defcheck solution-4a95fedb
  #(boolean (and (some #{true} %&) (not (every? #{true} %&)))))

(defcheck solution-4acfbfc7
  (fn [& bools] (and (not-every? true? bools) (not-every? false? bools) )))

(defcheck solution-4ad2393b
  (fn [& args]
    (let [num-trues (count (filter true? args))]
      (not (or (zero? num-trues) (= (count args) num-trues))))))

(defcheck solution-4aeda971
  (fn [ & b]
    (not (or (apply = (cons true b))
             (apply = (cons false b)) ))))

(defcheck solution-4ba55c0a
  (fn [& args]
    (let [l (count (filter true? args))]
      (not (or (= l 0) (= l (count args))))
      )
    ))

(defcheck solution-4c2a5ce1
  (fn [& args]
    (not(or
         (every? #(= true %) args)
         (every? #(= false %) args)
         ))
    ))

(defcheck solution-4c517f5f
  (fn [& s] (and (reduce #(or % %2) s) (not (reduce #(and % %2) s)) )))

(defcheck solution-4c77a717
  (fn half-true
    [& s]
    (and (not(not-any? #(= true %) s)) (not-every? #(= true %) s))))

(defcheck solution-4d40a66a
  (fn [& bools]
    (and
     (contains? (set bools) true)
     (contains? (set bools) false))))

(defcheck solution-4d618626
  #(= 2 (count (frequencies %&))))

(defcheck solution-4d8ffe80
  (fn [& bs]
    (and (reduce #(or %1 %2) bs) (not (reduce #(and %1 %2) bs)))))

(defcheck solution-4dc9d84a
  (fn [& more] (= 2 (count (set more)))))

(defcheck solution-4dd2f615
  (fn [& bs]
    (and
     ((fn my-or [bs2]
        (reduce #(if (= [false false] [% %2]) false true) false bs2))
      bs)
     (not (
           (fn [bs2] (reduce #(if (= [true true] [% %2]) true false) true bs2))
           bs)))))

(defcheck solution-4dee59c1
  #(if (and (some #{true} %&) (not-every? #{true} %&)) true false))

(defcheck solution-4e2e18e3
  (fn [& args] (and (not (nil? (some true? args))) (not (nil? (some false? args))))))

(defcheck solution-4e6a12bf
  (fn [& val]
    (and (not (empty? (filter false? val))) (not (empty? (filter (comp not false?) val))))))

(defcheck solution-4f750996
  (fn [& x] (if (not (every? true? x)) (let [y (filter true? x)] (not (empty? y))) false)))

(defcheck solution-503f6f87
  (fn [& s] (true? (and (some true? s) (not-every? true? s)))))

(defcheck solution-510e5890
  (fn [& x] (and (not (reduce #(and %1 %2) x)) (reduce #(or %1 %2) x) )))

(defcheck solution-516c176f
  (fn half-true [& args]
    (and (not (every? true? args))
         (not (every? false? args)))))

(defcheck solution-52a88982
  (fn [& bs] (not (or (not-any? true? bs) (not-any? false? bs)))))

(defcheck solution-52b0dd25
  (fn [& args]
    (and (true? (some true? args))
         (not (every? true? args)))))

(defcheck solution-52c0cc9a
  (fn [& s] (= 2 (count (set s)))))

(defcheck solution-532d03a2
  (fn half-truth
    [& z]
    (and (not-every? true? z) (not-every? false? z))))

(defcheck solution-53566a6c
  (fn [& args] (and (not (every? not args)) (not-every? identity args))))

(defcheck solution-53ea445f
  #(and (not-every? (partial = false) %&)
        (not-every? (partial = true) %&)))

(defcheck solution-54a62f85
  (fn [& bools] (if (and
                     (not (every? identity bools))
                     (some identity bools))
                  true
                  false)))

(defcheck solution-5516ca09
  (fn [& xs]
    (if (every? identity xs) false
                             (if (some identity xs) true false))))

(defcheck solution-55549761
  (fn [& args] (if (and (some identity args) (not-every? identity args)) true false)))

(defcheck solution-556d7841
  (fn [& args]
    (not= nil (and (some identity args) (some not args)))))

(defcheck solution-558c29ba
  (fn [& bools] (not (nil? (and (some true? bools) (some false? bools))))))

(defcheck solution-55cbd606
  #(= (count (set %&)) 2))

(defcheck solution-56696e51
  (fn [& bs] (and (reduce #(or % %2) bs) (not (reduce #(and % %2) bs)))))

(defcheck solution-577aa5ad
  (fn [& b-vals]
    (not (empty? (rest (distinct b-vals))))))

(defcheck solution-5820572e
  (fn [& args] (= (count (set args)) 2)))

(defcheck solution-58affc01
  (fn [& bools] (and (true? (some true? bools)) (not (every? true? bools)))))

(defcheck solution-58caed98
  (fn [& bools]
    (loop [bools bools, res false]
      (if (= (count bools) 1)
        res
        (recur (rest bools) (not (= (first bools) (second bools))))))))

(defcheck solution-590428b7
  (fn [hd & tl]
    (not
      (or
       (every? true? (cons hd tl))
       (every? false? (cons hd tl))))))

(defcheck solution-592de2a3
  (fn [& args]
    (and (reduce #(or %1 %2) args)
         (not (reduce #(and %1 %2) args)))
    ))

(defcheck solution-593fae3c
  (fn [& cl] (let [nt (count (for [v cl :when (= true v)] v)), m (count cl)] (and (< 0 nt) (not= nt m) ))
    ))

(defcheck solution-5969e69e
  (fn half-truth? [& col]
    (and (not-every? true? col) (not-every? false? col))))

(defcheck solution-5986c92e
  (fn [& bs] (not
               (or (nil? (some true? bs)) (every? true? bs)))))

(defcheck solution-5a58209a
  (fn[& b](= #{true false}(set b))))

(defcheck solution-5a5efb18
  (fn [& args]
    (if (and (some identity args)
             (some #(not %) args))
      true
      false)))

(defcheck solution-5a69016c
  #(cond
     (every? true? %&) false
     (every? false? %&) false
     :else true))

(defcheck solution-5a791e38
  (fn [& x] (and (boolean (some true? x)) (not (every? true? x)))))

(defcheck solution-5aa22738
  (fn f [& args] (let [c (count (filter true? args))] (if (and (> c 0) (< c (count args))) true false))))

(defcheck solution-5b5b0f7b
  (fn [& p] (= 2 (count (distinct p)))))

(defcheck solution-5bb87e48
  (fn [& l] (if (some #(identity %) l) (not (every? #(identity %) l)) false)))

(defcheck solution-5bba3eaa
  #(-> (group-by identity %&) count (= 2)))

(defcheck solution-5c42710
  (fn [x1 & xs]
    (let [ys (conj xs x1) L (count (filter #(= true %) ys))]
      (if (or (= 0 L) (= (count ys) L) )
        false
        true
        ))))

(defcheck solution-5c770cb1
  #(and (not-every? identity %&) (not-every? not %&)))

(defcheck solution-5ca712ac
  (fn [& a]
    (and (true? (some identity a))
         (not (every? identity a)))))

(defcheck solution-5caeeb20
  (fn  [& args]
    (and (>= (count (filter true? args)) 1)
         (>= (count (filter false? args)) 1))))

(defcheck solution-5db16402
  (fn [b & bs]
    (if (= 1 (count (distinct (cons b bs))))
      false true)))

(defcheck solution-5dc2ec70
  (fn [& bs]
    (and (> (count bs) 1)
         (not (every? true? bs))
         (not (every? false? bs)))))

(defcheck solution-5e5eade9
  (fn [& s]
    (let [t (reduce #(or %1 %2) false s)
          f (reduce #(or %1 (not %2)) false s)]
      (and t f))))

(defcheck solution-5e5ed3f
  (fn [& bools]
    (if (every? true? bools)
      false
      (if (some true? bools)
        true
        false))))

(defcheck solution-5eb65f83
  (fn [q & s]
    (not (every? #(= % q) s))))

(defcheck solution-5fa4511b
  (fn
    [& args]
    (boolean (and (some true? args) (some false? args)))))

(defcheck solution-5fab99ca
  #(= 2 (count (group-by true? %&))))

(defcheck solution-5fd883b0
  (fn [& args]
    (and (not-every? identity args)
         (not-every? not      args))))

(defcheck solution-6019d721
  (fn [& args] (not (or (every? true? args) (every? false? args)))))

(defcheck solution-60ca272c
  (fn [& a] (not (or (every? true? a) (every? false? a)))))

(defcheck solution-61194961
  #(and
    (not-every? true? %&)
    (not-every? false? %&)))

(defcheck solution-6119b312
  (fn [& seq] (and
               (not-every? true? seq)
               (not (not-any? true? seq)))))

(defcheck solution-61992661
  (fn [& x] (and (reduce #(or % %2) x) (not(reduce #(and % %2) x)))))

(defcheck solution-62157b07
  (fn [& c]
    (and (not-every? true? c)
         (not-every? false? c)
         )
    ))

(defcheck solution-62760ef5
  (fn half-truth
    [& bols]
    (if (nil? (next bols))
      false
      (> (count (reduce #(clojure.set/union #{%2} %) #{}  bols)) 1))))

(defcheck solution-62b5556c
  (fn [& v] (and (not-every? identity v) (boolean (some identity v)))))

(defcheck solution-635d5d23
  (fn ulna [& bools]
    (and (not (reduce #(and %2 %1) bools))
         (reduce #(or %2 %1) bools))))

(defcheck solution-63818372
  #(if (= 2 (count (set %&))) true false))

(defcheck solution-638dfbe
  (fn [& args]
    (and (contains? (set args) false)
         (contains? (set args) true))))

(defcheck solution-638e27e3
  (fn [& args]
    (and
     (not (every? (partial = true) args))
     (not (every? (partial = false) args))
     )
    ))

(defcheck solution-63b29c46
  (fn [& xs]
    (cond
      ; if all false, false
      (empty? (filter true? xs)) false
      ; otherwise it's just the inversion of AND
      :else (not (every? identity xs))
      )
    ))

(defcheck solution-641dc68f
  (fn [& args] (if (and (some true? args) (not (every? true? args)))
                 true
                 false)))

(defcheck solution-64e05f90
  (fn half-truth
    [ & xs]
    (if (some true? xs)
      (if (every? true? xs)
        false
        true)
      false)))

(defcheck solution-655a8c02
  (fn prob83 [& args]
    (let [true-count (count (filter true? args))
          length (count args)]
      (and (> true-count 0) (< true-count length)))))

(defcheck solution-6603ff11
  (fn half-truth?
    [& coll]
    (= #{true false} (into #{} (distinct coll)))))

(defcheck solution-66923e25
  (fn [& bools] (boolean (and (some true? bools) (not-every? true? bools)))))

(defcheck solution-66b19c57
  (fn this [& p]
    (let [has-true-f (fn [l _has-true]
                       (cond (= (count l) 0) _has-true
                             :else (recur (rest l) (or _has-true (true? (first l))))))
          has-false-f (fn [l _has-false]
                        (cond (= (count l) 0) _has-false
                              :else (recur (rest l) (or _has-false (false? (first l))))))]
      (and (has-true-f p false) (has-false-f p false)))))

(defcheck solution-66c75f7a
  (fn [& alist]
    (not (or (every? true? alist) (every? false? alist)))))

(defcheck solution-66e8f30d
  (fn [& s]
    (and (if (some true? s) true false) (not-every? true? s))))

(defcheck solution-677fe36d
  (fn [& bools] (not= 1 (count (distinct bools)))))

(defcheck solution-68352ef5
  (fn xor [& bools]
    (let [binned (group-by identity bools)
          total  (count bools)]
      (not (or (= total (count (binned true)))
               (= total (count (binned false))))))))

(defcheck solution-6883ec4c
  (fn [& ts]
    (let [n1 (count (filter identity ts))
          n2 (count ts)]
      (cond
        (= n1 n2) false
        (> n1 0) true
        :else false))))

(defcheck solution-68e94720
  (fn [& s] (boolean (and (some true? s) (some false? s)))))

(defcheck solution-68eaddc3
  #(boolean (and (some identity %&) (not (every? identity %&)))))

(defcheck solution-69239cbf
  (fn [& xs]
    (let [
          and* (fn and* [xs]
                 (reduce #(and %1 %2) true xs))
          or* (fn or* [xs]
                (reduce #(or %1 %2) false xs))]
      (and (= true (or* xs))
           (= false (and* xs))))))

(defcheck solution-6959c5da
  (fn [& args] (< 1 (count (set args)))))

(defcheck solution-69cf5d37
  (fn [& x] (and (boolean (some true? x)) (not-every? true? x))))

(defcheck solution-69eac0a0
  (fn [& args] ((complement nil?) (and (some true? args) (some false? args)))))

(defcheck solution-6a1be3b
  (fn [& v] (= #{false true} (set v))))

(defcheck solution-6b3750b7
  (fn [& bools]
    (= (set bools) #{true false})))

(defcheck solution-6b5dcd59
  (fn [ & ar]
    (if (and (some true? ar) (some false? ar))
      true
      false)))

(defcheck solution-6b792a26
  (fn [& c] (and (not-every? false? c) (not-every? true? c))))

(defcheck solution-6bc092f0
  (fn halftruth [& x] (and (true? (some true? x))(not-every? true? x))))

(defcheck solution-6c41af73
  (fn [& coll] (and (not-every? true? coll) (not-every? false? coll))))

(defcheck solution-6c9b78e3
  (fn [& xs] (and (not-every? true? xs)
                  (not-every? false? xs))))

(defcheck solution-6cbb1d6c
  (fn [& args]
    (let [s (set args)]
      (if
       (= s #{ true })
        false
        (contains? s true)))))

(defcheck solution-6cee0bce
  (fn [& b] (= (set b) #{true false})))

(defcheck solution-6d5a3642
  (fn [& bs] (= 2 (count (distinct bs)))))

(defcheck solution-6db5206b
  (fn [& bools]
    (and (not (every? identity bools))
         (not (nil? (some identity bools))))))

(defcheck solution-6e10ef35
  (fn[& args](and (not-every? #(false? %) args) (not-every? #(true? %) args))))

(defcheck solution-6e15cd56
  #(= 2 (count (distinct %&))))

(defcheck solution-6e1b9e54
  (fn onetrue [& bs]
    (and
     (> (count (filter #(= true %1) bs)) 0)
     (> (count bs) (count (filter #(= true %1) bs)))
     )
    ))

(defcheck solution-6e432256
  #(and (not (every? not %&)) (not (every? identity %&))))

(defcheck solution-6e4d03f1
  not=)

(defcheck solution-6ea46531
  not=)

(defcheck solution-6f1a5597
  (fn [& args] (boolean (and (not-every? identity args) (some identity args)))))

(defcheck solution-6f51d71b
  (fn [& args]
    (let
     [
      varor (fn vo
              [preds]
              (if
               (= 1 (count preds))
                (first preds)
                (or (first preds) (vo (rest preds)))
                )
              ),
      varand (fn va
               [preds]
               (if
                (= 1 (count preds))
                 (first preds)
                 (and (first preds) (va (rest preds)))
                 )
               )
      ]
      (and (varor args) (not (varand args)))
      )
    ))

(defcheck solution-6f9f3075
  (fn [& s]
    (if (reduce #(and %1 %2) true s)
      false
      (reduce #(or %1 %2) false s))))

(defcheck solution-6fba0724
  (fn not-all
    [& b]
    (if (every? identity b)
      false
      (if-not (some identity b)
        false
        true))))

(defcheck solution-6fc220a3
  #(true? (and (some false? %&) (some true? %&) )))

(defcheck solution-70d56635
  #(and (boolean (some identity %&)) (not-every? identity %&)))

(defcheck solution-72120dfb
  (fn[& x](= (count (set x)) 2)))

(defcheck solution-72165c5f
  (fn [& rst]
    (if (and (some identity rst) (not (every? identity rst)))
      true
      false)))

(defcheck solution-721deade
  (fn half-truth [& s]
    (and (not (every? true? s)) (not (not-any? true? s)))))

(defcheck solution-72402e55
  (fn
    ([x] false)
    ([x & more]
     (let [l (concat (list x) more)]
       (cond
         (every? true? l) false
         (some true? l) true
         :else false)))))

(defcheck solution-72bd3a56
  (fn [& args]
    (if (and (some true? args) (not (every? true? args)))
      true false)))

(defcheck solution-73098671
  (fn [& xs]
    (and
     (boolean (some true? xs))
     (not (every? true? xs))
     )
    ))

(defcheck solution-735ad308
  (fn some- [& bools]
    "83. Write a function which takes a variable number of
    booleans. Your function should return true if some of the parameters
    are true, but not all of the parameters are true. Otherwise your
    function should return false."
    (and (not-every? identity bools) (not-every? (complement identity) bools))))

(defcheck solution-7363e960
  (comp #(and (true? (some identity %)) (not (every? identity %))) vector))

(defcheck solution-73e26f56
  #(every? true? [(some true? %&) (not-every? true? %&)]))

(defcheck solution-73e571bd
  (fn peu [& x] (and (reduce #(or %1 %2) false x) (false? (reduce #(and %1 %2) true x)))))

(defcheck solution-747a4e01
  (fn [& x] (and (not-every? true? x) (not-every? false? x))))

(defcheck solution-751e1fa9
  (fn [& rest] (if (and (some true? rest) (some false? rest)) true false)))

(defcheck solution-753143f
  (fn [& args]
    (let [ct (count args)
          tct (count (filter #(= % true) args))]
      (and (> tct 0) (< tct ct)))))

(defcheck solution-754412c8
  (fn [& s]
    (loop [trues 0
           falses 0
           remain s]
      (if (and (> trues 0) (> falses 0)) true
                                         (if (empty? remain) false
                                                             (let [nxt (first remain)]
                                                               (if nxt
                                                                 (recur (inc trues) falses (rest remain))
                                                                 (recur trues (inc falses) (rest remain))
                                                                 )))))))

(defcheck solution-754f5215
  (fn [& oth]
    (if (and (some true? oth) (some false? oth)) true false)))

(defcheck solution-76353884
  #(nil? (reduce % %&)) #(if (= % %2) % nil))

(defcheck solution-764416c3
  #(boolean (and (some true? %&) (not (every? true? %&)))))

(defcheck solution-7651c788
  (fn [& args] (> (count (distinct args)) 1)))

(defcheck solution-76f7708d
  (fn bs [& bools]
    (not (or (every? true? bools) (not-any? true? bools)))))

(defcheck solution-7712bc5
  (fn [& xs] (and (not-every? #(= false %) xs)
                  (not-every? #(= true %) xs))))

(defcheck solution-772d5df5
  #(and (not= nil (some true? %&)) (not-every? true? %&)))

(defcheck solution-7754a26d
  (fn [& args]
    (boolean (and (some boolean args)
                  (not= (set args) #{true})))))

(defcheck solution-77c4b1a2
  (fn [& xs] (and (reduce #(or %1 %2) false xs)
                  (not (apply = xs)))))

(defcheck solution-77eea351
  (fn [& args] (and (not-every? identity args) (not-every? not args))))

(defcheck solution-7890db2e
  (fn xor [& args]
    (and (not (empty? (filter true? args)))
         (not (empty? (filter false? args))))))

(defcheck solution-789b1223
  (fn ht [& v]
    (and (not-every? false? v) (not-every? true? v))))

(defcheck solution-78c0c7d4
  (fn [& bools] (if (and (some #(= true %) bools)
                         (not (every? #(= true %) bools)))
                  true
                  false)))

(defcheck solution-79276d00
  (fn [& arg] (= 2 (count (distinct arg)))))

(defcheck solution-7947f792
  (fn [& args] (let [ss (set args)] (and (contains? ss true) (contains? ss false)))))

(defcheck solution-798398c5
  (fn[& x] (= 2 (count (distinct (seq x))))))

(defcheck solution-79a780cb
  (fn [& bs]
    (boolean (and (not (every? identity bs))
                  (some identity bs)))))

(defcheck solution-79e67f70
  (fn [& col](not
               (empty?
                 (rest (set col) )
                 )
               )
    ))

(defcheck solution-7b09dcf2
  (fn [& xs]
    (and (reduce #(or %1 %2) false xs)
         (not (reduce #(and %1 %2) true xs)))))

(defcheck solution-7b29ac80
  (fn [b & bs]
    (let [true-count (count (filter true? (conj bs b)))
          arg-count (inc (count bs))]
      (if (= true-count 0)
        false
        (if (= true-count arg-count)
          false
          true)))))

(defcheck solution-7b8b16b6
  (fn [ & r]
    (not (or (every? false? r) (every? true? r)))))

(defcheck solution-7b9e2df4
  (fn [& bs]
    (if (true? (every? identity bs))
      false
      (not (nil? (some identity bs)))
      )
    ))

(defcheck solution-7bc265c4
  (fn [& bs]
    (= #{true false} (set bs))))

(defcheck solution-7bd23189
  (fn half-truth
    [& xs]
    (if
     (and (some true? xs) (some false? xs)) true
                                            false)))

(defcheck solution-7c449abd
  (fn  [& bs]
    (cond
      (true? (reduce  #(and %1 %2) bs)) false
      (false? (reduce #(or %1 %2) bs)) false
      :else true)))

(defcheck solution-7c516ab1
  (fn [& args] (and (not-every? true? args) (not-every? false? args))))

(defcheck solution-7c89d55d
  (comp (partial = 2) count set vector))

(defcheck solution-7c9a4087
  (fn ht [& x]
    (and (not-every? true? x) (not-every? false? x))
    ))

(defcheck solution-7cb2dfff
  (fn [ & pms ] (if (apply = pms) false (if (some true? pms) true
                                                             false))))

(defcheck solution-7d04bf41
  (fn f [& c] (and (true? (some true? c)) (not-every? true? c ))))

(defcheck solution-7dcfbf75
  (fn [x & xs] (= (count (into #{x} xs)) 2)))

(defcheck solution-7e0d83a7
  (fn [bool & more]
    (let [bool-list (conj more bool)]
      (cond (and
             (not-every? #{true} bool-list)
             (some #{true} bool-list))
            true
            :else false))))

(defcheck solution-7e5009f0
  (fn [a & b] (= (count (set (cons a b))) 2)))

(defcheck solution-7e8019e6
  (fn [& bools]
    (boolean
      (and
       (some identity bools)
       (not (every? identity bools))))))

(defcheck solution-7ecc9650
  (fn [& xs]
    (loop [xs xs
           seen-true? false
           seen-false? false]
      (cond (and seen-true? seen-false?) true
            (empty? xs) false
            (first xs) (recur (rest xs) true seen-false?)
            :else (recur (rest xs) seen-true? true)))))

(defcheck solution-7f7eb5f4
  (fn [& args]
    (cond
      (every? true? args) false
      (every? false? args) false
      :else true)))

(defcheck solution-7f920214
  (fn !
    ([x & xs]
     (if (sequential? x)
       (if (empty? x)
         false
         (let [t (first xs)
               f (first x)
               r (! (rest x) t)]
           (if (= t 0)
             (or (not f) r)
             (or f r)
             )))
       (let [l (concat [x] xs)]
         (and (! l 0) (! l 1))
         )))))

(defcheck solution-7fdd5048
  (fn [& args] (let [some (reduce (fn [a b] (or a b)) false args)
                     all (reduce (fn [a b] (and a b)) true args)]
                 (and some (not all)))))

(defcheck solution-8018d85a
  (fn [& bools] (== 2 (count (set bools)))))

(defcheck solution-802a4fae
  #(not (or (every? false? %&)
            (every? true? %&))))

(defcheck solution-80504436
  #(and (boolean(some true? %&)) (boolean (some false? %&))))

(defcheck solution-80b1a863
  (fn [& args] (let [s (set args)] (= s #{false true}))))

(defcheck solution-80b8da49
  (fn [& args]
    (and (not (reduce #(and %1 %2) args)) (reduce #(or %1 %2) args))))

(defcheck solution-80d2bbc5
  (fn [ & xs]
    (let [con (partial contains? (set xs))]
      (and (con true)
           (con false)))))

(defcheck solution-810e04a
  (fn [& a] (and (boolean (some identity a)) (not-every? identity a))))

(defcheck solution-81256933
  (fn [& a] (boolean (and (some true? a) (some false? a)))))

(defcheck solution-81258b82
  (fn [& coll] (= 2 (count (distinct coll)))))

(defcheck solution-812661d6
  #(< 1 (count(distinct %&))))

(defcheck solution-8152dcc7
  (fn [& l]
    (and (reduce #(or % %2) (map not l))
         (reduce #(or % %2) l))))

(defcheck solution-817f9c9e
  (fn [& rest] (boolean (and
                         (some true? rest)
                         (not (every? true? rest))))))

(defcheck solution-82b4ea7b
  (fn [& args] (and (not (not-any? true? args)) (not (every? true? args)))))

(defcheck solution-82e4523b
  (fn [& bools]
    (<= 1 (apply + (map #(if % 1 0) bools)) (dec (count bools)))))

(defcheck solution-832d46df
  (fn some-true [& args]
    (let [c (count (filter identity args))]
      (and (> c 0) (< c (count args)))
      )))

(defcheck solution-8347d02e
  (fn [& args] (boolean (and (some false? args) (some true? args)))))

(defcheck solution-83b19687
  (fn [& args]
    (boolean (and (some true? args) (not (every? true? args))))))

(defcheck solution-8400744
  (fn [& bools] (boolean (and (some true? bools) (some false? bools)))))

(defcheck solution-844d1281
  (fn [& xs] (and (not-every? (partial = true) xs) ( (comp boolean some) (partial = true) xs) ) ))

(defcheck solution-84c98b39
  (fn half-truth [& coll]
    (not (or (every? false? coll) (every? true? coll)))))

(defcheck solution-8561b289
  (fn [& a]
    (true? (and (some true? a)
                (some false? a)))))

(defcheck solution-856bfded
  #(cond
     (every? identity %&) false
     (some identity %&) true
     :else false))

(defcheck solution-857892de
  (fn [& bools]
    (and (not-every? true? bools) (not-every? false? bools))))

(defcheck solution-85a44213
  (fn [& args]
    (boolean (and (some true? args) (some false? args)))
    ))

(defcheck solution-85acf9a0
  #(and (not-every? (partial = false) %&) (not-every? (partial = true) %&) ))

(defcheck solution-85bec7f8
  (fn [& args]
    (if (every? true? args)
      false
      (reduce #(or %1 %2) false args))))

(defcheck solution-85e2a1e2
  (fn [& args] (if (= 1 (count (partition-by #(= % false) args))) false true)))

(defcheck solution-861de033
  (fn [& x] (= (count (into #{} x)) 2)))

(defcheck solution-863e738a
  (fn [& s]
    (and
     (not (reduce #(and % %2) true s))
     (reduce #(or % %2) false s))))

(defcheck solution-86801a4c
  (fn [& bools]
    (not (or (every? identity bools)
             (every? not bools)))))

(defcheck solution-86bc98ca
  #(< 0 (mod (count(filter true? %&))
          (count %&))))

(defcheck solution-87350676
  (fn [& args]
    (if (every? true? args) false
                            (true? (some true? args)))
    ))

(defcheck solution-873d32c8
  (fn [& c]
    (cond
      (every? true? c) false
      (every? false? c) false
      (not-any? true? c) false
      :else true)))

(defcheck solution-877f5c19
  (fn [& bools]
    (if (apply = bools) false true)))

(defcheck solution-87c69bd
  (fn [& a] (true? (and (some true? a)(some false? a)))))

(defcheck solution-87d7b4df
  (fn [& bools]
    (if (and (some identity bools) (not-every? identity bools)) true false)
    ))

(defcheck solution-87edc7a
  (fn [& args]
    (and (reduce #(or %1 %2) false args)
         (not (reduce #(and %1 %2) true args)))))

(defcheck solution-888e9878
  (fn [& col] (= 2 (count (set col)))))

(defcheck solution-88eba813
  (fn [& args] (= (into #{} args) #{true false})))

(defcheck solution-88fab8d0
  (fn [& all]
    (and (reduce #(or %1 %2) all)
         (not (apply = all)))))

(defcheck solution-89682102
  (comp (partial every? identity)
        (juxt (partial some true?)
          (partial not-every? true?))
        list))

(defcheck solution-899cd0ea
  (fn some? [& args]
    (and
     (not (every? true? args))
     (not (every? false? args)))))

(defcheck solution-89b4ce6d
  #(or (and (some identity %&) (not-every? identity %&)) false))

(defcheck solution-89bbfb22
  (fn some-true? [& bs]
    (loop [bs bs acc #{}]
      (if (= (count acc) 2)
        true
        (if (empty? bs)
          false
          (recur (rest bs) (conj acc (first bs))))))))

(defcheck solution-8a7e958e
  (fn [& s]( if-not(reduce #(and %1 %2) (seq s)) (reduce #(or %1 %2) (seq s)) false)))

(defcheck solution-8ae0992
  (fn [& vs] (not (or (nil? (some identity vs)) (every? identity vs)))))

(defcheck solution-8b0d8026
  (fn half-truth [& bools] (= 2 (count (set bools)))))

(defcheck solution-8b76e86f
  (fn [& xs] (if (and (some true? xs) (some false? xs)) true false)))

(defcheck solution-8bf67793
  (fn [& x] (= 2 (count (set (list* x))))))

(defcheck solution-8bfee56f
  (fn [& bools]
    (= true (and (some true? bools)
                 (some false? bools)))))

(defcheck solution-8c2cabe2
  (fn [& bools] (apply not= bools)))

(defcheck solution-8c38fa70
  #(let [any? (complement not-any?)]
     (and
      (any? true? %&)
      (any? false? %&))))

(defcheck solution-8c4748b9
  (fn half-truth [& xs]
    (let [a (not-every? identity xs)
          b (not (not (some identity xs)))]
      (and a b))))

(defcheck solution-8cbeced9
  (fn ht [& args]
    (boolean (and (not-every? identity args) (some identity args)))))

(defcheck solution-8d026ff6
  (fn [& coll]
    (and (not (every? true? coll)) (not (every? false? coll)))))

(defcheck solution-8db49b28
  (fn half-truth [& preds]
    (letfn [(convert [bool] (if bool 1 0))]
      (let [mid (map convert preds)
            muls (reduce * mid)
            adds (reduce + mid)]
        (and (= muls 0) (> adds 0))))))

(defcheck solution-8dbd4191
  #(= true (some false? %&) (some true? %&)))

(defcheck solution-8df14b21
  (fn [& a] (every? true? (map #(some % a) [true? false?]))))

(defcheck solution-8e9744b3
  #(< 0 (count (filter true? %&)) (count %&)))

(defcheck solution-8eb5b508
  #(or (and (some true? %&) (not (every? true? %&))) false))

(defcheck solution-8ed8ecc6
  (fn [& bs] (boolean (and (some boolean bs) (not (every? identity bs))))))

(defcheck solution-8f1dd166
  (fn sometrue [& bools]
    (= (count (set bools)) 2)))

(defcheck solution-8f989358
  (fn [& args]
    (if (every? true? args)
      false
      (boolean (some true? args)))))

(defcheck solution-8fa87e21
  (fn [& args] (= 2 (count (into #{} args)))))

(defcheck solution-8fb98759
  #(and (not (not-any? not %&)) (not (nil? (some identity %&)))))

(defcheck solution-9050e655
  (fn [& a] (cond (every? true? a) false (every? false? a) false (some true? a) true)))

(defcheck solution-905bce4d
  (fn [& args]
    (or (and (some identity args)
             (not (apply = args)))
        false)))

(defcheck solution-90601aff
  #(and (reduce (fn [a b] (if a true b)) %&) (not (reduce (fn [a b] (if b true false)) %&))))

(defcheck solution-91278d98
  (fn [& s] (= true (and (some true? s) (not (every? true? s))))))

(defcheck solution-91435867
  (fn [& more] (if (and (some true? more) (not-every? true? more)) true false)))

(defcheck solution-91ae41fa
  (fn f [& bools]
    (true? (and (some true? bools) (some false? bools)))))

(defcheck solution-91cbdfb3
  (fn [& xs] (if (or (empty? (filter true? xs)) (empty? (filter false? xs))) false true)))

(defcheck solution-9235ac69
  (fn [& args]
    (if (and (some #(= true %) args) (some #(= false %) args))
      true false)))

(defcheck solution-9384a928
  #(or (and (some identity %&) (some not %&)) false))

(defcheck solution-93c30645
  (fn half-truth [& args] (if (and (some #(= true %) args) (some #(= false %) args)) true false)))

(defcheck solution-9408baf8
  (fn [& b]
    (and (reduce #(or %1 %2) b) (not (reduce #(and %1 %2) b)))))

(defcheck solution-9455d315
  (fn [& x]
    (let [len (count x)
          trues (reduce (fn [a b] (if b (inc a) a)) 0 x)]
      (and (not= len trues) (not (zero? trues))))))

(defcheck solution-94a1731
  (fn [& args] (boolean (and
                         (some #(= true %) args)
                         (some #(= false %) args)))))

(defcheck solution-94df8180
  (fn f [& params]
    (and (true? (some true? params)) (not-every? true? params))))

(defcheck solution-951b612d
  (fn weird-or [& args]
    (and (not= nil (some identity args))
         (not (every? identity args)))))

(defcheck solution-95925400
  (fn [& bools]
    (if (and (some identity bools)
             (not-every? identity bools))
      true
      false)))

(defcheck solution-95e21491
  (fn [& bools]
    (if (= 1 (count bools))
      false
      (and (not (every? identity bools))
           (boolean (some identity bools))))))

(defcheck solution-95e30fb0
  (fn [& args]
    (true? (and (some false? args)
                (some true? args)))))

(defcheck solution-95ec56ab
  (fn [& args] (boolean (and (some false? args) (some true? args))) ))

(defcheck solution-968bc5c9
  (fn [& b] (= [true false] [(some true? b) (every? true? b)])))

(defcheck solution-969b34f6
  (fn [& lst] (and (or (some true? lst) false) (not-every? true? lst))))

(defcheck solution-96bec652
  (fn [& x] (= 2 (count (set x)))))

(defcheck solution-96df02c0
  (fn [& bools]
    (and
     (true? (some #(= true %) bools))
     (not (every? #(= true %) bools))
     )
    ))

(defcheck solution-96e1afb7
  (fn [& args] (if (some true? args)
                 (true?(some false? args)) false)))

(defcheck solution-976553ba
  (fn [& args]
    (let [seen-bools (set args)]
      (and (contains? seen-bools true) (contains? seen-bools false)))))

(defcheck solution-97d072c8
  (fn some-true? [& cols]
    (= (set cols) #{true false})))

(defcheck solution-97e927f2
  #(and (> (.indexOf %& true) -1) (> (.indexOf %& false) -1)))

(defcheck solution-9805338e
  #(and (not (every? true? %&)) (not (every? false? %&))))

(defcheck solution-9853ce06
  (fn [& r]
    (and (contains? (set r) true)
         (contains? (set r) false))))

(defcheck solution-98dbb47e
  (fn [& x] (> (count (distinct x)) 1)))

(defcheck solution-98f439d0
  #(and (not-every? true? %&) (not-every? false? %&)))

(defcheck solution-991491b3
  (fn [& col](every? identity ((juxt some not-every?) identity col))))

(defcheck solution-9a0e7f4b
  #(not-any? nil? (map (set %&) [true false])))

(defcheck solution-9b83934
  (fn [& b] (= true (and (some #(= true %) b) (some #(= false %) b)))))

(defcheck solution-9ba1a1b4
  (fn [& z]
    (if (and (some true? z)
             (some false? z))
      true
      false)))

(defcheck solution-9c4ca9c5
  (fn [ & xs]
    (let [t (count (filter true? xs))]
      (if (> t 0)
        (not= t (count xs))
        false
        )
      )))

(defcheck solution-9c56a47a
  (fn [& args] (let [s (sort args)]
                 (if (not= (first s) (last s)) true false))))

(defcheck solution-9db6e747
  (fn [& args] (let [groups (group-by true? args)] (if (and (groups true) (groups false)) true false))))

(defcheck solution-9e192c99
  (fn [& s]
    (loop [s_ s]
      (cond
        (not s_) false
        (not (next s_)) false
        (not= (first s_) (first (next s_))) true
        :else (recur (next s_))))))

(defcheck solution-9e2b63ac
  #(boolean (and (some true?  %&)
                 (some false? %&))))

(defcheck solution-9e375ff0
  (fn [& coll]
    (and (boolean (some true? coll)) (boolean (some false? coll)))))

(defcheck solution-9e63e65f
  (fn [& args]
    (= 2 (count (keys (group-by identity args))))))

(defcheck solution-9eb53112
  (fn [& xs] (= #{true false} (into #{} xs))))

(defcheck solution-9ee31153
  (fn [& bs] (= 2 (count (set bs)))))

(defcheck solution-9eebc889
  (fn [& x]
    (and (not-every? true? x) (not-every? false? x))))

(defcheck solution-9f0a6f0d
  (fn sortof-xor
    [& bools]
    (not (or (every? true? bools) (every? false? bools)))))

(defcheck solution-9fb98681
  (fn [& p]
    (= #{true false} (set p))))

(defcheck solution-9fc086b9
  (fn [& xs] ((every-pred (partial some identity) (partial not-every? identity)) xs)))

(defcheck solution-9fc67e95
  (fn [& bs]
    (and (-> (some identity bs) not not)
         (not-every? identity bs))))

(defcheck solution-a02a46ca
  (fn [& b]
    (and (if (some identity b) true false) (not-every? identity b))))

(defcheck solution-a0bb17fb
  (fn [& bools]
    (boolean (and (some true? bools) (not-every? true? bools)))))

(defcheck solution-a0cd53c2
  (fn [& xv]
    (= 2 (count (group-by true? xv)))))

(defcheck solution-a1509e17
  (fn [& booleans]
    (= (set booleans) #{true false})))

(defcheck solution-a15ddc70
  (fn [& bools]
    (if (and (seq (filter identity bools)) (seq (filter not bools))) true false)))

(defcheck solution-a18be5c9
  (fn [x & y]
    (let [z (conj y x)]
      (if (every? true? z)
        false
        (if (some true? z)
          true
          false)))))

(defcheck solution-a20004dc
  (fn[& r](= 2 (count (distinct r)))))

(defcheck solution-a231a2f1
  (fn [h & t]
    (loop [all-true (if h (= (inc (count (filter true? t))) (inc (count t))) false)
           frst h
           rst t
           rslt false]
      (if (empty? rst)
        (if all-true false rslt)
        (recur all-true (first rst) (rest rst) (or frst rslt))))))

(defcheck solution-a23aea44
  (fn [& bs] (if (and (some true? bs) (some false? bs)) true false)))

(defcheck solution-a2b8911d
  (fn [& a]
    (and
     (not-every? (fn [x] (not x)) a)
     (not-every? (fn [x] x) a))))

(defcheck solution-a2c8b6b6
  (fn [& args] (and (not-every? false? args) (not-every? true? args))))

(defcheck solution-a341f5fc
  (fn [& args]
    (and
     (or (some identity args) false)
     (not (every? identity args)))))

(defcheck solution-a365bb9d
  (fn half-truth [& rest]
    (and
     (not (empty? (drop-while #(= %1 true) rest)))
     (not (empty? (drop-while #(= %1 false) rest))))))

(defcheck solution-a37d71cf
  #(= (count (into #{} %&)) 2))

(defcheck solution-a3c13cfc
  (fn [& args]
    (= true
      (and
       (some true? args)
       (some false? args)))))

(defcheck solution-a59be58f
  (fn [& s] (let [f #(not (nil? ((set s) %)))]
              (and (f true) (f false)))))

(defcheck solution-a5e6a1ae
  (fn [& p]
    (not (apply = p))))

(defcheck solution-a5e852
  (fn [& xs]
    (and
     (not-every? true? xs)
     (not (nil? (some true? xs)))
     )
    ))

(defcheck solution-a62780a2
  (fn [ & x]
    (and
     (> (count (filter #(= true %) x)) 0)
     (> (count (filter #(= false %) x)) 0))))

(defcheck solution-a69b3778
  #(and (or (some true? %&) false) (not-every? true? %&)))

(defcheck solution-a6ddfd39
  (fn some? [& s]
    (and (not (not-any? true? s)) (not-every? true? s))))

(defcheck solution-a72efca8
  (fn m [& coll]
    (cond
      (every? true? coll) false
      (every? false? coll) false
      :else true)))

(defcheck solution-a78ca30b
  (fn [& args]
    (loop [has-true false
           has-false false
           rem args]
      (if (and has-true has-false)
        true
        (if (empty? rem)
          false
          (let [this-one (first rem)]
            (recur (or has-true (true? this-one))
              (or has-false (false? this-one))
              (rest rem))))))))

(defcheck solution-a8bbbd7f
  (fn [& x] (and (not (reduce #(and %1 %2) x)) (reduce #(or %1 %2) x))))

(defcheck solution-a8c766e2
  #(and (contains? (set %&) true) (contains? (set %&) false)))

(defcheck solution-a8d19880
  (fn [& b] (and (or (some true? b) false) (not-every? true? b))))

(defcheck solution-a8ec2ea3
  (fn [& xs]
    (and
     (not (every? identity xs))
     (boolean (some identity xs)))))

(defcheck solution-a951eb5a
  (fn [& bools] (if (and (some true? bools) (not (every? true? bools)))
                  true
                  false)))

(defcheck solution-aa02ee19
  (fn [b & bs]
    (true?
      (#(and (some true? %)
             (some false? %))
       (conj bs b)))))

(defcheck solution-aa4464d2
  (fn [& bs] (and (reduce #(or % %2) bs)
                  (not (reduce #(and % %2) bs)))))

(defcheck solution-aa7aa3d5
  (fn [& b] (= [true false] [(reduce #(or % %2) b) (reduce #(and % %2) b)])))

(defcheck solution-aa93574b
  (fn some-not-all [& args]
    (if (and (some true? args)
             (some false? args)) true false)))

(defcheck solution-ab37ed2c
  #(if (not-any? false? %&) false (if (some true? %&) true false) ))

(defcheck solution-abcc1bbe
  (fn [& xs]
    (let [c (reduce #(if %2 (inc %1) (dec %1)) 0 xs)]
      (and (> c (- (count xs))) (< c (count xs))))))

(defcheck solution-ac03a315
  (fn [& truths] (and (not-every? identity truths) (or (some identity truths) false))))

(defcheck solution-ac18d86f
  (fn [& params]
    (true? (and (some true? params)
                (not (every? true? params))))))

(defcheck solution-acaffb17
  #(if (and(some true? %&) (some false? %&)) true false))

(defcheck solution-ad217125
  (fn [& args]
    (if (nil?
          (and
           (some true? args)
           (some false? args)))
      false
      true)))

(defcheck solution-ad34cf2c
  (fn [ & args] (true? (and (some true? args) (not-every? true? args)))))

(defcheck solution-ad8843ed
  #(boolean
     (and (not (every? identity %&))
          (some identity %&))))

(defcheck solution-ade3f9ec
  (fn [& xs]
    (if (some true? xs)
      (not-every? true? xs)
      false)))

(defcheck solution-ae84cfeb
  (fn [& xs] (and (not-every? #(= true %) xs) (not-every? #(= false %) xs))))

(defcheck solution-ae923da8
  (fn half-truth [& args] (and (not (every? identity args)) (contains? (set args) true))))

(defcheck solution-af1a4dce
  (fn [& coll]
    (and (false? (reduce #(and %1 %2) coll))
         (true? (reduce #(or %1 %2) coll)))))

(defcheck solution-af6d5215
  (fn [& args] (= 2 (count (set args)))))

(defcheck solution-afe1a41d
  (fn [& args]

    (let [t (if (nil? (get (frequencies args) true)) 0 (get (frequencies args) true))
          f (if (nil? (get (frequencies args) false)) 0 (get (frequencies args) false))
          ]
      (cond
        (and (>= t 1) (>= f 1)) true
        :else false
        )
      )
    ))

(defcheck solution-b001d15f
  (fn [& coll] (true? (and (some true? coll) (some false? coll)))))

(defcheck solution-b07bfe11
  (fn [& bools]
    (let [any #(some identity %)
          all #(every? identity %)]
      (boolean (and (any bools) (not (all bools)))))))

(defcheck solution-b0a70d78
  #(-> %& set count (> 1)))

(defcheck solution-b0bdadbe
  (fn [& bools] (and (boolean (some true? bools)) (not-every? true? bools))))

(defcheck solution-b10b8cfe
  (fn [& x] (true? (and (some true? x)
                        (false? (reduce #(and %1 %2) x ))))))

(defcheck solution-b123bb9f
  (fn [& bs] (boolean (and (some identity bs) (not (every? identity bs))))))

(defcheck solution-b12c6a7
  (fn [& args] (-> args distinct count (= 2))))

(defcheck solution-b191ba5d
  (fn [& args]
    (if (and (some true? args) (not-every? true? args))
      true
      false)))

(defcheck solution-b1aaa49b
  #(not-every? (fn [x] (= x %)) %&))

(defcheck solution-b1dbfab7
  (fn [& args]
    (cond (every? true? args) false
          (some true? args) true
          :else false)))

(defcheck solution-b1fc290c
  (fn [& x]
    (let [ctruex (count (filter true? x))
          cx (count x)]
      (cond
        (= ctruex cx) false
        (zero? ctruex) false
        :else true))))

(defcheck solution-b20a63b9
  (fn [& bools]
    (cond
      (every? true? bools) false
      (every? false? bools) false
      :else true)))

(defcheck solution-b2147512
  (fn [& args]
    (if (and
         (some true? args)
         (some false? args))
      true false)))

(defcheck solution-b225e4bf
  (fn [ & args]
    (and (reduce (fn [a b] (or a b)) args) (not (reduce (fn [a b] (and a b)) args)))))

(defcheck solution-b24e48bf
  (fn [& coll]
    (if (and (some true? coll)
             (some false? coll))
      true
      false)))

(defcheck solution-b273d2d1
  (fn [& more]
    (-> (remove true? more)
      (#(if (empty? %)
          false
          (if (empty? (remove false? more))
            false
            true))))))

(defcheck solution-b2866762
  (fn [& a] (and
             (not-every? true? a)
             ((complement not-any?) true? a))))

(defcheck solution-b2e582d9
  (fn [& args]
    (true?
      (and (some true? args)
           (not-every? true? args)))))

(defcheck solution-b374ca41
  (fn [& c] (and ((comp boolean some) true? c) (not-every? true? c))))

(defcheck solution-b3e40bc7
  (fn [& coll] (not (or (every? true? coll) (every? false? coll)))))

(defcheck solution-b3e98a9b
  (fn [& x]
    (if (every? true? x)
      false
      (reduce #(or %1 %2) x))))

(defcheck solution-b4ed5e45
  (fn [& bs] (boolean (and (some false? bs) (some true? bs)))))

(defcheck solution-b5218860
  (fn [& more] (= 2 (count (into #{} more)))))

(defcheck solution-b54b581f
  (fn [& args]
    (and (not-every? false? args) (not (every? true? args)))
    ))

(defcheck solution-b59716c6
  (fn half-truth [x & xs] (if (some true? (into #{} (conj xs x))) (if (some false? (into #{} (conj xs x))) true false) false)))

(defcheck solution-b5a0f38f
  #(= 2 (count (group-by not %&))))

(defcheck solution-b6065302
  (fn[& args]
    (and (boolean (some true? args))  (not-every? true? args))))

(defcheck solution-b62c9668
  (fn [& xs] (if (some true? xs) (not-every? true? xs) false)))

(defcheck solution-b6559494
  not=)

(defcheck solution-b6813a96
  (fn [& c] (and (apply not= c) (some true? c))))

(defcheck solution-b6b970aa
  (fn [& args] (not (or (nil? (some true? args)) (nil? (some false? args))))))

(defcheck solution-b6d11e11
  (fn [& vals]
    (boolean (and
              (some identity vals)
              (not (every? identity vals))))))

(defcheck solution-b6d1488d
  (fn [& coll]
    (true?
      (and
       (some true? coll)
       (not-every? true? coll)))))

(defcheck solution-b6ee867e
  (fn [& args]
    (and (not (every? true? args))
         (not (every? false? args)))))

(defcheck solution-b7061534
  (fn [& args] (and (not (every? true? args)) (not (nil? (some true? args))))))

(defcheck solution-b7bd5bae
  (fn [& s] (if (and (> (/ (count (filter true? s)) (float (count s))) 0) (< (/ (count (filter true? s)) (float (count s))) 1)) true false)))

(defcheck solution-b7cf884e
  (fn [& v] (and (not (reduce #(and % %2) v)) (reduce #(or % %2) v))))

(defcheck solution-b9288672
  (fn [& xs]
    (and (not-every? true? xs) (not-every? false? xs))))

(defcheck solution-b9343678
  #(< 1 (count (group-by list %&))))

(defcheck solution-b944c570
  #(and (reduce (fn [a b] (or a b)) %&) (not (reduce (fn [a b] (and a b)) %&))))

(defcheck solution-b9492e35
  (fn half-truth
    [& bools]
    (let [s (set bools)]
      (and (contains? s true)
           (contains? s false)))))

(defcheck solution-b988095d
  #(boolean (and (some identity %&) (some not %&))))

(defcheck solution-b9dc9510
  (fn [& l]
    (= #{true false}
      (reduce
        (fn [l e] (if (contains? l e) l
                                      (conj l e))) #{} l))))

(defcheck solution-ba0a7093
  #(or(and(some true? %&)(some false? %&))false))

(defcheck solution-baac0b09
  (fn [& b] (and (not-every? false? b) (not-every? true? b))))

(defcheck solution-bb056015
  (fn [& l]
    (and  (reduce #(or % %2) false l )  (not (reduce #(and % %2) true l )))

    ))

(defcheck solution-bb18ae0c
  (fn [& args]
    (and (not-every? true? args)
         (not-every? false? args))))

(defcheck solution-bb1c3011
  (fn [& args]
    (let [c1 (count args)
          c2 (count (filter true? args))]
      (and (> c2 0) (not= c1 c2)))))

(defcheck solution-bb2ca2e7
  (fn [& vs]
    (not (nil? (and (some true? vs) (some false? vs))))))

(defcheck solution-bbc42d29
  (fn [& bools] (if (and (some true? bools) (not-every? true? bools)) true false)))

(defcheck solution-bbc72c4d
  #(true? (and (some false? %&) (some true? %&))))

(defcheck solution-bbd5c753
  (fn [& bs]
    (if (and (some true? bs) (some false? bs))
      true
      false)))

(defcheck solution-bbf4af77
  (fn [b & s] (if (true? (some #(not (= % b)) s)) true false )))

(defcheck solution-bc29c08a
  #(boolean (and (some identity %&) (not-every? identity %&))))

(defcheck solution-bc6269f5
  (fn [& f]
    (let [c (count (filter identity f))]
      (and (> c 0) (< c (count f))))))

(defcheck solution-bdcaef4e
  (fn [ & bols]
    (and (not (nil? (some true? bols)))
         (not (nil? (some false? bols))))))

(defcheck solution-be37b246
  (fn my-judge [& r]
    (true? (and (some true? r) (not (every? true? r))))))

(defcheck solution-be4b515
  (fn [& args]
    (->>
      (into #{} args)
      count
      (= 2))))

(defcheck solution-be92b2
  (fn [& v] (and (true? (some true? v)) (not-every? true? v))))

(defcheck solution-beab5c14
  (fn [& bools]
    (= true (and (some #{true} bools) (not-every? #{true} bools)))))

(defcheck solution-bf4d2c14
  #(and
    (not= nil (some (partial = true) %&))
    (not (every? (partial = true) %&))))

(defcheck solution-c00f7923
  (fn [& v] (= #{true false} (set v))))

(defcheck solution-c027b0ef
  (fn [& more]
    (if (and (some identity more) (not (every? identity more)))
      true
      false)))

(defcheck solution-c04097cd
  (fn [& input]
    (and
     (<
       (count (filter #(true? %) input))
       (count input)
       )
     (not (=
            (count (filter #(true? %) input))
            0))
     )
    ))

(defcheck solution-c0fd6d48
  (fn [x & more]
    (if (empty? more)
      false
      (not (= (count (filter #(= x %) more))
             (count more))))))

(defcheck solution-c1ae77d0
  (fn [& xs]
    (cond (every? true? xs) false
          (some true? xs) true
          :default false)))

(defcheck solution-c1e5763a
  (fn [& vals] (= 2 (count (group-by identity vals)))))

(defcheck solution-c1f47cc5
  #(->> %&
     set
     (apply =)
     not))

(defcheck solution-c2338a8e
  (fn [& x] (let [i (count (filter false? x))] (and (> i 0) (< i (count x))))))

(defcheck solution-c26176d5
  (fn [& x] (not (or (not-any? true? x) (not-any? false? x)))))

(defcheck solution-c261fbf4
  (fn [& args] (and (not-every? true? args) (boolean (some true? args)))))

(defcheck solution-c2acd1f9
  #(not= nil (and (some identity %&) (some not %&))))

(defcheck solution-c301e628
  (fn [& l] (and (not (reduce (fn [a b] (and a b)) l)) (reduce (fn [a b] (or a b)) l) )))

(defcheck solution-c3399c5
  #(and (not= nil (some true? %&)) (not (every? true? %&))))

(defcheck solution-c3705ff7
  (fn [& s] (= 2 (count (distinct s)))))

(defcheck solution-c391ea84
  (fn half-truth [& ts]
    (let [tts (partition 2 1 ts)]
      (if (some #(not= (first %)
                   (second %)) tts)
        true
        false))))

(defcheck solution-c405ad8c
  (fn [& v] (and (not-every? false? v) (not-every? true? v))))

(defcheck solution-c444e7b5
  #(or (and (some identity %&) (not (every? identity %&))) false))

(defcheck solution-c47795ca
  (fn [& coll]
    (if
     (and
      (first (filter identity coll))
      (first (filter identity (map not coll))))
      true
      false)))

(defcheck solution-c4c41983
  (fn [& args]
    (and
     (reduce #(or %1 %2) args)
     (not (reduce #(and %1 %2) args)))))

(defcheck solution-c4d2bbcb
  (fn [& args]
    (if (and (some identity args)
             (not (every? identity args)))
      true
      false)))

(defcheck solution-c50b3ce6
  (fn [& args]
    (true? (and
            (some identity args)
            (not-every? identity args)))))

(defcheck solution-c5344471
  (fn
    [& args]
    (not (not (and
               (some #(= false %) args)
               (some #(= true %) args) )))))

(defcheck solution-c555828f
  (fn [& x]
    (boolean (and (not (every? true? x)) (some true? x)))))

(defcheck solution-c5805af8
  (fn [& xs]
    (boolean (and (some true? xs) (some false? xs)))))

(defcheck solution-c63329b4
  (fn [& coll]
    (cond
      (reduce #(and % (not %2)) true coll) false
      (reduce #(or % (not %2)) false coll) true
      :else false)))

(defcheck solution-c674bdf8
  (fn [ & b]
    (let [oor (fn [l] (if (empty? l) false (if (first l) true (recur (rest l)) ) ) )
          aand (fn [l] (if (empty? l) true (if (not (first l)) false (recur (rest l)) ) ) )]
      (and (oor b) (not (aand b)))
      )
    ))

(defcheck solution-c6768b0b
  (fn half-truth [& more]
    (if (or
         (every? true? more)
         (not-any? true? more))
      false true)))

(defcheck solution-c7c86885
  (fn [& args]
    (boolean (and (some identity args)
                  (not (every? identity args))))))

(defcheck solution-c7cfb520
  (fn [& args] (and (not (nil? (some true? args))) (not-every? true? args))))

(defcheck solution-c86d370b
  (fn [& col]
    (if (some #(= true %) col)
      (if (some #(= false %) col) true false)
      false)))

(defcheck solution-c8941853
  (fn [& params](let [x (count (filter true? params)) y (count params)] (and (< x y) (< 0 x))) ))

(defcheck solution-c901ec64
  (fn [& xs] (and (not (every? true? xs)) (boolean (some true? xs)))))

(defcheck solution-c9307175
  (fn [& xs]
    (and (true? (some true? xs)) (not (every? true? xs)))))

(defcheck solution-c9e6b664
  (fn [& b] (if (and (some identity b) (not (every? identity b))) true false)))

(defcheck solution-cb1351de
  (fn [& args] (if (and (some true? args) (some false? args)) true false)))

(defcheck solution-cb25987b
  (fn [& args]
    (and (not-every? true? args) (true? (some true? args)))))

(defcheck solution-cb35cc09
  (fn [& p] (boolean (and (some true? p) (some false? p)))))

(defcheck solution-cc9c6e87
  (fn [& list] (boolean (and (some #{true} list) (not-every? #{true} list)))))

(defcheck solution-cc9e50c
  (fn [ & xs ] (and (not (every? identity xs)) (boolean (some identity xs)))))

(defcheck solution-ccd4bc64
  (fn [x & y]
    (if

     (not
       (or
        (and
         (= x true)
         (=
           (get (frequencies y) true 0)
           (count y)
           )
         )
        (and
         (= x false)
         (=
           (get (frequencies y) false  0)
           (count y)
           )
         )
        )
       )

      true
      false
      )
    ))

(defcheck solution-ccfc040
  (fn [& args]
    (let [or-test (reduce #(or % %2) args)
          and-test (reduce #(and % %2) args)]
      (and or-test (not and-test)))))

(defcheck solution-cd237dab
  (fn [& args]
    (= 2 (count (set args)))))

(defcheck solution-cdfbab75
  (fn mixed-bag [& bools]
    (if (and (some true? bools) (some false? bools))
      true
      false)))

(defcheck solution-ce20f854
  (fn [& args] (boolean (and (some true? args)
                             (some false? args)))))

(defcheck solution-ce7d86ba
  (fn half-true [& coll] (= true (and (some true? coll)(some false? coll)))))

(defcheck solution-ce7e73b8
  (fn [& bools]
    (boolean (and (some identity bools) (not (reduce #(and % %2) bools))))))

(defcheck solution-ce7ec98c
  #(and
    (not-every? identity %&)
    (not-every? not %&)))

(defcheck solution-cff2a266
  (fn half [& coll]
    (true? (and
            (some true? coll)
            (some false? coll)))))

(defcheck solution-d05a8047
  (fn
    ([x] (if (true? x) false x))
    ([x y & more]
     (let [booleans (conj more x y)]
       (cond
         (empty? (filter true? booleans)) false
         (every? true? booleans) false
         :else true)))))

(defcheck solution-d0680beb
  (fn [& bs] (let [result (and (not (every? true? bs)) (some true? bs))]
               (if (contains? #{false true} result) result false))))

(defcheck solution-d0dbca49
  #(if (and (some true? %&)
            (not-every? true? %&))
     true
     false))

(defcheck solution-d1f5e073
  (fn [& bools]
    (and (= true (some true? bools))
         (not (every? true? bools)))))

(defcheck solution-d232fc3a
  (fn [& a] (and (not-every? true? a) (not-every? false? a))))

(defcheck solution-d2737b08
  #(boolean (and (some identity %&)
                 (not (every? identity %&)))))

(defcheck solution-d290ee9f
  (fn [ & bools]
    (true? (and (some true? bools) (some false? bools)))))

(defcheck solution-d2e0be93
  (fn [& args]
    (not (not (and (some true? args)
                   (some false? args))))))

(defcheck solution-d2e3f615
  (fn [& bools]
    (boolean
      (and (some true? bools)
           (not-every? true? bools)))))

(defcheck solution-d2e4536
  (fn [& bools]
    (not (nil? (and (some identity bools) (some not bools))))))

(defcheck solution-d32ad0f8
  (fn [& c]
    (true? (and (some false? c) (some true? c)))))

(defcheck solution-d3599600
  (fn [& args] (boolean (and (not (every? identity args))
                             (some identity args)))))

(defcheck solution-d371cb22
  (fn [& xs]
    (and
     (not-every? identity xs)
     (not-every? not xs)
     )
    ))

(defcheck solution-d3c46256
  (fn [& bs] (and (not (apply = (cons true bs))) (reduce #(or %1 %2) bs))))

(defcheck solution-d415b938
  #(let [[t f] (partition-by true? %&)] (boolean (and (seq t) (seq f)))))

(defcheck solution-d4796f4f
  #(not (or (not-any? true? %&) (every? true? %&))))

(defcheck solution-d4fb11d1
  (fn [& v] (= 2 (count (set v)))))

(defcheck solution-d539706
  (fn [& bools]
    (if (and (some identity bools) (some not bools))
      true
      false)))

(defcheck solution-d55bc15b
  (fn [ & arg ]
    (every? true? ((juxt (partial some true?) (partial  some false?)) arg))))

(defcheck solution-d62d8d5b
  (fn [& args](= 2 (count (set args)))))

(defcheck solution-d68eb21d
  (fn [& args] (cond
                 (reduce #(and % %2) args) false
                 (reduce #(or % %2) args) true
                 :else false)))

(defcheck solution-d69cd0b5
  (fn [& the-rest]
    (if
     (and
      (some true? the-rest)
      (some false? the-rest))
      true
      false
      )))

(defcheck solution-d6accdf3
  (fn [& c] (= 2 (count (distinct c)))))

(defcheck solution-d6b9dd34
  (fn [& bools] (let [c (frequencies bools)] (if-not (and (c true) (c false)) false true))))

(defcheck solution-d6e92c02
  (fn [& args]
    (and (boolean (some true? args))
         (not (every? true? args)))))

(defcheck solution-d72803fd
  (fn [& a] (and (not (not-any? identity a)) (not-every? identity a))))

(defcheck solution-d762352b
  #(boolean (and (some #{true} (distinct %&)) (< 1 (count (distinct %&))))))

(defcheck solution-d781a900
  #(= (set %&) #{true false}))

(defcheck solution-d8b827b8
  #(> (count (frequencies %&)) 1))

(defcheck solution-d8c504fa
  (fn half-truth [& bools]
    (and (not-every? true? bools) (not-every? false? bools))
    ))

(defcheck solution-d8c927f
  (fn [& a] (= 2 (count(set a)))))

(defcheck solution-d906822f
  (fn ([b] (not b))
    ([b1 b2] (or b1 b2))
    ([b1 b2 b3] (not (and b1 b2 b3)))
    ([b1 b2 b3 b4] (and b1 b2 b3 (not b4)))))

(defcheck solution-d90d865d
  (fn [& xs] (and (not (every? not xs)) (not (every? identity  xs)))))

(defcheck solution-d9575b2c
  (fn [& coll]
    (not
      (or
       (every? #(= true %) coll)
       (every? #(= false %) coll)))))

(defcheck solution-d9660bf6
  (fn truth [ & more ] (and (not(not-any? true? more)) (not-every? true? more))))

(defcheck solution-d9947ceb
  #(= (set %&) #{true false}))

(defcheck solution-d9c73130
  (fn [& l]
    (not (apply = l))))

(defcheck solution-db5aa748
  (fn [& bs] (and (not-every? false? bs) (not-every? true? bs))))

(defcheck solution-db69ac4e
  (fn [& args]
    (if (and (some false? args) (some true? args)) true false)))

(defcheck solution-db8aa6e3
  (fn [& xs]
    (and (reduce #(or %1 %2) xs) (not (reduce #(and %1 %2) xs)))))

(defcheck solution-dcaef1ec
  #(and (not-every? false? %&) (not-every? true? %&)))

(defcheck solution-dd4733cc
  (fn half-truth [& xs]
    (and (if (some identity xs) true false)
         (not (every? identity xs)))
    ))

(defcheck solution-de26428d
  (fn [& xs]
    (boolean
      (and (some identity xs)
           (some not xs)))))

(defcheck solution-de79c2e4
  #(boolean (and (some identity %&) (not-every? identity %&))))

(defcheck solution-defcd59c
  (fn[& bools](if (= (distinct bools) '(true)) false (if (= (distinct bools) '(false)) false true))))

(defcheck solution-df462898
  (fn [& args] (and (not (not-any? true? args)) (not-every? true? args))))

(defcheck solution-df89e0f2
  (fn [& bs] (= #{true false} (into #{} bs))))

(defcheck solution-df9a74f7
  (fn [& bools]
    (and (or (some #(= true %) bools) false)
         (or (some not bools) false))))

(defcheck solution-dfcfe950
  not=)

(defcheck solution-dff17b6
  (fn [& xs](and (not (nil? (some true? xs))) (not (every? true? xs)))))

(defcheck solution-dffcc45d
  (fn [& xs] (and (not-every? true? xs) (not-every? false? xs))))

(defcheck solution-e0212d6a
  #(and (= true (some identity %&)) ((complement every?) identity %&)))

(defcheck solution-e048d954
  (fn [& lst] (and (not (not (some identity lst)))
                   (not (every? identity lst)))))

(defcheck solution-e0c78700
  (fn [& a] (not (apply = a))))

(defcheck solution-e13a4af
  (fn[& args](if (= 2 (count (distinct args)) ) true false )))

(defcheck solution-e13b523c
  (fn [& args]
    (if (and (some true? args)
             (not-every? true? args))
      true false)))

(defcheck solution-e18bf99c
  (fn [& c](every? true? [(some true? c)(some false? c)])))

(defcheck solution-e1a5b2ae
  (fn [ & bools] (< 0 (count (filter true? bools)) (count bools))))

(defcheck solution-e2511ace
  (fn [& args] (not (or (every? not args) (every? identity args)))))

(defcheck solution-e2eed49c
  #(and (boolean (some identity %&)) (not (every? identity %&))))

(defcheck solution-e2f4e061
  (fn [& b] ((complement empty?) (rest (partition-by true? b)))))

(defcheck solution-e2f9c418
  (fn [& bs]
    (let [trues (count (filter true? bs))]
      (and (not (zero? trues))
           (< trues (count bs))))))

(defcheck solution-e3136bde
  (fn [& a]
    (not (not (and (some true? a) (some false? a))))))

(defcheck solution-e34477f3
  (fn [& args]
    (and (not (nil? (some identity args)))
         (not (every? identity args)))))

(defcheck solution-e3847cb5
  (fn [& x] (and (not (every? false? x)) (not (every? true? x)))))

(defcheck solution-e394c85e
  (fn [& args]
    (and (not-every? #(true? %) args) (not-every? #(false? %) args))
    ))

(defcheck solution-e3e6c4ec
  (fn [& s]
    (if (some #(= true %) (seq s))
      (if (some #(= false %) (seq s))
        true
        false)
      false)))

(defcheck solution-e3f4efc6
  (fn [& s]
    (true? (and
            (some identity s)
            (some not s)))))

(defcheck solution-e40032cd
  (fn [& args]
    (boolean
      (and (some true? args)
           (some false? args)))))

(defcheck solution-e4282a50
  #(> (count (set %&)) 1))

(defcheck solution-e43439
  (fn [& bools]
    (and (= 0 (apply * (map #(if % 1 0) bools))) (< 0 (apply + (map #(if % 1 0) bools))))))

(defcheck solution-e5046100
  #(and (not (nil? (some identity %&))) (not-every? identity %&)))

(defcheck solution-e57dd406
  (fn [& vals]
    (true? (and (some true? vals) (some false? vals)))))

(defcheck solution-e602a892
  (fn partialtrue [& x]
    (let [y (vec x)]
      (loop [result y, counterf 0, countert 0]
        (if (not (empty? result))
          (if (and (pos? counterf) (pos? countert))
            true
            (if (false? (peek result))
              (recur (pop result) (inc counterf) countert)
              (recur (pop result) counterf (inc countert))))
          (and (pos? counterf) (pos? countert)))))))

(defcheck solution-e649cd24
  #(not= nil (nth (distinct %&) 1 nil)))

(defcheck solution-e6a4bdba
  (fn check [& args]
    (boolean (and (some true? args)
                  (some false? args)))))

(defcheck solution-e6ab31e6
  (fn [& vals] (cond (apply = vals) false
                     (some true? vals) true)))

(defcheck solution-e6ba65b1
  (fn [& args] (true? (and (some true? args) (some false? args)))))

(defcheck solution-e6e81f4f
  (fn [& pred] (and (not (not-any? true? pred)) (not-every? true? pred))))

(defcheck solution-e6ec2355
  (fn [& s]
    (and (not-every? true? s) (not-every? false? s))))

(defcheck solution-e7869fbc
  (fn [& s]
    (and (not-every? (complement identity) s) (not-every? identity s))))

(defcheck solution-e7af90d0
  (fn [& bs] (if (and (some true? bs) (not (every? true? bs))) true false)))

(defcheck solution-e8bb588c
  (fn [& bools]
    (and (not-every? true? bools) (not (nil? (some true? bools))))))

(defcheck solution-e907372e
  (letfn [(xor [& bs]
            (let [m (group-by identity bs)]
              (boolean (and (m true) (m false)))))]
    xor))

(defcheck solution-e929d6cc
  (fn most [& args]
    (boolean (and (some true? args) (not (every? true? args))))))

(defcheck solution-e986bae0
  #(and (not (apply = %&))
        ((set %&) true)))

(defcheck solution-e9932d2
  (fn [& v]
    (and (not-every? true? v) (not-every? false? v))))

(defcheck solution-e9c46ba7
  (fn [& preds]
    (true? (and (some true? preds) (some false? preds)))))

(defcheck solution-e9eceb9a
  (fn [& args]
    (let [l (count args), t (count (filter true? args))]
      (and
       (> l 0)
       (> t 0)
       (< t l)))))

(defcheck solution-e9fd1392
  (fn [& xs] (not (every? (fn [[a b]] (= a b)) (partition 2 1 xs)))))

(defcheck solution-ea0fdd05
  (fn [& flags] (and
                 (reduce #(or %1 %2) false flags)
                 (not (reduce #(and %1 %2) true flags)))))

(defcheck solution-ea1baa4
  (fn [& xs]
    (apply not= xs)))

(defcheck solution-ea33964c
  (fn [& x] (if (and (some true? x) (some false? x)) true false)))

(defcheck solution-ea345e81
  (fn [& coll]
    (if (and (some true? coll) (not (every? true? coll)))
      true
      false)))

(defcheck solution-ea346782
  (fn [& preds] (and (not (every? #(and %) preds)) ((complement nil?) (some #(or %) preds)))))

(defcheck solution-ea9a082e
  (fn [& coll]
    (not (nil? (and (some true? coll) (some false? coll))))))

(defcheck solution-eaca8cb7
  (fn [& x]
    (let [a (count x) b (count (filter true? x))]
      (if (> b 0) (> a b) false))))

(defcheck solution-eb87c55
  (fn [& args] (and
                (not-every? true? args)
                (not-every? false? args)
                )
    ))

(defcheck solution-eb919c78
  (fn [& args] (and ((comp true? some) true? args) ((complement every?) true? args))))

(defcheck solution-eba9c36e
  (fn [& v] (if (and (some identity v) (some not v)) true false)))

(defcheck solution-ec0fb7bc
  (fn [& x] (and ((complement not-any?) identity x) (not-every? identity x))))

(defcheck solution-ec30dd4
  (fn half-truth
    [& xs]
    (and
     (not (every? identity xs))
     (boolean (some identity xs)))))

(defcheck solution-ec312ba0
  (fn [& in]
    (and (reduce #(or %1 %2) in) (not (reduce #(and %1 %2) in)))))

(defcheck solution-ec364d92
  (fn [ & args]
    (= (count (frequencies args)) 2)
    ))

(defcheck solution-ec3b5395
  (fn [& args]
    (if (and (some #(= true %) args) (some #(= false %) args))
      true
      false)))

(defcheck solution-ec59d80a
  (fn [& c]
    (if ((complement every?) true? c)
      (if (some true? c) true false)
      false)))

(defcheck solution-ec743d4e
  (fn [& bools]
    (true?
      (and
       (some true? bools)
       (some false? bools)))))

(defcheck solution-ecc3e04f
  (fn myf [ & a ] (if (reduce #(and %1 %2) a) false (reduce #(or %1 %2) a))))

(defcheck solution-ecd822c4
  #(and (not (not-any? true? %&)) (not-every? true? %&)))

(defcheck solution-ece7142d
  (fn [& args] (or (and (some identity args) (not-every? identity args)) false)))

(defcheck solution-ed884a7a
  (fn [& coll] (true? (and (some false? coll) (some true? coll) ))))

(defcheck solution-ee50f43a
  (fn [& x] (< 1 (count (distinct x)))))

(defcheck solution-ee650988
  #(true?
     (and
      (some true? %&)
      (some false? %&))))

(defcheck solution-ee7b149d
  (fn [& s] (true? (and (some true? s) (some false? s)))))

(defcheck solution-ee8e45ae
  (fn [& args] (and (not (empty? (filter true? args))) (not (empty? (filter false? args))))))

(defcheck solution-ee9afdd0
  #(= (count (set %&))
     2))

(defcheck solution-eed9090c
  (fn [& bs]
    (every? true? [(some true? bs) (some false? bs)])))

(defcheck solution-eee5cbb0
  (fn [& args]
    (true? (and (some identity args)
                (not-every? identity args)))))

(defcheck solution-ef608dee
  (fn [& args]
    (or (and (some true? args) (some false? args)) false)))

(defcheck solution-efcbcdd9
  (fn [& coll]
    (if (every? (fn [x] (= x true)) coll)
      false
      (not (every? (fn [x] (= x false)) coll)))))

(defcheck solution-f011cc9b
  (fn [& bs]
    (let [tcount (count (filter identity bs))]
      (and (> tcount 0)
           (< tcount (count bs))))))

(defcheck solution-f085f5b2
  (fn [& args] (boolean (and (some true? args) (some false? args)))))

(defcheck solution-f091e05f
  (fn [& xs] (let [s (into #{} xs)] (and (contains? s true) (contains? s false)))))

(defcheck solution-f0c2f51c
  (fn [& items]
    (and (not(not-any? true? items)) (not-every? true? items))))

(defcheck solution-f1a36f95
  (fn [& args] (= 2 (count (distinct args)))))

(defcheck solution-f25a0eec
  (fn [& args]
    (and (not-every? true? args)
         (not-every? false? args))))

(defcheck solution-f276edf5
  (fn [& boos]
    (and (contains? (group-by true? boos) true)
         (contains? (group-by true? boos) false))))

(defcheck solution-f2d55add
  (fn [& bools]
    ((comp not nil? next distinct) bools)))

(defcheck solution-f2d5889f
  (fn [& xs]
    (and (not (reduce #(and %1 %2) xs))
         (reduce #(or %1 %2) xs))))

(defcheck solution-f30c189a
  (fn [& s] (= true (second (map first (partition-by identity (sort s)))))))

(defcheck solution-f31f7627
  (fn [& x] (boolean (and (not-every? true? x) (some true? x)))))

(defcheck solution-f35cc98c
  (fn [& bs]
    (let [f #(if (true? %) true false)]
      (f
        (and
         (some true? bs)
         (not
           (every? true? bs)))))))

(defcheck solution-f3920d07
  (fn[& args]
    (not (nil? (and (some #(= false %) args)
                    (some #(= true %) args))))))

(defcheck solution-f3a57e59
  (fn [ & args]
    (not (nil?
           (and (some true? args) (some false? args))))))

(defcheck solution-f3ae5aec
  (fn [& xs] (= 2 (count (set xs)))))

(defcheck solution-f3d8564f
  #(< 1 (count (set %&))))

(defcheck solution-f41af96b
  (fn [& args]
    (and (not-every? true? args) (not-every? false? args))
    ))

(defcheck solution-f49f8776
  #(contains? (set (range 1 (count %&))) (count (filter identity %&))))

(defcheck solution-f4a28938
  (fn [& vs] (= 3 (reduce (fn [acc x] (if (true? x) (bit-or acc 1) (bit-or acc 2))) 0 vs))))

(defcheck solution-f4f4e8b8
  (fn myHalfTruth
    [& bools]
    (= 2 (count (set bools)))))

(defcheck solution-f506a82a
  (fn [& x] (= 2 (count (into #{} x)))))

(defcheck solution-f53c789
  (fn half-truth [& s] (= 2 (count (distinct s)))))

(defcheck solution-f5576120
  (fn [& args]
    (let [all (reduce #(and %1 %2) args)
          some (reduce #(or %1 %2) args)]
      (cond
        all false
        some true
        :else false))))

(defcheck solution-f59bfd0d
  (fn [& xs]
    (let [xs' (sort xs)]
      (not= (first xs') (last xs')))))

(defcheck solution-f5ae56e4
  (fn [& coll]
    (true? (and (some true? coll) (some false? coll)))))

(defcheck solution-f619a90a
  (fn [& args]
    (not (nil? (and (some true? args) (some false? args)) ))))

(defcheck solution-f620cd08
  (fn [& vars]
    (cond (every? true? vars) false
          (some true? vars) true
          :else false)))

(defcheck solution-f64a1fd1
  (fn [& coll]
    (true? (and (some true? coll) (some false? coll)))))

(defcheck solution-f683654b
  (fn [& x] (loop [fs 0
                   ts 0
                   xs x]
              (if (nil? xs)
                (and (> ts 0) (> fs 0))
                (if (first xs)
                  (recur fs (inc ts) (next xs))
                  (recur (inc fs) ts (next xs))
                  )
                )
              )
    ))

(defcheck solution-f69f1e46
  (fn [& args]
    (if (apply = true args) false
                            (boolean (some identity args)))))

(defcheck solution-f6b092a4
  (fn [& args]
    (and ((complement nil?) (some true? args)) (not (every? true? args)))))

(defcheck solution-f7f70d3e
  (fn [& bool]
    (cond
      (every? true? bool) false
      (every? false? bool) false
      :else true)))

(defcheck solution-f81eb831
  (fn half-true
    [& bools]
    (if-let [truthy (seq (filter true? bools))]
      (not (empty? (remove true? bools)))
      false)))

(defcheck solution-f83191cd
  (fn [& b] (not (apply = b))))

(defcheck solution-f89a12a3
  #(if (and (some true? %&) (some false? %&)) true false))

(defcheck solution-f8a05340
  (fn [& bools] (boolean (and (some true? bools)
                              (not (nil? (some false? bools)))))))

(defcheck solution-f8adc636
  (fn [& args]
    (if (or (not-any? #(= true %) args)
            (not-any? #(= false %) args))
      false true)))

(defcheck solution-f8e7add0
  (fn half [& args]
    (if (and (some identity args) (not (every? identity args)))
      true
      false)))

(defcheck solution-f90088de
  #(-> %& distinct count (= 2)))

(defcheck solution-f9109af
  #(= #{true false} (set %&)))

(defcheck solution-f9ec00b3
  (fn [& xs]
    (let [ys (remove #(= false %) xs)]
      (and (> (count xs ) (count ys))
           (not (empty? ys))))))

(defcheck solution-fa0f13c1
  (fn [& more] (true? (and (some true? more) (some false? more)))))

(defcheck solution-fa55501
  #(= 2 (count (set %&))))

(defcheck solution-fa62934c
  (fn [a & b]
    (and (not (nil? (some true? (cons a b))))
         (not (every? true? (cons a b))))))

(defcheck solution-fb06e36c
  #(not (apply = %&)))

(defcheck solution-fb4bd665
  #(and (not (every? identity %&)) (not (nil? (some identity %&)))))

(defcheck solution-fb9ff9cc
  (fn [& args]
    (and (not (nil? (some identity args))) (not (every? identity args)))))

(defcheck solution-fbbf66d5
  #(if (and (some (partial = true) %&) (some (partial = false)  %&)) true false))

(defcheck solution-fbcb16d4
  (fn [& xs] (apply not= xs)))

(defcheck solution-fbf48e6e
  (fn [& bs]
    (boolean (and (some true? bs)
                  (not (every? true? bs))))))

(defcheck solution-fc80831f
  (fn half-truth [x & xs]
    (not-every? #(= % x) xs)))

(defcheck solution-fd03def9
  (fn [& a]
    (if (every? true? a)
      false
      (if (some true? a)
        true
        false))))

(defcheck solution-fd1f79b1
  (fn [& bools]
    (boolean
      (and
       (some true? bools)
       (not (every? true? bools))
       )
      )
    ))

(defcheck solution-fd71570e
  (fn [& xs]
    (not (not (and (some true? xs)
                   (not (every? true? xs)))))))

(defcheck solution-fef246a5
  #(> (count (group-by identity %&)) 1))

(defcheck solution-ff02d78
  (fn [& l] (< 1 (count (set l)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-83))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
