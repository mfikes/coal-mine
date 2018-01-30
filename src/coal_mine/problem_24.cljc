(ns coal-mine.problem-24
  (:require [coal-mine.checks :refer [defcheck-24] :rename {defcheck-24 defcheck}]
            [clojure.test]))

(defcheck solution-169a25da
  reduce (partial + 0))

(defcheck solution-177490e2
  (fn[s] (reduce + 0 s)))

(defcheck solution-17c54075
  (fn [s]
    (reduce + s)))

(defcheck solution-188695fd
  (fn [col] (apply + col)))

(defcheck solution-1e121270
  (fn [se]
    (loop [s se c 0]
      (if (empty? s)
        c
        (recur (rest s)
          (+ c (first s)))))))

(defcheck solution-21d0ce2e
  (fn f[l](if (empty? l) 0 (+ (first l) (f (rest l))))))

(defcheck solution-2528411c
  (fn rec [n] (if (= (count n) 1) (first n) (+ (first n) (rec (rest n))))))

(defcheck solution-25da164e
  #(letfn [(worker [x n]
             (if (= x ())
               n
               (recur (rest x) (+ n (first x)))))]
     (worker % 0)))

(defcheck solution-26019176
  (fn sol0024-reduce
    [coll]
    (reduce + coll)))

(defcheck solution-28952e40
  (fn sum-seq [x]
    (reduce + x)))

(defcheck solution-2be233f9
  (fn [c] (reduce + c)))

(defcheck solution-2ca0b194
  (fn this [l] (if (<= (count l) 0) 0 (+ (first l) (this (rest l))))))

(defcheck solution-2dd95da2
  reduce #(+ % %2) 0)

(defcheck solution-2e92a2fe
  (fn [elems]
    (reduce + 0 elems)))

(defcheck solution-2eaf879
  (fn problem23-reverse-seq [seq]
    (reduce + seq)))

(defcheck solution-2fb3ab73
  (fn [mySequence]
    (loop [s mySequence acc 0]
      (if (empty? s)
        acc
        (recur (rest s) (+ acc (first s)))
        )
      )
    ))

(defcheck solution-3129cde1
  (fn [xs] (apply + xs)))

(defcheck solution-359c6459
  (fn [l]
    (reduce + l)))

(defcheck solution-35a906e8
  #(reduce + %))

(defcheck solution-37c6527c
  #(reduce + 0 %))

(defcheck solution-3b8b29
  (fn [lat]
    (reduce + lat)))

(defcheck solution-3cf6834a
  #(reduce (fn [acc elt] (+ acc elt)) 0 %))

(defcheck solution-3e0d82ee
  (fn [seq]
    (reduce + 0 seq)))

(defcheck solution-3f701c67
  reduce (fn [a b] (+ a b)) 0)

(defcheck solution-4146a24e
  (fn [x] (reduce (fn [i j] (+ i (first (list j)))) 0 x)))

(defcheck solution-41aca247
  (fn [l] (reduce + l)))

(defcheck solution-4404b252
  (fn [seq] (reduce + 0 seq )))

(defcheck solution-443a5604
  (fn [x]
    (reduce + x)
    ))

(defcheck solution-44c43541
  reduce (fn [sum x] (+ sum x)))

(defcheck solution-452e0075
  (fn f [s]
    (if (empty? s)
      0
      (+ (first s) (f (rest s))))))

(defcheck solution-4560dcc7
  reduce #(+ %1 %2))

(defcheck solution-47fe36e2
  (fn [input]
    (loop [sum 0 elements input]
      (if (empty? elements)
        sum
        (recur (+ sum (first elements)) (rest elements))
        )
      )
    ))

(defcheck solution-4af6cd3a
  (fn [a-seq]
    (reduce + a-seq)))

(defcheck solution-4c7aca1a
  (fn hey
    ([x] (hey x 0))
    ([x y] (if (empty? (rest x))
             (+ (first x) y)
             (hey (rest x) (+ (first x) y))))))

(defcheck solution-4d69c6b
  ;(fn add-seq [coll]
  ;  (loop [coll coll
  ;         sum 0]
  ;    (if (= () coll)
  ;      sum
  ;      (recur (rest coll) (+ sum (first coll))))))


  (fn [coll] (reduce + coll)))

(defcheck solution-4fe29c85
  #(reduce (fn [acc x] (+ acc x)) %))

(defcheck solution-5049f998
  (fn my-sum [s]
    (if (empty? s) 0 (+ (first s) (my-sum (rest s))))))

(defcheck solution-52b51143
  #(reduce + (seq %)))

(defcheck solution-5435f79
  (fn [seq]
    (reduce #(+ %1 %2) seq)))

(defcheck solution-54d7f26e
  (fn sumall
    [arr]
    (reduce + arr)))

(defcheck solution-557a0d3
  (fn [s]
    (apply + s)))

(defcheck solution-56e4ca5c
  (fn [start]
    (loop [xs start, n 0]
      (if (empty? xs) n (recur (rest xs) (+ (first xs) n))
                      ))))

(defcheck solution-5a93082
  (fn f[lista]
    (if (empty? lista)
      0
      (+ (first lista) (f (rest lista))))))

(defcheck solution-5b0c8530
  (fn [l] (apply + l)))

(defcheck solution-61cbe5ef
  (fn [x]
    (reduce + x)))

(defcheck solution-63c81f98
  (fn [xs](apply + xs)))

(defcheck solution-64b81997
  (fn [x] (reduce (fn [acc el] ( + el acc)) 0 x)))

(defcheck solution-666a524
  (fn my-sum
    ([coll] (my-sum coll 0))
    ([coll acc]
     (if (seq coll)
       (my-sum (rest coll) (+ (first coll) acc))
       acc))))

(defcheck solution-695a25c3
  (fn [s]
    (reduce #(+ %1 %2) 0 s)))

(defcheck solution-6ae7cff9
  reduce #(+ %1 %2) 0)

(defcheck solution-6f93e9ce
  apply +)

(defcheck solution-70760d2d
  (fn [x] (reduce + x)))

(defcheck solution-728cb9fd
  #(reduce + %1))

(defcheck solution-766db4dd
  (fn [x]
    (loop [l x, c 0]
      (if (empty? l)
        c
        (recur (rest l) (+ c (first l)))))))

(defcheck solution-7738ec57
  (partial reduce + 0))

(defcheck solution-77bea33d
  (fn [xs]
    (reduce + 0 xs)))

(defcheck solution-791b5aec
  #(apply + (seq %)))

(defcheck solution-79321bc0
  (fn a [s]
    (if (not (seq s))
      0
      (+ (first s) (a (rest s))))
    ))

(defcheck solution-79f2ff46
  #(reduce (fn [a,b] (+ a b)) %))

(defcheck solution-7ad59b6b
  (fn [x]
    (loop [lst x sum 0]
      (if (empty? lst) sum
                       (recur (rest lst)
                         (+ (first lst)
                            sum))))))

(defcheck solution-7c4b159a
  (fn [x] (reduce + 0 x)))

(defcheck solution-7dc80515
  (fn [coll] (reduce + coll)))

(defcheck solution-7ee74501
  (fn [x] (reduce #(+ %1 %2) x)))

(defcheck solution-80c2470
  (fn mysum [x] (if (= (count x) 0) 0 (+ (first x) (mysum (rest x))))))

(defcheck solution-81d6bf1
  (fn [x] (loop [[h & t] (into [] x) s 0] (let [news (+ h s)] (if (= t nil) news (recur t news))))))

(defcheck solution-82645ea3
  (fn [seq]
    (reduce + seq)))

(defcheck solution-84bd7b83
  (fn [seq] (reduce + seq)))

(defcheck solution-85584514
  (fn [xs] (reduce #(+ %1 %2) 0 xs)))

(defcheck solution-864ee675
  reduce (fn [acc val] (+ acc val)) 0)

(defcheck solution-86f8eb6d
  (fn sum [xs]
    ((fn impl [xs acc]
       (if (empty? xs)
         acc
         (impl (rest xs) (+ acc (first xs)))))
     xs 0)))

(defcheck solution-88acf04c
  #(apply + %1))

(defcheck solution-89fb8c11
  (fn [l]
    (loop [mylist l s 0]
      (if (= mylist '())
        s
        (recur (rest mylist) (+ s (first mylist)))))))

(defcheck solution-8cdc593c
  (fn [xs] (reduce + xs)))

(defcheck solution-8ef102c5
  (fn [sec]
    (reduce #(+ %1 %2) 0 sec)))

(defcheck solution-8fef5638
  (fn [ns]
    (reduce + ns)))

(defcheck solution-909cd566
  (fn [coll] (apply + coll)))

(defcheck solution-90c03485
  (fn
    [list]
    (reduce + list)))

(defcheck solution-911d23a0
  reduce (fn [t v] (+ t v)) 0)

(defcheck solution-9179a42b
  (fn [x]   (loop [r 0     i x]	(if (nil? (first i))	  r	  (recur (+ r (first i)) (rest i))))))

(defcheck solution-9307558c
  (fn [col]
    (reduce #(+ %1 %2) 0 col)
    ))

(defcheck solution-93620ea4
  (partial apply +))

(defcheck solution-93f498e3
  (fn [x] (reduce #(+ %1 %2)x)))

(defcheck solution-9597fe7e
  (fn [s]
    (reduce #(+ %1 %2) 0 s)))

(defcheck solution-96881703
  (fn sum [x] (if (seq x) (+ (first x) (sum (drop 1 x))) 0)))

(defcheck solution-976e0493
  (fn [x] (apply + x)))

(defcheck solution-99277d42
  #(reduce (fn [a b] (+ a b)) %))

(defcheck solution-9a6f6622
  (partial reduce +))

(defcheck solution-a08e5ce2
  (fn [s]
    (loop [rem (seq s), acc 0]
      (cond (empty? rem) acc
            :else (recur (rest rem) (+ (first rem) acc))))))

(defcheck solution-a09944e4
  #(reduce (fn [x y] (+ x y) ) 0 %))

(defcheck solution-a3e3a2b3
  (fn [myseq] (reduce + myseq)))

(defcheck solution-a5111575
  #(loop [s % accum 0]
     (if (empty? s) accum
                    (recur (next s) (+ accum (first s))))))

(defcheck solution-a9ab7cc7
  (fn [xs]
    (reduce + xs)))

(defcheck solution-acdc80e
  (fn sum[seq] (if (empty? seq) 0 (+ (first seq) (sum (rest seq))))))

(defcheck solution-aef16b68
  (fn [l]
    (reduce + l)
    ))

(defcheck solution-b1035f4d
  (fn [xs] (reduce #(+ %1 %2) 0 (seq xs))))

(defcheck solution-b28837c7
  #(reduce (fn [x _] (+ _ x)) 0 %))

(defcheck solution-b2b0b01f
  #(reduce (fn [a b] (+ a b)) 0 %))

(defcheck solution-b4367b21
  (fn [input] (reduce + 0 input)))

(defcheck solution-b4df79a4
  (fn sum [x]
    (reduce #(+ %1 %2) x)))

(defcheck solution-b76b06e6
  (fn sum [a-seq]
    (apply + a-seq)))

(defcheck solution-b90f6d59
  (fn [s] (reduce #(+ %1 %2) s)))

(defcheck solution-b990ea97
  (fn sum [x]
    (if (empty? (rest x))
      (first x)
      (+ (first x) (sum (rest x))))))

(defcheck solution-b9da16cd
  (fn [c] (reduce #(+ %1 %2) 0 c)))

(defcheck solution-bbef1bb3
  #(reduce (fn [x y] (+ x y)) %))

(defcheck solution-bcca59b1
  (fn [s] (apply + s)))

(defcheck solution-bd480847
  (fn [col]
    (loop [c col ret 0]
      (if (nil? (seq c))
        ret
        (recur (rest c) (+ ret (first c)) )))))

(defcheck solution-bdca9769
  apply + 0)

(defcheck solution-be872234
  (fn sum' [xs] (if (empty? xs) 0 (+ (first xs) (sum' (rest xs))))))

(defcheck solution-bed5f031
  (fn [coll] (reduce + 0 coll)))

(defcheck solution-beeddbdf
  (fn sum [x]
    (if (empty? x)
      0
      (+ (sum (rest x)) (first x)))))

(defcheck solution-c03eb00e
  (fn [coll] (reduce (fn [a b] (+ a b)) 0 coll)))

(defcheck solution-c1b35726
  (fn [the-sequence]
    (reduce + the-sequence)
    ))

(defcheck solution-c3d38089
  (fn sumit [alist] (if (= (seq alist) nil) 0 (+ (sumit (rest alist)) (first alist)))))

(defcheck solution-c480e3b0
  (fn [nums] (reduce + nums)))

(defcheck solution-c67fa6d3
  (fn my-sum [s] (if (empty? s)
                   0
                   (+ (first s)
                      (my-sum (rest s))))))

(defcheck solution-c72d6664
  (fn [values]
    (reduce + values)))

(defcheck solution-c76371c9
  (fn [s] (reduce + s)))

(defcheck solution-ca679095
  (fn sumOfList
    [sequence]
    (apply + sequence)))

(defcheck solution-cb632fd5
  #(apply + %))

(defcheck solution-cbe781c1
  (fn [col]
    (apply + col)))

(defcheck solution-ce410674
  (fn [coll]
    (reduce + coll)))

(defcheck solution-d13fed61
  ; #(reduce + %)
  (partial reduce +))

(defcheck solution-d3d63080
  (fn [s] (reduce + 0 s)))

(defcheck solution-d4554d07
  reduce +)

(defcheck solution-d552220b
  (fn foo
    ([s] (foo s 0))
    ([s acc] (if (empty? s) acc (recur (rest s) (+ (first s) acc))))))

(defcheck solution-d747a8b0
  (fn [seq] (loop [s (into [] seq)
                   sum 0]
              (if (not (= s []))
                (recur (rest s) (+ sum (first s)))
                sum))))

(defcheck solution-d7966dac
  (fn [col]
    (reduce + col)))

(defcheck solution-d80c0df6
  reduce (fn [x y] (+ x y)) 0)

(defcheck solution-d90f1614
  (fn [l]
    (reduce (fn [_ sum] (+ sum _)) 0 l)))

(defcheck solution-df1cf1f9
  (fn [l]
    (reduce + l)
    ))

(defcheck solution-df3e3d1a
  (fn [coll]
    "24. Write a function which returns the sum of a sequence of numbers."
    (reduce (fn [l r] (+ l r)) coll)))

(defcheck solution-e195abae
  (fn sum [x]
    (if (< 1 (count x))
      (+ (first x) (sum (rest x) ) )
      (nth x 0)
      )
    ))

(defcheck solution-e2a675b9
  (fn [v] (apply + v)))

(defcheck solution-e5dd5578
  (fn [coll]
    (apply + coll)))

(defcheck solution-e671fc8f
  (fn sum [nums]
    (if (empty? nums)
      0
      (+ (first nums) (sum (rest nums))))))

(defcheck solution-e8c2eb23
  (fn [xs] (reduce (fn [a b] (+ a b)) 0 xs)))

(defcheck solution-e8dd7eb0
  (fn [xs] ((fn calc[acc os] (if (empty? os) acc (calc (+ acc (first os)) (rest os)))) 0 xs)))

(defcheck solution-e9cd00c6
  #(reduce (fn [x y] (+ x y)) 0 %))

(defcheck solution-ea903a5a
  #((fn sum [x,acc] (if (empty? x) acc (sum (rest x) (+ acc (first x)))))
    % 0))

(defcheck solution-ee28a597
  #(
    (fn sum [sq, sm]
      (if
       (= nil (first sq))
        sm
        (sum
          (rest sq) (+ sm (first sq))
          )
        )
      )
    % 0
    ))

(defcheck solution-ef82b1a9
  (fn [sq] ((fn summ[s acc] (if (= nil (first s))
                              acc
                              (summ (rest s) (+ acc (first s))))) sq 0)))

(defcheck solution-efcdd331
  (fn [s]
    (reduce + 0 s)
    ))

(defcheck solution-efe20405
  (fn acc [sq] (if (empty? sq) 0 (+ (first sq) (acc (rest sq))))))

(defcheck solution-f1f79ee2
  (fn mysum [x]
    (if (empty? x)
      0
      (+ (mysum (rest x)) (first x)))))

(defcheck solution-f28c6eed
  reduce + 0)

(defcheck solution-f69ab0bf
  (fn sum
    ([sq] (sum sq 0))
    ([sq acc] (if (empty? sq)
                acc (recur (rest sq) (+ acc (first sq)))))))

(defcheck solution-f6fdd04d
  (fn [lst] (reduce + (seq lst))))

(defcheck solution-fad82c48
  (fn [x]
    (loop [input x
           result 0]
      (if (empty? input)
        result
        (recur (rest input) (+ result (first input)))))))

(defcheck solution-fb79aff3
  (fn adder [sq]
    (reduce + sq)))