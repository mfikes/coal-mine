(ns coal-mine.checks
  (:require clojure.test))

#?(:clj
   (defn system-time []
     (System/currentTimeMillis)))

(defmacro deftimedtest [name threshold & body]
  `(clojure.test/deftest ~name
     (let [start# (coal-mine.checks/system-time)]
       ~@body
       (let [elapsed# (- (coal-mine.checks/system-time) start#)]
         (when (> elapsed# ~threshold)
           (println '~name "took" (int elapsed#) "ms to execute."))))))

(defmacro defcheck-1 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms true))))

(defmacro defcheck-2 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (- 10 (* 2 3)) ~@forms))))

(defmacro defcheck-3 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (.toUpperCase "hello world")))))

(defmacro defcheck-4 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (list ~@forms) '(:a :b :c)))))

(defmacro defcheck-5 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (conj '(2 3 4) 1)))
     (clojure.test/is (= ~@forms (conj '(3 4) 2 1)))))

(defmacro defcheck-6 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= [~@forms] (list :a :b :c) (vec '(:a :b :c)) (vector :a :b :c)))))

(defmacro defcheck-7 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (conj [1 2 3] 4)))
     (clojure.test/is (= ~@forms (conj [1 2] 3 4)))))

(defmacro defcheck-8 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (set '(:a :a :b :c :c :c :c :d :d))))
     (clojure.test/is (= ~@forms (clojure.set/union #{:a :b :c} #{:b :c :d})))))

(defmacro defcheck-9 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= #{1 2 3 4} (conj #{1 4 3} ~@forms)))))

(defmacro defcheck-10 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms ((hash-map :a 10, :b 20, :c 30) :b)))
     (clojure.test/is (= ~@forms (:b {:a 10, :b 20, :c 30})))))

(defmacro defcheck-11 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= {:a 1, :b 2, :c 3} (conj {:a 1} ~@forms [:c 3])))))

(defmacro defcheck-12 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (first '(3 2 1))))
     (clojure.test/is (= ~@forms (second [2 3 4])))
     (clojure.test/is (= ~@forms (last (list 1 2 3))))))

(defmacro defcheck-13 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (rest [10 20 30 40])))))

(defmacro defcheck-14 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms ((fn ~'add-five [x#] (+ x# 5)) 3)))
     (clojure.test/is (= ~@forms ((fn [x#] (+ x# 5)) 3)))
     (clojure.test/is (= ~@forms (#(+ % 5) 3)))
     (clojure.test/is (= ~@forms ((partial + 5) 3)))))

(defmacro defcheck-15 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 2) 4))
     (clojure.test/is (= (~@forms 3) 6))
     (clojure.test/is (= (~@forms 11) 22))
     (clojure.test/is (= (~@forms 7) 14))))

(defmacro defcheck-16 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms "Dave") "Hello, Dave!"))
     (clojure.test/is (= (~@forms "Jenn") "Hello, Jenn!"))
     (clojure.test/is (= (~@forms "Rhea") "Hello, Rhea!"))))

(defmacro defcheck-17 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (map #(+ % 5) '(1 2 3))))))

(defmacro defcheck-18 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (filter #(> % 5) '(3 4 5 6 7))))))

(defmacro defcheck-19 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 2 3 4 5]) 5))
     (clojure.test/is (= (~@forms '(5 4 3)) 3))
     (clojure.test/is (= (~@forms ["b" "c" "d"]) "d"))))

(defmacro defcheck-20 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms (list 1 2 3 4 5)) 4))
     (clojure.test/is (= (~@forms ["a" "b" "c"]) "b"))
     (clojure.test/is (= (~@forms [[1 2] [3 4]]) [1 2]))))

(defmacro defcheck-21 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms '(4 5 6 7) 2) 6))
     (clojure.test/is (= (~@forms [:a :b :c] 0) :a))
     (clojure.test/is (= (~@forms [1 2 3 4] 1) 2))
     (clojure.test/is (= (~@forms '([1 2] [3 4] [5 6]) 2) [5 6]))))

(defmacro defcheck-22 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms '(1 2 3 3 1)) 5))
     (clojure.test/is (= (~@forms "Hello World") 11))
     (clojure.test/is (= (~@forms [[1 2] [3 4] [5 6]]) 3))
     (clojure.test/is (= (~@forms '(13)) 1))
     (clojure.test/is (= (~@forms '(:a :b :c)) 3))))

(defmacro defcheck-23 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 2 3 4 5]) [5 4 3 2 1]))
     (clojure.test/is (= (~@forms (sorted-set 5 7 2 7)) '(7 5 2)))
     (clojure.test/is (= (~@forms [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]]))))

(defmacro defcheck-24 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 2 3]) 6))
     (clojure.test/is (= (~@forms (list 0 -2 5 5)) 8))
     (clojure.test/is (= (~@forms #{4 2 1}) 7))
     (clojure.test/is (= (~@forms '(0 0 -1)) -1))
     (clojure.test/is (= (~@forms '(1 10 3)) 14))))

(defmacro defcheck-25 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms #{1 2 3 4 5}) '(1 3 5)))
     (clojure.test/is (= (~@forms [4 2 1 6]) '(1)))
     (clojure.test/is (= (~@forms [2 2 4 6]) '()))
     (clojure.test/is (= (~@forms [1 1 1 3]) '(1 1 1 3)))))

(defmacro defcheck-26 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 3) '(1 1 2)))
     (clojure.test/is (= (~@forms 6) '(1 1 2 3 5 8)))
     (clojure.test/is (= (~@forms 8) '(1 1 2 3 5 8 13 21)))))

(defmacro defcheck-27 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (false? (~@forms '(1 2 3 4 5))))
     (clojure.test/is (true? (~@forms "racecar")))
     (clojure.test/is (true? (~@forms [:foo :bar :foo])))
     (clojure.test/is (true? (~@forms '(1 1 3 3 1 1))))
     (clojure.test/is (false? (~@forms '(:a :b :c))))))

(defmacro defcheck-28 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
     (clojure.test/is (= (~@forms ["a" ["b"] "c"]) '("a" "b" "c")))
     (clojure.test/is (= (~@forms '((((:a))))) '(:a)))))

(defmacro defcheck-29 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms "HeLlO, WoRlD!") "HLOWRD"))
     (clojure.test/is (empty? (~@forms "nothing")))
     (clojure.test/is (= (~@forms "$#A(*&987Zf") "AZ"))))

(defmacro defcheck-30 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (apply str (~@forms "Leeeeeerrroyyy")) "Leroy"))
     (clojure.test/is (= (~@forms [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
     (clojure.test/is (= (~@forms [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))))

(defmacro defcheck-31 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
     (clojure.test/is (= (~@forms [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
     (clojure.test/is (= (~@forms [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))))

(defmacro defcheck-32 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 2 3]) '(1 1 2 2 3 3)))
     (clojure.test/is (= (~@forms [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
     (clojure.test/is (= (~@forms [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
     (clojure.test/is (= (~@forms [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))))

(defmacro defcheck-33 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 2 3] 2) '(1 1 2 2 3 3)))
     (clojure.test/is (= (~@forms [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
     (clojure.test/is (= (~@forms [4 5 6] 1) '(4 5 6)))
     (clojure.test/is (= (~@forms [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
     (clojure.test/is (= (~@forms [44 33] 2) [44 44 33 33]))))

(defmacro defcheck-34 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 1 4) '(1 2 3)))
     (clojure.test/is (= (~@forms -2 2) '(-2 -1 0 1)))
     (clojure.test/is (= (~@forms 5 8) '(5 6 7)))))

(defmacro defcheck-35 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (let [x# 5] (+ 2 x#))))
     (clojure.test/is (= ~@forms (let [x# 3, y# 10] (- y# x#))))
     (clojure.test/is (= ~@forms (let [x# 21] (let [y# 3] (/ x# y#)))))))

(defmacro defcheck-36 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 10 (let ~@forms (+ ~'x ~'y))))
     (clojure.test/is (= 4 (let ~@forms (+ ~'y ~'z))))
     (clojure.test/is (= 1 (let ~@forms ~'z)))))

(defmacro defcheck-37 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))))

(defmacro defcheck-38 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 1 8 3 4) 8))
     (clojure.test/is (= (~@forms 30 20) 30))
     (clojure.test/is (= (~@forms 45 67 11) 67))))

(defmacro defcheck-39 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
     (clojure.test/is (= (~@forms [1 2] [3 4 5 6]) '(1 3 2 4)))
     (clojure.test/is (= (~@forms [1 2 3 4] [5]) [1 5]))
     (clojure.test/is (= (~@forms [30 20] [25 15]) [30 25 20 15]))))

(defmacro defcheck-40 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 0 [1 2 3]) [1 0 2 0 3]))
     (clojure.test/is (= (apply str (~@forms ", " ["one" "two" "three"])) "one, two, three"))
     (clojure.test/is (= (~@forms :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))))

(defmacro defcheck-41 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
     (clojure.test/is (= (~@forms [:a :b :c :d :e :f] 2) [:a :c :e]))
     (clojure.test/is (= (~@forms [1 2 3 4 5 6] 4) [1 2 3 5 6]))))

(defmacro defcheck-42 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 1) 1))
     (clojure.test/is (= (~@forms 3) 6))
     (clojure.test/is (= (~@forms 5) 120))
     (clojure.test/is (= (~@forms 8) 40320))))

(defmacro defcheck-43 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
     (clojure.test/is (= (~@forms (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
     (clojure.test/is (= (~@forms (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))))

(defmacro defcheck-44 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 2 [1 2 3 4 5]) '(3 4 5 1 2)))
     (clojure.test/is (= (~@forms -2 [1 2 3 4 5]) '(4 5 1 2 3)))
     (clojure.test/is (= (~@forms 6 [1 2 3 4 5]) '(2 3 4 5 1)))
     (clojure.test/is (= (~@forms 1 '(:a :b :c)) '(:b :c :a)))
     (clojure.test/is (= (~@forms -4 '(:a :b :c)) '(:c :a :b)))))

(defmacro defcheck-45 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (take 5 (iterate #(+ 3 %) 1))))))

(defmacro defcheck-46 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 3 ((~@forms nth) 2 [1 2 3 4 5])))
     (clojure.test/is (= true ((~@forms >) 7 8)))
     (clojure.test/is (= 4 ((~@forms quot) 2 8)))
     (clojure.test/is (= [1 2 3] ((~@forms take) [1 2 3 4 5] 3)))))

(defmacro defcheck-47 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (contains? #{4 5 6} ~@forms))
     (clojure.test/is (contains? [1 1 1 1 1] ~@forms))
     (clojure.test/is (contains? {4 :a 2 :b} ~@forms))
     (clojure.test/is (not (contains? [1 2 4] ~@forms)))))

(defmacro defcheck-48 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (some #{2 7 6} [5 6 7 8])))
     (clojure.test/is (= ~@forms (some #(when (even? %) %) [5 6 7 8])))))

(defmacro defcheck-49 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
     (clojure.test/is (= (~@forms 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
     (clojure.test/is (= (~@forms 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))))

(defmacro defcheck-50 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (set (~@forms [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
     (clojure.test/is (= (set (~@forms [:a "foo" "bar" :b])) #{[:a :b] ["foo" "bar"]}))
     (clojure.test/is (= (set (~@forms [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))))

(defmacro defcheck-51 [name & forms]
  `(deftimedtest ~name 128
     (clojure.test/is (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a# b# & c# :as d#] ~@forms] [a# b# c# d#])))))

(defmacro defcheck-52 [name & forms]
  `(deftimedtest ~name 128
     (clojure.test/is (= [2 4] (let [[~'a ~'b ~'c ~'d ~'e] [0 1 2 3 4]] ~@forms)))))

(defmacro defcheck-53 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 0 1 2 3 0 4 5]) [0 1 2 3]))
     (clojure.test/is (= (~@forms [5 6 1 3 2 7]) [5 6]))
     (clojure.test/is (= (~@forms [2 3 3 4 5]) [3 4 5]))
     (clojure.test/is (= (~@forms [7 6 5 4]) []))))

(defmacro defcheck-54 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
     (clojure.test/is (= (~@forms 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
     (clojure.test/is (= (~@forms 3 (range 8)) '((0 1 2) (3 4 5))))))

(defmacro defcheck-55 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
     (clojure.test/is (= (~@forms [:b :a :b :a :b]) {:a 2, :b 3}))
     (clojure.test/is (= (~@forms '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))))

(defmacro defcheck-56 [name & forms]
  `(deftimedtest ~name 256
     (clojure.test/is (= (~@forms [1 2 1 3 1 2 4]) [1 2 3 4]))
     (clojure.test/is (= (~@forms [:a :a :b :b :c :c]) [:a :b :c]))
     (clojure.test/is (= (~@forms '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
     (clojure.test/is (= (~@forms (range 50)) (range 50)))))

(defmacro defcheck-57 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms ((fn foo# [x#] (when (> x# 0) (conj (foo# (dec x#)) x#))) 5)))))

(defmacro defcheck-58 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= [3 2 1] ((~@forms rest reverse) [1 2 3 4])))
     (clojure.test/is (= 5 ((~@forms (partial + 3) second) [1 2 3 4])))
     (clojure.test/is (= true ((~@forms zero? #(mod % 8) +) 3 5 7 9)))
     (clojure.test/is (= "HELLO" ((~@forms #(.toUpperCase %) #(apply str %) take) 5 "hello world")))))

(defmacro defcheck-59 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= [21 6 1] ((~@forms + max min) 2 3 5 1 6 4)))
     (clojure.test/is (= ["HELLO" 5] ((~@forms #(.toUpperCase %) count) "hello")))
     (clojure.test/is (= [2 6 4] ((~@forms :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))))

(defmacro defcheck-60 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (take 5 (~@forms + (range))) [0 1 3 6 10]))
     (clojure.test/is (= (~@forms conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
     (clojure.test/is (= (last (~@forms * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))))

(defmacro defcheck-61 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
     (clojure.test/is (= (~@forms [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
     (clojure.test/is (= (~@forms [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))))

(defmacro defcheck-62 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (take 5 (~@forms #(* 2 %) 1)) [1 2 4 8 16]))
     (clojure.test/is (= (take 5 (~@forms #(* 2 %) 1)) [1 2 4 8 16]))
     (clojure.test/is (= (take 5 (~@forms #(* 2 %) 1)) [1 2 4 8 16]))))

(defmacro defcheck-63 [name & forms]
  `(deftimedtest ~name 512
     (clojure.test/is (= (~@forms #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
     (clojure.test/is (= (~@forms #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
                        {(/ 2) [[1 2] [2 4] [3 6]], (/ 2 3) [[4 6]]}))
     (clojure.test/is (= (~@forms count [[1] [1 2] [3] [1 2 3] [2 3]])
                        {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))))

(defmacro defcheck-64 [name & forms]
  `(deftimedtest ~name 64
     (clojure.test/is (= 15 (reduce ~@forms [1 2 3 4 5])))
     (clojure.test/is (= 0 (reduce ~@forms [])))
     (clojure.test/is (= 6 (reduce ~@forms 1 [2 3])))))

(defmacro defcheck-65 [name & forms]
  `(deftimedtest ~name 128
     (clojure.test/is (= :map (~@forms {:a 1, :b 2})))
     (clojure.test/is (= :list (~@forms (range (rand-int 20)))))
     (clojure.test/is (= :vector (~@forms [1 2 3 4 5 6])))
     (clojure.test/is (= :set (~@forms #{10 (rand-int 5)})))
     (clojure.test/is (= [:map :set :vector :list] (map ~@forms [{} #{} [] ()])))))

(defmacro defcheck-66 [name & forms]
  `(deftimedtest ~name 128
     (clojure.test/is (= (~@forms 2 4) 2))
     (clojure.test/is (= (~@forms 10 5) 5))
     (clojure.test/is (= (~@forms 5 7) 1))
     (clojure.test/is (= (~@forms 1023 858) 33))))

(defmacro defcheck-67 [name & forms]
  `(deftimedtest ~name 512
     (clojure.test/is (= (~@forms 2) [2 3]))
     (clojure.test/is (= (~@forms 5) [2 3 5 7 11]))
     (clojure.test/is (= (last (~@forms 100)) 541))))

(defmacro defcheck-68 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms
                        (loop [x#      5
                               result# []]
                          (if (> x# 0)
                            (recur (dec x#) (conj result# (+ 2 x#)))
                            result#))))))

(defmacro defcheck-69 [name & forms]
  `(deftimedtest ~name 128
     (clojure.test/is (= (~@forms * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
                        {:a 4, :b 6, :c 20}))
     (clojure.test/is (= (~@forms - {1 10, 2 20} {1 3, 2 10, 3 15})
                        {1 7, 2 10, 3 15}))
     (clojure.test/is (= (~@forms concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
                        {:a [3 4 5], :b [6 7], :c [8 9]}))))

(defmacro defcheck-70 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms "Have a nice day.")
                        ["a" "day" "Have" "nice"]))
     (clojure.test/is (= (~@forms "Clojure is a fun language!")
                        ["a" "Clojure" "fun" "is" "language"]))
     (clojure.test/is (= (~@forms "Fools fall for foolish follies.")
                        ["fall" "follies" "foolish" "Fools" "for"]))))

(defmacro defcheck-71 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms (sort (rest (reverse [2 5 4 1 3 6]))))
                        (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (~@forms))
                        5))))

(defmacro defcheck-72 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms (map inc (take 3 (drop 2 [2 5 4 1 3 6]))))
                        (->> [2 5 4 1 3 6] (drop 2) (take 3) (map inc) (~@forms))
                        11))))

(defmacro defcheck-73 [name & forms]
  `(deftimedtest ~name 64
     (clojure.test/is (= nil (~@forms [[:e :e :e]
                                       [:e :e :e]
                                       [:e :e :e]])))
     (clojure.test/is (= :x (~@forms [[:x :e :o]
                                      [:x :e :e]
                                      [:x :e :o]])))
     (clojure.test/is (= :o (~@forms [[:e :x :e]
                                      [:o :o :o]
                                      [:x :e :x]])))
     (clojure.test/is (= nil (~@forms [[:x :e :o]
                                       [:x :x :e]
                                       [:o :x :o]])))
     (clojure.test/is (= :x (~@forms [[:x :e :e]
                                      [:o :x :e]
                                      [:o :e :x]])))
     (clojure.test/is (= :o (~@forms [[:x :e :o]
                                      [:x :o :e]
                                      [:o :e :x]])))
     (clojure.test/is (= nil (~@forms [[:x :o :x]
                                       [:x :o :x]
                                       [:o :x :o]])))))

(defmacro defcheck-74 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms "4,5,6,7,8,9") "4,9"))
     (clojure.test/is (= (~@forms "15,16,25,36,37") "16,25,36"))))

(defmacro defcheck-75 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 1) 1))
     (clojure.test/is (= (~@forms 10) (count '(1 3 7 9)) 4))
     (clojure.test/is (= (~@forms 40) 16))
     (clojure.test/is (= (~@forms 99) 60))))

(defmacro defcheck-76 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms
                        (letfn
                         [(foo# [x# y#] #(bar# (conj x# y#) y#))
                          (bar# [x# y#] (if (> (last x#) 10)
                                       x#
                                       #(foo# x# (+ 2 y#))))]
                          (trampoline foo# [] 1))))))

(defmacro defcheck-77 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms ["meat" "mat" "team" "mate" "eat"])
                        #{#{"meat" "team" "mate"}}))
     (clojure.test/is (= (~@forms ["veer" "lake" "item" "kale" "mite" "ever"])
                        #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))))

(defmacro defcheck-78 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (letfn [(triple# [x#] #(sub-two# (* 3 x#)))
                                 (sub-two# [x#] #(stop?# (- x# 2)))
                                 (stop?# [x#] (if (> x# 50) x# #(triple# x#)))]
                           (~@forms triple# 2))))
     (clojure.test/is (= (letfn [(my-even?# [x#] (if (zero? x#) true #(my-odd?# (dec x#))))
                                 (my-odd?# [x#] (if (zero? x#) false #(my-even?# (dec x#))))]
                           (map (partial ~@forms my-even?#) (range 6)))
                        [true false true false true false]))))

(defmacro defcheck-79 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 7 (~@forms '([1]
                                      [2 4]
                                      [5 1 4]
                                      [2 3 4 5]))))
     (clojure.test/is (= 20 (~@forms '([3]
                                       [2 4]
                                       [1 9 3]
                                       [9 9 2 4]
                                       [4 6 6 7 8]
                                       [5 7 3 5 1 4]))))))

(defmacro defcheck-80 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 6) true))
     (clojure.test/is (= (~@forms 7) false))
     (clojure.test/is (= (~@forms 496) true))
     (clojure.test/is (= (~@forms 500) false))
     (clojure.test/is (= (~@forms 8128) true))))

(defmacro defcheck-81 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms #{0 1 2 3} #{2 3 4 5}) #{2 3}))
     (clojure.test/is (= (~@forms #{0 1 2} #{3 4 5}) #{}))
     (clojure.test/is (= (~@forms #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))))

(defmacro defcheck-82 [name & forms]
  `(deftimedtest ~name 1024
     (clojure.test/is (= true (~@forms #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
     (clojure.test/is (= false (~@forms #{"cot" "hot" "bat" "fat"})))
     (clojure.test/is (= false (~@forms #{"to" "top" "stop" "tops" "toss"})))
     (clojure.test/is (= true (~@forms #{"spout" "do" "pot" "pout" "spot" "dot"})))
     (clojure.test/is (= true (~@forms #{"share" "hares" "shares" "hare" "are"})))
     (clojure.test/is (= false (~@forms #{"share" "hares" "hare" "are"})))))

(defmacro defcheck-83 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= false (~@forms false false)))
     (clojure.test/is (= true (~@forms true false)))
     (clojure.test/is (= false (~@forms true)))
     (clojure.test/is (= true (~@forms false true false)))
     (clojure.test/is (= false (~@forms true true true)))
     (clojure.test/is (= true (~@forms true true true false)))))

(defmacro defcheck-84 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (let [divides# #{[8 4] [9 3] [4 2] [27 9]}]
                        (= (~@forms divides#) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))
     (clojure.test/is (let [more-legs#
                            #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
                        (= (~@forms more-legs#)
                          #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
                            ["spider" "cat"] ["spider" "man"] ["spider" "snake"]})))
     (clojure.test/is (let [progeny#
                            #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
                        (= (~@forms progeny#)
                          #{["father" "son"] ["father" "grandson"]
                            ["uncle" "cousin"] ["son" "grandson"]})))))

(defmacro defcheck-85 [name & forms]
  `(deftimedtest ~name 2048
     (clojure.test/is (= (~@forms #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))
     (clojure.test/is (= (~@forms #{}) #{#{}}))
     (clojure.test/is (= (~@forms #{1 2 3})
                        #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))
     (clojure.test/is (= (count (~@forms (into #{} (range 10)))) 1024))))

(defmacro defcheck-86 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 7) true))
     (clojure.test/is (= (~@forms 986543210) true))
     (clojure.test/is (= (~@forms 2) false))
     (clojure.test/is (= (~@forms 3) false))))

(defmacro defcheck-88 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
     (clojure.test/is (= (~@forms #{:a :b :c} #{}) #{:a :b :c}))
     (clojure.test/is (= (~@forms #{} #{4 5 6}) #{4 5 6}))
     (clojure.test/is (= (~@forms #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))))

(defmacro defcheck-89 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= true (~@forms [[:a :b]])))
     (clojure.test/is (= false (~@forms [[:a :a] [:b :b]])))
     (clojure.test/is (= false (~@forms [[:a :b] [:a :b] [:a :c] [:c :a]
                                         [:a :d] [:b :d] [:c :d]])))
     (clojure.test/is (= true (~@forms [[1 2] [2 3] [3 4] [4 1]])))
     (clojure.test/is (= true (~@forms [[:a :b] [:a :c] [:c :b] [:a :e]
                                        [:b :e] [:a :d] [:b :d] [:c :e]
                                        [:d :e] [:c :f] [:d :f]])))
     (clojure.test/is (= false (~@forms [[1 2] [2 3] [2 4] [2 5]])))))

(defmacro defcheck-90 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
                        #{["ace" "♠"] ["ace" "♥"] ["ace" "♦"] ["ace" "♣"]
                          ["king" "♠"] ["king" "♥"] ["king" "♦"] ["king" "♣"]
                          ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
     (clojure.test/is (= (~@forms #{1 2 3} #{4 5})
                        #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
     (clojure.test/is (= 300 (count (~@forms (into #{} (range 10))
                                     (into #{} (range 30))))))))

(defmacro defcheck-91 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= true (~@forms #{[:a :a]})))
     (clojure.test/is (= true (~@forms #{[:a :b]})))
     (clojure.test/is (= false (~@forms #{[1 2] [2 3] [3 1]
                                          [4 5] [5 6] [6 4]})))
     (clojure.test/is (= true (~@forms #{[1 2] [2 3] [3 1]
                                         [4 5] [5 6] [6 4] [3 4]})))
     (clojure.test/is (= false (~@forms #{[:a :b] [:b :c] [:c :d]
                                          [:x :y] [:d :a] [:b :e]})))
     (clojure.test/is (= true (~@forms #{[:a :b] [:b :c] [:c :d]
                                         [:x :y] [:d :a] [:b :e] [:x :a]})))))

(defmacro defcheck-92 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 14 (~@forms "XIV")))
     (clojure.test/is (= 827 (~@forms "DCCCXXVII")))
     (clojure.test/is (= 3999 (~@forms "MMMCMXCIX")))
     (clojure.test/is (= 48 (~@forms "XLVIII")))))

(defmacro defcheck-93 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [["Do"] ["Nothing"]])
                        [["Do"] ["Nothing"]]))
     (clojure.test/is (= (~@forms [[[[:a :b]]] [[:c :d]] [:e :f]])
                        [[:a :b] [:c :d] [:e :f]]))
     (clojure.test/is (= (~@forms '((1 2) ((3 4) ((((5 6)))))))
                        '((1 2) (3 4) (5 6))))))

(defmacro defcheck-94 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms ["      "
                                   " ##   "
                                   " ##   "
                                   "   ## "
                                   "   ## "
                                   "      "])
                        ["      "
                         " ##   "
                         " #    "
                         "    # "
                         "   ## "
                         "      "]))
     (clojure.test/is (= (~@forms ["     "
                                   "     "
                                   " ### "
                                   "     "
                                   "     "])
                        ["     "
                         "  #  "
                         "  #  "
                         "  #  "
                         "     "]))
     (clojure.test/is (= (~@forms ["      "
                                   "      "
                                   "  ### "
                                   " ###  "
                                   "      "
                                   "      "])
                        ["      "
                         "   #  "
                         " #  # "
                         " #  # "
                         "  #   "
                         "      "]))))

(defmacro defcheck-95 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms '(:a (:b nil nil) nil))
                        true))
     (clojure.test/is (= (~@forms '(:a (:b nil nil)))
                        false))
     (clojure.test/is (= (~@forms [1 nil [2 [3 nil nil] [4 nil nil]]])
                        true))
     (clojure.test/is (= (~@forms [1 [2 nil nil] [3 nil nil] [4 nil nil]])
                        false))
     (clojure.test/is (= (~@forms [1 [2 [3 [4 nil nil] nil] nil] nil])
                        true))
     (clojure.test/is (= (~@forms [1 [2 [3 [4 false nil] nil] nil] nil])
                        false))
     (clojure.test/is (= (~@forms '(:a nil ()))
                        false))))

(defmacro defcheck-96 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms '(:a (:b nil nil) (:b nil nil))) true))
     (clojure.test/is (= (~@forms '(:a (:b nil nil) nil)) false))
     (clojure.test/is (= (~@forms '(:a (:b nil nil) (:c nil nil))) false))
     (clojure.test/is (= (~@forms [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                                   [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
                        true))
     (clojure.test/is (= (~@forms [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                                   [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
                        false))
     (clojure.test/is (= (~@forms [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                                   [2 [3 nil [4 [6 nil nil] nil]] nil]])
                        false))))

(defmacro defcheck-97 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 1) [1]))
     (clojure.test/is (= (map ~@forms (range 1 6))
                        [[1]
                         [1 1]
                         [1 2 1]
                         [1 3 3 1]
                         [1 4 6 4 1]]))
     (clojure.test/is (= (~@forms 11)
                        [1 10 45 120 210 252 210 120 45 10 1]))))

(defmacro defcheck-98 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms #(* % %) #{-2 -1 0 1 2})
                        #{#{0} #{1 -1} #{2 -2}}))
     (clojure.test/is (= (~@forms #(rem % 3) #{0 1 2 3 4 5})
                        #{#{0 3} #{1 4} #{2 5}}))
     (clojure.test/is (= (~@forms identity #{0 1 2 3 4})
                        #{#{0} #{1} #{2} #{3} #{4}}))
     (clojure.test/is (= (~@forms (constantly true) #{0 1 2 3 4})
                        #{#{0 1 2 3 4}}))))

(defmacro defcheck-99 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 1 1) [1]))
     (clojure.test/is (= (~@forms 99 9) [8 9 1]))
     (clojure.test/is (= (~@forms 999 99) [9 8 9 0 1]))))

(defmacro defcheck-100 [name & forms]
  `(deftimedtest ~name 512
     (clojure.test/is (== (~@forms 2 3) 6))
     (clojure.test/is (== (~@forms 5 3 7) 105))
     (clojure.test/is (== (~@forms 5 6) 30))
     (clojure.test/is (== (~@forms 9 2) 18))
     (clojure.test/is (== (~@forms 5 21 2) 210))))

(defmacro defcheck-101 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms "kitten" "sitting") 3))
     (clojure.test/is (= (~@forms "closure" "clojure") (~@forms "clojure" "closure") 1))
     (clojure.test/is (= (~@forms "xyx" "xyyyx") 2))
     (clojure.test/is (= (~@forms "" "123456") 6))
     (clojure.test/is (= (~@forms "Clojure" "Clojure") (~@forms "" "") (~@forms [] []) 0))
     (clojure.test/is (= (~@forms [1 2 3 4] [0 2 3 4 5]) 2))
     (clojure.test/is (= (~@forms '(:a :b :c :d) '(:a :d)) 2))
     (clojure.test/is (= (~@forms "ttttattttctg" "tcaaccctaccat") 10))
     (clojure.test/is (= (~@forms "gaattctaatctc" "caaacaaaaaattt") 9))))

(defmacro defcheck-102 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms "something") "something"))
     (clojure.test/is (= (~@forms "multi-word-key") "multiWordKey"))
     (clojure.test/is (= (~@forms "leaveMeAlone") "leaveMeAlone"))))

(defmacro defcheck-103 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 1 #{4 5 6}) #{#{4} #{5} #{6}}))
     (clojure.test/is (= (~@forms 10 #{4 5 6}) #{}))
     (clojure.test/is (= (~@forms 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))
     (clojure.test/is (= (~@forms 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                                                    #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))
     (clojure.test/is (= (~@forms 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))
     (clojure.test/is (= (~@forms 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                                                 #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))))

(defmacro defcheck-104 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= "I" (~@forms 1)))
     (clojure.test/is (= "XXX" (~@forms 30)))
     (clojure.test/is (= "IV" (__ 4)))
     (clojure.test/is (= "CXL" (~@forms 140)))
     (clojure.test/is (= "DCCCXXVII" (~@forms 827)))
     (clojure.test/is (= "MMMCMXCIX" (~@forms 3999)))
     (clojure.test/is (= "XLVIII" (~@forms 48)))))

(defmacro defcheck-105 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= {} (~@forms [])))
     (clojure.test/is (= {:a [1]} (~@forms [:a 1])))
     (clojure.test/is (= {:a [1], :b [2]} (~@forms [:a 1, :b 2])))
     (clojure.test/is (= {:a [1 2 3], :b [], :c [4]} (~@forms [:a 1 2 3 :b :c 4])))))

(defmacro defcheck-106 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 1 (~@forms 1 1)))
     (clojure.test/is (= 3 (~@forms 3 12)))
     (clojure.test/is (= 3 (~@forms 12 3)))
     (clojure.test/is (= 3 (~@forms 5 9)))
     (clojure.test/is (= 9 (~@forms 9 2)))
     (clojure.test/is (= 5 (~@forms 9 12)))))

(defmacro defcheck-107 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 256 ((~@forms 2) 16),
                        ((~@forms 8) 2)))
     (clojure.test/is (= [1 8 27 64] (map (~@forms 3) [1 2 3 4])))
     (clojure.test/is (= [1 2 4 8 16] (map #((~@forms %) 2) [0 1 2 3 4])))))

(defmacro defcheck-108 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 3 (~@forms [3 4 5])))
     (clojure.test/is (= 4 (~@forms [1 2 3 4 5 6 7] [0.5 3/2 4 19])))
     (clojure.test/is (= 7 (~@forms (range) (range 0 100 7/6) [2 3 5 7 11 13])))
     (clojure.test/is (= 64 (~@forms (map #(* % % %) (range)) ;; perfect cubes
                             (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                             (iterate inc 20)))             ;; at least as large as 20
       )))


(defmacro defcheck-110 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= [[1 1] [2 1] [1 2 1 1]] (take 3 (~@forms [1]))))
     (clojure.test/is (= [3 1 2 4] (first (~@forms [1 1 1 4 4]))))
     (clojure.test/is (= [1 1 1 3 2 1 3 2 1 1] (nth (~@forms [1]) 6)))
     (clojure.test/is (= 338 (count (nth (~@forms [3 2]) 15))))))

(defmacro defcheck-111 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= true (~@forms "the" ["_ # _ _ e"])))
     (clojure.test/is (= false (~@forms "the" ["c _ _ _"
                                               "d _ # e"
                                               "r y _ _"])))
     (clojure.test/is (= true (~@forms "joy" ["c _ _ _"
                                              "d _ # e"
                                              "r y _ _"])))
     (clojure.test/is (= false (~@forms "joy" ["c o n j"
                                               "_ _ y _"
                                               "r _ _ #"])))
     (clojure.test/is (= true (~@forms "clojure" ["_ _ _ # j o y"
                                                  "_ _ o _ _ _ _"
                                                  "_ _ f _ # _ _"])))))

(defmacro defcheck-112 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 10 [1 2 [3 [4 5] 6] 7])
                        '(1 2 (3 (4)))))
     (clojure.test/is (= (~@forms 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
                        '(1 2 (3 (4 (5 (6 (7))))))))
     (clojure.test/is (= (~@forms 9 (range))
                        '(0 1 2 3)))
     (clojure.test/is (= (~@forms 1 [[[[[1]]]]])
                        '(((((1)))))))
     (clojure.test/is (= (~@forms 0 [1 2 [3 [4 5] 6] 7])
                        '()))
     (clojure.test/is (= (~@forms 0 [0 0 [0 [0]]])
                        '(0 0 (0 (0)))))
     (clojure.test/is (= (~@forms 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
                        '(-10 (1 (2 3 (4))))))))

(defmacro defcheck-113 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= "1, 2, 3" (str (~@forms 2 1 3))))
     (clojure.test/is (= '(2 1 3) (seq (~@forms 2 1 3))))
     (clojure.test/is (= '(2 1 3) (seq (~@forms 2 1 3 3 1 2))))
     (clojure.test/is (= '(1) (seq (apply ~@forms (repeat 5 1)))))
     (clojure.test/is (= "1, 1, 1, 1, 1" (str (apply ~@forms (repeat 5 1)))))
     (clojure.test/is (and (= nil (seq (~@forms)))
                           (= "" (str (~@forms)))))))

(defmacro defcheck-114 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= [2 3 5 7 11 13]
                        (~@forms 4 #(= 2 (mod % 3))
                         [2 3 5 7 11 13 17 19 23])))
     (clojure.test/is (= ["this" "is" "a" "sentence"]
                        (~@forms 3 #(some #{\i} %)
                         ["this" "is" "a" "sentence" "i" "wrote"])))
     (clojure.test/is (= ["this" "is"]
                        (~@forms 1 #{"a"}
                         ["this" "is" "a" "sentence" "i" "wrote"])))))

(defmacro defcheck-115 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= true (~@forms 11)))
     (clojure.test/is (= true (~@forms 121)))
     (clojure.test/is (= false (~@forms 123)))
     (clojure.test/is (= true (~@forms 0)))
     (clojure.test/is (= false (~@forms 88099)))
     (clojure.test/is (= true (~@forms 89098)))
     (clojure.test/is (= true (~@forms 89089)))
     (clojure.test/is (= (take 20 (filter ~@forms (range)))
                        [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]))))

(defmacro defcheck-116 [name & forms]
  `(deftimedtest ~name 4096
     (clojure.test/is (= false (~@forms 4)))
     (clojure.test/is (= true (~@forms 563)))
     (clojure.test/is (= 1103 (nth (filter ~@forms (range)) 15)))))

(defmacro defcheck-117 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= true (~@forms ["M   C"])))
     (clojure.test/is (= false (~@forms ["M # C"])))
     (clojure.test/is (= true (~@forms ["#######"
                                        "#     #"
                                        "#  #  #"
                                        "#M # C#"
                                        "#######"])))
     (clojure.test/is (= false (~@forms ["########"
                                         "#M  #  #"
                                         "#   #  #"
                                         "# # #  #"
                                         "#   #  #"
                                         "#  #   #"
                                         "#  # # #"
                                         "#  #   #"
                                         "#  #  C#"
                                         "########"])))
     (clojure.test/is (= false (~@forms ["M     "
                                         "      "
                                         "      "
                                         "      "
                                         "    ##"
                                         "    #C"])))
     (clojure.test/is (= true (~@forms ["C######"
                                        " #     "
                                        " #   # "
                                        " #   #M"
                                        "     # "])))
     (clojure.test/is (= true (~@forms ["C# # # #"
                                        "        "
                                        "# # # # "
                                        "        "
                                        " # # # #"
                                        "        "
                                        "# # # #M"])))))

(defmacro defcheck-118 [name & forms]
  `(deftimedtest ~name 64
     (clojure.test/is (= [3 4 5 6 7]
                        (~@forms inc [2 3 4 5 6])))
     (clojure.test/is (= (repeat 10 nil)
                        (~@forms (fn [_#] nil) (range 10))))
     (clojure.test/is (= [10 11]
                        (->> (~@forms inc (range 20))
                          (drop (dec 10))
                          (take 2))))))

(defmacro defcheck-119 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms :x [[:o :e :e]
                                      [:o :x :o]
                                      [:x :x :e]])
                        #{[2 2] [0 1] [0 2]}))
     (clojure.test/is (= (~@forms :x [[:x :o :o]
                                      [:x :x :e]
                                      [:e :o :e]])
                        #{[2 2] [1 2] [2 0]}))
     (clojure.test/is (= (~@forms :x [[:x :e :x]
                                      [:o :x :o]
                                      [:e :o :e]])
                        #{[2 2] [0 1] [2 0]}))
     (clojure.test/is (= (~@forms :x [[:x :x :o]
                                      [:e :e :e]
                                      [:e :e :e]])
                        #{}))
     (clojure.test/is (= (~@forms :o [[:x :x :o]
                                      [:o :e :o]
                                      [:x :e :e]])
                        #{[2 2] [1 1]}))))

(defmacro defcheck-120 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 8 (~@forms (range 10))))
     (clojure.test/is (= 19 (~@forms (range 30))))
     (clojure.test/is (= 50 (~@forms (range 100))))
     (clojure.test/is (= 50 (~@forms (range 1000))))))

(defmacro defcheck-121 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 2 ((~@forms '(/ a b))
                            '{b 8 a 16})))
     (clojure.test/is (= 8 ((~@forms '(+ a b 2))
                            '{a 2 b 4})))
     (clojure.test/is (= [6 0 -4]
                        (map (~@forms '(* (+ 2 a)
                                         (- 10 b)))
                          '[{a 1 b 8}
                            {b 5 a -2}
                            {a 2 b 11}])))
     (clojure.test/is (= 1 ((~@forms '(/ (+ x 2)
                                        (* 3 (+ y 1))))
                            '{x 4 y 1})))))

(defmacro defcheck-122 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 0 (~@forms "0")))
     (clojure.test/is (= 7 (~@forms "111")))
     (clojure.test/is (= 8 (~@forms "1000")))
     (clojure.test/is (= 9 (~@forms "1001")))
     (clojure.test/is (= 255 (~@forms "11111111")))
     (clojure.test/is (= 1365 (~@forms "10101010101")))
     (clojure.test/is (= 65535 (~@forms "1111111111111111")))))

(defmacro defcheck-124 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
                        (~@forms '[[e e e e]
                                   [e w b e]
                                   [e b w e]
                                   [e e e e]] 'w)))
     (clojure.test/is (= {[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}
                        (~@forms '[[e e e e]
                                   [e w b e]
                                   [w w w e]
                                   [e e e e]] 'b)))
     (clojure.test/is (= {[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}
                        (~@forms '[[e e e e]
                                   [e w b e]
                                   [w w b e]
                                   [e e b e]] 'w)))
     (clojure.test/is (= {[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}
                        (~@forms '[[e e w e]
                                   [b b w e]
                                   [b w w e]
                                   [b w w w]] 'b)))))

(defmacro defcheck-125 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (str '~@forms) (~@forms)))))

#_(defmacro defcheck-126 [name & forms]
    `(deftimedtest ~name 32
       (clojure.test/is (let [x __]
                          (and (= (class x) x) x)))))

(defmacro defcheck-127 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 10 (~@forms [15 15 15 15 15])))
     (clojure.test/is (= 15 (~@forms [1 3 7 15 31])))
     (clojure.test/is (= 3 (~@forms [3 3])))
     (clojure.test/is (= 4 (~@forms [7 3])))
     (clojure.test/is (= 6 (~@forms [17 22 6 14 22])))
     (clojure.test/is (= 9 (~@forms [18 7 14 14 6 3])))
     (clojure.test/is (= nil (~@forms [21 10 21 10])))
     (clojure.test/is (= nil (~@forms [0 31 0 31 0])))))

(defmacro defcheck-128 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= {:suit :diamond :rank 10} (~@forms "DQ")))
     (clojure.test/is (= {:suit :heart :rank 3} (~@forms "H5")))
     (clojure.test/is (= {:suit :club :rank 12} (~@forms "CA")))
     (clojure.test/is (= (range 13) (map (comp :rank ~@forms str)
                                      '[S2 S3 S4 S5 S6 S7
                                        S8 S9 ST SJ SQ SK SA])))))

(defmacro defcheck-130 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= '(n)
                        (~@forms 'n '(n))))
     (clojure.test/is (= '(a (t (e)))
                        (~@forms 'a '(t (e) (a)))))
     (clojure.test/is (= '(e (t (a)))
                        (~@forms 'e '(a (t (e))))))
     (clojure.test/is (= '(a (b (c)))
                        (~@forms 'a '(c (b (a))))))
     (clojure.test/is (= '(d
                            (b
                              (c)
                              (e)
                              (a
                                (f
                                  (g)
                                  (h)))))
                        (~@forms 'd '(a
                                       (b
                                         (c)
                                         (d)
                                         (e))
                                       (f
                                         (g)
                                         (h))))))
     (clojure.test/is (= '(c
                            (d)
                            (e)
                            (b
                              (f
                                (g)
                                (h))
                              (a
                                (i
                                  (j
                                    (k)
                                    (l))
                                  (m
                                    (n)
                                    (o))))))
                        (~@forms 'c '(a
                                       (b
                                         (c
                                           (d)
                                           (e))
                                         (f
                                           (g)
                                           (h)))
                                       (i
                                         (j
                                           (k)
                                           (l))
                                         (m
                                           (n)
                                           (o)))))))))

(defmacro defcheck-131 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= true (~@forms #{-1 1 99}
                               #{-2 2 888}
                               #{-3 3 7777})))
     (clojure.test/is (= false (~@forms #{1}
                                #{2}
                                #{3}
                                #{4})))
     (clojure.test/is (= true (~@forms #{1})))
     (clojure.test/is (= false (~@forms #{1 -3 51 9}
                                #{0}
                                #{9 2 81 33})))
     (clojure.test/is (= true (~@forms #{1 3 5}
                               #{9 11 4}
                               #{-3 12 3}
                               #{-3 4 -2 10})))
     (clojure.test/is (= false (~@forms #{-1 -2 -3 -4 -5 -6}
                                #{1 2 3 4 5 6 7 8 9})))
     (clojure.test/is (= true (~@forms #{1 3 5 7}
                               #{2 4 6 8})))
     (clojure.test/is (= true (~@forms #{-1 3 -5 7 -9 11 -13 15}
                               #{1 -3 5 -7 9 -11 13 -15}
                               #{1 -1 2 -2 4 -4 8 -8})))
     (clojure.test/is (= true (~@forms #{-10 9 -8 7 -6 5 -4 3 -2 1}
                               #{10 -9 8 -7 6 -5 4 -3 2 -1})))))

(defmacro defcheck-132 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= '(1 :less 6 :less 7 4 3) (~@forms < :less [1 6 7 4 3])))
     (clojure.test/is (= '(2) (~@forms > :more [2])))
     (clojure.test/is (= [0 1 :x 2 :x 3 :x 4] (~@forms #(and (pos? %) (< % %2)) :x (range 5))))
     (clojure.test/is (empty? (~@forms > :more ())))
     (clojure.test/is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
                        (take 12 (->> [0 1]
                                   (iterate (fn [[a b]] [b (+ a b)]))
                                   (map first)              ; fibonacci numbers
                                   (~@forms (fn [a b]       ; both even or both odd
                                              (= (mod a 2) (mod b 2)))
                                    :same)))))))

(defmacro defcheck-134 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (true? (~@forms :a {:a nil :b 2})))
     (clojure.test/is (false? (~@forms :b {:a nil :b 2})))
     (clojure.test/is (false? (~@forms :c {:a nil :b 2})))))

(defmacro defcheck-135 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 7 (~@forms 2 + 5)))
     (clojure.test/is (= 42 (~@forms 38 + 48 - 2 / 2)))
     (clojure.test/is (= 8 (~@forms 10 / 2 - 1 * 2)))
     (clojure.test/is (= 72 (~@forms 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))))

(defmacro defcheck-137 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= [1 2 3 4 5 0 1] (~@forms 1234501 10)))
     (clojure.test/is (= [0] (~@forms 0 11)))
     (clojure.test/is (= [1 0 0 1] (~@forms 9 2)))
     (clojure.test/is (= [1 0] (let [n# (rand-int 100000)] (~@forms n# n#))))
     (clojure.test/is (= [16 18 5 24 15 1] (~@forms 2147483647 42)))))

(defmacro defcheck-138 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 2 2) ["2"]))
     (clojure.test/is (= (~@forms 2 4) [" 2 "
                                        "* 4"
                                        " * "]))
     (clojure.test/is (= (~@forms 3 81) [" 3 "
                                         "1 9"
                                         " 8 "]))
     (clojure.test/is (= (~@forms 4 20) [" 4 "
                                         "* 1"
                                         " 6 "]))
     (clojure.test/is (= (~@forms 2 256) ["  6  "
                                          " 5 * "
                                          "2 2 *"
                                          " 6 4 "
                                          "  1  "]))
     (clojure.test/is (= (~@forms 10 10000) ["   0   "
                                             "  1 0  "
                                             " 0 1 0 "
                                             "* 0 0 0"
                                             " * 1 * "
                                             "  * *  "
                                             "   *   "]))))

(defmacro defcheck-140 [name & forms]
  `(deftimedtest ~name 1024
     (clojure.test/is (= (~@forms #{#{'a 'B 'C 'd}
                                    #{'A 'b 'c 'd}
                                    #{'A 'b 'c 'D}
                                    #{'A 'b 'C 'd}
                                    #{'A 'b 'C 'D}
                                    #{'A 'B 'c 'd}
                                    #{'A 'B 'c 'D}
                                    #{'A 'B 'C 'd}})
                        #{#{'A 'c}
                          #{'A 'b}
                          #{'B 'C 'd}}))
     (clojure.test/is (= (~@forms #{#{'A 'B 'C 'D}
                                    #{'A 'B 'C 'd}})
                        #{#{'A 'B 'C}}))
     (clojure.test/is (= (~@forms #{#{'a 'b 'c 'd}
                                    #{'a 'B 'c 'd}
                                    #{'a 'b 'c 'D}
                                    #{'a 'B 'c 'D}
                                    #{'A 'B 'C 'd}
                                    #{'A 'B 'C 'D}
                                    #{'A 'b 'C 'd}
                                    #{'A 'b 'C 'D}})
                        #{#{'a 'c}
                          #{'A 'C}}))
     (clojure.test/is (= (~@forms #{#{'a 'b 'c}
                                    #{'a 'B 'c}
                                    #{'a 'b 'C}
                                    #{'a 'B 'C}})
                        #{#{'a}}))
     (clojure.test/is (= (~@forms #{#{'a 'B 'c 'd}
                                    #{'A 'B 'c 'D}
                                    #{'A 'b 'C 'D}
                                    #{'a 'b 'c 'D}
                                    #{'a 'B 'C 'D}
                                    #{'A 'B 'C 'd}})
                        #{#{'a 'B 'c 'd}
                          #{'A 'B 'c 'D}
                          #{'A 'b 'C 'D}
                          #{'a 'b 'c 'D}
                          #{'a 'B 'C 'D}
                          #{'A 'B 'C 'd}}))
     (clojure.test/is (= (~@forms #{#{'a 'b 'c 'd}
                                    #{'a 'B 'c 'd}
                                    #{'A 'B 'c 'd}
                                    #{'a 'b 'c 'D}
                                    #{'a 'B 'c 'D}
                                    #{'A 'B 'c 'D}})
                        #{#{'a 'c}
                          #{'B 'c}}))
     (clojure.test/is (= (~@forms #{#{'a 'B 'c 'd}
                                    #{'A 'B 'c 'd}
                                    #{'a 'b 'c 'D}
                                    #{'a 'b 'C 'D}
                                    #{'A 'b 'c 'D}
                                    #{'A 'b 'C 'D}
                                    #{'a 'B 'C 'd}
                                    #{'A 'B 'C 'd}})
                        #{#{'B 'd}
                          #{'b 'D}}))
     (clojure.test/is (= (~@forms #{#{'a 'b 'c 'd}
                                    #{'A 'b 'c 'd}
                                    #{'a 'B 'c 'D}
                                    #{'A 'B 'c 'D}
                                    #{'a 'B 'C 'D}
                                    #{'A 'B 'C 'D}
                                    #{'a 'b 'C 'd}
                                    #{'A 'b 'C 'd}})
                        #{#{'B 'D}
                          #{'b 'd}}))))

(defmacro defcheck-141 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (let [notrump (~@forms nil)]
                        (and (= {:suit :club :rank 9} (notrump [{:suit :club :rank 4}
                                                                {:suit :club :rank 9}]))
                             (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                                                 {:suit :club :rank 10}])))))
     (clojure.test/is (= {:suit :club :rank 10} ((~@forms :club) [{:suit :spade :rank 2}
                                                                  {:suit :club :rank 10}])))
     (clojure.test/is (= {:suit :heart :rank 8}
                        ((~@forms :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                                           {:suit :diamond :rank 10} {:suit :heart :rank 4}])))))

(defmacro defcheck-143 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 0 (~@forms [0 1 0] [1 0 0])))
     (clojure.test/is (= 3 (~@forms [1 1 1] [1 1 1])))
     (clojure.test/is (= 32 (~@forms [1 2 3] [4 5 6])))
     (clojure.test/is (= 256 (~@forms [2 5 6] [100 10 1])))))

(defmacro defcheck-144 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (take 3 (~@forms 3.14 int double)) [3.14 3 3.0]))
     (clojure.test/is (= (take 5 (~@forms 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
     (clojure.test/is (= (take 12 (~@forms 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))))

(defmacro defcheck-145 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (for [x (range 40)
                                       :when (= 1 (rem x 4))]
                                   x)))
     (clojure.test/is (= ~@forms (for [x (iterate #(+ 4 %) 0)
                                       :let [z (inc x)]
                                       :while (< z 40)]
                                   z)))
     (clojure.test/is (= ~@forms (for [[x y] (partition 2 (range 20))]
                                   (+ x y))))))

(defmacro defcheck-146 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms '{a {p 1, q 2}
                                    b {m 3, n 4}})
                        '{[a p] 1, [a q] 2
                          [b m] 3, [b n] 4}))
     (clojure.test/is (= (~@forms '{[1] {a b c d}
                                    [2] {q r s t u v w x}})
                        '{[[1] a] b, [[1] c] d,
                          [[2] q] r, [[2] s] t,
                          [[2] u] v, [[2] w] x}))
     (clojure.test/is (= (~@forms '{m {1 [a b c] 3 nil}})
                        '{[m 1] [a b c], [m 3] nil}))))

(defmacro defcheck-147 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (second (~@forms [2 3 2])) [2 5 5 2]))
     (clojure.test/is (= (take 5 (~@forms [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
     (clojure.test/is (= (take 2 (~@forms [3 1 2])) [[3 1 2] [3 4 3 2]]))
     (clojure.test/is (= (take 100 (~@forms [2 4 2])) (rest (take 101 (~@forms [2 2])))))))

(defmacro defcheck-148 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 0 (~@forms 3 17 11)))
     (clojure.test/is (= 23 (~@forms 10 3 5)))
     (clojure.test/is (= 233168 (~@forms 1000 3 5)))
     (clojure.test/is (= "2333333316666668" (str (~@forms 100000000 3 5))))
     (clojure.test/is (= "110389610389889610389610"
                        (str (~@forms (* 10000 10000 10000) 7 11))))
     (clojure.test/is (= "1277732511922987429116"
                        (str (~@forms (* 10000 10000 10000) 757 809))))
     (clojure.test/is (= "4530161696788274281"
                        (str (~@forms (* 10000 10000 1000) 1597 3571))))))

(defmacro defcheck-150 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (take 26 (~@forms 0))
                        [0 1 2 3 4 5 6 7 8 9
                         11 22 33 44 55 66 77 88 99
                         101 111 121 131 141 151 161]))
     (clojure.test/is (= (take 16 (~@forms 162))
                        [171 181 191 202
                         212 222 232 242
                         252 262 272 282
                         292 303 313 323]))
     (clojure.test/is (= (take 6 (~@forms 1234550000))
                        [1234554321 1234664321 1234774321
                         1234884321 1234994321 1235005321]))
     (clojure.test/is (= (first (~@forms (* 111111111 111111111)))
                        (* 111111111 111111111)))
     (clojure.test/is (= (set (take 199 (~@forms 0)))
                        (set (map #(first (~@forms %)) (range 0 10000)))))
     (clojure.test/is (= true
                        (apply < (take 6666 (~@forms 9999999)))))
     (clojure.test/is (= (nth (~@forms 0) 10101)
                        9102019))))

(defmacro defcheck-152 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms '[[A B C D]
                                    [A C D B]
                                    [B A D C]
                                    [D C A B]])
                        {}))
     (clojure.test/is (= (~@forms '[[A B C D E F]
                                    [B C D E F A]
                                    [C D E F A B]
                                    [D E F A B C]
                                    [E F A B C D]
                                    [F A B C D E]])
                        {6 1}))
     (clojure.test/is (= (~@forms '[[A B C D]
                                    [B A D C]
                                    [D C B A]
                                    [C D A B]])
                        {4 1, 2 4}))
     (clojure.test/is (= (~@forms '[[B D A C B]
                                    [D A B C A]
                                    [A B C A B]
                                    [B C A B C]
                                    [A D B C A]])
                        {3 3}))
     (clojure.test/is (= (~@forms [[2 4 6 3]
                                   [3 4 6 2]
                                   [6 2 4]])
                        {}))
     (clojure.test/is (= (~@forms [[1]
                                   [1 2 1 2]
                                   [2 1 2 1]
                                   [1 2 1 2]
                                   []])
                        {2 2}))
     (clojure.test/is (= (~@forms [[3 1 2]
                                   [1 2 3 1 3 4]
                                   [2 3 1 3]])
                        {3 1, 2 2}))
     (clojure.test/is (= (~@forms [[8 6 7 3 2 5 1 4]
                                   [6 8 3 7]
                                   [7 3 8 6]
                                   [3 7 6 8 1 4 5 2]
                                   [1 8 5 2 4]
                                   [8 1 2 4 5]])
                        {4 1, 3 1, 2 7}))))

(defmacro defcheck-153 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
                        true))
     (clojure.test/is (= (~@forms #{#{:a :b :c :d :e}
                                    #{:a :b :c :d}
                                    #{:a :b :c}
                                    #{:a :b}
                                    #{:a}})
                        false))
     (clojure.test/is (= (~@forms #{#{[1 2 3] [4 5]}
                                    #{[1 2] [3 4 5]}
                                    #{[1] [2] 3 4 5}
                                    #{1 2 [3 4] [5]}})
                        true))
     (clojure.test/is (= (~@forms #{#{'a 'b}
                                    #{'c 'd 'e}
                                    #{'f 'g 'h 'i}
                                    #{''a ''c ''f}})
                        true))
     (clojure.test/is (= (~@forms #{#{'(:x :y :z) '(:x :y) '(:z) '()}
                                    #{#{:x :y :z} #{:x :y} #{:z} #{}}
                                    #{'[:x :y :z] [:x :y] [:z] [] {}}})
                        false))
     (clojure.test/is (= (~@forms #{#{(= "true") false}
                                    #{:yes :no}
                                    #{(class 1) 0}
                                    #{(symbol "true") 'false}
                                    #{(keyword "yes") ::no}
                                    #{(class '1) (int \0)}})
                        false))
     (clojure.test/is (= (~@forms #{#{distinct?}
                                    #{#(-> %) #(-> %)}
                                    #{#(-> %) #(-> %) #(-> %)}
                                    #{#(-> %) #(-> %) #(-> %)}})
                        true))
     (clojure.test/is (= (~@forms #{#{(#(-> *)) + (quote mapcat) #_nil}
                                    #{'+ '* mapcat (comment mapcat)}
                                    #{(do) set contains? nil?}
                                    #{,,, #_,, empty?}})
                        false))))


(defmacro defcheck-156 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
     (clojure.test/is (= (~@forms "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
     (clojure.test/is (= (~@forms [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))))

(defmacro defcheck-157 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (~@forms [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
     (clojure.test/is (= (~@forms [0 1 3]) '((0 0) (1 1) (3 2))))
     (clojure.test/is (= (~@forms [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]]))))

(defmacro defcheck-158 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= 10 ((~@forms (fn [a]
                                        (fn [b]
                                          (fn [c]
                                            (fn [d]
                                              (+ a b c d))))))
                             1 2 3 4)))
     (clojure.test/is (= 24 ((~@forms (fn [a]
                                        (fn [b]
                                          (fn [c]
                                            (fn [d]
                                              (* a b c d))))))
                             1 2 3 4)))
     (clojure.test/is (= 25 ((~@forms (fn [a]
                                        (fn [b]
                                          (* a b))))
                             5 5)))))

(defmacro defcheck-161 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (clojure.set/superset? ~@forms #{2}))
     (clojure.test/is (clojure.set/subset? #{1} ~@forms))
     (clojure.test/is (clojure.set/superset? ~@forms #{1 2}))
     (clojure.test/is (clojure.set/subset? #{1 2} ~@forms))))

(defmacro defcheck-162 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= ~@forms (if-not false 1 0)))
     (clojure.test/is (= ~@forms (if-not nil 1 0)))
     (clojure.test/is (= ~@forms (if true 1 0)))
     (clojure.test/is (= ~@forms (if [] 1 0)))
     (clojure.test/is (= ~@forms (if [0] 1 0)))
     (clojure.test/is (= ~@forms (if 0 1 0)))
     (clojure.test/is (= ~@forms (if 1 1 0)))))

(defmacro defcheck-164 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= #{"a" "ab" "abc"}
                        (set (~@forms '{:states      #{q0 q1 q2 q3}
                                        :alphabet    #{a b c}
                                        :start       q0
                                        :accepts     #{q1 q2 q3}
                                        :transitions {q0 {a q1}
                                                      q1 {b q2}
                                                      q2 {c q3}}}))))
     (clojure.test/is (= #{"hi" "hey" "hello"}
                        (set (~@forms '{:states      #{q0 q1 q2 q3 q4 q5 q6 q7}
                                        :alphabet    #{e h i l o y}
                                        :start       q0
                                        :accepts     #{q2 q4 q7}
                                        :transitions {q0 {h q1}
                                                      q1 {i q2, e q3}
                                                      q3 {l q5, y q4}
                                                      q5 {l q6}
                                                      q6 {o q7}}}))))
     (clojure.test/is (= (set (let [ss "vwxyz"] (for [i ss, j ss, k ss, l ss] (str i j k l))))
                        (set (~@forms '{:states      #{q0 q1 q2 q3 q4}
                                        :alphabet    #{v w x y z}
                                        :start       q0
                                        :accepts     #{q4}
                                        :transitions {q0 {v q1, w q1, x q1, y q1, z q1}
                                                      q1 {v q2, w q2, x q2, y q2, z q2}
                                                      q2 {v q3, w q3, x q3, y q3, z q3}
                                                      q3 {v q4, w q4, x q4, y q4, z q4}}}))))
     (clojure.test/is (let [res (take 2000 (~@forms '{:states      #{q0 q1}
                                                      :alphabet    #{0 1}
                                                      :start       q0
                                                      :accepts     #{q0}
                                                      :transitions {q0 {0 q0, 1 q1}
                                                                    q1 {0 q1, 1 q0}}}))]
                        (and (every? (partial re-matches #"0*(?:10*10*)*") res)
                             (= res (distinct res)))))
     (clojure.test/is (let [res (take 2000 (~@forms '{:states      #{q0 q1}
                                                      :alphabet    #{n m}
                                                      :start       q0
                                                      :accepts     #{q1}
                                                      :transitions {q0 {n q0, m q1}}}))]
                        (and (every? (partial re-matches #"n*m") res)
                             (= res (distinct res)))))
     (clojure.test/is (let [res (take 2000 (~@forms '{:states      #{q0 q1 q2 q3 q4 q5 q6 q7 q8 q9}
                                                      :alphabet    #{i l o m p t}
                                                      :start       q0
                                                      :accepts     #{q5 q8}
                                                      :transitions {q0 {l q1}
                                                                    q1 {i q2, o q6}
                                                                    q2 {m q3}
                                                                    q3 {i q4}
                                                                    q4 {t q5}
                                                                    q6 {o q7}
                                                                    q7 {p q8}
                                                                    q8 {l q9}
                                                                    q9 {o q6}}}))]
                        (and (every? (partial re-matches #"limit|(?:loop)+") res)
                             (= res (distinct res)))))))

(defmacro defcheck-166 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= :gt (~@forms < 5 1)))
     (clojure.test/is (= :eq (~@forms (fn [x y] (< (count x) (count y))) "pear" "plum")))
     (clojure.test/is (= :lt (~@forms (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
     (clojure.test/is (= :gt (~@forms > 0 2)))))

(defmacro defcheck-168 [name & forms]
  `(deftimedtest ~name 32
     (clojure.test/is (= (take 5 (map #(take 6 %) (~@forms str)))
                        [["00" "01" "02" "03" "04" "05"]
                         ["10" "11" "12" "13" "14" "15"]
                         ["20" "21" "22" "23" "24" "25"]
                         ["30" "31" "32" "33" "34" "35"]
                         ["40" "41" "42" "43" "44" "45"]]))
     (clojure.test/is (= (take 6 (map #(take 5 %) (~@forms str 3 2)))
                        [["32" "33" "34" "35" "36"]
                         ["42" "43" "44" "45" "46"]
                         ["52" "53" "54" "55" "56"]
                         ["62" "63" "64" "65" "66"]
                         ["72" "73" "74" "75" "76"]
                         ["82" "83" "84" "85" "86"]]))
     (clojure.test/is (= (~@forms * 3 5 5 7)
                        [[15 18 21 24 27 30 33]
                         [20 24 28 32 36 40 44]
                         [25 30 35 40 45 50 55]
                         [30 36 42 48 54 60 66]
                         [35 42 49 56 63 70 77]]))
     (clojure.test/is (= (~@forms #(/ % (inc %2)) 1 0 6 4)
                        [[1/1 1/2 1/3 1/4]
                         [2/1 2/2 2/3 1/2]
                         [3/1 3/2 3/3 3/4]
                         [4/1 4/2 4/3 4/4]
                         [5/1 5/2 5/3 5/4]
                         [6/1 6/2 6/3 6/4]]))
     (clojure.test/is (= (class (~@forms (juxt bit-or bit-xor)))
                        (class (~@forms (juxt quot mod) 13 21))
                        (class (lazy-seq))))
     (clojure.test/is (= (class (nth (~@forms (constantly 10946)) 34))
                        (class (nth (~@forms (constantly 0) 5 8) 55))
                        (class (lazy-seq))))
     (clojure.test/is (= (let [m        377 n 610 w 987
                               check    (fn [f s] (every? true? (map-indexed f s)))
                               row      (take w (nth (~@forms vector) m))
                               column   (take w (map first (~@forms vector m n)))
                               diagonal (map-indexed #(nth %2 %) (~@forms vector m n w w))]
                           (and (check #(= %2 [m %]) row)
                                (check #(= %2 [(+ m %) n]) column)
                                (check #(= %2 [(+ m %) (+ n %)]) diagonal)))
                        true))))

(defmacro defcheck-171 [name & forms]
  `(deftimedtest ~name 64
     (clojure.test/is (= (~@forms [1 2 3]) [[1 3]]))
     (clojure.test/is (= (~@forms [10 9 8 1 2 3]) [[1 3] [8 10]]))
     (clojure.test/is (= (~@forms [1 1 1 1 1 1 1]) [[1 1]]))
     (clojure.test/is (= (~@forms []) []))
     (clojure.test/is (= (~@forms [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
                        [[1 4] [6 6] [9 11] [13 17] [19 19]]))))
