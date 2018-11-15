(ns coal-mine.problem-86
  (:require [coal-mine.checks :refer [defcheck-86] :rename {defcheck-86 defcheck}]
            [clojure.test]
            [clojure.set]
            [clojure.walk]))

(defn str->int [s] #?(:clj (Integer/parseInt s)
                      :cljs (js/parseInt s)))

(defn char->ascii
  ([c] (get {\0 48 \1 49 \2 50 \3 51 \4 52 \5 53 \6 54 \7 55 \8 56 \9 57} c)))

(defn char->num
  ([c] (get {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9} c))
  ([c radix] (assert (= radix 10)) (char->num c)))

(defcheck solution-10b5481a
  (fn sol [a x]
    ( if (a x) (
                 if (= x 1) true false
                            ) ( let [nn
                                     ((fn stp[n] (
                                                   ->> n (str) (
                                                                 reduce #(+ % ((fn [x] (* x x)) (- (char->ascii %2) 48))) 0
                                                                 )
                                                   )
                                        ) x)] (sol (conj a x) nn)
                                              )
               )
    ) #{})

(defcheck solution-10e82d8e
  (fn happy-numbers
    ([n] (happy-numbers n #{n}))
    ([n prev]
     (letfn [(squared-sum [i] (->>
                                (map #(str->int (str %)) (str i))
                                (map #(* % %))
                                (reduce +)))]
       (let [s (squared-sum n)]
         (if (= s 1) true
                     (if (contains? prev s) false
                                            (recur s (conj prev s)))))))))

(defcheck solution-1136eb10
  (fn [n]
    (let [nxt (fn [n]
                (reduce
                  #(let [n (str->int (str %2))] (+ %1 (* n n)))
                  0
                  (str n)))]
      (loop [c [] nn n]
        (cond
          (= nn 1) true
          (some #(= nn %) c) false
          :else (recur (conj c nn) (nxt nn)))))))

(defcheck solution-117a1bea
  (fn [x]
    ((fn [testnum]
       (let [digits (map str->int (map str (seq (str (last testnum)))))
             digitsum (apply + (map #(* % %) digits))]
         (if (= 1 digitsum)
           true
           (if (some #(= % digitsum) testnum)
             false
             (recur (conj testnum digitsum))))))
     [x])))

(defcheck solution-11a76de3
  (fn ! [n]
    (loop [h n s []]
      (cond
        (= h 1) true
        (some #(= % h) s) false
        :else (recur (int (apply + (map #(Math/pow (- (char->ascii %) 48) 2) (seq (str h))))) (conj s h))))))

(defcheck solution-11bf66d
  (fn [n]
    (letfn [(hap-step [n]
              (apply +
                (map #(* % %)
                  (map #(str->int (str %)) (str n)))))]
      (loop [ a n
             as #{a}]
        (if (= 1 a)
          true
          (let [a (hap-step a)]
            (if (as a)
              false
              (recur a (conj as a)))))))))

(defcheck solution-11c2df86
  (fn [n]
    (letfn [(squaredsum [x] (reduce + (map #(* % %) (map #(str->int %) (map str (str x))))))]
      (loop [seen #{}
             n (squaredsum n)]
        (cond
          (seen n) false
          (= 1 n) true
          :else (recur (conj seen n) (squaredsum n)))))))

(defcheck solution-11d37014
  (fn [n]
    (letfn [(sum-of-digit-squares [number]
              (if (= number 0)
                0
                (let [digit (rem number 10)]
                  (+ (* digit digit) (sum-of-digit-squares (quot number 10))))))]
      (loop [current n past-numbers #{}]
        (cond (= current 1) true
              (past-numbers current) false
              :else (recur (sum-of-digit-squares current) (conj past-numbers current)))))))

(defcheck solution-12074588
  (fn is-happy
    [n]
    (loop [n n
           seen #{}]
      (let [ssq (apply + (map #(* % %) (map #(str->int (str %)) (str n))))]
        (cond (= ssq 1) true
              (contains? seen ssq) false
              :else (recur ssq (conj seen ssq)))))))

(defcheck solution-127fb575
  (fn [n]
    (letfn [(sqs [n](->> n str (map #(- (char->ascii %) 48))
                      (map #(* % %))(apply +)))]
      (loop [memo #{}, n n]
        (cond
          (= 1 n)   true
          (memo n)  false
          :else (recur (conj memo n) (sqs n)))))))

(defcheck solution-128ce94
  (fn [n]
    (loop [n n p #{}]
      (let [x (->>
                (map (comp str->int str) (str n))
                (map #(* % %))
                (reduce +))]
        (cond
          (= x 1) true
          (contains? p x) false
          :else (recur x (conj p x)))))))

(defcheck solution-12fac84e
  (fn happy
    ([n] (happy n []))
    ([n s]
     (letfn [(digits [n]
               (map #(str->int (str %)) (str n)))]
       (if (empty? (filter #(= n %) s))
         (if (= n 1 )
           true
           (happy
             (reduce + (map  #(* % %) (digits n)))
             (cons n s)))
         false)))))

(defcheck solution-132f3b3f
  (fn [x] (letfn [(f [n i]
                    (if (> n 9)
                      (recur (quot n 10) (+ i (#(* % %) (mod n 10))))
                      (+ i (* n n))))]
            (loop [x x a #{}]
              (cond
                (= x 1) true
                (a x) false
                1 (recur (f x 0) (conj a x)))))))

(defcheck solution-1372a1d6
  (letfn [(digits [n]
            (loop [n n digits '()]
              (if (< n 10)
                (cons n digits)
                (recur (quot n 10) (cons (rem n 10) digits)))))]
    (fn happy?
      ([num]
       (happy? num #{}))
      ([num seen]
       (let [sqsum (reduce + (map #(* % %) (digits num)))]
         (cond
           (= 1 sqsum) true
           (seen sqsum) false
           :else (recur sqsum (conj seen sqsum))))))))

(defcheck solution-13770cdf
  (fn [n]
    (let [f (fn [x] (apply + (map #(* % %) (map #(- (char->ascii %) 48) (str x)))))
          stuck? (fn [set key] (contains? set key))]
      (loop [happy n s #{}]
        (cond (= 1 happy) true
              (stuck? s happy) false
              :else
              (recur (f happy) (conj s happy)) )))))

(defcheck solution-137a5d6
  (fn happy-number?
    ([n] (happy-number? n #{}))
    ([n visited-numbers]
     (letfn [(digit-sq-sum [k]
               (apply +
                 (map #(* (str->int (str %))
                         (str->int (str %)))
                   (apply list (str k)))))]
       (cond
         (= n 1) true
         (contains? visited-numbers n) false
         :else (happy-number? (digit-sq-sum n) (conj visited-numbers n)))))))

(defcheck solution-13a3f8ff
  (fn [n]
    (letfn
     [(toDigits [x] (map #(- (char->ascii %) 48) (seq (str x))))
      (sos [y] (apply + (map #(* % %) y)))]
      (loop [curr n seen #{}]
        (if (= 1 curr) true
                       (if (seen curr) false
                                       (recur (sos (toDigits curr)) (conj seen curr))))))))

(defcheck solution-13cd7476
  (fn [n]
    (loop [n n s #{}]
      (cond
        (= 1 n) true
        (s n) false
        :else (recur
                (apply
                  +
                  (map
                    (comp (fn [p] (* p p)) str->int str)
                    (str n)))
                (conj s n))))))

(defcheck solution-13d0774e
  (fn happy? [n]
    (letfn [(to-digits [n]
              (map #(- (char->num %) (char->num \0)) (seq (str n))))
            (square-sum [v]
              (reduce + (map #(* % %) v)))]
      (loop [x n, s #{}]
        (cond (= 1 x) true
              (contains? s x) false
              :else (recur (square-sum (to-digits x)) (conj s x)))))))

(defcheck solution-13da3b3b
  (fn [n]
    (let [char-to-digit (fn [n] (- (char->num n) (char->num \0)))
          sum-of-squares-of-digits (fn [n] (apply + (map #(* (char-to-digit %) (char-to-digit %)) (str n))))]
      (loop [n n
             seen #{}]
        (cond (= n 1) true
              (some seen #{n}) false
              :else (recur (sum-of-squares-of-digits n) (conj seen n)))))))

(defcheck solution-13e87694
  (fn [n]
    (loop [x n known #{}]
      (let [s (->> (loop [n x res []]
                     (if (zero? n) res
                                   (recur (quot n 10)
                                     (conj res (rem n 10)))))
                (map #(* % %))
                (reduce +))]
        (cond
          (= 1 s) true
          (known s) false
          :else (recur s (conj known s)))))))

(defcheck solution-14dd6fe
  (fn happy? [x]
    (letfn [(ss [x]
              (reduce + (map #(* % %) (map #(- (char->num %) (char->num \0)) (str x)))))]
      (loop [v x vs #{}]
        (cond (= v 1) true
              (vs v) false
              :else (recur (ss v) (conj vs v)))))))

(defcheck solution-1513df9f
  #((fn [history num]
      (let [nn (apply + (map (fn [a] (* a a))
                          (map (fn [s] (str->int (str s))) (str num))))]
        (cond (= 1 nn) true
              (history nn) false
              :else (recur (conj history nn) nn))))
    #{} %))

(defcheck solution-15464f7c
  #(loop [i % o #{}]
     (let [[& s] (str i) c (apply + (map (fn [x] (* (- (char->ascii x) 48) (- (char->ascii x) 48))) s))]
       (if (= c 1)
         true
         (if (o c)
           false
           (recur c (conj o c)))))))

(defcheck solution-157cc1c4
  (fn [n]
    (letfn [(iter [hist i]
              (cond
                (= 1 i) true
                (some #{i} hist) false
                :else (let [new-hist (conj hist i)
                            j (->> (str i)
                                (map str)
                                (map #(str->int %))
                                (map #(* % %))
                                (apply +))]
                        (iter new-hist j))))]
      (iter [] n))))

(defcheck solution-158eb75a
  (fn [x]
    (let [sqr (fn [x] (* x x))
          next-num (fn [x] (apply + (map #(sqr (- (char->ascii %) 48)) (seq (str x)))))]
      (loop [seen #{}
             n x]
        (if (contains? seen n)
          false
          (let [m (next-num n)]
            (if (= 1 m)
              true
              (recur (conj seen n) m)
              )
            )
          )
        )
      )
    ))

(defcheck solution-15c234f1
  (fn [n]
    (letfn [
            (sumsquare [i] (reduce #(+ (* %2 %2) %1) 0 (map #(char->num % 10) (str i))))]
      (loop [sumsquares (iterate sumsquare n) prevset #{}]
        (let [e (first sumsquares)]
          (if (prevset e)
            (= e 1) ; when a cycle is found, happy if one else not
            (recur (next sumsquares) (conj prevset e))))))))

(defcheck solution-160ab0ea
  (partial < 4))

(defcheck solution-168b7c9d
  (fn [n]
    (loop [seen #{}, v n]
      (cond
        (= v 1) true
        (seen v) false
        :else (recur (conj seen v)
                (reduce + (map (comp #(* % %) #(mod % 10))
                            (take-while pos? (iterate #(int (/ % 10)) v)))))))))

(defcheck solution-16ae60ad
  (fn [n]
    (letfn
     [(sumthisbitch [n]
        (if (= n 0) 0
                    (let [ln (mod n 10) lika (quot n 10)]
                      (+ (* ln ln) (sumthisbitch lika))
                      )))
      (tryx [n pastsums]
        (let [thissum (sumthisbitch n)]
          (if (= thissum 1) true
                            (if (contains? pastsums thissum) false
                                                             (tryx thissum (conj pastsums thissum))
                                                             ))))]
      (tryx n []))))

(defcheck solution-16f0ec0a
  (fn [n]
    (loop [cur-n n i 0]
      (if (< i 1000)
        (let [new (reduce + (map #(* % %) (for [digit (str cur-n)]
                                            (str->int (str digit)))))]
          (if (= new 1)
            true
            (recur new (inc i)))) false))))

(defcheck solution-17193467
  (fn happy [n]
    (letfn [(digits [n]
              (map #(str->int (str %)) (str n)))
            (sqrt-sum [n]
              (reduce + (map #(* % %) (digits n))))]
      (boolean (some #{1} (take 100 (iterate sqrt-sum n)))))))

(defcheck solution-1720dd23
  (fn [nb]
    (letfn [(formula [n] (reduce +
                           (map
                             (comp #(* % %) #(char->num % 10))
                             (seq (str n)))))]
      (loop [n nb, iter-left 100]
        (if (= 1 n)
          true
          (if (< 0 iter-left)
            (recur (formula n) (dec iter-left))
            false
            ))))))

(defcheck solution-17860a8a
  (fn [x]
    (letfn [(hn [x] (apply + (map #(* % %) (map #(- (char->ascii %) 48) (str x)))))]
      (loop [n x s #{}]
        (let [nn (hn n)]
          (if (= 1 nn)
            true
            (if (nil? (s nn))
              (recur nn (conj s nn))
              false)))))))

(defcheck solution-17dbe238
  (fn [number]
    (letfn [(h [n] (apply + (map #(* % %) (map (comp str->int str) (str n)))))]
      (loop [number number history #{}]
        (cond
          (= number 1) true
          (history number) false
          :else (recur (h number) (conj history number)))))))

(defcheck solution-18047fba
  (fn happy
    ([n] (happy n 0))
    ([n step]
     (letfn [(get-digits [n]
               (map str->int (map str (-> n str vec))))
             (sum-squared [digits]
               (apply + (map #(* % %) digits)))]
       (let [new-num (sum-squared (get-digits n))]
         (if (> step 1000)
           false
           (if (= new-num 1)
             true
             (happy new-num (inc step)))))))))

(defcheck solution-18c2fbba
  (let [explode-digits (fn  [number] (map #(- (char->num %) (char->num \0)) (str number)))
        squared-digits (fn [number] (reduce + (map #(* % %) (explode-digits number))))]
    (fn happy?
      ([number tries]
       (cond
         (= 1 number) true
         (pos? tries) (happy? (squared-digits number) (dec tries))
         :else false))
      ([number] (happy? number 1000)))))

(defcheck solution-1900b3d3
  (fn happy? [n]
    (letfn [(sq [x]
              (* x x))
            (sumsq [s]
              (reduce + (map #(sq (- (char->ascii %) 48)) s)))]
      (loop [ssq (sumsq (.toString n)),
             cnt 0]
        (if (= ssq 1)
          true
          (if (> cnt 30)
            false
            (recur (sumsq (.toString ssq)) (inc cnt))))))))

(defcheck solution-1994ed42
  (fn f [x]
    (let [dgs (fn [n] (map #(str->int (str %)) (str n)))
          sum-sdgs (fn [n] (reduce + (map #(* % %) n)))
          s (sum-sdgs (dgs x))]
      (cond (= s 1) true
            (= s 4) false
            :else (f s)))))

(defcheck solution-19d145f4
  (fn happy?
    ([n]    (happy? #{} n))
    ([s n]  (let [sum-of-squares  (fn [n] ((fn [v] (apply + (map * v v)))  (map #(str->int (str %)) (seq (str n)))))
                  sos             (sum-of-squares n)]
              (cond
                (= sos 1)         true
                ; If s contains sos, then we've hit a cycle, so n is not happy.
                ; See http://en.wikipedia.org/wiki/Happy_number for why.
                (contains? s sos) false
                :else             (happy? (conj s sos) sos))))))

(defcheck solution-1a01ee35
  (fn happy-number [n] (let [next-number (reduce + (map #(* % %) (map #(char->num % 10) (str n))))] (cond (= next-number 1) true (= next-number 4) false :else (happy-number next-number)))))

(defcheck solution-1a2ede0e
  (fn [n]
    (loop [n n, seen #{}]
      (cond
        (= n 1) true
        (seen n) false
        :else
        (recur (->> (str n)
                 (map #(char->num % 10))
                 (map #(* % %))
                 (apply +)) (conj seen n))))))

(defcheck solution-1a7721e8
  (fn [n]
    (let [d (fn d [x] (if (< x 10) [x] (cons (mod x 10) (d (quot x 10)))))
          q #(* % %)]
      (loop [n n s #{}]
        (cond (s n) false
              (= n 1) true
              1 (recur (reduce + (map q (d n)))
                  (conj s n)))))))

(defcheck solution-1b0cbae8
  (fn [n]
    (loop [i n
           sofar #{}]
      (if (= i 1)
        true
        (if (contains? sofar i)
          false
          (recur (reduce + (map #(* (- (char->ascii %) 48) (- (char->ascii %) 48)) (seq (str i)))) (conj sofar i)))))))

(defcheck solution-1b152c1a
  (fn [n]
    (loop [p n s []]
      (if (= p 1) true
                  (if (some #(= p %) s) false
                                        (recur
                                          (reduce #(+ % (* %2 %2))
                                            0
                                            (map #(- (char->ascii %) 48) (str p)))
                                          (conj s p)))))))

(defcheck solution-1b172238
  (fn [n]
    (loop [n n
           h '()]
      (cond (= n 1)
            true
            (some #(= % n) h)
            false
            :else
            (recur (apply + (map (fn [c]
                                   (let [i (str->int (str c))]
                                     (* i i)))
                              (.toString n)))
              (conj h n))))))

(defcheck solution-1b7aaff1
  (letfn [(digits [n r] (if (zero? n) r (recur (quot n 10) (cons (rem n 10) r))))
          (sqr-sum-seq [n] (cons n (lazy-seq (sqr-sum-seq (reduce + (map #(* % %) (digits n '())))))))
          (is-happy? [n v [f & r]]
            (cond (v f) false
                  (= 1 f) true
                  :else (recur n (conj v f) r)))]
    #(is-happy? % #{} (drop 1 (sqr-sum-seq %)))))

(defcheck solution-1c77a6bc
  (fn [n]
    (let [seen (transient #{})]
      (letfn [(unseen? [x]
                (let [here (seen x)]
                  (conj! seen x)
                  (not here)))]
        (= 1
          (last (take-while unseen?
                  (iterate (fn [n]
                             (->> (map str (str n))
                               (map #(str->int %))
                               (map #(* % %))
                               (reduce +)))
                    n))))))))

(defcheck solution-1c8b19ef
  (fn [n]
    (letfn [(dgg [n]
              (reduce + (map (comp #(* % %) (partial + -48) char->ascii) (str n))))]
      (loop [a {}
             v n]
        (if (= 1 v)
          true
          (if (contains? a v)
            false
            (recur (assoc a v 1) (dgg v))))))))

(defcheck solution-1d444192
  (fn [x]
    (letfn [(square-digits [x]
              (#(if (zero? %2) %1 (let [md (mod %2 10) qt (quot %2 10)] (recur (+ %1 (* md md)) qt)))
               0 x))]
      (#(let [digits-squared (square-digits %2)]
          (if (= 1 digits-squared)
            true
            (if (%1 digits-squared)
              false
              (recur (conj %1 digits-squared) digits-squared))))
       #{x} x))))

(defcheck solution-1de462ae
  (fn
    [n]
    (loop [n n
           tried ()]
      (let [try (->> n
                  str
                  seq
                  (map #((fn [x] (* x x)) (- (char->ascii %) 48)))
                  (apply +))]
        (cond
          (= try 1) true
          (some #(= % try) tried) false
          :else (recur try (conj tried try)))))))

(defcheck solution-1e21c807
  (fn [x] (let [op (fn  [x] (->> x str (map char->ascii) (map #(- % 48)) (map #(* % %)) (apply +) )) ]  (loop [n x others #{} ] (if (= n 1) true (if (others n) false (recur (op n) (conj others n)))))) ))

(defcheck solution-1e2e0ec9
  (fn happy [n]
    (letfn [(sumsquare [n]
              (loop [current n sum 0]
                (if (= current 0)
                  sum
                  (let [digit (mod current 10)]
                    (recur (/ (- current digit) 10) (+ sum (* digit digit)))))))]
      (loop [current n seen #{}]
        (cond
          (= current 1) true
          (seen current) false
          :else (recur (sumsquare current) (conj seen current)))))))

(defcheck solution-1e410dc1
  #(not (or (= % 2) (= % 3))))

(defcheck solution-1f129f7f
  (fn [x]
    (letfn [(digits [n] (map #(- (char->num %) (char->num \0)) (str n)))
            (nextval [n] (reduce + (map #(* % %) (digits n))))
            (cycle? [vals] (< (count (set vals)) (count vals)))]
      (loop [seen [], val x]
        (if (cycle? seen)
          false
          (if (= 1 (last seen))
            true
            (let [nval (nextval val)] (recur (conj seen nval) nval))))))))

(defcheck solution-1f19573c
  (fn [x]
    (let  [step (fn  [y]
                  (->> (str y)
                    (map #(str->int (str %)))
                    (map #(* % %))
                    (apply +)))
           step-seq (iterate step x)
           first-small (first (filter #(< % 1000) step-seq))
           thousand (take 1000 (iterate step first-small))]
      (contains? (set thousand) 1))))

(defcheck solution-1f1f4658
  (fn happy-number? [n]
    (letfn [(digits [n]
              (map
                first
                (take-while
                  #(not= [0 0] %)
                  (drop 1
                    (iterate
                      (fn [[a b]]
                        [(mod b 10) (quot b 10)])
                      [0 n])))))
            (happy-number-next [n]
              (reduce + (map #(* % %) (digits n))))
            (happy-number-seq [n] (iterate  happy-number-next n))
            ]
      (loop [seen #{} [i & more]
             (happy-number-seq n)]
        (cond
          (= 1 i) true
          (contains? seen i) false
          :else (recur (conj seen i) more)))

      )))

(defcheck solution-1f28ff16
  (fn [x] (loop [h (list x)]
            (let [r (apply + (map #((comp (fn [c] (* c c)) str->int str) %) (str (first h))))]
              (cond (= 1 r) true
                    (contains? (set h) r) false
                    :else (recur (cons r h)))))))

(defcheck solution-1fb3565d
  (fn [n]
    (letfn [(_ [acc n]
              (let [ns (map (fn [n] (str->int (str n))) (str n))
                    n (reduce + (map (fn [n] (* n n)) ns))]
                (if (= 1 n)
                  true
                  (if (some (fn [n'] (= n n')) acc)
                    false
                    (_ (cons n acc) n)))))]
      (_ [] n))))

(defcheck solution-1ff731d1
  (fn [m] (let [digits #(loop [ds (), n' %]
                          (if (< n' 10) (cons n' ds)
                                        (recur (cons (rem n' 10) ds) (quot n' 10))))
                h (->> (digits m) (map #(* % %)) (apply +))]
            (if (< h 10) (= 1 h)
                         (recur h)))))

(defcheck solution-206e96af
  (fn [x]
    (= 1 (first(drop-while #(and (not= % 1)(not= % 4))(rest(iterate (fn [n] (apply + (map #(* % %) (map #(- (char->ascii %) 48) (str n))))) x)))))
    ))

(defcheck solution-20c8337f
  (fn happy?
    ([x] (happy? #{} x))
    ([sadset x]
     (let [pow2 (fn [z] (let [z' (str->int (str z))] (* z' z')))
           x' (apply + (map pow2 (str x)))]
       (cond
         (= x' 1) true
         (sadset x') false
         :else (happy? (conj sadset x') x'))))))

(defcheck solution-20e73507
  (fn [val]
    (letfn [(digits [n]
              (let [radix 10]
                (reverse
                  (map
                    #(int (rem % radix))
                    (take-while (complement zero?)
                      (iterate #(quot % radix)
                        n))))))
            (happy? [n seen]
              (cond (= 1 n) true
                    (contains? seen n) false
                    :else (recur
                            (reduce +
                              (map #(* % %)
                                (digits n)))
                            (conj seen n))))]
      (happy? val #{}))))

(defcheck solution-20e75e96
  (fn happy? [number]
    (loop [n number]
      (if (= 1 n)
        true
        (if (= 4 n)
          false
          (recur
            (apply +
              (map #(* % %)
                (map #(- (char->ascii %) 48) (str n))))))))))

(defcheck solution-20ec81b9
  (fn [init]
    (loop [n init, visited? #{}]
      (cond
        (= 1 n)      true
        (visited? n) false
        :else
        (recur
          (->> (str n)
            (map (comp #(* % %)
                       str->int
                       str))
            (reduce +))
          (conj visited? n))))))

(defcheck solution-211eba68
  (letfn [(seq-n [x n] (loop [c () v x] (if (< v n) (conj c v) (recur (conj c (mod v n)) (quot v n)))))
          (hnn [x] (->> (seq-n x 10) (map #(* % %)) (apply +)))]
    (fn [x] (loop [c #{} v x] (if (c v) (= 1 v) (recur (conj c v) (hnn v)))))))

(defcheck solution-215b3ba4
  (fn hn [z]
    (let [n (apply + (map #(* % %) (map #(- (char->num %) (char->num \0))(str z))))]
      (cond
        (= 1 n) true
        (= 4 n) false
        :else (recur n)))))

(defcheck solution-215c289c
  #(loop [n % seen #{}]
     (cond (= n 1) true
           (seen n) false
           :else (let [ns ((fn f [i]
                             (if (zero? i)
                               ()
                               (cons (rem i 10)
                                 (f (quot i 10))))) n)
                       m (reduce + (map (fn [x] (* x x)) ns))]
                   (recur m (conj seen n))))))

(defcheck solution-21b9d10c
  (fn happy? [n]
    (loop [curr n
           used #{}]
      #_(println "curr" curr "used" used)
      (cond
        (= 1 curr) true
        (contains? used curr) false
        :else (let [digits
                              (loop [n curr res []]
                                (if (zero? n)
                                  res
                                  (recur (quot n 10) (conj res (mod n 10)))))
                    sq-digits (map #(* % %) digits)
                    sum-sq (apply + sq-digits)]
                (recur sum-sq (conj used curr)))))))

(defcheck solution-21debda5
  (fn [num]
    (boolean (some #(= 1 %)
               (take 100
                 ((fn [n]
                    (iterate (fn [x] (apply + (map #(* % %) (map #(- (char->num %) (char->num \0)) (str x))))) n))
                  num))))))

(defcheck solution-2222e3b1
  (fn [n]
    (letfn[ (digseq [i] (->> i str seq (map #(str %)) (map #(str->int %)) ))
           (magic [i] (reduce + (map #(* % %) (digseq i))))
           ]
      (loop [current n
             seen #{}]
        (if (= 1 current)
          true
          (let [nxt (magic current)]
            (if (seen nxt)
              false
              (recur nxt (conj seen nxt)))))))))

(defcheck solution-229020bf
  (fn [n]
    (loop [n (-> n str seq)
           cnt 0]
      (let [parse #(str->int (str %))
            m (map #(* (parse %)
                      (parse %)) n)
            sum (->> m (map parse) (apply +))]
        (cond
          (= sum 1) true
          (> cnt 1000) false
          :else (recur (str sum) (inc cnt)))))))

(defcheck solution-231dc196
  (fn happy? [n]
    (letfn [(digits [n] (map (fn [d] (- (char->num d) (char->num \0))) (str n)))
            (new-num [n] (reduce + (map (fn [x] (* x x)) (digits n))))]
      (loop [seen (hash-set) nx n]
        (cond (contains? seen nx) false
              (= 1 nx) true
              :else (recur (conj seen nx) (new-num nx)))))))

(defcheck solution-235b4551
  (letfn [(digs [n]
            (if (zero? n)
              []
              (conj (digs (quot n 10)) (mod n 10))))
          (sum-squar [n]
            (->> n
              digs
              (reduce #(+ %1 (* %2 %2)) 0)))
          (seqtor [n]
            (cons n (lazy-seq (seqtor (sum-squar n)))))]
    (fn [n]
      (loop [s (seqtor n)
             visited #{}]
        (condp #(%1 %2) (first s)
          #{1} true
          visited false
          (recur (next s) (conj visited (first s))))))))

(defcheck solution-23efb721
  (fn [z] (= 1 (->
                 (filter #(not (apply distinct? %))
                   (for [i (iterate inc 2)] (take i (iterate
                                                      (fn [x]
                                                        (apply + (map #(* % %) (map #(- (char->num %) (char->num \0)) (str x)))))
                                                      z)))) first last))))

(defcheck solution-23ffe33b
  (fn [num]
    (letfn [(num->digits [num]
              (loop [n num res []]
                (if (zero? n)
                  res
                  (recur (quot n 10) (cons (mod n 10) res)))))
            (square [n] (* n n))
            (happify [n] (reduce + (map square (num->digits n))))]
      (loop [n num res #{}]
        (cond (= 1 n) true
              (res n) false
              :else (recur (happify n) (into res [n])))))))

(defcheck solution-24153a23
  (fn [n]
    (let [next-n (fn [n]
                   (reduce + (map #(-> %
                                     str
                                     str->int
                                     ((fn [n] (* n n))))
                               (str n))))
          do-the-needful (fn [n seen]
                           (cond (= 1 n) true
                                 (seen n) false
                                 :else (recur (next-n n) (conj seen n))))]
      (do-the-needful n #{}))))

(defcheck solution-24e076ff
  (fn happy-number?
    ([num] (happy-number? [] num))
    ([col num]
     (let [s (loop [n num s 0]
               (if (= n 0)
                 s
                 (recur (quot n 10) (let [r (rem n 10) sqr (* r r)] (+ s sqr)))))
           ]
       (if (= s 1)
         true
         (if (nil? (some #(= s %) col))
           (recur (conj col s) s)
           false))))))

(defcheck solution-253969bc
  (fn [n]
    (loop [a n v #{}]
      (cond
        (= 1 a) true
        (contains? v a) false
        :else (recur (reduce + (map #(* % %) (map #(- (char->num %) (char->num \0))(str a)))) (conj v a))
        )
      )
    ))

(defcheck solution-25ad09d4
  (fn happy [n]
    (letfn [(sq [x] (* x x))
            (ch->int [ch] (- (char->num ch) (char->num \0)))
            (sum-of-squares [n]
              (reduce + (map (comp sq ch->int) (str n))))]
      (loop [num (sum-of-squares n), seen #{}]
        (cond
          (= num 1)            true
          (contains? seen num) false
          :else                (recur (sum-of-squares num), (conj seen num)))))))

(defcheck solution-25bcf1be
  (letfn [(digits[n] (map #(- (char->num %) (char->num \0)) (str n)))]
    (fn [n]
      (loop [n n seen #{}]
        (cond
          (= n 1) true
          (seen n) false
          :else (recur (reduce #(+ %1 (* %2 %2)) 0 (digits n))
                  (conj seen n)))))))

(defcheck solution-25e15150
  (fn perfectNum[n]
    (let[stop #{4 16 37 58 89 145 42 20},
         sqrtsum (fn sqrsum[n]
                   (apply +
                     (map #(* % %)
                       (map #(- (char->ascii %) 48)
                         (seq (str n) ) ) )
                     )
                   ),
         perfect (fn[n]
                   (cond (contains? stop n)  (keyword "unless")
                         (= n 1) (keyword "good")
                         :else (sqrtsum n)
                         )
                   )
         ]
      (get {:unless false,:good true}
        (some  #{:unless :good}
          (iterate perfect n) ) )
      )
    ))

(defcheck solution-26208f15
  (fn [n]
    (loop
     [n n
      v #{}]
      (cond
        (= n 1) true
        (v n) false
        :else (recur (->> n
                       str
                       (map #(- (char->ascii %) 48))
                       (map #(* % %))
                       (apply +))
                (conj v n))))))

(defcheck solution-268192fd
  (fn [x]
    (loop [i x s #{}]
      (if (= i 1)
        true
        (let [n (apply + (map #(* (- (char->ascii %) 48) (- (char->ascii %) 48)) (str i)))]
          (if (not (nil? (s n)))
            false
            (recur n (conj s n))))))))

(defcheck solution-2717a9ae
  (fn [n]
    (letfn [(ssd [n] (loop [r 0 n n]
                       (if (zero? n) r
                                     (recur (+ r (* (mod n 10) (mod n 10)))
                                       (quot n 10)))))]
      (not (some #(> (count (val %)) 1)
             (group-by identity
               (remove #(= 1 %) (take 25 (iterate ssd n)))))))))

(defcheck solution-27223b55
  (fn [n]
    (letfn [(digits [n] (if (zero? n) '(0) (cons (rem n 10) (digits (quot n 10)))))
            (sos [n] (reduce + (map #(* % %) (digits n))))]
      (loop [tried #{} m n]
        (cond
          (tried m) false
          (= 1 m) true
          :else (recur (conj tried m) (sos m)))))))

(defcheck solution-27ab9ebb
  (fn [n]
    (->> (iterate
           (fn [x]
             (->> (iterate #(* 10 %) 1)
               (map #(quot x %))
               (take-while #(> % 0))
               (map #(rem % 10))
               (map #(* % %))
               (apply +)))
           n)
      (reductions
        (fn [acc x]
          (cond
            (== 1 x) true
            (acc x) false
            :else (conj acc x)))
        #{})
      (drop-while coll?)
      first)))

(defcheck solution-27e56d79
  (fn [x] (letfn [
                  (sumsq [c]
                    (reduce #(let [q (- (char->ascii %2) 48)]
                               (+ % (* q q)))
                      0
                      (str c)))
                  (happy? [seen c]
                    (cond
                      (= 1 c) true
                      (seen c) false
                      :else (happy? (conj seen c) (sumsq c))))]
            (happy? #{} x))))

(defcheck solution-2893666c
  (fn [n]
    (loop [n n s #{}]
      (let [r (conj s n)]
        (if (= s r)
          (if (= n 1) true false)
          (recur (apply + (map (comp #(* % %) int str->int str) (str n))) r))))))

(defcheck solution-28c19750
  (fn [n]
    (letfn [(dig-10 [n]
              ((fn [n d]
                 (loop [r n dig (if (zero? n) [0] [])]
                   (if (zero? r) (reverse dig) (recur (int (/ r d)) (conj dig (mod r d)))))) n 10))
            (gen-happy-num [n]
              (reduce + (map #(* % %) (dig-10 n))))]
      (let [l (iterate gen-happy-num n)
            k (first (filter #(or (= % 1) (= % 4)) l))]
        (cond
          (= k 1) true
          (= k 4) false)
        ))))

(defcheck solution-2927153e
  (fn [n]
    (loop [n n, old-ns #{}]
      (cond
        (= n 1) true
        (old-ns n) false
        :else (recur (reduce +
                       (map #(let [digit (- (char->ascii %) 48)]
                               (* digit digit))
                         (str n)))
                (conj old-ns n))))))

(defcheck solution-29c7a09d
  (fn [n]
    (let [q #(* % %)
          d #(-> % str str->int q)
          s (iterate #(apply + (map d (str %))) n)]
      (loop [i (first s)
             l (rest s)
             t #{}]
        (if (= 1 i)
          true
          (if (contains? t i)
            false
            (recur (first l) (rest l) (conj t i))))))))

(defcheck solution-29cdb840
  (fn happy?
    ([n] (happy? n 1))
    ([n i]
     (let [m (apply + (map
                        #(* % %)
                        (loop [n n digits []]
                          (if (< n 10)
                            (cons (int n) digits)
                            (recur
                              (quot n 10)
                              (cons (int (rem n 10)) digits))))))]
       (if (= 1 m)
         true
         (if (= i 1000)
           false
           (happy? m (inc i))))))))

(defcheck solution-2a2f26f5
  #(> % 3))

(defcheck solution-2a3fb895
  (fn happy?
    [n]
    (loop [n n seen #{n}]
      (let [ds (map #(str->int (str %)) (seq (str n)))
            sos (reduce + (map #(* % %) ds))]
        (cond
          (= sos 1) true
          (seen sos) false
          :else (recur sos (conj seen sos)))))))

(defcheck solution-2a53e255
  (letfn [(str-int-pow [n] (-> n (str) (str->int) (#(* % %))))
          (dig-sqr-sum [n] (reduce + (map str-int-pow (seq (str n)))))]
    (fn happy? ([n] (happy? n #{n}))
      ([n s] (let [dss (dig-sqr-sum n)]
               (cond
                 (= 1 dss) true
                 (contains? s dss) false
                 :else (recur dss (conj s dss))))))))

(defcheck solution-2afa17a6
  (fn f [n & c]
    (let [v (reduce #(+ % (let [x (- (char->ascii %2) 48)] (* x x))) 0 (str n))]
      (cond
        (= 1 v) true
        (some #(= v %) c) false
        :else (recur v
                (cons v (seq c)))))))

(defcheck solution-2b2b76ab
  (letfn [(d [x] (when (pos? x) (cons (mod x 10) (d (int (/ x 10))))))
          (h [x] (reduce + (map #(* % %) (d x))))]
    #(case % 1 true 4 false (recur (h %)))))

(defcheck solution-2b6274f8
  (fn happy-number? [n]
    (loop [n n
           so-far #{}]
      (if (contains? so-far n)
        false
        (let [new-n (reduce + (map #(* % %) (map #(str->int (str %)) (str n))))]
          (if (not= 1 new-n)
            (recur new-n (conj so-far n))
            true
            )
          ))
      )
    ))

(defcheck solution-2b704579
  (fn[start]
    (let [f (fn [n]
              (->>
                (loop [r (quot n 10) s [(mod n 10)]]
                  (if (zero? r) s (recur (quot r 10) (conj s (mod r 10)))))
                (map (fn [x] (* x x)))
                (reduce +)))]
      (loop [fx (f start) fails #{}]
        (cond
          (#{1} fx) true
          (fails fx) false
          :else (recur (f fx) (conj fails fx)))))))

(defcheck solution-2b8dc7c4
  (fn [d]
    (= 1 (letfn [(happy? [s]
                   (reduce + (map
                               (comp int #(Math/pow % 2) str->int str)
                               (seq (str s)))))]
           (loop [n (happy? d)  stock #{}]
             (if (and (> n 1) (not (get stock n)))
               (recur (happy? n) (conj stock n))
               n))))))

(defcheck solution-2bd2fb9e
  (fn [y]
    (let [f (fn [x] (->> (str x)
                      (map #(let [n (- (char->ascii %) 48)] (* n n)))
                      (reduce +)))]
      (loop [n y
             s #{}]
        (cond
          (s n) false
          (= n 1) true
          :t (recur (f n) (conj s n)))))))

(defcheck solution-2c00153f
  (fn[n]
    (letfn[(next-happy[k]
             (reduce + (map (fn[x] (let [x (str->int x)] (* x x))) (map str (seq (str k))))))]
      (loop [occ #{}
             k n
             cnt 0]
        (if (not= (count occ) cnt)
          (if (= k 1) true false)
          (recur (conj occ k) (next-happy k) (inc cnt)))))))

(defcheck solution-2c2546d7
  (letfn [
          (to-sequence [n] (map #(- (char->ascii %) 48) (seq (str n))))
          (new-number [n] (reduce #(+ %1 (* %2 %2)) 0 (to-sequence n)))
          (is-happy? [n] (loop [n' n seen #{}]
                           (cond
                             (= n' 1) true
                             (get seen n') false
                             :else (recur (new-number n') (conj seen n')))))]
    is-happy?))

(defcheck solution-2cd5cbc
  (fn [n]
    (let [f (fn [x] (apply + (map (comp #(* % %) str->int str) (str x))))]
      (loop [y (f n) seen #{}]
        (if (contains? seen y)
          (if (= 1 y) true false)
          (recur (f y) (conj seen y)))))))

(defcheck solution-2d92c62a
  (fn happy? ([current previous]
              (let [char-to-digit (fn [ch] (- (char->num ch) (char->num \0)))
                    digits        (map char-to-digit (str current))
                    square        (fn [x] (* x x))
                    squares       (map square digits)
                    total         (reduce + squares)]
                (if (contains? previous current)
                  false
                  (if (= 1 total)
                    true
                    (recur total (conj previous current))))))
    ([current] (happy? current #{}))))

(defcheck solution-2df8f866
  (fn happy? [number]
    (let [sum-of-sqr
          (fn [n]
            (loop [sum 0, x n]
              (if (= x 0)
                sum
                (let [m (mod x 10)]
                  (recur (+ sum (* m m)) (quot x 10))
                  )
                )
              )
            )]

      (loop [h number, times 100000]
        (if (> times 0)
          (if (= h 1)
            true
            (recur (sum-of-sqr h) (dec times))
            )
          false
          )
        )
      )
    ))

(defcheck solution-2e3c1f07
  (fn [z] (loop [s #{} k z] (let
                             [m (apply + (map (fn [x] (#(* % %) (- (char->ascii x) 48))) (str k)))]
                              (if (= 1 m) true
                                          (if (contains? s m) false
                                                              (recur (conj s m) m)))))))

(defcheck solution-2e5196f8
  (fn happy [n]
    (if (= n 1)
      true
      (if (= n 4)
        false
        (let [digits (map #(- (char->ascii %) 48) (str n))]
          (recur (reduce + (map #(* % %) digits))))))))

(defcheck solution-2e5c8af2
  (fn perfect[i] (loop [j i attempts #{}] (if (contains? attempts j) false (if (= 1 j) true (recur (reduce (fn [r v] (+ r (* v v))) 0 (map #(str->int %) (re-seq (re-pattern #"[\d]") (str j)))) (into attempts [j])))))))

(defcheck solution-2ec6b49c
  (fn [n]
    (loop [s #{}
           n n]
      (cond
        (= n 1) true
        (s n) false
        :else (recur (conj s n)
                (int (reduce + (map #(-> %
                                       str
                                       (str->int)
                                       ((fn [x] (* x x))))
                                 (str n)))))))))

(defcheck solution-2edb571f
  (fn hn [num]
    (let [pow (fn [n1 n2] (apply * (repeat n2 n1)))
          n2v (fn [n]
                (vec
                  (reverse
                    (for [i (range (count (str n)))]
                      (rem (quot n (pow 10 i)) 10)))))]
      (loop [i num j 0 s false]
        (if (> j 1000)
          s
          (recur (apply + (map #(* % %) (n2v i)))
            (inc j)
            (if (= 1 (apply + (map #(* % %) (n2v i)))) true false)))))))

(defcheck solution-2ef66ba5
  (fn happy-num [n]
    (letfn[(get-digits
             [n]
             (loop[digits []
                   n n]
               (let[a (quot n 10)
                    b (rem n 10)]
                 (if(> a 0)
                   (recur (conj digits b) a)
                   (conj digits b)))))
           (get-sum
             [digits]
             (reduce #(+ %1 (* %2 %2)) 0 digits))]
      (loop[digits #{n}
            n n]
        (let[new (->
                   (get-digits n)
                   (get-sum))]
          (cond
            (= 1 new) true
            (contains? digits new) false
            :else (recur (conj digits new) new)))))))

(defcheck solution-2efc799f
  (fn  [n]
    (let [num->digits
                 (fn [num]
                   (loop [n num res []]
                     (if (zero? n)
                       res
                       (recur (long (/ n 10)) (cons (mod n 10) res)))))
          happy? (fn [n sads]
                   (let [sum (reduce +  (map #(* % %) (num->digits n)))]
                     (cond  (=  sum 1) true
                            (some #(= % n) sads) false
                            :else (recur sum (cons n sads)))))]
      (happy? n []))))

(defcheck solution-2f067d15
  (fn [i]
    (letfn [(f [n]
              (lazy-seq
                (cons n (f (reduce + (map #(int (Math/pow (str->int (str %)) 2)) (str n)))))))
            (g [s t]
              (cond
                (= 1 (first s)) true
                (contains? t (first s)) false
                :else (recur (rest s) (conj t (first s)))))]
      (g (f i) #{}))))

(defcheck solution-2f91648
  (fn ! [num]
    (let [not-happy #{4 16 20 37 42 58 89 145}
          sum ((fn [number]
                 (loop [digits [] div number]
                   (if (< div 10)
                     (reduce + (map #(* % %) (conj digits div)))
                     (recur (conj digits (mod div 10)) (int (/ div 10)))
                     )
                   )) num)
          ]
      (cond
        (= 1 num) true
        (contains? not-happy num) false
        :else (! sum)
        )
      )
    ))

(defcheck solution-2ff4cf16
  (letfn
   [(digit-seq [n]
      (if (zero? n)
        nil
        (lazy-seq
          (cons (mod n 10)
            (digit-seq (quot n 10))))))

    (dig-square-sum [n]
      (->>
        (digit-seq n)
        (map #(* % %))
        (reduce +)))]

    (fn q4q86 [n]
      "Happy numbers are positive integers that follow a particular formula: take each individual digit, square it, and then sum the squares to get a new number. Repeat with the new number and eventually, you might get to a number whose squared sum is 1. This is a happy number. An unhappy number (or sad number) is one that loops endlessly. Write a function that determines if a number is happy or not."
      ((fn happy [x tried]
         (cond
           (contains? tried x) false
           (= 1 x)             true
           :default
           (recur (dig-square-sum x) (conj tried x))))
       n #{}))))

(defcheck solution-3013ec16
  (fn happy? [n & [seen?]]
    (let [seen? (or seen? #{})]
      (if (or (= 1 n) (seen? n))
        (= 1 n)
        (->> (for [digit (str n)
                   :let [num (str->int (str digit))]]
               (* num num))
          (reduce +)
          (#(happy? % (conj seen? n))))))))

(defcheck solution-303a4e83
  (fn [n]
    (let [nums (fn [x] (map #(str->int (str %)) (seq (str x))) )
          next-n (fn [a] (apply + (map #(* % %) (nums a))))
          ]
      (loop [s (iterate next-n n) acc #{}]
        (let [x (first s)]
          (if (= x 1)
            true
            (if (contains? acc x)
              false
              (recur (next s) (conj acc x))
              )
            )
          )
        )
      )
    ))

(defcheck solution-3075073f
  (fn happy? [n]
    (let [digs (fn digs [n]
                 (if (< n 10) [n]
                              (conj (digs (int (/ n 10))) (mod n 10))))
          sumdigs (fn [n]
                    (apply + (map #(* % %) (digs n))))]
      (loop [test n
             seen #{}]
        (let [sd (sumdigs test)]
          (cond
            (= sd 1) true
            (some #{sd} seen) false
            :default
            (recur sd (conj seen sd))))))))

(defcheck solution-30da1c7e
  (fn [n]
    (loop [x n seen #{n}]
      (let [n (let [nums (loop [c [] x x]
                           (if (= 0 x) c (recur (conj c (mod x 10)) (quot x 10))))]
                (reduce + (map #(* % %) nums)))]
        (if (seen n)
          false
          (if (= 1 n)
            true
            (recur n (conj seen n))))))))

(defcheck solution-30df59dd
  (fn [n]
    ((fn [n s]
       (if (= n 1) true
                   (if (not (nil? (some #{n} s))) false
                                                  (recur
                                                    ((fn [n r]
                                                       (if (< n 10) (+ r (* n n))
                                                                    (recur (quot n 10) (+ r (* (mod n 10) (mod n 10))))))
                                                     n 0)
                                                    (conj s n)))))
     n [])))

(defcheck solution-3139d2fa
  (fn happy? [n]
    (if
     (< n 4)
      (= n 1)
      (happy?
        (->>
          n
          str
          (map #(char->num % 10))
          (map #(* % %))
          (apply +))))))

(defcheck solution-317faf6
  (fn [n]
    (loop [n n h #{}]
      (let [x
            (->> (str n)
              (map #(str->int (str %)))
              (map #(* % %)) (reduce +))]
        (if (h x)
          false
          (or (= x 1) (recur x (conj h n))))))))

(defcheck solution-31c3a6fb
  (fn [a]
    (let [parse-int (fn [s] (str->int (str s)))
          col (iterate (fn [n] (reduce + (map (comp (fn [x] (* x x)) parse-int) (str n)))) a)]
      (if (= (-> (drop-while (fn [[x y]] (nil? y)) (map (fn [x] [x (#{1 4} x)]) col)) first second)
            1)
        true
        false))))

(defcheck solution-32db0b36
  (fn happy
    ([n] (happy n []))
    ([n seen]
     (cond
       (some #{n} seen) false
       (= n 1) true
       :else (let [ss (fn [x] (reduce + (map #(* % %) (map #(- (char->ascii %) 48) (seq (str x))))))]
               (happy (ss n) (conj seen n)))))))

(defcheck solution-33869708
  (fn [n]
    (letfn [(squared [x] (* x x))
            (sos [l] (apply + (map squared l)))
            (digits [s] (map #(- (char->ascii %) 48) s))
            (next-happy [n] (sos (digits (str n))))]
      (loop [seen #{}
             n n]
        (cond
          (= n 1) true
          (contains? seen n) false
          :else (recur
                  (conj seen n)
                  (next-happy n)))))))

(defcheck solution-33e7fdd1
  (fn happy? [x]
    (let [sum-of-squares (fn [x] (->> (.toString x)
                                   (seq)
                                   (map #(- (char->num %) (char->num \0)))
                                   (map #(* % %))
                                   (apply +) ))]
      (loop [y (iterate sum-of-squares x) z1 #{}]
        (let [v (first y) z2 (conj z1 v)]
          (cond
            (= 1 v) true
            (= (count z2) (count z1)) false
            :else (recur (rest y) z2) ))))))

(defcheck solution-341e318d
  (fn [n]
    (let [char->int (fn [c]
                      (- (char->ascii c) 48))
          square (fn [n]
                   (* n n))
          run (fn [n]
                (->> n
                  (str)
                  (map char->int)
                  (map square)
                  (apply +)))]
      (loop [history #{} result n]
        (cond
          (= 1 result) true
          (history result) false
          :else (let [new-n (run result)
                      history (into history [result])]
                  (recur history new-n))))) ))

(defcheck solution-344affc6
  (fn happy? [n]
    (letfn [(digits-of [n]
              (loop [digits []
                     n      n]
                (if (< n 10)
                  (conj digits n)
                  (recur (conj digits (rem n 10)) (quot n 10)))))]
      (loop [n    n
             seen #{}]
        (cond
          (= 1 n)
          true

          (seen n)
          false

          :default
          (recur (reduce + (map #(* % %) (digits-of n)))
            (conj seen n)))))))

(defcheck solution-3493b753
  (fn happy [n]
    (letfn [(square-sum-digits [n]
              (let [digits (map #(char->num %) (str n))]
                (apply + (map #(* % %) digits))))

            (h [n seen]
              (let [s (square-sum-digits n)]
                (if (nil? (seen s)) (recur s (conj seen s))
                                    (= s 1))))]
      (h n #{}))))

(defcheck solution-34f68fb0
  (fn [y] (= 1 (last (take 10 (iterate #(apply + (for [n (for [m (vec (str %))] (- (char->ascii m) 48))] (* n n))) y))))))

(defcheck solution-352ce84b
  (fn f
    ([n] (f n #{}))
    ([n s]
     (if (s n)
       false
       (if (= n 1)
         true
         (f
           (apply +
             (map #(* % %)
               (map #(- (char->num %) (char->num \0)) (str n))))
           (conj s n)))))))

(defcheck solution-3538f53c
  (fn happy-number? [n]
    (letfn [(f [x] (reduce + 0 (map #(let [s (char->num % 10)] (* s s)) (str x))))]
      (loop [x n s #{}]
        (cond
          (= 1 x) true
          (contains? s x) false
          :else (do #_(println x) (recur (f x) (conj s x))))))))

(defcheck solution-35b4608
  (fn
    [n]
    (let [nf
          (fn [p]
            (reduce #(let [t (str->int (str %2))]
                       (+ % (* t t)))
              0
              (str p)))]
      (loop [n (nf n) s #{}]
        (cond
          (s n) false
          (= n 1) true
          :d (recur (nf n) (into s #{n})))))))

(defcheck solution-35bb0fff
  (fn [x]
    ((fn ! [set x]
       (if (contains? set x)
         false
         (if (= 1 x)
           true
           (! (conj set x) (
                            (fn sum-square [x] (apply + (map #(* %1 %1) ((fn num-to-seq [x]
                                                                           (reverse   (loop [result [] num x]
                                                                                        (if (zero? num)
                                                                                          result
                                                                                          (recur (conj result (mod num 10)) (int (/ num  10)))
                                                                                          )
                                                                                        )
                                                                             )) x)) ))


                            x))
           )
         )) #{} x)
    ))

(defcheck solution-35c693ac
  (fn [n]
    (letfn [(digits [x0]
              (loop [xi x0 ds []]
                (if (zero? xi)
                  ds
                  (recur (quot xi 10) (conj ds (rem xi 10))))))]
      (loop [h n visited #{}]
        (let [t (reduce + (map #(* % %) (digits h)))]
          (if (visited t)
            false
            (if (= t 1)
              true
              (recur t (conj visited t)))))))))

(defcheck solution-35d7a886
  (fn [sads x] (let [new (reduce #(+ % (* (str->int %2) (str->int %2))) 0 (re-seq #"\d" (str x)))]
                 (cond
                   (sads new) false
                   (= 1 new) true
                   :else (recur (conj sads new) new)))) #{})

(defcheck solution-35f30ab7
  (fn f
    ([x] (f x []))
    ([x seen]
     (let [numz (->> (str x)
                  (map #(char->num % 10))
                  (map #(* % %))
                  (reduce +))]
       (cond
         (contains? seen x) false
         (= numz 1) true
         :else (recur numz (conj seen x)))))))

(defcheck solution-364156f8
  (fn happy? [n]
    (letfn [(digits [n] (map #(char->num %) (str n)))
            (next-happy [n] (apply + (map #(* % %) (digits n))))]
      (case n
        1 true
        4 false
        (happy? (next-happy n))))))

(defcheck solution-364e7f17
  (fn [x] (letfn [(helper [n] (reduce (fn [acc d] (+ acc (* d d))) 0 (map #(str->int (str %)) (str n))))
                  (is-happy [acc n] (let [z (helper n)]  (if (= 1 z) true (if (contains? acc z) false (is-happy (conj acc z) z)))))]
            (is-happy [] x))))

(defcheck solution-3788e55b
  (fn happy [x]
    (letfn [(happyiter [y]
              (loop [y y sum 0]
                (let [r (rem y 10) q (quot y 10)]
                  (if (zero? q)
                    (+ sum (* r r))
                    (recur q (+ sum (* r r)))
                    )
                  )
                )
              )]
      (loop [x x sofar '[]]
        (cond (= x 1) true
              (some #{x} sofar) false
              :else (recur (happyiter x) (conj sofar x))
              )
        )
      )
    ))

(defcheck solution-37d973b3
  (fn [n]
    (letfn [(step [x] (reduce + (map #(* % %) (map (comp #(- % 48) char->ascii) (.toString x)))))]
      (loop [tried #{}
             n     n]
        (if (= 1 n)
          true
          (if (tried n)
            false
            (recur (conj tried n) (step n))))))))

(defcheck solution-384fd93f
  (fn happy? [n]
    (letfn [
            (sqsum [n]
              (reduce +
                (map (comp #(* % %)
                           #(str->int %)
                           str)
                  (.toString n))))]
      (loop [n n, seen #{n}]
        (let [s (sqsum n)]
          (cond
            (= 1 s)  true
            (seen s) false
            :else    (recur s (conj seen s))))))))

(defcheck solution-38973fc5
  (letfn [(sum-square-digits
            [k]
            (if (< k 1)
              0
              (let [d (mod k 10)
                    n (/ (- k d) 10)]
                (+ (* d d) (sum-square-digits n)))))]
    (fn [x]
      (loop [n x
             sofar #{}]
        (cond
          (= 1 n) true
          (contains? sofar n) false
          :else (recur (sum-square-digits n) (conj sofar n)))))))

(defcheck solution-389a4a8a
  (fn f [n]
    (cond
      (= 1 n) true
      (= 4 n) false
      :else (f (apply +
                 (map #(let [x (str->int (str %))]
                         (* x x))
                   (seq (str n))))))))

(defcheck solution-38c3334f
  (fn happy? [n]
    (letfn [(to-digits [n]
              (map #(- (char->num %) (char->num \0))  (seq (str n))))
            (sq-sum [nseq]
              (reduce #(+ %1 (* %2 %2)) 0 nseq))
            (sq-dig [n]
              (sq-sum (to-digits n)))]
      (loop [nn (sq-dig n) prev #{n}]
        (if (= nn 1) true
                     (if (contains? prev nn) false (recur (sq-dig nn) (conj prev nn))))))))

(defcheck solution-38e222c6
  (fn [x]
    (letfn [(sum-squared [n]
              (reduce + (map #(let [m (str->int (str %))] (* m m))
                          (str n))))]
      (loop [n x
             seen #{}]
        (if (and (> n 1) (nil? (seen n)))
          (recur (sum-squared n) (into seen [n]))
          (= n 1))))))

(defcheck solution-390d444b
  (fn [hn] (let [f (fn [v] (loop [v1 v r []] (let [d (long (/ v1 10)) m (mod v1 10)]
                                               (if (== 0 d) (->> (conj r m) (map #(* % %)) (reduce +) ) ;(conj r m)
                                                            (recur d (conj r m))
                                                            ))))]
             (loop [vf (f hn)]
               (cond
                 (= 1 vf) true
                 (> 10 vf) false
                 :else (recur (f vf))
                 )))))

(defcheck solution-39433027
  (fn checkmy [n]

    (let [transferDigits (fn [n]

                           (->> n str seq (map #(char->num % 10))
                             (reduce #(+ %1 (* %2 %2) ) 0)
                             )
                           )]



      (loop [
             cur n
             history #{}

             ]
        ;(println history)
        ;(println cur)
        ;(println (transferDigits cur))
        (if (= cur 1)
          true
          (if (contains? history cur)
            false
            (recur (transferDigits cur) (clojure.set/union history (hash-set cur )))
            )
          )




        )


      )
    ))

(defcheck solution-397134e4
  (fn happy?
    ([n]
     (happy? n #{n}))
    ([n tried]
     (let [next
           (reduce +
             (map
               #(let [n (- (char->num %) (char->num \0))]
                  (* n n))
               (str n)))]
       (if (= 1 next)
         true
         (if (contains? tried next)
           false
           (happy? next (conj tried next))))))))

(defcheck solution-39ad4000
  (fn f [n]
    (let [ss {\0 0, \1 1, \2 4, \3 9, \4 16, \5 25, \6 36, \7 49, \8 64, \9 81}
          l 1000]
      (letfn [(s [n] (reduce + (map ss (str n))))
              (g [n] (when (> n 1) (cons n (lazy-seq (g (s n))))))]
        (->> (g n)
          (take l)
          count
          (> l))))))

(defcheck solution-3aac23b7
  (fn happy? [n]
    (letfn [(happy-calc [n]
              (apply + (map #((comp (fn [x] (* x x)) str->int str) %) (seq (str n))))
              )]
      (loop [x n seen #{}]
        (if (= 1 x) true
                    (if (some #{x} seen) false (recur (happy-calc x) (cons x seen)))
                    )
        )
      )))

(defcheck solution-3abbd361
  (fn [n]
    (letfn [(sum2 [n] (reduce + (map #(* % %) (map (comp str->int str) (str n)))))]
      (loop [hist #{}
             res (sum2 n)]
        (if (= hist (conj hist res))
          (== 1 (apply min hist))
          (recur (conj hist res) (sum2 res)))))))

(defcheck solution-3b91a677
  (fn happy [n]
    (let [newnum (reduce + (map
                             #(* % %)
                             (map #(- (char->ascii %) 48) (to-array (str n)))))]
      (if (< newnum 10)
        (= newnum 1)
        (happy newnum)))))

(defcheck solution-3bf2a23e
  (fn [n]
    (letfn [(ss [n]
              (->> (seq (str n))
                (map #(- (char->ascii %) 48))
                (map #(* %1 %1))
                (reduce +)))]
      (loop [nn (ss n) seen #{}]
        (cond (= 1 nn) true
              (seen nn) false
              :else (recur (ss nn) (conj seen nn)))))))

(defcheck solution-3c46830d
  (fn __
    [number]
    (letfn [(next-number [n]
              (->> n
                str
                (map str)
                (map #(str->int %))
                (map #(* % %))
                (reduce +)))]
      (loop [n number]
        (let [next-n (next-number n)]
          (case next-n
            4 false
            1 true
            (recur next-n)))))))

(defcheck solution-3c482b40
  (fn [n]
    (loop [n n acc #{}]
      (let [digits (fn [a]
                     (loop [a a acc []]
                       (if (zero? a)
                         acc
                         (recur (quot a 10) (cons (rem a 10) acc)))))
            s (reduce + (map #(* % %) (digits n)))]
        (if (= 1 s)
          true
          (if (acc s)
            false
            (recur s (conj acc s))))))))

(defcheck solution-3c8e376e
  (fn [x]
    (loop [x x
           seen #{x}]
      (let [next-x (->> x str (map #(Math/pow (- (char->ascii %) 48) 2)) (reduce +) int)]
        (cond
          (== next-x 1) true
          (seen next-x) false
          :else (recur next-x (conj seen next-x)))))))

(defcheck solution-3cf7d33a
  (fn happy?
    ([n] (happy? n #{n}))
    ([n s]
     (letfn
      [(digits [n] (if (= n 0) [] (conj (digits (quot n 10)) (rem n 10))))
       (sum-squares [s] (apply + (map #(* % % ) s)))]
       (let [ssd (sum-squares (digits n))]
         (cond
           (= ssd 1) true
           (contains? s ssd) false
           :else (happy? ssd (conj s ssd))))))))

(defcheck solution-3d839c1d
  (fn [x]
    (letfn [(h [n] (->> (str n) (map #(- (char->ascii %) 48)) (map #(* % %)) (apply +)))]
      (loop [n x cache #{}]
        (cond
          (= n 1) true
          (cache n) false
          :else (recur (h n) (conj cache n)))))))

(defcheck solution-3db21e65
  (fn [num]
    (let [hh (fn [n]
               (->> n
                 (str)
                 (seq)
                 (map #(- (char->num %) (char->num \0)))
                 (map #(* % %))
                 (apply +)))]
      (loop [ret [], n num]
        (if (not= -1 (.indexOf ret n))
          (do #_(println (conj ret n)) false)
          (let [new_ret (conj ret n), newn (hh n)]
            (if (= 1 newn)
              (do
                #_(println (conj new_ret newn))
                true)
              (recur new_ret newn))))))))

(defcheck solution-3dcf29bb
  (fn [n]
    (letfn [(digits [n]
              (if (= n 0)
                ()
                (cons (mod n 10) (digits (quot n 10)))))]
      (loop [n n
             seen #{}]
        (cond
          (= n 1) true
          (seen n) false
          :else (recur
                  (reduce + (map #(* % %) (digits n)))
                  (conj seen n)))))))

(defcheck solution-3ddc884b
  #(< 3 %))

(defcheck solution-3ee63394
  (fn happy[n]
    (let [stx (str n)
          l (fn ehsan[x](if (seq x) (cons (- (char->ascii (first x)) 48) (ehsan (next x)))))
          sumsq (fn j[x y] (+ x (* y y)))
          j (reduce sumsq 0 (l stx))]
      (if (< j 10)
        (= 1 j)
        (happy j)
        )
      )
    ))

(defcheck solution-3f450e28
  (fn __
    ([n] (__ n #{}))
    ([n seen]
     #_(println n seen)
     (cond (= n 1) true
           (= n (seen n)) false
           :else (recur
                   (reduce (fn [acc item] (+ acc (* item item))) 0 (map (fn [y] (- y 48)) (map char->ascii (seq (str n)))))
                   (conj seen n))))))

(defcheck solution-3faf804b
  (fn [x]
    {:pre [(pos? x)]}
    (letfn [(sum-of-square-digits [n]
              (->> (str n)
                (map #(char->num % 10))
                (map #(* % %))
                (reduce +)))]
      (loop [r #{}
             i x]
        (let [s (sum-of-square-digits i)]
          (cond
            (= s 1) true
            (contains? r s) false
            :else (recur (conj r s) s)))))))

(defcheck solution-3fc3bbb9
  (fn happy [x]
    (letfn [
            (digits [x]
              (if (< x 10)
                [x]
                (conj (digits (int (/ x 10))) (mod x 10))))
            (next-no [x]
              (reduce + (map #(* % %) (digits x))))
            (happyr [x seen]
              (cond
                (= 1 x) true
                (contains? seen x) false
                :otherwise (happyr (next-no x) (conj seen x))))]
      (happyr x #{}))))

(defcheck solution-3fc77933
  (fn [n]
    (loop [n n m {n nil}]
      (let [n (apply + (for [y (iterate #(quot % 10) n ) :let [x (mod y 10)] :while (> y 0)](* x x)))]
        (cond
          (= n 1) true
          (contains? m n) false
          :else (recur n (assoc m n nil)))))))

(defcheck solution-3fe55279
  (fn happy [n]
    (cond
      (= n 1) true
      (= n 4) false
      (< n 10) (happy (* n n))
      :else (happy  (apply + (map #(* (str->int (str %)) (str->int (str %))) (str n)))))))

(defcheck solution-3fee0f4b
  (fn __ [n]
    (loop [n n visited #{}]
      (cond
        (contains? visited n) false
        (= 1 n) true
        :else
        (recur
          (reduce + (map
                      #(* (str->int %) (str->int %))
                      (re-seq #"[0-9]" (str n))))
          (conj visited n))))))

(defcheck solution-40173258
  (fn happy?[journey in-num]
    (let [sum-squares ((fn sumsq[lst num]
                         (if (= (quot num 10) 0) (reduce + (map #(* % %) (conj lst (rem num 10))))
                                                 (sumsq (conj lst (rem num 10)) (quot num 10)))) [] in-num)]
      (if (= 1 sum-squares) true
                            (if (contains? journey sum-squares) false (happy? (conj journey sum-squares) sum-squares))))) #{})

(defcheck solution-4018d4cb
  (fn happy-number? [x]
    (let [unhappy-cycle #{4 16 37 58 89 145 42 20}
          digits (map #(str->int (str %)) (str x))
          sum (reduce + (map #(* % %) digits))]
      (cond
        (= sum 1) true
        (contains? unhappy-cycle sum) false
        :else (recur sum)))))

(defcheck solution-402c2ae9
  (fn is-happy-number?
    [n]
    (letfn [(sum-of-digit-squares [n]
              (reduce + (map #(let [sn (str->int %)] (* sn sn)) (re-seq #"\d" (str n)))))]
      (loop [n n historis #{}]
        (let [sn (sum-of-digit-squares n)]
          (cond (= sn 1) true
                (contains? historis sn) false
                :else (recur sn (conj historis sn))))))))

(defcheck solution-408747d1
  (fn newHappy [n] (letfn
                    [(hNum [x] (reduce + (map #(* % %) (intToDigits x))))
                     (intToDigits [x] (map #(str->int (str %)) (seq (str x))))]
                     (loop
                      [prev #{}
                       initial (hNum n)]
                       (cond
                         (= initial 1) true
                         (contains? prev initial) false
                         :else (recur (conj prev initial) (hNum initial)))))))

(defcheck solution-40e1e77f
  (fn [n]
    (let [
          digits (fn digits [n]
                   (let [
                         head (mod n 10)
                         tail (int (/ n 10))]
                     (if (= 0 tail)
                       [head]
                       (cons head (digits tail)))))
          sum-digits-sq (fn [n]
                          (reduce + (map #(* % %) (digits n))))]
      (loop [n (sum-digits-sq n), depth 10]
        (if (= 1 n)
          true
          (if (= 0 depth)
            false
            (recur (sum-digits-sq n) (dec depth))))))))

(defcheck solution-412488ec
  (fn [n]
    (letfn [(digits [n] (map #(- (char->num %) (char->num \0)) (seq (str n))))]
      (loop [current-n n, past-ns #{}]
        (cond
          (= current-n 1)               true
          (contains? past-ns current-n) false
          :else                         (recur
                                          (reduce #(+ %1 (* %2 %2)) 0 (digits current-n))
                                          (conj past-ns current-n)))))))

(defcheck solution-41b118f3
  (fn happy? [n]
    (letfn [(digits [a]
              (map #(str->int %) (map str (str a))))
            (sum-of-squares [coll]
              (int (reduce + (map #(Math/pow % 2) coll))))]
      (boolean
        (some
          #(= 1 %)
          (take 100 (iterate (comp sum-of-squares digits) n)))))))

(defcheck solution-41b4b679
  (fn happy [n]
    (let [sq-n (fn [n]
                 (* n n))
          parse-n (fn [n]
                    (map str->int (map str (str n))))
          seen? (fn [n a-set] (if (some #{n} a-set) true false))]
      (loop [n n
             ;;c 0
             seen #{}]
        (cond (= n 1) true
              ;;(= c 50000) false
              (seen? n seen) false
              :else (recur (reduce + (map sq-n (parse-n n)))
                      ;;(inc c)
                      (conj seen n)))))))

(defcheck solution-4233731b
  (fn [n] (let [digits (fn digits [x] (if (< x 10) [x] (conj (digits (quot x 10)) (rem x 10))))]
            (loop [i n
                   c #{i}]
              (let [x (reduce #(+ %1 (* %2 %2)) 0 (digits i))]
                (cond (= 1 x) true
                      (some #{x} c) false
                      :else (recur x (conj c x))))))))

(defcheck solution-4245726c
  (fn [n]
    (let [digits (fn [n]
                   (loop [n n res []]
                     (let [n' (int (/ n 10))
                           r  (rem n 10)]
                       (if (zero? n)
                         res
                         (recur n' (concat [r] res))))))]
      (loop [seen #{} n n]
        (let [d (digits n)
              r (reduce + (map #(* % %) d))]
          (cond (contains? seen r) false
                (= 1 r) true
                :else (recur (conj seen r) r)))))))

(defcheck solution-426442c5
  (fn [n]
    (letfn [(digits [num]
              (loop [acc ()
                     num num]
                (if (< num 10)
                  (conj acc num)
                  (recur (conj acc (rem num 10))
                    (quot num 10)))))
            (squared-sum [& digits]
              (reduce + (apply map #(* % %) digits)))]
      (loop [n n
             acc #{}]
        (let [new-number (squared-sum (digits n))]
          (if (= 1 new-number)
            true
            (if (contains? acc new-number)
              false
              (recur new-number
                (conj acc new-number)))))))))

(defcheck solution-426a6f18
  (fn [i-n]
    (letfn [(sq-sum
              [x]
              (->> (str x) (re-seq #"\d")
                (map str->int)
                (#(map * % %))
                (apply +)))]
      (loop [found #{} n i-n]
        (let [x (sq-sum n)]
          (cond (= 1 x) true
                (found x) false
                :else (recur (conj found x) x)))))))

(defcheck solution-42a6f850
  (fn happy-number? [n]
    (let [sum-square-digits (fn [n] (->> (map #(str->int (str %)) (str n))
                                      (map #(* % %))
                                      (reduce +)))]
      (loop [n (sum-square-digits n), seen #{}]
        (cond
          (= n 1) true
          (seen n) false
          :else (recur (sum-square-digits n) (conj seen n)))))))

(defcheck solution-431f31f7
  (fn happy-numbers [n]
    (let [m {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9}
          parse-digits
            (fn [n] (mapv m (str n)))
          sum-of-squares
            (fn [nums]
              (apply + (map #(* % %) nums)))]
      (loop [ret n vists #{} cnt 0]
        (cond
          (= ret 1) true
          (contains? vists ret) false
          :else
          (recur (sum-of-squares (parse-digits ret))
            (conj vists ret) (inc cnt)))))))

(defcheck solution-4337a2ea
  (fn [t]
    (= 1
      ((fn f
         ([x d]
          (let [s (reduce + (map (comp #(* % %) str->int str) x))]
            (if (d s)
              s
              (f (str s) (conj d s)))))
         ([x] (f (str x) #{})))
       t))))

(defcheck solution-43a25011
  #(if (< % 10) (if (> % 5) true false) true))

(defcheck solution-444985ec
  (fn [n] (letfn [
                  (digits [n] (map #(- (char->ascii %) 48) (str n)))
                  (sos [c] (apply + (map #(* % %) c)))
                  (check [n seen]
                    (let [r (->> n digits sos)]
                      (if (= 1 r)
                        true
                        (if (seen r)
                          false
                          (check r (conj seen r))))))]
            (check n #{}))))

(defcheck solution-44661af
  (fn [n]
    (loop [x n nums []]
      (let [sum ((fn [n]
                   (reduce
                     +
                     (map (fn [x] ((fn [x] (* x x)) (-> x str str->int)))
                       (str n)))) x)]
        (if (= sum 1)
          true
          (if (some #(= % sum) nums)
            false
            (recur sum (conj nums sum))))))))

(defcheck solution-44717f1c
  (fn [n0] (if (some #(= 1 %) (take 10 (iterate (fn [n]
                                                  (apply + (map
                                                             (fn [n1] (let [numr (- (char->num n1) (char->num \0))] (* numr numr)))
                                                             (seq (str n))))
                                                  ) n0 ))) true false)))

(defcheck solution-4479a1e9
  (fn [x]
    (loop [s 0 i x r #{}]
      (if (= i 0)
        (if (= s 1)
          true
          (if (r s)
            false
            (recur 0 s (conj r s))))
        (let [d (mod i 10)]
          (recur (+ s (* d d)) (int (/ i 10)) r))))))

(defcheck solution-44e6312b
  (fn happy? [n]
    (let [square #(* % %)
          sum-square-digits #(reduce + (map (comp square str->int str) (str %)))]
      (loop [n n, seen #{}]
        (cond
          (= n 1) true
          (contains? seen n) false
          :else (recur (sum-square-digits n) (conj seen n)))))))

(defcheck solution-459ae76
  (fn [n]
    (loop [s n
           detector #{}]
      (let [t (apply + (map #(* % %) (map #(- (char->ascii %) 48) (str s))))]
        (cond
          (= t 1) true
          ((complement nil?) (get detector t)) false
          :else (recur t (set (cons t detector))))))))

(defcheck solution-45a82760
  (fn [n]
    (loop [n n
           s (set (list n))]
      (let [ss (->> n
                 (iterate #(/ % 10))
                 (take-while #(>= % 1))
                 (map #(-> % (float) (Math/floor) (Math/round) (mod 10)))
                 (map #(* % %))
                 (apply +))]
        (cond
          (= ss 1)         true
          (contains? s ss) false
          :else            (recur ss (conj s ss)))))))

(defcheck solution-45cfe047
  (fn [n]
    (loop [i n a {}]
      (cond (= i 1) true
            (a i) false
            :else (recur
                    ((fn [n] (apply + (map (comp #(* % %) str->int)
                                        (re-seq #"\d" (str n))))) i)
                    (conj a [i i]))))))

(defcheck solution-46695bdf
  (fn happy? [acc xs]
    (let [x (apply + (map #(* (- (char->ascii %) 48) (- (char->ascii %) 48)) (str xs)))]
      (cond (= 1 x) true
            (acc x) false
            :else (recur (conj acc x) x)))) #{})

(defcheck solution-46708dc5
  (fn [n]
    (letfn [(d [i]
              (if (zero? i) 0
                            (+ (* (mod i 10) (mod i 10)) (d (int (/ i 10))))))]
      (loop [n n
             s #{}]
        (let [h (d n)]
          (cond (= h 1) true
                (contains? s h) false
                :else (recur h (conj s h))))))))

(defcheck solution-468595ad
  (fn [n] (let
           [digits (fn [a] (map #(str->int (str %)) (str a)))
            iteration (fn [s] (reduce + (map #(* % %) s)))]
            (loop [visited #{} cur n]
              (if (contains? visited cur)
                false
                (if (= cur 1)
                  true
                  (recur (conj visited cur) (iteration (digits cur)))))))))

(defcheck solution-46acb7d2
  (fn [n]
    (loop [n n s #{}]
      (cond
        (= n 1) true
        (s n) false
        :else (recur (reduce + (map (comp #(* % %) str->int str) (str n))) (conj s n))))))

(defcheck solution-46fa3898
  (fn i
    ([n] (i n #{}))
    ([n s]
     (letfn
      [(ds[n] (map last (take-while #(> (first %) 0) (iterate (fn [[a b]] (let [m (/ (- a b) 10)] [m (mod m 10)])) [n (mod n 10)]))))
       (hn[n] (apply + (map #(* % %) (ds n))))]
       (let [h (hn n)] (cond (= h 1) true (s h) false :else (recur h (conj s h))))))))

(defcheck solution-473a4885
  (letfn [
          (happy [x] (apply + (for [
                                    i (str x)]
                                (#(* % %) (- (char->ascii i) 48)))))
          (happy? [s x] (cond
                          (s x) false
                          (= 1 x) true
                          true (recur (conj s x) (happy x))))]
    happy?) #{})

(defcheck solution-4756920f
  #(loop [n % s #{}]
     (cond (s n) false, (= 1 n) true, :else
           (recur
             (loop [p n v 0]
               (if (zero? p) v
                             (let [m (mod p 10)]
                               (recur (quot p 10) (+ v (* m m))))))
             (conj s n)))))

(defcheck solution-478b6a6d
  (fn [n]
    (loop [x n seen #{}]
      (let [new
            (reduce #(+ % (* %2 %2))
              0
              (map #(str->int (str %)) (str x)))
            ; xx (println new seen)
            ]
        (cond
          (seen new) false
          (= new 1) true
          :else (recur new (conj seen new)))
        ))
    ))

(defcheck solution-47bb8c1
  (fn [n]
    (let [digits (fn [n] (map #(- (char->num %) (char->num \0)) (str n)))
          sum #(reduce + 0 %)
          happy (fn [curr nums] (cond
                                  (nums curr) false
                                  (= curr 1) true
                                  :else (recur (sum (map #(* % %) (digits curr))) (conj nums curr))))]
      (happy n #{}))))

(defcheck solution-47d52760
  #(not (or (= 2 %) (= 3 %))))

(defcheck solution-47ef2748
  (fn foo [n]
    (letfn [(digits [x] (map #(str->int (str %)) (str x)))
            (square [x] (* x x))]
      (let [rv (->> n digits (map square) (reduce +))]
        (cond
          (= rv 1) true
          (= rv 37) false ; magic number
          :else (recur rv))))))

(defcheck solution-47f61db1
  (fn isHappy?
    [x]
    (let [digits (map #(- (char->num %) (char->num \0) ) (str x))
          sum (apply + (map #(* % %) digits))]
      (cond
        (= 1 sum) true
        (#{4 16 37 58 89 145 42 20} sum) false ; All unhappy numbers reduce to one of these
        :else (recur sum)))))

(defcheck solution-4827853d
  (fn happy-n?
    ([n] (happy-n? n #{}))
    ([n seen]
     (letfn [(n->digits
               ([n_] (n->digits nil n_))
               ([ds n_]
                (if (< 0 n_)
                  (recur (conj ds (rem n_ 10)) (quot n_ 10))
                  ds)))]
       (let [new-n (reduce #(+ %1 (* %2 %2)) 0 (n->digits n))]
         (if (= 1 new-n)
           true
           (if (contains? seen new-n)
             false
             (recur new-n (conj seen new-n)))))))))

(defcheck solution-482a7301
  (fn [i]
    ((fn [i acc] (cond
                   (= i 1) true
                   (contains? acc i) false
                   :else (recur
                           (apply + (map #(* % %) ((fn digits [n r]
                                                     (if (< n r) [n]
                                                                 (cons (mod n r) (lazy-seq (digits (long (/ n r)) r))))) i 10)))
                           (conj acc i))))
     i #{})))

(defcheck solution-483dd1a9
  #(= 1
     (nth (iterate
            (fn [n] (apply + (map (zipmap "0123456789" (map * (range)(range))) (str n))))
            %)
       9)))

(defcheck solution-48e66683
  (fn[x]
    (loop [n x]
      (cond
        (= n 1) true
        (= n 2 ) false
        (= n 3)  false
        (= n 4 ) false
        (= 5 n)  false
        (= 6 n)  false
        (= 7 n ) true
        (=  8 n ) false
        ( = 9 n ) false
        :else (recur ((fn [m]
                        (loop [sum 0 r (quot m 10) module (mod m 10) ]
                          (if (= r 0)
                            (+ sum (* module module))
                            (recur (+ sum (* module module) )  (quot r 10 ) (mod r 10) )
                            )
                          )
                        ) n))
        )
      )
    ))

(defcheck solution-4a71a390
  (fn happy? [n]
    (letfn [(sqsum [n] (reduce + (map #(* % %) (map #(- (char->num %) (char->num \0)) (vec (str n))))))]
      (loop [nset #{n} lastn n]
        #_(println (str nset))
        (let [newn (sqsum lastn)]
          (if (nset newn)
            false
            (if (== 1 newn)
              true
              (recur (conj nset newn) newn))))))))

(defcheck solution-4a790e
  (fn happy-number
    ([n] (happy-number n #{}))
    ([n s]
     (let [hn (clojure.walk/walk
                #(let [x (str->int (str %))]
                   (* x x))
                #(apply + %)
                (seq (str n)))]
       (cond (= hn 1) true
             (contains? s hn) false
             :else (recur hn (conj s hn)))))))

(defcheck solution-4a843af
  (letfn [
          (digits [n] (map #(str->int %) (re-seq #"\d" (str n))))
          (sqr-digits-sum [n] (reduce + (map #(* % %) (digits [n]))))
          ]
    #(loop [n % r #{}]
       (cond
         (= 1 n) true
         (r n)   false
         :else   (recur (sqr-digits-sum n) (conj r n))))))

(defcheck solution-4aaf4323
  (fn [n]
    (loop [n n, seen #{}]
      (cond (= n 1)  true
            (seen n) false
            :else
            (recur (reduce +
                     (map (comp #(* % %) #(- (char->ascii %) 48)) (str n)))
              (conj seen n))))))

(defcheck solution-4ae77f9a
  (fn happy? [f n]
    (loop [i n]
      (condp = i
        1 true
        4 false
        i (recur (f i))))) (fn [i]
                             (apply + (map #(let [x (str->int (str %))] (* x x)) (str i)))))

(defcheck solution-4b1970e4
  (fn happy [n]
    #_(println n)
    (cond (= n 4) false
          (= n 1) true
          :else   (let [ssd (->> (str n)
                              (map #(- (char->num %) (char->num \0)))
                              (map #( * % % ))
                              (reduce +)
                              )]
                    (recur ssd)
                    )
          )
    ))

(defcheck solution-4b29168d
  (fn [x]
    ((fn r [y s]
       (cond
         (= y 1) true
         (s y) false
         :else (r
                 (reduce + (map #(* % %) (map #(char->num %) (str y))))
                 (conj s y))))
     x #{})))

(defcheck solution-4c1e0e92
  (fn happy-number?
    [n]
    (cond (= n 4) false
          (= n 1) true
          :else (happy-number? (apply + (map #(* % %) ((fn digits
                                                         [n xs]
                                                         (if (zero? n)
                                                           xs
                                                           (digits (quot n 10) (cons (rem n 10) xs))))
                                                       n [])))))))

(defcheck solution-4c472359
  (fn happy? [n]
    (loop [s #{}
           i n]
      (if (= 1 i)
        true
        (if (contains? s i)
          false
          (recur (conj s i)
            (loop [sum 0
                   j i]
              (if (= 0 j)
                sum
                (recur (+ sum (* (mod j 10)
                                (mod j 10)))
                  (quot j 10))))))))))

(defcheck solution-4c48bbbe
  (fn happy? [n]
    (letfn [(digits [n]
              (map #(str->int (str %)) (str n)))
            (sum-of-squares [n]
              (reduce + (map #(* % %) (digits n))))]
      (boolean (some #{1} (take 100 (iterate sum-of-squares n)))))))

(defcheck solution-4c631781
  (fn f [s n]
    (or (= 1 n)
        (if (s n)
          false
          (f (conj s n)
            (apply + (for [i (iterate #(int (/ % 10)) n)
                           :while (pos? i)
                           d [(mod i 10)]]
                       (* d d))))))) #{})

(defcheck solution-4c76d256
  (fn [n]
    (loop [s #{}
           n n]
      (cond
        (= n 1) true
        (s n) false
        :else
        (recur
          (conj s n)
          (loop
           [m 0
            n n]
            (if (= n 0)
              m
              (recur
                (+ m (* (rem n 10) (rem n 10)))
                (quot n 10)))))))))

(defcheck solution-4c8d1e74
  (fn [x] (= 1 (last (take 1000 (iterate
                                  #(reduce + (map (fn[x] (let [y (str->int (str x))] (* y y))) (str  %))) x))))))

(defcheck solution-4cf10f28
  (fn [z] (letfn [(d [n l] (if (= 1 n)
                             true
                             (if (nil? (l n))
                               (d (reduce + (map #(* % %)(map #(- (char->ascii %) 48) (str n)))) (conj l n))
                               false)))] (d z #{}))))

(defcheck solution-4d52ecbb
  (fn happy-nums
    [n]
    (letfn [(get-next
              [x]
              (apply + (map #(* % %)
                         (map #(str->int (str %)) (str x)))))]
      (loop [r []
             nn (get-next n)]
        (if (= nn 1)
          true
          (if (some #(= % nn) r)
            false
            (recur (conj r nn) (get-next nn))))
        ))))

(defcheck solution-4da4f397
  (fn [n]
    (loop [m n soln #{}]
      (letfn [(partnum [v] (if (< v 10) (vector v) (conj (partnum (quot v 10)) (rem v 10))))]
        (let [newm (reduce + (map #(* % %) (partnum m)))]
          (cond
            (= newm 1) true
            (contains? soln newm) false
            :else (recur newm (conj soln newm))))))))

(defcheck solution-4e106a8f
  (fn [n]
    (letfn [
            (bd [n]
              (map
                #(rem (quot n %) 10)
                (take-while
                  #(<= % n)
                  (iterate
                    #(* % 10) 1))))
            ]
      (loop [vst #{n} n n]
        (let
         [nx
          (reduce
            +
            (map
              #(* % %)
              (bd n)))]
          (cond (= nx 1) true
                (vst nx) false
                :else
                (recur
                  (conj
                    vst
                    nx)
                  nx))))
      )))

(defcheck solution-4e843daa
  (fn [n]
    (loop [c n s #{}]
      (cond
        (= c 1) true
        (contains? s c) false
        :else (recur (reduce + (map #(* (str->int (str %)) (str->int (str %))) (str c))) (conj s c))))))

(defcheck solution-4e9b4ee3
  (fn [n]
    (loop [rs #{}, r n]
      (let [p #(* % %)
            r (apply + (map #(-> % char->ascii(- 48) p) (str r)))]
        (cond (= r 1) true
              (rs r) false
              :else (recur (conj rs r) r))))))

(defcheck solution-4eabf52a
  (letfn [(sqr-digits [n]
            (apply +
              (map #(* % %)
                (map #(str->int (str %)) (str n)))))]
    (fn happy?
      ([n] (happy? n #{}))
      ([n checked]
       (let [acc (sqr-digits n)]
         (cond
           (= acc 1) true
           (checked acc) false
           :else (recur acc (conj checked acc))))))))

(defcheck solution-4eac4ef8
  (let [digits (fn [i]
                 (loop [d '() q i]
                   (if (= 0 q)
                     d
                     (recur (cons (mod q 10) d) (quot q 10)))))
        step (fn [i]
               (let [d (digits i)
                     sq (map #(* % %) d)]
                 (reduce + sq)))]
    (fn [i0]
      (loop [i (step i0) past #{i0}]
        (if (= i 1)
          true
          (if (past i)
            false
            (recur (step i) (conj past i))))))))

(defcheck solution-4ef23475
  (fn [n]
    (letfn [(sqsum [n] (reduce #(let [x (str->int (str %2))]
                                  (+ %1 (* x x))) 0 (str n)))]
      (loop [seen #{} x n]
        (let [ss (sqsum x)]
          (cond (seen ss) false
                (= ss 1) true
                :else (recur (conj seen ss) ss)))))))

(defcheck solution-4f36450d
  (fn [n]
    (letfn [(sum-sq-digits [n]
              (reduce +
                (for
                 [c (str n) :let [d (- (char->num c) (char->num \0))]]
                  (* d d))))]
      (loop [i n found #{}]
        (cond
          (= i 1) true
          (found i) false
          :else
          (recur
            (sum-sq-digits i)
            (conj found i)))))))

(defcheck solution-4f54b6a
  (fn [m]
    (loop [n m
           s #{}]
      (let [x (->> n str
                (map #(str->int (str %)))
                (map #(* % %))
                (apply +))]
        (cond
          (= x 1) true
          (contains? s x) false
          :else (recur x (conj s x)))))))

(defcheck solution-4f70f301
  (fn thisfunc
    ([n] (thisfunc n #{}))
    ([n unhappy]
     (cond
       (= 1 n) true
       (unhappy n) false
       true (let [x (reduce + (map #(* % %) (map #(str->int %) (map str (str n)))))]
              (thisfunc x (conj unhappy n)))))))

(defcheck solution-4f97a438
  (fn happy?
    ([x] (happy? x #{}))
    ([x history]
     (letfn [(digits [x]
               (if (< x 10)
                 [x]
                 (conj (digits (quot x 10)) (rem x 10))))
             (square-sum [x]
               (->> x digits (map #(* % %)) (apply +)))]
       (let [y (square-sum x)]
         (cond
           (== 1 y) true
           (history y) false
           :else (recur y (conj history y))))))))

(defcheck solution-50417f1e
  (fn [s]
    ((fn ! [visited val]
       (letfn [(digits [i] (map #(str->int (str  %)) (str i)))]
         (let [hpy  (reduce + (map #(* % %) (digits val)))]
           (if (= hpy 1)
             true
             (if (contains? visited hpy)
               false
               (! (conj visited hpy) hpy)))))) #{} s)))

(defcheck solution-507b0034
  #(letfn [(f1 [x] (* x x))
           (f2 [x]
             (if (< x 10)
               [x]
               (let [a (mod x 10)
                     b (/ (- x a) 10)]
                 (lazy-cat [a] (f2 b)))))
           (f3 [x]
             (apply + (map f1 (f2 x))))]
     ((fn f [x y]
        (let [a (f3 x)]
          (cond
            (= a 1) true
            (y a) false
            :els (f a (conj y a))))) % #{})))

(defcheck solution-50d6bf6e
  (fn happy-num? [x]
    (letfn [(happy-seq [x]
              (lazy-seq (cons x (happy-seq
                                  (reduce + (map #(let [x (- (char->ascii %) 48)]
                                                    (* x x)) (str x)))))))]
      (loop [[h & t] (happy-seq x) result #{}]
        (cond
          (result 1) true
          (result h) false
          :else (recur t (conj result h)))))))

(defcheck solution-510792cf
  (fn [n]
    (let [newnr (fn [n] (apply + (map #(* % %) (map #(str->int (str %)) (str n)))))]
      (loop [l [] n n]
        (if (= 1 n)
          true
          (if (contains? l n)
            false
            (recur (conj l n) (newnr n))))))))

(defcheck solution-51261a9b
  (fn [n]
    (loop [n n
           s #{}]
      (let [x (apply + (map #(let [i (- (char->num %) (char->num \0))] (* i i)) (str n)))]
        (cond (= x 1) true
              (s x) false
              :else (recur x (conj s x)))))))

(defcheck solution-51c2b252
  (fn happy? [n]
    (let [
          split-num
          (fn [n]
            (loop [n n result '()]
              (if (= n 0)
                (if (= 0 (count result)) [0] result)
                (recur (quot n 10) (cons (mod n 10) result)))))

          get-new-num
          (fn [n]
            (reduce + (map #(* % %)
                        (split-num n))))
          ]
      (loop [n n history [n]]
        (let [newn (get-new-num n)]
          (if (= newn 1)
            true
            (if (some #{newn} history)
              false
              (recur newn (conj history newn)))))))))

(defcheck solution-52d72fd4
  (fn happy? ([n] (happy? n #{}))
    ([n acc]
     (let [c2i (zipmap "0123456789" (range))
           val (->> n
                 str
                 (map c2i)
                 (map #(* % %))
                 (reduce +))]
       (cond
         (= 1 val) true
         (acc val) false
         :else (recur val (conj acc val)))))))

(defcheck solution-52e3ef15
  (fn [x]
    (letfn [(digits [x]
              (loop [x x
                     digits '()]
                (if (> x 0)
                  (recur (int (/ x 10)) (cons (mod x 10) digits))
                  digits)))
            (sum-sqs [xs] (->> xs (map #(* % %)) (reduce +)))]
      (loop [x x
             seen #{}]
        (let [y (sum-sqs (digits x))]
          (cond
            (= 1 y) true
            (seen y) false
            :else (recur y (conj seen y))))))))

(defcheck solution-535116b5
  (fn [n]
    (letfn [(digits [n]
              (loop [n n
                     r '()]
                (if (= n 0)
                  r
                  (recur (quot n 10) (cons (rem n 10) r)))))
            (squared-sum [n]
              (reduce (fn [a b] (+ a (* b b))) 0 (digits n)))]
      (loop [n n
             i 0]
        (if (= 1 (squared-sum n))
          true
          (if (< i 10000)
            (recur (squared-sum n) (inc i))
            false))))))

(defcheck solution-5368d61f
  (fn happynumber [n]
    (cond (= n 1) true
          (= n 4) false
          :ELSE
          (recur (apply +
                   (map (fn [m] (let [x (- (char->ascii m) 48)] (* x x)))
                     (seq (str n))))))))

(defcheck solution-5395754a
  #(let
    [nextnum (fn [x]
               (->> x
                 str
                 (map (comp str->int str))
                 (map (fn [a] (* a a)))
                 (apply +)))

     ]
     ((fn e [a b]
        (and
         (not (b a))
         (or
          (== a 1)
          (e (nextnum a) (conj b a)))))
      % #{})))

(defcheck solution-53c50a6a
  (fn happy? [n]
    (let [happy (fn [x] (reduce +
                          (map #(* %1 %1)
                            (map str->int
                              (re-seq #"." (str x))))))]
      (= 1 (happy
             (last (take-while #(and (not= 1 %1) (not= 4 %1))
                     (iterate happy n))))))))

(defcheck solution-53c7e57a
  (fn [n]
    (letfn [(sum-squared-digits [n]
              (->> n
                str
                (map #(str->int (str %)))
                (map #(* % %))
                (apply +)))]
      (loop [accs #{}
             i n]
        (let [nxt (sum-squared-digits i)]
          (cond
            (accs nxt) false
            (= nxt 1) true
            :else (recur (conj accs i)
                    nxt)))))))

(defcheck solution-54032a9e
  (fn [x] (letfn [(s [n] (apply + (map (fn [x] (let [y (- (char->ascii x) 48)] (* y y))) (str n))))] (= 1 (first (drop-while #(not (#{1 4} %)) (iterate s x)))))))

(defcheck solution-567bfe16
  (fn [n]
    (loop [n n
           res #{}]
      (cond
        (= n 1) true
        (contains? res n ) false
        :else (recur (reduce + (map (comp #(* % %) str->int str)(str n))) (conj res n)) ))))

(defcheck solution-5697c83c
  (fn [x]
    (let [sq (fn [n]
               (reduce #(+ %1 (int (Math/pow (rem %2 10)2)))
                 0
                 (take-while #(not= 0 %)
                   (iterate #(int (/ % 10)) n))))
          s (iterate sq x)]
      (= 1
        (loop [ac [] n (first s) s (rest s)]
          (if (or (some #(= % n) ac)
                  (= 1 n))
            n
            (recur (conj ac n) (first s) (rest s))))))))

(defcheck solution-5716ae
  (fn [n]
    (= 1
      (loop [n n cache #{}]
        (if (or (< n 2) (cache n)) n
                                   (recur (reduce + (map (fn [ch] (let [i (str->int (str ch))] (* i i))) (str n)))
                                     (conj cache n)))))))

(defcheck solution-572fc1b2
  (fn [n]
    (loop [ns #{n} n1 n]
      (let [r (loop [m1 (quot n1 10) m2 (rem n1 10) acc 0]
                (if (= 0 m1)
                  (+ acc (* m2 m2))
                  (recur (quot m1 10) (rem m1 10) (+ acc (* m2 m2)))
                  )

                )]
        (cond
          (= 1 r) true
          (contains? ns r) false
          :else (recur (conj ns r) r))
        )



      )))

(defcheck solution-57408a1a
  (fn [i] ({1 true 4 false} (some #(when (#{4 1} %) %) (iterate (fn [d] (->> d (str) (map str) (map str->int) (map #(* % %)) (apply +))) i )))))

(defcheck solution-57a76328
  (fn [n] (let [the-sum
                (fn [k] (loop [curr k result 0]
                          (if (= 0 curr) result (recur (quot curr 10) (+ result (* (rem curr 10) (rem curr 10)))))
                          )
                  )]
            (loop [curr n so-far #{}]
              (cond
                (= curr 1) true
                (contains? so-far curr) false
                :else (let [z (the-sum curr)]
                        (recur z (conj so-far curr))
                        )
                )
              )
            )
    ))

(defcheck solution-57d0db56
  (fn [n]
    (loop [seen #{} n n]
      (let [v (->> n
                str
                (map (fn [x] (char->num x 10)))
                (map #(* % %))
                (reduce + ))]
        (cond
          (= v 1) true
          (contains? seen v) false
          :else (recur (conj seen v) v))))))

(defcheck solution-57fae935
  (fn [num]
    (letfn [(square-sum-of-digits [num]
              (reduce + (map #(* % %) (map #(str->int (str %)) (.toString num)))))
            (step [num mid-results]
              (let [mid-result (square-sum-of-digits num)]
                (if (= 1 mid-result)
                  true
                  (if (some #{mid-result} mid-results)
                    false
                    (step mid-result (cons mid-result mid-results))))))]
      (step num [num]))))

(defcheck solution-584e628c
  (fn
    [n]
    (letfn [(happy-number?
              [seen? n]
              (cond
                (= n 1) true
                (seen? n) false
                :else (let [m (reduce (fn [acc x]
                                        (let [y (str->int (str x))]
                                          (+ acc (* y y)))) 0 (str n))]
                        (recur (conj seen? n) m))))]
      (happy-number? #{} n))))

(defcheck solution-58aefebb
  (fn [n] (letfn
           [(digits [n] (map #(str->int (str %)) (str n)))
            (happy? [n xs]
              (if (= n 1)
                true
                (if (contains? xs n)
                  false
                  (let
                   [m (reduce + (map #(* % %) (digits n)))]
                    (happy? m (conj xs n))))))]
            (happy? n #{}))))

(defcheck solution-5931a9a9
  (fn [n]
    (let [nseq #(map str->int (map str (str %)))
          sumsq (fn [s] (reduce #(+ %1 (* %2 %2)) 0 (nseq s)))]
      (loop [ss (sumsq n) results #{}]
        (if (= ss 1)
          true
          (if (contains? results ss)
            false
            (recur (sumsq ss) (conj results ss))))))))

(defcheck solution-59424a48
  (fn prob-0086
    [in-n]
    (let [digits-of (fn digits-of
                      [n]
                      (map #(str->int (str %)) (seq (str n))))

          sum-of-squares (fn sum-of-squares
                           [xs]
                           (apply + (map #(* % %) xs)))]

      (loop [n in-n, seen #{}]
        (let [sqd-sum (sum-of-squares (digits-of n))]
          (cond
            (= sqd-sum 1)            true
            (contains? seen sqd-sum) false
            :else                    (recur sqd-sum (conj seen sqd-sum))))))))

(defcheck solution-5945c9f9
  (fn [n]
    (letfn [(which-repeat [coll]
              (loop [s #{} [x & xs] coll]
                (if (contains? s x) x
                                    (recur (conj s x) xs))))
            (one-num [n]
              (reduce (fn [a c] (+ a (let [n1 (- (char->num c) (char->num \0))] (* n1 n1)))) 0 (str n)))]
      (= 1 (which-repeat (iterate one-num n))))))

(defcheck solution-5949f590
  (fn [x]
    (loop [x x seen '()]
      (cond (= 1 x) true
            (some #(= % x) seen) false
            true
            (recur
              (reduce +
                (map #(int (Math/pow
                             (str->int (str %)) 2))
                  (str x)))
              (conj seen x))))))

(defcheck solution-5a172819
  (fn [x] (not= nil (some (fn [x] (= x 1))
                      (take 100
                        (iterate #(apply + (for [y (str %)] (let [c (- (char->ascii y) 48)] (* c c)))) x))))))

(defcheck solution-5a2b7e3c
  (letfn [(digits [n]
            (if (zero? n) [0] (cons (mod n 10) (digits (quot n 10)))))
          (hnext [n]
            (reduce + (map #(* % %) (digits n))))
          (happys [s n]
            (if (contains? s n)
              s
              (happys (conj s n) (hnext n))))]
    (fn [n] (contains? (happys #{} n) 1))))

(defcheck solution-5a4a1e4d
  (fn f
    ([n] (f n #{}))
    ([n s]
     (letfn [(d [n] (map (comp str->int str) (str n)))
             (u [n] (apply + (map #(* % %) (d n))))]
       (let [x (u n)]
         (cond
           (= x 1) true
           (s x) false
           :else (recur x (conj s x))))))))

(defcheck solution-5a950879
  (fn [n]
    (let [digits (fn digits [n]
                   (if (= 0 (quot n 10))
                     (list n)
                     (conj (digits (quot n 10)) (rem n 10))))
          step (fn [m] (apply + (map #(* % %) (digits m))))
          find-dup (fn find-dup ( [xs] (find-dup xs #{}))
                     ([ [x & xs] acc] (if (acc x) x (recur xs (conj acc x)))))]
      (= 1 (find-dup (iterate step n))))))

(defcheck solution-5ad4fee3
  (fn happy?
    ([n]
     (happy? n []))
    ([n seen-numbers]
     (let [to-digits (fn [n] (map (comp str->int str) (str n)))
           square    #(* % %)
           happy-n (reduce + (map square (to-digits n)))]
       (cond (= 1 happy-n)
             true
             (some (set seen-numbers) #{happy-n})
             false
             :else
             (recur happy-n (cons happy-n seen-numbers)))))))

(defcheck solution-5b44d3ff
  (fn [n]
    (letfn [(ds [n]
              (if (pos? n)
                (conj (ds (int (/ n 10))) (mod n 10))))]
      (loop [n n ns []]
        (cond
          (some #{n} ns) false
          (= 1 n) true
          :e (recur (->> (ds n) (map #(* % %)) (reduce +)) (conj ns n)))))))

(defcheck solution-5b4ab6c7
  (fn [m] (= 1
            (->> m
              (iterate (fn [n] (let [d (map (zipmap "0123456789" (range)) (str n)) ]
                                 (apply + (map #(* % %) d) ))) )
              (take 20)
              (last) ) )))

(defcheck solution-5bc9dace
  (fn [n]
    (letfn [(ds [x] (reduce + (map #(* % %) (map #(str->int (str %)) (seq (str x))))))]
      (loop [i 1 t n]
        (if (= 1 t)
          true
          (if (= 50 i)
            false
            (recur (inc i) (ds t))))))))

(defcheck solution-5be6a9ee
  (fn happy-numbers [x]
    (letfn [(calc [x]
              (apply + (map
                         (fn [x] (let [x (- (char->ascii x) 48)] (* x x) ))
                         (vec (str x)))))]

      (loop [x x a #{}]
        (cond (= x 1) true
              (contains? a x) false
              :else (recur (calc x) (conj a x))))

      )))

(defcheck solution-5c36a457
  (fn [num]
    (letfn [(sum-of-squares
              [x]
              (reduce + (map #(* % %) (map #(str->int %) (re-seq #"\d" (str x))))))]
      (let [bla (iterate #(cons (sum-of-squares (first %)) %) (cons num '()))]
        (= 1 (first (first (drop-while #(= (count %) (count (distinct %))) bla))))
        ))))

(defcheck solution-5d39ed6a
  (fn [n]
    (loop [n n seen #{}]
      (cond
        (= 1 n) true
        (seen n) false
        :else (recur
                (reduce + (map #(-> % str str->int (Math/pow 2) int) (str n)))
                (conj seen n))))))

(defcheck solution-5d4d5ac3
  (fn task-86 [n]
    ((fn happy-number [n known-numbers]
       (let [next-number (apply +
                           (->>
                             (str n)
                             (map #(- (char->ascii %) 48))
                             (map #(* % %))))]
         (if (= 1 next-number) true
                               (if (known-numbers next-number) false (recur next-number (conj known-numbers next-number)))))) n #{})))

(defcheck solution-5d748d68
  (fn [n] (= 1 (some #{1 20}
                 (iterate
                   (fn [n]
                     (->> (str n)
                       (map #(str->int (str %)))
                       (map #(* % %))
                       (apply +)))

                   n)))))

(defcheck solution-5d966064
  (partial
    (fn happy? [acc n]
      (let [s (int (reduce + (map #(Math/pow (str->int (str %)) 2)  (seq (str n)))))]
        (cond (= s 1) true
              (= true (some #(= s %) acc)) false
              :else (recur (conj acc s) s)))) '()))

(defcheck solution-5db8d18d
  (fn [x]
    (loop [n x, i 0]
      (cond (= 1 n) true
            (> i 100) false ;; over-limit condition (allow 100 tries)
            :else (recur (reduce + (->> n str (re-seq #"[0-9]")(map #(let [n (str->int %)] (* n n)))))
                    (inc i))))))

(defcheck solution-5dc1fe0a
  (fn [num]
    (letfn [(sum [a] (reduce + (map #(* % %) a)))
            (digits [b] (map #(char->num % 10) (seq (str b))))]
      (loop [x num seen (set [])]
        (cond
          (seen x) false
          (= 1 x) true
          :else (recur (sum (digits x)) (conj seen x)))))))

(defcheck solution-5defb48e
  (fn happy [n]
    (letfn [(sum-squares [n]
              (->> n
                str
                (map str)
                (map #(str->int %))
                (map #(* % %))
                (reduce +)))

            (happy' [n visited]
              (let [s (sum-squares n)]
                (cond
                  (visited s)
                  false

                  (= 1 s)
                  true

                  :else
                  (recur s (conj visited s)))))]
      (happy' n #{}))))

(defcheck solution-5e4e6778
  (letfn [(ds [n] (if (pos? n) (conj (ds (quot n 10)) (rem n 10)) []))
          (sq-sum [n] (->> (ds n) (map #(* % %)) (reduce +)))]
    #(->> % (iterate sq-sum) (some #{1 4}) (= 1))))

(defcheck solution-5e985612
  (fn [n]
    (letfn [(sqr [x] (* x x))
            (sqrdig [x] (apply + (map #(sqr (- (char->ascii %) 48)) (str x))))]
      (loop [n n sn #{}]
        (cond
          (= n 1) true
          (sn n) false
          :default (recur (sqrdig n) (conj sn n)))))))

(defcheck solution-5eefcaf0
  (fn happyX [n] ((fn happyRec [n seen]
                    (if (= n 1) true
                                (if (contains? seen n) false
                                                       (happyRec ((fn squaresDigits [n] ((fn sumsquares [x] (reduce + (map (fn [n] (* n n)) x))) (
                                                                                                                                                  (fn digits[n]
                                                                                                                                                    (if (< n 10) (list n)
                                                                                                                                                                 (cons (mod n 10) (digits (quot n 10))))) n)))
                                                                  n) (conj seen n))
                                                       )))
                  n #{})))

(defcheck solution-5f23e805
  (let [sum-of-squares (fn fun [n]
                         (let [digits (map second
                                        (take-while #(not= [0 0] %)
                                          (iterate (fn [[a b]] [(quot a 10) (rem a 10)])
                                            [(quot n 10) (rem n 10)])))
                               new-num (int (apply + (map #(Math/pow % 2) digits)))]
                           (cons new-num (lazy-seq (fun new-num)))))
        happy-number? (fn happy-number? [n]
                        (if (= 1 (first (drop-while #(not (#{1 4} %)) (sum-of-squares n))))
                          true
                          false))]
    happy-number?))

(defcheck solution-5f3a386d
  (fn [y]
    (let [f (fn [x](apply + (map (comp #(* % %) #(- % 48) char->ascii) (seq (str x)))))]
      (= (some #{4 1} (iterate f y)) 1))))

(defcheck solution-5fc5d505
  (fn [n]
    (letfn [
            (sumsq [n] (if (zero? n) 0 (let [d (rem n 10)] (+ (* d d) (sumsq (int (/ n 10)))))))
            (end [st n] (if (contains? st n) n (recur (conj st n) (sumsq n))))
            ]
      (= 1 (end #{} n)))))

(defcheck solution-6030a2cf
  (fn [n]
    (loop [n n
           seen #{}]
      (cond
        (= n 1)
        true
        (contains? seen n)
        false
        :else
        (recur
          (->> n
            (str)
            (map #(- (char->num %) (char->num \0)))
            (map #(* % %))
            (reduce +))
          (conj seen n))))))

(defcheck solution-604f5514
  (fn [n]
    (letfn [(gen [n]
              (->> n
                str seq
                (map str)
                (map #(str->int %))
                (map #(* % %))
                (reduce +)))]
      (boolean (some #(= 1 %) (take 100 (iterate gen n)))))))

(defcheck solution-60e5696c
  (fn happy? [n]
    (letfn [(next-num [x]
              (->> x
                str
                (map str)
                (map #(str->int %))
                (map #(* % %))
                (reduce + 0)))
            (happy' [x seen]
              (let [x' (next-num x)]
                (cond
                  (= 1 x') true
                  (some #{x'} seen) false
                  :default (happy' x' (conj seen x')))))]
      (happy' n #{}))))

(defcheck solution-61b86870
  (fn happy-number?
    [n]
    (letfn [(step [seen n]
              (let [digits (map #(char->num %) (str n))
                    squared (map (comp int #(Math/pow % 2)) digits)
                    summed (apply + squared)]
                (cond (= 1 summed) true
                      (seen summed) false
                      :else (recur (conj seen summed) summed))))]
      (step #{} n))))

(defcheck solution-61bcd8dd
  (fn happy? [n]
    (letfn [(step [x]
              (->> x
                (str)
                (re-seq #"\d")
                (map #(str->int %))
                (map #(* % %))
                (reduce +)))
            (generate [x xs]
              (cond
                (= x 1) (conj xs x)
                (contains? xs x) (conj xs x)
                :else (generate (step x) (conj xs x))))]
      (= 1 (last (generate n []))))))

(defcheck solution-61eafaff
  (fn [n]
    (letfn [
            (dsquare [n]
              (loop [i n ds 0]
                (if (zero? i)
                  ds
                  (let [r (mod i 10)]
                    (recur (quot i 10) (+ ds (* r r)))
                    )
                  )
                )
              )
            ]
      (loop [i n s #{}]
        (if (= 1 i)
          true
          (if (s i)
            false
            (recur (dsquare i) (conj s i))
            )
          )
        )
      )
    ))

(defcheck solution-61f8e356
  (fn [n] (= 1 (nth (iterate (fn [x] (reduce #(+ (* %2 %2) %1)
                                       0
                                       (map #(str->int (str %))
                                         (seq (str x))))) n) 10))))

(defcheck solution-623ad1dc
  (fn [n]
    (loop [n n p #{}]
      (let [s (reduce #(+ % (* %2 %2)) 0 (map #(- (char->ascii %) 48) (str n)))]
        (cond
          (= 1 s) true
          (p s)   false
          :else   (recur s (conj p n)))))))

(defcheck solution-62454345
  (fn happy [number]
    (let [sumofsq (fn [n]
                    (loop [q (quot n 10) r (mod n 10) result [r]]
                      (if (= q 0)
                        (reduce + (map #(* % %) result))
                        (let [irem (mod q 10)]
                          (recur (quot q 10) irem (conj result irem))))))]
      (loop [newnum (sumofsq (sumofsq number))
             seenset #{number}]
        (do
          #_(prn newnum seenset)
          (if (contains? seenset newnum)
            (do
              false)
            (if (= newnum 1)
              true
              (let [nnum (sumofsq newnum)]
                #_(prn (type (sumofsq newnum)))
                (recur nnum (conj seenset newnum))))))))))

(defcheck solution-631de4d4
  (fn [x]
    (letfn [(next-val [n] (reduce + (map #(* % %) (map #(- (char->num %) (char->num \0)) (seq (str n))))))]
      (loop [v x vs #{}]
        (cond (= v 1) true
              (contains? vs v) false
              :else (recur (next-val v) (conj vs v)))))))

(defcheck solution-632017e4
  (letfn [(s [x] (* x x))]
    (fn [n]
      (loop [i (iterate (fn [x] (reduce #(+ % (s (- (char->ascii %2) 48))) 0 (str x))) n) e #{}]
        (let [z (first i)]
          (if (e z)
            false
            (if (= 1 z)
              true
              (recur (rest i), (conj e z)))))))))

(defcheck solution-63604529
  (fn happy-number? [a-num]
    (letfn [(digits-seq [a-num]
              (map #(str->int (str %)) (seq (str a-num)))
              )
            (sums-to-one? [a-num]
              (= 1 (reduce + (digits-seq a-num)))
              )
            (next-num [a-num]
              (reduce + (map #(* % %) (digits-seq a-num)))
              )]
      (loop [curr-num a-num visited #{}]
        (if (sums-to-one? curr-num)
          true
          (let [nxt (next-num curr-num)]
            (if (visited nxt)
              false
              (recur nxt (conj visited nxt))
              )
            )
          )
        )
      )
    ))

(defcheck solution-63d7b129
  (fn happy [n]
    (let [to-int #(str->int(str %))
          sum-of-squares (fn [x] (reduce + (map
                                             #(* (to-int %) (to-int %)) (str x))))]
      (loop [x n xs []]
        (cond (= x 1) true
              (not= -1 (.indexOf xs x)) false
              :else (recur (sum-of-squares x) (conj xs x)))))))

(defcheck solution-63fba2f3
  (fn [n]
    (letfn
     [(xform [n]
        (->> n
          str
          seq
          (map str)
          (map #(str->int %))
          (map #(* % %)) (apply +)))
      (iha [n ac]
        (cond
          (= 1 n) true
          (ac n) false
          :else (iha (xform n) (conj ac n))))]
      (iha n #{}))))

(defcheck solution-648bc519
  (fn happy-number? [n]
    (letfn [(next-n [curr-n]
              (->> (str curr-n)
                seq
                (map #(str->int (str %)))
                (map #(* % %))
                (reduce +)))]
      (loop [acc #{} n_ n]
        (if (= 1 n_)
          true
          (if (contains? acc n_)
            false
            (recur (conj acc n_) (next-n n_))))))))

(defcheck solution-64f676f0
  (fn happy?
    ([n] (happy? n #{n}))
    ([n s]
     (let [m (->> (clojure.string/split (str n) #"")
               (remove #{""})
               (map (comp #(* % %) #(str->int %)))
               (apply +))]
       (cond
         (= 1 m) true
         (contains? s m) false
         true (happy? m (conj s m)))))))

(defcheck solution-65293edd
  (fn [j]
    (let [i2d (fn [i] (->> i str seq (map #(- (char->num %) (char->num \0)))))
          ssd (fn [i] (reduce + (map #(* % %) (i2d i))))
          tol 100000]
      ((fn [c j]
         (let [ss (ssd j)]
           (if (= ss 1)
             true
             (if (> c tol)
               false
               (recur (inc c) ss))))) 0 j))))

(defcheck solution-65331b9
  (fn [a] (loop [d a v #{1} s #(* % %)]
            (if (v d)
              (= d 1)
              (recur
                (reduce + (map #(s (- (char->ascii %) 48)) (str d)))
                (conj v d) s)))))

(defcheck solution-654a609c
  (fn [n]
    (loop [i n seen #{}]
      (cond (= i 1) true
            (some #(= % i) seen) false
            true (let [next (apply + (map #(* % %)
                                       (map #(- (char->ascii %) 48) (str i))))]
                   (recur next (conj seen i)))))))

(defcheck solution-65876bc0
  (fn [x]
    (loop [n x seen []]
      (let [result (reduce + (map (comp #(* % %) str->int str) (str n)))]
        (cond
          (= 1 result) true
          (true? (contains? seen result)) false
          :default (recur result (conj seen result)))))))

(defcheck solution-6627b20
  (fn [n]
    (loop [n n
           s #{}]
      (let [res
            (->> n
              str
              seq
              (map #(str->int (str %)))
              (map #(* % %))
              (reduce +))]
        (cond
          (= res 1) true
          (s res) false
          :else (recur res (conj s res)))))))

(defcheck solution-664524a1
  (fn happy?
    ([n] (happy? n #{}))
    ([n visited] (if (contains? visited n)
                   false
                   (if (= n 1)
                     true
                     (recur
                       (letfn [(charToInt [i] (- (char->num i) (char->num \0)))]
                         (apply + (map #(* % %) (map charToInt (seq (str n))))))
                       (conj visited n)))))))

(defcheck solution-664e0207
  (fn [n] (->> n
            (iterate (fn [n] (->> (str n)
                               (map (comp #(* % %) str->int str))
                               (reduce +))))
            (some {1 true, 4 0, 8 true, 9 0})
            (true?))))

(defcheck solution-666bbb7c
  (fn
    [number]

    (letfn [
            (** [n] (* n n))
            (squared [n] (reduce + (map #(** (- (char->num %) (char->num \0))) (str n))))]
      (loop [number number, seen #{}]
        (if (contains? seen number)
          false
          (let [new-value (squared number)]
            (if (= new-value 1)
              true
              (recur new-value (conj seen number))
              )
            )
          )
        )
      )
    ))

(defcheck solution-668f5612
  (fn [x]
    (letfn
     [
      (it [a]
        (reduce + (map #(* %1 %1) (map #(- (char->num %) (char->num \0)) (str a))))
        )
      ]
      ((fn hpy [n s]
         (let [nx (it n)]
           (if (= nx 1)
             true
             (if (s nx)
               false
               (hpy nx (conj s nx))
               )
             )
           )
         )
       x #{}
       )
      )
    ))

(defcheck solution-669d8988
  (fn happy? [n]
    (loop [terms [n]]
      (if (== (peek terms) 1)
        true
        (if (some #{(peek terms)} (pop terms))
          false
          (recur (conj terms (apply + (map #(* % %)
                                        ((fn f [q]
                                           (if (< q 10)
                                             (vector q)
                                             (conj (f (/ (- q (mod q 10)) 10)) (mod q 10)))) (peek terms)))))))))))

(defcheck solution-670845b8
  (fn happy
    ([nb] (happy nb #{}))
    ([nb res]
     (let
      [r (int (reduce +
                (map #(Math/pow (- (char->ascii %) 48) 2) (seq (str nb)))))]
       (if (= 1 r)
         true
         (if (contains? res r)
           false
           (happy r (conj res r))))))))

(defcheck solution-67203651
  (fn  [num]
    (let [num-sums ((fn [n]
                      (iterate
                        (fn [x]
                          (->> (str x)
                            (map #(char->num % 10))
                            (map #(Math/pow % 2))
                            (reduce +)
                            int))
                        n))
                    num)]
      (loop [[n & ns] num-sums found #{}]
        (cond
          (= n 1) true
          (contains? found n) false
          :default (recur ns (conj found n)))))))

(defcheck solution-672b290c
  (fn [n]
    (loop [n n old []]
      #_(println n old)
      (cond (= n 1) true
            (some #{n} old) false
            :else (recur
                    (reduce + (map #(* % %)
                                (map #(str->int (str %))
                                  (str n))))
                    (conj old n))))))

(defcheck solution-67b2a9b4
  (fn hn? [n]
    (letfn [(ne [m] (reduce + (map #(* % %) (map #(- (char->num %) (char->num \0)) (seq (str m))))))]
      (loop [i n s #{}]
        (cond
          (= 1 i) true
          (contains? s i) false
          :else (recur (ne i) (conj s i)))))))

(defcheck solution-68530f73
  (fn giggity [n]
    (letfn [(get-digits [n] (map #(str->int (str %)) (str n)))
            (get-happy  [n] (reduce + (map #(* % %) (get-digits n))))]
      (loop [visits 1 n n]
        (let [n (get-happy n)]
          (cond
            (= 1 n) true
            (< 1000 visits) false
            :else (recur (inc visits) n)))))))

(defcheck solution-689e2be0
  (fn happy? [n]
    (loop [seen #{}
           i n]
      (cond
        (= 1 i) true
        (seen i) false
        true  (let [digits (map #(char->num % 10) (str i))]
                (recur(conj seen i)  (reduce #(+ %1 (* %2 %2)) 0 digits))
                )
        )
      )
    ))

(defcheck solution-68b88cf2
  (fn [n]
    (let [digits (fn [x] (reverse (map #(rem (int (/ x %)) 10) (take-while #(<= % x) (iterate (partial * 10) 1)))))]
      (loop [cur n
             visited #{}]
        (let [ds (digits cur)
              next (apply + (map * ds ds))]
          #_(println cur)
          (cond
            (visited cur) false
            (= 1 next)    true
            :otherwise    (recur next (conj visited cur))))))))

(defcheck solution-68be0406
  (fn [x] ((fn [x s] (cond (= x 1) true
                           (s x) false
                           1 (recur (apply + (map #(* % %) (map #(str->int (str %))
                                                             (seq (str x)))))
                               (conj s x))))
           x #{})))

(defcheck solution-68ec5852
  (fn happy?
    ([n] (happy? n #{}))
    ([n sofar]
     (let [to-int (fn [s] (str->int (str s)))

           squared-sum
                  (reduce +
                    (map (comp  #(* % %) to-int)
                      (seq (str n))))]

       (if (= squared-sum 1)
         true
         (if (sofar n)
           false
           (recur squared-sum (conj sofar n))))))))

(defcheck solution-68fbfa68
  (fn f ([x] (f x #{}))
    ([x s] (cond (= x 1) true
                 (s x) (do #_(println x) false)
                 :else (f
                         (->> x
                           str
                           seq
                           (map str)
                           (map str->int)
                           (map #(* % %))
                           (reduce +)
                           )
                         (into s #{x}))))))

(defcheck solution-692d806d
  (fn [n]
    (if (= n 1)
      true
      (if (= n 4)
        false
        (recur (int (reduce #(+ % (Math/pow (char->num %2 10) 2)) 0 (str n))))))))

(defcheck solution-693c14bc
  (fn [n]
    (let [digit-sum (fn [n] (loop [r n
                                   ds 0]
                              (if (zero? r)
                                ds
                                (let [m (mod r 10)]
                                  (recur (/ (- r m) 10)
                                    (+ ds (* m m)))))))]
      (loop [sum (digit-sum n)
             seen #{n}]
        (if (= sum 1)
          true
          (if (seen sum)
            false
            (recur (digit-sum sum)
              (conj seen sum))))))))

(defcheck solution-697ac7a1
  (fn f [h n]
    (let [ss (reduce + (map (fn [x] (-> x str str->int (#(* % %)))) (str n)))]
      (cond
        (= n 1) true
        (h ss) false
        :else (f (conj h ss) ss)))) #{})

(defcheck solution-69c1d52a
  (fn happy? [n]
    (let [digits (fn [n]
                   (loop [ds '() current n]
                     (if (zero? current)
                       ds
                       (recur (cons (mod current 10) ds)
                         (int (/ current 10))))))
          square-sum (fn [coll]
                       (apply + (map #(* % %) coll)))
          tester (fn [seq]
                   (loop [seen '() r seq]
                     (cond
                       (= 1 (first r)) true
                       (nil? (some #{(first r)} seen)) (recur (cons (first r) seen) (rest r))
                       :else false)))]
      (tester (iterate (comp square-sum digits) n)))))

(defcheck solution-6a208f36
  (fn [x]
    (letfn [(derive-num [x]
              (->> (str x)
                (map #(str->int (str %)))
                (map #(* % %))
                (reduce +)))]
      (loop [x x, past #{}]
        (cond (= 1 x) true
              (past x) false
              :else (recur
                      (derive-num x)
                      (conj past x)))))))

(defcheck solution-6a8219f5
  (fn [x]
    (letfn [(digits [x] (map (comp #(str->int %) str) (str x)))
            (f [x] (reduce + (map #(* % %) (digits x))))
            (distinct?' [xs] (or (empty? xs) (apply distinct? xs)))]
      (distinct?' (take-while #(> % 1) (iterate f x))))))

(defcheck solution-6b22d5aa
  (fn happy-number?
    ([n]
     (happy-number? n #{}))
    ([n seen-before]
     (let [n' (->> n
                str
                seq
                (map #(char->num %))
                (map #(* % %)))
           sum (reduce + n')]
       (cond (= 1 sum) true
             (contains? seen-before sum) false
             :else (happy-number? sum (conj seen-before n)))))))

(defcheck solution-6b54f7c5
  (fn [k] (loop [c k p #{}]
            (let [n (reduce #(+ % (* %2 %2)) 0 (map #(- (char->ascii %) 48) (str c)))]
              (cond (= 1 n) true
                    (p n  ) false
                    1      (recur n (conj p c)))))))

(defcheck solution-6b556de2
  (letfn
   [(digits [n] (map (fn [c] (str->int (str c))) (str n)))
    (sq [n] (* n n))]
    (fn [n]
      (loop [n n seen #{}]
        (if (and (> n 1) (not (contains? seen n))) ; check for cycle
          (recur (reduce + (map sq (digits n))) (conj seen n))
          (= n 1))))))

(defcheck solution-6b5ec980
  (fn [x] (let [digits (fn digits
                         [x] (->> x
                               str
                               vec
                               (map #(str %))
                               (map #(str->int %))))
                squares (fn squares [x] (map #(* % %) (digits x)))
                sum-of-squares (fn sum-of-squares [x] (reduce + (squares x)))
                initial-value x]
            (loop [new-sum (sum-of-squares x)
                   old-numbers #{x}]
              (cond
                (= 1 new-sum) true
                (old-numbers new-sum) false
                :else (recur (sum-of-squares new-sum) (conj old-numbers new-sum)))))))

(defcheck solution-6b831287
  (fn [n]
    (= (nth (iterate
              #(int (apply + (map (fn [x]
                                    (Math/pow (- 48 (char->ascii x)) 2)) (str %)))) n) 20) 1)))

(defcheck solution-6bebaff
  (fn happy-num? [n]
    (letfn [(sum-of-sqare-of-digits [number]
              (reduce + (map (fn [c]
                               (let [digit (- (char->num c) (char->num \0))]
                                 (* digit digit)))
                          (str number))))]
      (loop [curr n results #{}]
        (cond
          (= 1 curr) true
          (results curr) false
          :else (recur (sum-of-sqare-of-digits curr) (conj results curr)))
        )
      )))

(defcheck solution-6c756f97
  (fn [n]
    (->
      (for [q (next (range))
            :let [s (take q (iterate (fn [x]
                                       (->>
                                         x
                                         (str)
                                         (map #(-> % str str->int ((fn [x] (* x x)))))
                                         (apply +)))
                              n))]
            :while (apply distinct? s)]
        s)
      last
      last
      (#(= 1 %)))))

(defcheck solution-6c75856c
  #(loop [s #{}
          c %]
     (cond (= 1 c) true
           (s c) false
           1 (recur (conj s c) ((fn [i] (reduce (fn [u v] (+ u (* v v))) 0 ((fn f [a x] (if (pos? x) (f (cons (rem x 10) a) (quot x 10)) a)) [] i))) c)))))

(defcheck solution-6c8aec6c
  (fn [x] (if (or (= x 2) (= x 3)) false true)))

(defcheck solution-6da6109a
  (fn [n]
    (letfn [(sum-sq [num]
              (reduce #(+ % (* %2 %2))
                0 (map (comp str->int str) (str num))))]
      (loop [t (sum-sq n)
             happened #{n}]
        (if (= t 1)
          true
          (if (contains? happened t)
            false
            (recur (sum-sq t) (conj happened t))))))))

(defcheck solution-6dc65b38
  (fn [n]
    (let [get-digits (fn [n b]
                       (if (zero? n)
                         (list n)
                         ((fn [n b r]
                            (if (zero? n)
                              r
                              (recur (quot n b) b (conj r (rem n b))))) n b nil)))
          square-sum (fn [coll]
                       (reduce #(+ %1 (* %2 %2)) 0 coll))
          calc (fn [n] (square-sum (get-digits n 10)))]
      ((fn [n calculated]
         (cond
           (= 1 n) true
           (contains? calculated (calc n)) false
           :else (recur (calc n) (conj calculated (calc n))))) n #{}))))

(defcheck solution-6dedfb42
  (fn happynumber?
    [n]
    (loop [base n
           results #{}]
      (let [sum ((fn cal [x] (if (< x 10) (* x x) (+ (* (rem x 10) (rem x 10)) (cal (quot x 10))))) base)]
        (do #_(prn sum)
            (cond
              (results sum) false
              (= 1 sum) true
              :else (recur sum (conj results sum))))))))

(defcheck solution-6dfdd84c
  (fn happy-num [n]
    (letfn [(square-sum [num1]
              (let [digit (rem num1 10)
                    rest-digits (quot num1 10)]
                (if (zero? rest-digits)
                  (* digit digit)
                  (+ (* digit digit) (square-sum rest-digits)))))]
      (loop [path #{}
             n n]
        (if (= 1 n)
          true
          (if (contains? path n)
            false
            (recur (conj path n) (square-sum n))))))))

(defcheck solution-6f3dc570
  (letfn
   [(digits [x acc]
      (if (= 0 x)
        acc
        (digits (quot x 10) (cons (rem x 10) acc))))

    (sum-square [x]
      (apply + (map #(* % %) (digits x '()))))

    (happy? [n acc]
      (let [nn (sum-square n)]
        ; If sum of squared digits = 1, it's happy
        (if (== nn 1)
          true
          ; If not, if sum of squared digits is in accumulated list, we are looping.
          (if (some #(= nn %) acc)
            false
            ; If not, add to list and recurse
            (recur nn (cons nn acc))))))]

    (fn [n] (happy? n '()))))

(defcheck solution-6f413e10
  (fn[n] (loop [current n history #{}]
           (if (= 1 current) true
                             (if (history current) false
                                                   (recur (reduce + (map #(int (Math/pow (str->int (str %)) 2)) (str current))) (conj history current)))
                             ))))

(defcheck solution-6f51706f
  (fn happy?
    [n] {:pre [(integer? n), (pos? n)]}

    ;; We begin by defining some helper functions.
    (let [
          ;; First we have a squaring function, just for convenience.
          sq (fn [x] (* x x))

          ;; Next, we have the function f. This is the iterator used in the
          ;; definition of a happy number; that is, f takes a positive integer n
          ;; and returns the sum of the squares of the digits of n.
          f (fn [n]
              (loop [acc 0, n n]
                (if (< n 1)
                  (+ acc (sq n))
                  (recur (+ acc (sq (mod n 10)))
                    (quot n 10)))))]

      ;; For the main body of the function, we loop through the iterates of f
      ;; until we either find a 1 (indicating that n is happy) or find a number
      ;; we've already seen (indicating that the sequence repeats and n is
      ;; unhappy).
      (loop [seen #{}, [i & tail] (iterate f n)]
        (cond
          (= i 1)   true
          (seen i)  false
          :else     (recur (conj seen i) tail))))))

(defcheck solution-6f598053
  (fn happy? [n]
    (let [square #(* % %)
          ctoi #(char->num %)
          sum #(->> (str %)
                 (map ctoi)
                 (map square)
                 (reduce +))
          res (loop [k n l #{}]
                (if (contains? l k)
                  k
                  (recur (sum k) (conj l k))))]
      (= res 1))))

(defcheck solution-6f984f7f
  (fn [n]
    (let [s #(* % %)
          i #(s (- (char->ascii %) 48))
          f #(->> % str (map i) (apply +))]
      (loop [x n a #{}]
        (cond
          (= x 1) true
          (contains? a x) false
          :else (recur (f x) (conj a x)))))))

(defcheck solution-70d6178d
  (fn happy-number [x]
    (letfn [
            (happy [t tested]
              (let [ hnr (reduce + (map #(* % %) (map #(str->int (str %)) (str t)))) ]
                (cond
                  ( = 1 hnr) true
                  (contains? tested hnr) false
                  :else (happy hnr (conj tested x)))))
            ]  (happy x []))))

(defcheck solution-713ee78e
  (fn happy? [number]
    (letfn [(digits [n]
              (when-not (zero? n)
                (cons (mod n 10)
                  (digits (quot n 10)))))

            (sq-digit-sum [n]
              (->> n
                digits
                (map #(* % %))
                (reduce +)))]

      (loop [curr number
             seen #{}]
        (cond
          (= curr 1) true
          (seen curr) false
          :else (recur (sq-digit-sum curr)
                  (conj seen curr)))))))

(defcheck solution-7257041
  (fn [num]
    (loop [n num]
      (condp = n
        1 true
        4 false
        (recur (reduce #(+ %1 (* (str->int (str %2)) (str->int (str %2)))) 0 (seq (str n))))
        )


      )
    ))

(defcheck solution-725e4e7a
  (fn [s]
    (let [f (fn [n]
              (let [digits (map #(- (char->ascii %) 48) (str n))
                    q (apply + (map #(* % %) digits))] q))
          h (iterate f s)
          k (fn i [m [l & others]]
              (cond (= 1 l) true (contains? m l) false true (i (conj m l) others)))]
      (k #{} h))))

(defcheck solution-726e0f89
  (fn happy?
    [n]
    (letfn [(sq [n] (* n n))
            (n->digits [num] (map #(char->num % 10) (str num)))]
      (loop [n n seen #{}]
        (cond
          (= 1 n) true
          (contains? seen n) false
          :else (recur
                  (reduce + (map sq (n->digits n)))
                  (conj seen n)))))))

(defcheck solution-72763ab9
  (fn [n]
    (let [nxt (fn nxt [n]
                (loop [n n rv 0]
                  (if (zero? n)
                    rv
                    (recur (quot n 10) (+ rv (* (rem n 10) (rem n 10)))))))]
      (loop [n n seen #{}]
        (cond
          (= n 1) true
          (seen n) false
          :else (recur (nxt n) (conj seen n)))))))

(defcheck solution-72983003
  (fn [n]
    (letfn [(digits [n] (loop [acc () n n] (if (< n 10) (conj acc n) (recur (conj acc (mod n 10)) (quot n 10)))))
            (square-sum [xs] (->> xs (map #(* % %)) (reduce +)))]
      (loop [n n coll #{}]
        (let [ss (square-sum (digits n))]
          (cond
            (= ss 1) true
            (contains? coll ss) false
            :else (recur ss (conj coll ss))))))))

(defcheck solution-72c670c0
  (fn [n]
    (let
     [ha (fn [x]
           (->> x
             str
             (map #(- (char->ascii %) 48))
             (map #(* % %))
             (apply +)))]
      (loop [x n accu #{}]
        (cond
          (= 1 x) true
          (contains? accu x) false
          :else (recur (ha x) (conj accu x)))))))

(defcheck solution-72f21d06
  (fn [n]
    (loop [n n
           seen []]
      #_(print n)
      (if (= n 1)
        true
        (if (contains? seen n)
          false
          (recur (reduce + (map #(* % %) (map (comp str->int str) (str n))))
            (conj seen n)
            ))))))

(defcheck solution-73e0612
  (fn happy? [n]
    (let [sumsq (fn [n]
                  (let [digs (map (fn [c] (str->int (str c)))
                               (str n))
                        sqd (map #(* % %) digs)
                        sum (reduce + 0 sqd)]
                    sum))]
      (loop [nums #{n}
             num (sumsq n)]
        (cond
          (= 1 num) true
          (contains? nums num) false
          :default (recur (conj nums num)
                     (sumsq num)))))))

(defcheck solution-7448602c
  (fn [x]
    (loop [x x
           seen #{}]
      (cond
        (= 1 x) true
        (seen x) false
        :else (let [digits (map #(-> % str str->int) (str x))
                    digits-squared (map #(* % %) digits)
                    x* (reduce + 0 digits-squared)]
                (recur x* (conj seen x)))))))

(defcheck solution-7490c362
  (fn [x]
    (loop [x x
           seen #{}]
      (if (seen x)
        false
        (let [n (apply + (map (comp #(* % %) str->int) (re-seq #"\d" (str x))))]
          (if (= 1 n)
            true
            (recur n (conj seen x))))))))

(defcheck solution-74cfcb41
  (fn [n]
    (= 1 ((fn [s n]
            (let [i (fn [n] (reduce #(+ % (* %2 %2)) 0 (map #(- (char->num %) (char->num \0)) (str n))))]
              (if (contains? s n)
                n
                (recur (conj s n) (i n))))) #{} n))))

(defcheck solution-74d224ea
  (fn happy? [n]
    (letfn [(to-digits [x] (map #(- (char->num %) (char->num \0)) (str x)))
            (iteration [x] (->> x to-digits (map #(* % %)) (reduce +)))]
      (loop [n n
             acc #{}]
        (if (== 1 n)
          true
          (if (contains? acc n)
            false
            (recur (iteration n) (conj acc n))))))))

(defcheck solution-74d97423
  (fn f
    ([cur stp]
     (let [nxt (->> cur
                 str
                 (map #(- (char->num %) (char->num \0)))
                 (map #(* % %))
                 (apply +))]
       (if (= nxt 1)
         true
         (if (stp nxt)
           false
           (recur nxt (conj stp cur))))))
    ([x] (f x #{}))))

(defcheck solution-7535aad7
  (letfn [(happy [n seen]
            (cond
              (seen n) false
              (= n 1) true
              :else (recur (step n) (conj seen n))))
          (digits [n]
            (if (zero? n)
              []
              (conj (digits (quot n 10)) (mod n 10))))
          (step [n]
            (reduce + 0 (map #(* % %) (digits n))))]
    (fn [n] (happy n #{}))))

(defcheck solution-758db34a
  (fn [number]
    (letfn [(digits [number]
              (loop [n number d '()]
                (if (zero? n)
                  d
                  (recur (quot n 10) (conj d (rem n 10)))
                  )
                )
              )]
      (loop [n number seen #{}]
        (cond
          (seen n) false
          (= n 1) true
          :else (recur (->> n digits (map #(* % %)) (reduce +)) (conj seen n))
          )
        )
      )
    ))

(defcheck solution-75a3bc22
  (fn happy?
    [n]
    (let* [ssdigits (fn [n]
                      (reduce + (map (fn
                                       [ch]
                                       (let [d (str->int (str ch))]
                                         (* d d)))
                                  (seq (str n)))))
           happy-step (fn
                        [reached n]
                        (let [nextn (ssdigits n)]
                          (if (= nextn 1)
                            true
                            (if (contains? reached nextn)
                              false
                              (recur (conj reached nextn) nextn)))))]
      (happy-step #{n} n))))

(defcheck solution-75d52085
  (fn happy? [n]
    (letfn [(split-square-sum [n]
              (->> n
                (str)
                (map #(char->ascii %))
                (map #(- % 48))
                (map #(* % %))
                (reduce +)))]
      (if (= n 4)
        false
        (if (= n 1)
          true
          (happy? (split-square-sum n)))))))

(defcheck solution-761112a7
  (fn happy-number? [num]
    (letfn [(square-sum [n]
              (apply + (map #(* % %) (map #(str->int (str %)) (str n)))))
            (step [n mid-results]
              (let [mid-result (square-sum n)]
                (if (= 1 mid-result)
                  true
                  (if (some #{mid-result} mid-results)
                    false
                    (step mid-result (conj mid-results mid-result))))))]
      (step num #{num}))))

(defcheck solution-7620c22c
  (fn [x & y]
    (let [seen (if (nil? y) #{} y)
          digits (map (comp #(str->int %) str) (str x))
          squareSum (reduce #(+ %1 (* %2 %2)) 0 digits)]
      (if (= 1 squareSum)
        true
        (if (seen squareSum)
          false
          (recur squareSum (conj seen squareSum)))))))

(defcheck solution-76727327
  #(loop [x % prev #{}]
     (cond
       (= 1 x) true
       (contains? prev x) false
       :else
       (recur (reduce + (map (comp (fn [a] (* a a)) str->int str) (str x))) (conj prev x)))))

(defcheck solution-76faff6c
  (fn happy? [n]
    (letfn [(digit-seq [n]
              (lazy-seq
                (if (zero? n)
                  nil
                  (cons (rem n 10)
                    (digit-seq (quot n 10))))))]
      (loop [seen #{}
             n n]
        (let [new-num (reduce #(+ %1 (* %2 %2)) 0 (digit-seq n))]
          (cond
            (= 1 new-num) true
            (contains? seen new-num) false
            :recur (recur (conj seen new-num)
                     new-num)))))))

(defcheck solution-774ec0f9
  (fn happy [dd]
    (loop [d dd s #{}]
      (if (= 1 d)
        true
        (if (s d)
          false
          (recur (loop [n d acc 0]
                   (if (== n 0)
                     acc
                     (recur
                       (quot n 10)
                       (+ acc (#(* % %) (mod n 10))))))
            (conj s d)))))))

(defcheck solution-781b711d
  (fn [n]
    (let [f (fn [x]
              (reduce + 0
                (map #(* (- (char->num %) (char->num \0))
                        (- (char->num %) (char->num \0))) (seq (str x)))))]
      (loop [ s #{} i (f n)]
        (if (= i 1) true
                    (if (contains? s i) false
                                        (recur (conj s i) (f i))))))))

(defcheck solution-785282d
  (fn [n]
    (let [sqs (fn [x] (apply + (map (comp #(* % %) str->int str) (str x))))]
      (->> [[] n]
        (iterate (fn [[a x]]
                   (cond (= x 1) true
                         (some (partial = x) a) false
                         :else [(conj a x) (sqs x)])))
        (drop-while vector?)
        (first)))))

(defcheck solution-787e8ed6
  (fn [n]
    (letfn [(ds [n] (map #(str->int %) (map str (str n))))
            (ss [ds] (apply + (map #(* % %) ds)))
            (f [n t]
              (if (> t 100)
                false
                (let [r (ss (ds n))]
                  (if (= 1 r)
                    true
                    (f r (inc t))))))]
      (f n 0))))

(defcheck solution-78ffe69e
  (fn[n]
    (let[ss #(if (zero? %2) % (recur (+ % (* (mod %2 10) (mod %2 10))) (quot %2 10)))]
      (= 1 (last (take 9 (iterate (partial ss 0) n)))))))

(defcheck solution-7994af65
  (fn happy? [n]
    (loop [n n, history #{n}]
      (let [square #(* % %)
            digits (fn [n] (map #(str->int (str %)) (str n)))
            n2 (reduce + (map square (digits n)))]
        (cond
          (= 1 n2) true
          (contains? history n2) false
          :else (recur n2 (conj history n2)))))))

(defcheck solution-7998cfd9
  (letfn
   [(digits [num]
      (if (= num 0) [0]
                    (loop [num num ds ()]
                      (if (> num 0)
                        (recur (quot num 10) (conj ds (rem num 10)))
                        (vec ds)))))
    (sqr [x] (* x x))
    (sum-squared-digits [n] (apply + (map sqr (digits n))))]
    (fn [n]
      (loop [n n rs #{}]
        (cond
          (= n 1) true
          (contains? rs n) false
          :else (recur (sum-squared-digits n) (conj rs n)))))))

(defcheck solution-79bbfad
  (fn [n]
    (let [step (fn [n] (loop [acc 0
                              n n]
                         (if (< n 1)
                           acc
                           (let [digit (rem n 10)]
                             (recur (+ acc (* digit digit)) (quot n 10))))))
          nums (iterate step n)]
      (loop [nums nums
             seen #{}]
        (let [num (first nums)]
          (cond
            (= num 1) true
            (contains? seen num) false
            :else (recur (rest nums) (conj seen num))))))))

(defcheck solution-79bf4de3
  (fn happy-num
    ([nmbr] (happy-num nmbr #{}))
    ([nmbr st]
     (let [square #(* % %),
           digits #(map (fn [n] (- (char->ascii n) 48)) (into [] (str %))),
           sum-sqrd (apply + (map square (digits nmbr)))]
       (if (contains? st sum-sqrd)
         false
         (if (= sum-sqrd 1)
           true
           (happy-num sum-sqrd (conj st sum-sqrd))))))))

(defcheck solution-79f2a527
  (fn [n]
    (loop [n n, nums #{}]
      (cond
        (= n 1) true
        (some #(= n %) (rest nums)) false
        :else
        (recur
          (reduce
            (fn [acc d]
              (let [i (str->int d)] (+ acc (* i i))))
            0
            (re-seq #"\d" (str n)))
          (conj nums n))))))

(defcheck solution-7a325dd
  (partial (fn [old new]
             (let [happy (fn [x]
                           (->>
                             x
                             (str)
                             (seq)
                             (map (comp str->int str))
                             (reduce #(+ %1 (* %2 %2)) 0)
                             )
                           )
                   ]
               (if (contains? old new)
                 false
                 (if (= new 1)
                   true
                   (recur (conj old new) (happy new))
                   )
                 )
               )
             ) #{}
    ))

(defcheck solution-7abd5874
  (fn [x]
    (let [f (fn [x] (apply + (map #(* % %) (map #(- (char->ascii %) 48) (str x)))))]
      (loop [x x ys ()]
        (let [y (f x)]
          (cond
            (= x 1) true
            (some #(= % y) ys) false
            :default (recur y (cons y ys))))))))

(defcheck solution-7b4ab95b
  (fn happy? [n]
    (letfn [(n-v [n] (map (comp str->int str) (str n)))
            (main [n st]
              (let [nn (apply + (map * n n))
                    nst (into st [nn])]
                (if (= 1 nn) true
                             (if (= st nst) false
                                            (main (n-v nn) nst)))))]
      (main (n-v n) (set [n])))))

(defcheck solution-7bbf4c69
  (fn is-happy [x]
    (loop [y x
           acc []]
      (let [sum-squares (fn sum-squares [a]
                          (let [xs (str a)
                                ys (for [ch xs]
                                     (-> (str ch)
                                       (str->int)) )]
                            (reduce #(+ % (* %2 %2)) 0 ys)))
            z (sum-squares y)]
        (if (= z 1)
          true
          (if (some #(= z %) acc)
            false
            (recur z (conj acc z))))))))

(defcheck solution-7c2e5e3e
  #(> % 6))

(defcheck solution-7c5c058e
  (fn [n]
    (let [s (iterate (fn [x] (apply + (map (comp #(* % %) str->int) (re-seq #"\d" (str x))))) n)]
      (loop [s1 (take 2 s)]
        (cond
          (= 1 (last s1)) true
          (some (hash-set (last s1)) (drop-last s1)) false
          :else (recur (take (inc (count s1)) s)))))))

(defcheck solution-7e16c295
  (fn [n]
    (let [cal (fn [n] (reduce +  0 (map #(let [d (- (char->ascii %) 48)] (* d d) ) (str n) ))) ]
      (loop [ps #{n} cur n]
        (let [nxt (cal cur)]
          (if (= 1 nxt) true   (if (ps nxt)  false  (recur (conj ps nxt) nxt)  ) ) ) )  ) ))

(defcheck solution-7e16ec65
  < 5)

(defcheck solution-7e2a027a
  (fn happy [n]
    (let [newn (fn [n'] (apply + (map #(let [x (char->num %)] (* x x)) (str n'))))]
      (loop [n n
             prev {}]
        (let [n' (newn n)]
          (if (= 1 n')
            true
            (if (prev n')
              false
              (recur n' (assoc prev n n')))))))))

(defcheck solution-7e516ae7
  (fn [n]
    (let [sqd (fn [n] (loop [n n s 0]
                        (if (zero? n)
                          s
                          (recur (quot n 10) (+ s (* (mod n 10) (mod n 10)))))))
          happy (fn [n p]
                  (let [s (sqd n)]
                    (if (= s 1)
                      true
                      (if (p s) false (recur s (conj p s))))))]
      (happy n #{}))))

(defcheck solution-7ea0f5c8
  (fn [n]
    (let [f (memoize (comp (partial reduce +)
                           (partial map #(* % %)) (partial map #(- (char->ascii %) 48))
                           str))
          FOREVER 1000]
      (= 1 (nth (iterate f n) FOREVER)))))

(defcheck solution-7ebbf59
  (fn [n]
    (let [square-sum
          (fn [n] (apply + (map #(* % %) (map #(- (char->ascii %) 48) (str n)))))
          test-number
          (fn test-number [t n]
            (cond (some #(= n %) t) false
                  (= 1 n) true
                  :else (test-number (conj t n) (square-sum n))))]
      (test-number [] n))))

(defcheck solution-7eed04e5
  (fn happy
    ([n] (happy n []))
    ([n seen]
     (let [calc (reduce + (map (comp #(* % %) #(str->int %) str) (str n)))]
       (if (= 1 calc)
         true
         (if (contains? seen calc)
           false
           (happy calc (into [] (cons n seen)))))))))

(defcheck solution-7f0f8315
  (fn [n]
    (loop [n n
           p #{}]
      (cond (p n) false
            (= n 1) true
            :else
            (recur
              (reduce +
                (map #(let [a (- (char->num %) (char->num \0))] (* a a)) (str n)))
              (conj p n))))))

(defcheck solution-7f2251e0
  (fn [happy]
    (let
     [
      contains (fn [needle haystack] (< 0 (count (filter (fn [item] (= item needle)) haystack)))),
      toInt (fn [char] (str->int (str char))),
      sqr (fn [x] (* x x)),
      step (fn [happy] (apply + (map sqr (map toInt (seq (str happy)))))),
      recursor
               (fn recurs
                 [col]
                 (let
                  [
                   item (last col),
                   nextitem (step item)
                   ]
                   (if
                    (= 1 item)
                     true
                     (if
                      (contains nextitem col)
                       false
                       (recurs (concat col (vector nextitem)))
                       )
                     )
                   )
                 )
      ]
      (recursor (vector happy))
      )
    ))

(defcheck solution-7f6230c7
  (fn f[x]
    (letfn [(q [x] (* x x))
            (s [x] (apply + (map #(q (- (char->ascii %) 48)) (str x))))]
      (loop [n x i 1]
        (cond
          (= n 1) true
          (= i 10) false
          :else (recur (s n) (inc i)))))))

(defcheck solution-7fe2d5b0
  (fn happy?
    ([n]
     (happy? n '()))
    ([n coll]
     (cond
       (some (set (list n)) coll)
       false

       (= 1 n)
       true

       :else
       (let [digits (fn digits [n]
                      (if (< n 10)
                        [n]
                        (conj (digits (int (/ n 10))) (mod n 10))))]
         (happy? (reduce + (map #(* % %) (digits n))) (conj coll n)))))))

(defcheck solution-7fed4feb
  (fn happy-number? [n]
    (letfn [(sum-square-digits [n] (apply + (map (comp #(* % %) str->int str) (seq (str n)))))]
      (loop [cur (sum-square-digits n) seen (hash-set n)]
        (cond (= cur 1)  true
              (seen cur) false
              :else      (recur (sum-square-digits cur) (conj seen cur)))))))

(defcheck solution-7feda815
  (fn [x]
    (loop [n x, e #{}]
      (cond
        (= n 1) true
        (contains? e n) false
        true (recur
               (reduce +
                 (map #(* % %)
                   (map #(str->int (str %))
                     (str n))))
               (conj e n))))))

(defcheck solution-8033d670
  (fn h? [n]
    (loop [n n
           s #{}]
      (cond
        (= n 1) true
        (s n) false
        :else (recur (apply + (map (comp #(* % %) #(- (char->ascii %) 48)) (str n)))
                (conj s n))))))

(defcheck solution-807c738f
  (fn happy ([s] (happy [s] #{}))
    ([s visited]
     (let [next (fn [i]
                  (reduce + (map #(* % %) (map #(str->int %) (re-seq #"[\d]" (str i))))))]
       (cond (visited s) false
             (= 1 s) true
             :else (happy (next s) (conj visited s)))))))

(defcheck solution-80f3e212
  (fn [num]
    (loop [n num seen? #{}]
      (let [sum (apply + (map (comp #(* % %) #(- (char->num %) (char->num \0))) (str n)))]
        (cond
          (= sum 1) true
          (seen? sum) false
          :else (recur sum (conj seen? sum)))))))

(defcheck solution-815d1b7a
  (fn [n]
    (loop [num n, s #{}]
      (cond
        (= num 1) true
        (contains? s num) false
        :else
        (recur
          (let [digits (map #(- (char->ascii %) 48) (str num))]
            (reduce + (map #(* % %) digits)))
          (conj s num))))))

(defcheck solution-815dfe82
  (fn happy? [n]
    (letfn [(digits [n] (map #(- (char->num %) (char->num \0)) (seq (str n))))
            (sum-sqr [xs] (apply + (map #(* % %) xs)))]
      (loop [nums (iterate (comp sum-sqr digits) n),
             used #{}]
        (let [m (first nums)]
          (if (= m 1) true
                      (if (used m) false
                                   (recur (rest nums) (conj used m)))))))))

(defcheck solution-81c25cd8
  (fn happy [x]
    (loop [y x
           seen-before #{}]
      (cond (= y 1) true
            (contains? seen-before y) false
            :else (letfn [(digits [x]
                            (loop [y x
                                   ds []]
                              (if (< y 10) (conj ds y)
                                           (let [dig (mod y 10)]
                                             (recur (/ (- y dig) 10) (conj ds dig))))))
                          (sumSquares [xs] (reduce + (map #(* % %) xs)))]
                    (recur (sumSquares (digits y)) (conj seen-before y)))))))

(defcheck solution-81cfe0b3
  (fn [n]
    (loop [seen #{} n n]
      (let [v (->> n
                str vec
                (map #(- (char->ascii %) 48))
                (map #(* % %))
                (reduce +))]
        (cond
          (= v 1) true
          (contains? seen v) false
          :else (recur (conj seen v) v))))))

(defcheck solution-82b95b93
  (fn hn [n]
    (loop [v n k #{}]
      (let [x (reduce + (map #(* % %) (map #(- (char->ascii %) 48) (seq (str v)))))]
        (cond
          (contains? k x) false
          (= 1 x) true
          :else (recur x (conj k x)))))))

(defcheck solution-83b762c0
  (fn [n]
    (letfn [(digs
              ([n] (digs n '()))
              ([n l] (if (zero? n) l (digs (quot n 10) (conj l (mod n 10))))))
            (is-happy?
              [n s]
              (if
               (some #{n} s) false
                             (let [sum-digs (reduce #(+ %1 (* %2 %2)) 0 (digs n))]
                               (if (= 1 sum-digs)
                                 true
                                 (recur sum-digs (conj s n))))))]
      (is-happy? n #{}))))

(defcheck solution-843685c2
  (fn [n]
    (loop [n n seen #{}]
      (let [to-digit (fn [n] (- (char->ascii n) 48))
            digits (map to-digit (str n))
            square (fn [n] (* n n))
            sum-squares (reduce + (map square digits))]
        (cond
          (= 1 sum-squares)  true
          (seen sum-squares) false
          :else              (recur sum-squares (conj seen sum-squares)))))))

(defcheck solution-84603e6e
  (letfn [(sss [n] (reduce + (map #(* % %) (map str->int (re-seq #"\d" (str n))))))]
    (fn happy
      ([n] (happy n #{n}))
      ([n acc] (let [s (sss n)]
                 (cond (= s 1) true
                       (contains? acc s) false
                       :else (recur s (conj acc s))))))))

(defcheck solution-84859cf3
  (fn happy?
    ([n] (happy? [] n))
    ([seen n]
     (cond
       (= 1 n)
       true
       (contains? seen n)
       false
       true
       (recur (conj seen n)
         (->> n
           (str)
           (map #(- (char->num %) (char->num \0)))
           (map #(* % %))
           (reduce +) ))))))

(defcheck solution-84a99e97
  (fn [arg]
    (letfn [(happyloop [xs]
              (->> xs str (map #(str->int (str %))) (map #(* % %)) (apply +)))]
      (loop [generated #{}
             xs (iterate happyloop arg)]
        (let [next (first xs)]
          (cond (= next 1) true
                ((complement nil?) (generated next)) false
                :else (recur (conj generated next) (drop 1 xs)))))
      )))

(defcheck solution-84ec8586
  (fn happy? [n]
    (letfn [(nums [n] (map #(str->int %) (map str (str n))))]
      (let [sumsq (reduce + (for [x (nums n)] (int (Math/pow x 2))))]
        (cond (= sumsq 4) false
              (= sumsq 1) true
              :else (recur sumsq))))))

(defcheck solution-8580f2e5
  (fn [N]
    (let [digits (fn [n]  (reverse (map #(mod % 10)(take-while pos? (iterate #(quot % 10) n)))))
          sqr (fn [n] (* n n))
          form (fn [n] (apply + (map sqr (digits n))))]
      (loop [n N s #{}]
        (let [m (form n)]
          (cond (= 1 m) true
                (s m) false
                :else (recur m (conj s m))))))  ))

(defcheck solution-86114e29
  (letfn [(num->digits [num]
            (loop [n num res []]
              (if (zero? n)
                res
                (recur (long (/ n 10)) (cons (mod n 10) res)))))
          (change [n]
            (apply + (map #(* % %) (num->digits n))))]
    (fn [init]
      (loop [curr init results #{}]
        #_(println curr " - " results)
        (cond
          (= 1 curr) true
          (results curr) false
          :else (let [new-n (change curr)]
                  #_(println curr new-n)
                  (recur new-n (into results [curr])))
          )))
    ))

(defcheck solution-86bc1fbd
  (fn [x]
    (letfn [(digits [n]
              (if (zero? n)
                []
                (conj (digits (quot n 10)) (mod n 10))))
            (square [n]
              (* n n))
            (sum-squared [n]
              (apply + (map square (digits n))))]
      (= 1 (last (take 1000 (iterate sum-squared x)))))))

(defcheck solution-86f4dbb6
  (fn [n]
    (letfn [(sumsq [n](reduce + (map (comp #(* % %) str->int str) (str n))))]
      (loop [seen #{} x n]
        (if (seen x)
          (= x 1)
          (recur (conj seen x) (sumsq x)))))))

(defcheck solution-877ca940
  (fn happy-number?
    [n]
    (letfn [(happy
              [n n-seen]
              (if (or (= 1 n) (n-seen n))
                n
                (let [n' (reduce + (map (comp #(* % %) #(str->int %)) (map str (str n))))]
                  (happy n' (conj n-seen n)))))]
      (if (= 1 (happy n #{})) true false))))

(defcheck solution-8793fad8
  trampoline (fn sqds [n]
               ({1 true 4 false} n #(sqds (apply + (map (zipmap "0123456789" [0 1 4 9 16 25 36 49 64 81]) (str n)))))))

(defcheck solution-879d7e4c
  (fn [x]
    (let [step (fn [n] (-> n str seq (->> (map str) (map #(str->int %))
                                       (map #(* % %)) (reduce + 0))))]
      (loop [x x cntr 0]
        (if (= x 1)
          true
          (if (> cntr 1000)
            false
            (recur (step x) (inc cntr))))))))

(defcheck solution-87c0789a
  (fn happy? [n]
    (loop [x n, s #{}]
      (cond
        (= 1 x) true
        (s x) false
        :else (let [y (apply + (for [c (str x) :let [d (char->num c)]] (* d d)))]
                (recur y (conj s x)))))))

(defcheck solution-87ed09ec
  (fn [n]
    (letfn [(digits [n] (if (< n 10) (list n) (cons (mod n 10) (digits (quot n 10)))))]
      (loop [n n seen #{}]
        (let [d (digits n) s (apply + (map #(* % %) d))]
          (if (= s 1) true (if (seen n) false (recur s (conj seen n)))))))))

(defcheck solution-8882b144
  (fn f
    ([x] (f x #{}))
    ([x coll]
     (cond
       (= 1 x) true
       (contains? coll x) false
       :else (f (->> x
                  (.toString)
                  (seq)
                  (map #(str->int (str %)))
                  (map #(* % %))
                  (apply +)
                  )
               (conj coll x)
               )
       )
     )
    ))

(defcheck solution-88d144c4
  #((fn f [a b]
      (cond
        (= [1 0] [a b]) true
        (= [4 0] [a b]) false
        (zero? a) (f b a)
        :e (f (quot a 10)
             (+ b (* (rem a 10) (rem a 10)))))) % 0))

(defcheck solution-88f98ebd
  (fn is-happy' [n]
    (letfn [(square [x] (* x x))]
      (cond (= 1 n) true
            (= 4 n) false
            :else (recur (->> n
                           str
                           seq
                           (map #(-> %
                                   str
                                   (str->int)
                                   square))
                           (apply +)))))))

(defcheck solution-8952c79b
  (fn [n]
    (letfn [(seq-n [k] (cons 0 (map #(str->int (str %)) (str k))))
            (compute-n [s] (reduce #(+ %1 (* %2 %2)) s))
            (happy-seq [m] (cons m (lazy-seq (happy-seq (compute-n (seq-n m))))))]
      (= 1 (compute-n (seq-n (first (drop-while #(not (contains? #{0 1 4 16 20 37 42 58 89 145} %)) (happy-seq n)))))))))

(defcheck solution-898f2479
  (fn f [n]
    (loop [n n, rond 0]
      (if (= n 1) true
                  (if (> rond 100) false
                                   (let [
                                         s (apply + (map #(* (- (char->num %) (char->num \0)) (- (char->num %) (char->num \0))) (str n)))]
                                     (recur s (inc rond))))))))

(defcheck solution-89ca993e
  (fn ff [v] (= 1 (last (take 100 (iterate (fn f [n]
                                             (reduce + (map #(* (- (char->num %) (char->num \0)) (- (char->num %) (char->num \0))) (seq (str n))))) v))))))

(defcheck solution-89dd2e58
  (fn happy?
    ([n] (happy? n #{}))
    ([n s]
     (if (= n 1)
       true
       (if (s n)
         false
         (letfn [(sq [n] (* n n))]
           (happy? (reduce + (map #(sq (- (char->ascii %) 48)) (str n))) (conj s n))))))))

(defcheck solution-8a9af0e6
  (fn myhappy[n]
    (let [sq #(* % %)]
      (loop [n n seen #{}]
        (let [sumsq (reduce #(+ % (sq (- (char->ascii %2) 48)))
                      0 (vec (str n)))
              #_#_d (println sumsq)]
          (if (= sumsq 1) true
                          (if (seen sumsq) false
                                           (recur sumsq (conj seen n)))))))))

(defcheck solution-8b403ca4
  (let [sosod (fn [n] (->> n (str) (map #(- (char->ascii %) 48)) (map #(* % %)) (reduce + 0)))
        first-dup (fn recur [seen [x & xs]] (if (seen x) x (recur (conj seen x) xs)))]
    #(= 1 (first-dup #{} (iterate sosod %)))))

(defcheck solution-8b49d359
  (fn [x]
    (let [y (->> x str (map (comp #(* % %) str->int str)) (apply +))]
      (cond
        (= y 1) true
        (= y 42) false
        :else (recur y)))))

(defcheck solution-8b5001d1
  (fn [x]
    ((fn h[k r]
       (cond
         (= k 1) true
         (some #(= k %) r) false
         :else
         (let [n ((fn s[y]
                    (if (> y 9)
                      (+ (s (quot y 10))
                         (s (rem  y 10)))
                      (* y y))) k)]
           (h n (cons k r))))) x [])))

(defcheck solution-8b5107d9
  (fn happy-num? [n]
    {:pre [(pos? n)]}
    (letfn [(digits [n] (map #(char->num %1) (str n)))
            (square-sum [xs] (long (reduce  #(+ %1 (Math/pow %2 2)) 0 xs)))]
      (loop [loop-detection #{}
             i              n]
        (let [sum (square-sum (digits i))]
          (cond
            (= 1 sum) true
            (contains? loop-detection sum) false
            :else (recur (conj loop-detection sum) sum)))))))

(defcheck solution-8b638566
  (fn [n]
    (letfn [(ssd [n]
              (->> n str (map (comp #(* % %) str->int str)) (apply +)))
            (happy' [n v]
              (let [s (ssd n)]
                (cond
                  (= s 1) true
                  (contains? v s) false
                  :else (happy' s (conj v n)))))]
      (happy' n #{}))))

(defcheck solution-8b92c65f
  (fn [s n]
    (if (= 1 n)
      true
      (let [nn (reduce #(+ %1 (* %2 %2)) 0 (map (comp str->int str) (str n)))]
        (if (s nn)
          false
          (recur (conj s nn) nn))))) #{})

(defcheck solution-8bf0751
  (fn [x]
    (loop [n x
           a '()]
      (let [r (apply + (map #(* % %) (map #(- (char->ascii %) 48) (seq (str n)))))]
        (cond
          (> (.indexOf a r) -1) false
          (= 1 r) true
          :default (recur r (conj a r)))))))

(defcheck solution-8c1be8cc
  (fn happy?
    ([n] (happy? n #{}))
    ([n history]
     (letfn [(sum-of-sq [items] (reduce + (map #(* % %) items)))
             (digits [x] (map #(str->int (str %)) (str x)))]
       (cond
         (= n 1) true
         (contains? history n) false
         true   (happy? (sum-of-sq (digits n)) (conj history n)))))))

(defcheck solution-8c27e24a
  (fn is-happy? [n]
    (cond (= n 4) false
          (= n 1) true
          :else (recur (reduce + (map #(* % %) (map #(char->num % 10) (str n))))))))

(defcheck solution-8c30f5f2
  (fn [number]
    (cond
      (= number 1) true
      (or (= number 2) (= number 3)) false
      true
      (recur
        (reduce +
          (map
            #(Math/round (Math/pow (str->int (str %)) 2))
            (str number)))))))

(defcheck solution-8c481504
  (fn [n]
    (letfn [(sumsqrs [x]
              (apply +
                (loop [a [] n x]
                  (if (pos? n)
                    (recur (conj a (apply * (repeat 2 (mod n 10))))
                      (quot n 10))
                    a))))]
      (loop [numbers [n]]
        (let [new-number (sumsqrs (last numbers))]
          (cond
            (= new-number 1) true
            (some (partial = new-number) numbers) false
            :else (recur (conj numbers new-number))))))))

(defcheck solution-8cb98812
  (fn [num]
    (let [sq (fn [x] (* x x))]
      (loop [n num seen #{}]
        (let [y (apply + (map (fn [n] (-> n str str->int sq)) (str n)))]
          (cond
            (= 1 y) true
            (contains? seen y) false
            :else (recur y (clojure.set/union #{y} seen))))))))

(defcheck solution-8cc6be72
  (fn [n]
    (loop [n n ns #{}]
      (cond
        (= 1 n) true
        (contains? ns n) false
        :else (recur
                (reduce
                  #(+ %1 (* %2 %2))
                  0
                  (map (comp str->int str) (str n))) (conj ns n))))))

(defcheck solution-8d12db4d
  (fn [x]
    (letfn [(is-happy [prevs n]
              (cond
                (= 1 n) true
                (prevs n) false
                :else (is-happy
                        (conj prevs n)
                        (apply + (map #(* % %) (map #(- (char->ascii %) 48) (str n)))))))]
      (is-happy #{} x))))

(defcheck solution-8d22232
  (let [zero (char->num \0)
        char->int #(- (char->num %) zero)
        digits #(map char->int (str %))
        square #(* % %)
        sum-of-squares #(apply + (map square (digits %)))]
    (fn [n]
      (loop [n n
             prev-ns #{}]
        (let [sum (sum-of-squares n)]
          (cond
            (= sum 1) true
            (contains? prev-ns sum) false
            :otherwise (recur sum (conj prev-ns sum))))))))

(defcheck solution-8d599259
  (fn [n]
    (letfn [(digitize [n]
              (loop [n n
                     ret ()]
                (if (< n 1)
                  ret
                  (recur (quot n 10) (conj ret (rem n 10))))))
            (change [n]
              (reduce + (map #(* % %) (digitize n))))]
      (loop [n (change n)
             ret #{}]
        (cond
          (= 1 n) true
          (ret n) false
          :else (recur (change n) (conj ret n)))))))

(defcheck solution-8d8f99c7
  (partial (fn [s n] (cond (= 1 n) true (s n) false
                           :else (let [ds (map (comp str->int str) (seq (str n)))]
                                   #_(println n ds)
                                   (recur (conj s n) (reduce + (map #(* % %) ds))))))
    #{}))

(defcheck solution-8dc36975
  (fn f [s n]
    (cond (= n 1) true
          (s n)   false
          1 (f (conj s n) (reduce #(+ % (* %2 %2)) 0 (map #(- (char->ascii %) 48) (seq (str n))))))) #{})

(defcheck solution-8e160bad
  (fn [n]
    (let [next (fn [x] (reduce + (map #(* % %) (map str->int (re-seq #"\d" (str x))))))]
      (loop [n n rec #{}]
        (cond
          (rec n) false
          (= 1 n) true
          :else (recur (next n) (conj rec n)))))))

(defcheck solution-8e459c2b
  (fn happy?
    ([n t prev]
     (let [ds (map #(str->int (str %)) (str t))
           sum (apply + (map #(* % %) ds))]
       (cond (= sum 1) true
             (prev sum) false
             :else (recur n sum (conj prev sum)))))
    ([n] (happy? n n #{}))))

(defcheck solution-8e5615f3
  (fn f
    ([n] (f n []))
    ([n ns]
     (cond
       (= n 1) true
       (some #(= n %) ns) false
       :else (recur (reduce + 0 (map #(let [a (- (char->ascii %) 48)]
                                        (* a a)) (str n)))
               (conj ns n))))))

(defcheck solution-8ea6bb92
  (fn happy [n]
    (let [squaresum (apply + (map #(* (str->int %) (str->int %))
                               (re-seq #"\d" (str n))))]
      (cond (= 1 squaresum) true
            (= 4 squaresum) false
            :else (recur squaresum)))))

(defcheck solution-8eeda7ed
  (fn [i]
    (loop [n i s #{}]
      (cond (= 1 n) true
            (contains? s n) false
            :else (recur (reduce + (map #(* % %) (map #(- (char->ascii %) 48) (str n))))
                    (conj s n))))))

(defcheck solution-8f3cf133
  (fn [n]
    (loop [x n]
      (condp = x
        4 false
        1 true
        (let [y (map #(str->int %) (re-seq #"\d" (str "" x)))]
          (recur (apply + (map #(* % %) y))))))))

(defcheck solution-8fd6d16
  (fn happy?
    ([n seen]
     (let [happified
           (loop [res 0 left-over n]
             (if (= left-over 0)
               res
               (recur (+ (* (mod left-over 10) (mod left-over 10)) res) (quot left-over 10))))]
       (cond (= 1 happified) true
             (contains? seen happified) false
             :else (happy? happified (conj seen happified)))))
    ([n] (happy? n #{}))))

(defcheck solution-90141859
  (fn [x]
    (loop [x x tot []]
      (cond
        (= x 1) true
        (some #{x} tot) false
        :otherwise (let [m (apply + (map #(let [n (- (char->ascii %) 48)] (* n n))
                                      (str x)))]
                     (recur m (conj tot x)))))))

(defcheck solution-909f3ee7
  (fn [num]
    (letfn
     [(hnext [n] (reduce #(+ % (* %2 %2)) 0 (map #(- (char->ascii %) 48) (str n))))]
      (loop [n num seen #{}]
        (cond
          (= n 1) true
          (seen n) false
          :else (recur (hnext n) (conj seen n)))))))

(defcheck solution-911a06b7
  (fn happy-numbers [n]
    (letfn [(sum-sq-digits [n]
              (let [digits (map #(- (char->ascii %) 48) (str n))]
                (apply + (map #(* % %) digits))))]
      (loop [x n ss #{}]
        (let [s (sum-sq-digits x)]
          (cond
            (= s 1) true
            (contains? ss s) false
            :else (recur s (conj ss s))))))))

(defcheck solution-9164f931
  (fn [x]
    (letfn [(to-digits [x]
              (map #(str->int (str %)) (seq (str x))))
            (sqs [x]
              (reduce #(+ %1 (* %2 %2)) 0 (to-digits x)))]
      (loop [x x s #{}]
        (cond
          (= x 1) true
          (contains? s x) false
          :else (recur (sqs x) (conj s x)))))))

(defcheck solution-91912e6d
  (fn [n]
    (letfn [(digits [n]
              (map #(str->int (str %)) (str n)))
            (sum-of-squares [n]
              (reduce + (map #(* % %) (digits n))))]
      (boolean (some #{1} (take 100 (iterate sum-of-squares n)))))))

(defcheck solution-91f4f519
  (fn [m]
    (loop [n m
           s []]
      (or (= n 1)
          (if (contains? s n)
            false
            (recur
              (->> (str n)
                (map #(char->num % 10))
                (map #(* % %))
                (reduce +))
              (conj s n)))))))

(defcheck solution-92372b25
  (fn happy
    ([n] (happy n #{}))
    ([n s]
     (if (= 1 n)
       true
       (if (s n)
         false
         (let [nn
               (apply +
                 (map #(* % %)
                   ((fn digits [x]
                      (if (zero? x)
                        ()
                        (cons (rem x 10) (digits (quot x 10)))
                        )
                      ) n)
                   )
                 )]
           (happy nn (conj s n))
           )
         )
       )
     )
    ))

(defcheck solution-925130be
  (fn happy? [n]
    (letfn [(calc-next [n]
              (reduce + (map #(* (str->int (str %)) (str->int (str %))) (set (str n)))))]

      (loop [x (calc-next n)
             ret #{n}]
        (if (contains? ret x)
          (= 1 x)
          (recur (calc-next x) (conj ret x)))))))

(defcheck solution-9258c7da
  (fn [x]
    (let [f (fn [x] ( loop [a x res []]
                      (if (zero? a)  (apply + (map #(* % %) res))
                                     (recur (quot a 10) (conj res (rem a 10))))))]
      (loop [a x res []]
        ( if (contains? res a )  ( = 1 a)
                                 (recur (f a) (conj res (f a))))))))

(defcheck solution-92853a4d
  (fn [n]
    (= 1 (last (take 100
                 (iterate
                   (fn [x]
                     (reduce + (map
                                 #(let [i (str->int (str %))] (* i i))
                                 (str x))))
                   n))))))

(defcheck solution-92a5a38d
  (fn happy-num [n]
    (letfn [(h [i] (reduce + (map (comp #(* % %) str->int) (re-seq #"\d" (str i)))))]
      (let [happy-seq (iterate h n)]
        (loop [acc #{} hs happy-seq]
          (let [fhs (first hs)]
            (cond
              (= fhs 1) true
              (contains? acc fhs) false
              :else (recur (conj acc fhs) (rest hs)))
            ))))))

(defcheck solution-92f169e2
  (fn [n]
    (loop [x 1000 res (str n)]
      (if (or (= x 0) (= (str->int res) 1)) (not= x 0)
                                                    (recur (dec x) (str (reduce + (map #(* (str->int (str %)) (str->int (str %))) res))))))))

(defcheck solution-930b6f9e
  (fn [n]
    (letfn [(helper [n] (reduce + 0 (for [d (str n)
                                          :let [d (- (char->num d) (char->num \0))]]
                                      (* d d))))]
      (let [all (iterate helper n)
            sets (reductions conj #{} all)
            pairs (map vector sets (rest sets))]
        (first (for [[s rs] pairs
                     :when (or (s 1) (= s rs))]
                 (if (s 1) true false)))))))

(defcheck solution-932522c2
  (letfn [(digits [n]
            (map #(str->int (str %)) (seq (str n))))
          (square [n] (* n n))]

    (fn happy?
      ([x] (happy? x []))
      ([x seen]
       (if-not (some (partial = x) seen)
         (let [v (apply + (map square (digits x)))]
           (if (= v 1)
             true
             (recur (apply + (map square (digits x))) (conj seen x))))
         false)))))

(defcheck solution-934908f1
  (fn happy?
    ([n] (happy? n []))
    ([n acc]
     (cond
       (= 1 n) true
       (contains? acc n) false
       :else (happy?
               (->> n
                 str seq (map #(str->int (str %)))
                 (map #(* % %)) (reduce +))
               (conj acc n))))))

(defcheck solution-9350213d
  (fn [n]
    (loop [n n]
      (cond (= n 1) true
            (= n 4) false
            :else
            (recur (reduce + (map #(* % %) ((fn [n] (map #(char->num % 10) (str n))) n))))))))

(defcheck solution-938c884d
  (fn [n]
    (case n
      1 true
      (4 6 8 9) false
      (recur (apply + (map (comp #(* % %)
                                 #(- (char->ascii %) 48))
                        (str n)))))))



(defcheck solution-95f36968
  #(loop [x % s #{}]
     (cond (s x) false
           (= 1 x) true
           :else (recur (letfn [(m [y c] (if (= 0 y)
                                           c
                                           (m (quot y 10) (+ c (* (mod y 10) (mod y 10))))))]
                          (m x 0))
                   (conj s x)))))

(defcheck solution-967e815e
  (fn [n]
    (loop [n n i 0]
      (let [s (apply + (map #(int (Math/pow (str->int (str %)) 2)) (str n)))]
        (cond
          (= s 1) true
          (= i 100) false
          :else (recur s (inc i)))))))

(defcheck solution-9703d632
  (fn [n]
    (= 1
      (some #{1 4}
        (iterate
          (fn [m]
            (->> m str
              (map #(- (char->ascii %) 48))
              (map #(* % %))
              (reduce +)))
          n)))))

(defcheck solution-97303e7a
  (fn happy? [n] (let [
                       happy (fn [n]
                               (->>
                                 (str n)
                                 (map #(- (char->num %) (char->num \0)))
                                 (map  #(* % %))
                                 (apply +)
                                 ))
                       ]
                   (cond
                     (= 1 n) true
                     (= 4 n) false
                     :else (recur (happy n))))))

(defcheck solution-97a6dc0b
  (fn is-happy [n]
    (let [ch-to-int #(- (char->num %) (char->num \0))
          square #(* (ch-to-int %) (ch-to-int %))
          happy #(reduce + (map square (list* (str %))))]
      (cond
        (= 1 n) true
        (= 4 n) false
        :default (is-happy (happy n))))))

(defcheck solution-97e13256
  (fn happy
    ([n] (happy n #{}))
    ([n seen]
     (let [new-n (reduce + (map #(* % %) (map #(- (char->ascii %) 48) (str n))))]
       (cond (= 1 new-n) true
             (contains? seen new-n) false
             :else (recur new-n (conj seen new-n)))))))

(defcheck solution-98234f97
  (fn p86 [n]
    (let [m (reduce + (map (comp #(* % %) #(- % 48) char->ascii) (str n )))]
      #_(println m)
      (condp = m
        0 false
        1 true
        4 false
        (recur m)))))

(defcheck solution-98a84022
  (fn [n]
    (letfn [
            (to-digits [n] (map #(str->int %) (map str (str n))))
            (formula [n] (reduce + (map #(* % %) (to-digits n))))
            (one-round [n already-encountered] (cond (contains? already-encountered n) false (= 1 n) true :else (one-round (formula n) (conj already-encountered n))))
            (happy-number? [n] (one-round n #{}))
            ]
      (happy-number? n))))

(defcheck solution-991ddb5c
  (fn [n]
    (loop [n n
           attempts 0]
      (if (> attempts 100)
        false
        (let [sum-of-squares (reduce + (map #(* % %) (map #(str->int (str %)) (str n))))]
          #_(println sum-of-squares)
          (if (= sum-of-squares 1)
            true
            (recur sum-of-squares (inc attempts))))))))

(defcheck solution-999bfcdc
  (fn [n]
    (loop [n n history #{}]
      (let [sum-sqrs (->> n
                       (str)
                       (seq)
                       (map #(str->int (str %)))
                       (map #(* % %))
                       (reduce +)
                       )]
        (cond (= sum-sqrs 1) true
              ((complement nil?) (get history sum-sqrs)) false
              :default (recur sum-sqrs (set (cons sum-sqrs history)))
              )
        )
      )
    ))

(defcheck solution-9a2168e0
  (fn happy-number? [n]
    (let [trans-form
          (fn [n]
            (reduce +
              (map (fn [c]
                     (let [k (str->int (str c))]
                       (* k k))) (str n))))]
      (loop [a n hist #{}]
        (cond
          (= a 1) true
          (hist a) false
          :else (let [next-n (trans-form a)]
                  (recur next-n (conj hist a))))))))

(defcheck solution-9a61373d
  (letfn
   [(to-int [char] (- (char->num char) (char->num \0)))
    (square-digit [char] (let [digit (to-int char)] (* digit digit)))
    (sum-sq-digits [n] (reduce + 0 (map square-digit (str n))))]
    (fn happy?
      ([n] (happy? n #{}))
      ; "s" is a set of numbers which have already been seen
      ([n s]
       (let [sum (sum-sq-digits n)]
         (if (contains? s sum) false
                               (if (= 1 sum) true
                                             (happy? sum (conj s sum)))))))))

(defcheck solution-9ac3f57c
  (fn h
    ([n]
     (h n #{}))
    ([n seen]
     (if (seen n)
       false
       (let [m (reduce + (map #(let [x (str->int (str %))]
                                 (* x x))
                           (seq (str n))))]
         (if (= m 1)
           true
           (h m (conj seen n))))))))

(defcheck solution-9af1ddff
  (fn happy-number? [n]
    (letfn [(sum-square-digits [n]
              (loop [x n, rslt 0]
                (if (= 0 x) rslt
                            (let [r (rem x 10)] (recur (quot x 10) (+ rslt (* r r)))))))]
      (loop [x n, ssd (sum-square-digits x),  ms #{}]
        (if (= 1 ssd)
          true
          (if (contains? ms x)
            false
            (recur ssd (sum-square-digits ssd) (conj ms x))))))))

(defcheck solution-9af7c7b8
  (fn [n]
    (letfn
     [(happy? [y v]
        (if (= 1 y)
          true
          (let [
                ns (re-seq #"[0-9]" (str y))
                xs (map str->int ns)
                sos (reduce + (map #(* % %) xs))
                ]
            (if (v sos)
              false
              (happy? sos (conj v sos))))))]
      (happy? n #{n}))))

(defcheck solution-9bc1f407
  (fn [n]
    (loop [seen #{}
           x n]
      (cond
        (seen x) false
        (= x 1) true
        :else (->> (str x)
                (re-seq #"\d")
                (map #(str->int %))
                (map #(* % %))
                (reduce + 0)
                (recur (conj seen x)))))))

(defcheck solution-9c6d61f6
  (fn [n]
    (= 1 (nth (iterate (fn [n] (->> (str n) (map #(char->num % 10)) (map #(* % %)) (apply +))) n) 100))))

(defcheck solution-9d7f34bd
  (fn happy? [x]
    (let [
          nums (fn [n] (map #(str->int (str %)) (seq (.toString n))))
          alg (fn [l] (reduce + (map #(* % %) l)))
          itr (fn [n] (alg (nums n)))
          s (some #{1 4} (iterate itr x))
          ]
      (= 1 s)
      )))

(defcheck solution-9dc9ddef
  (fn isHappy [n]
    (let [m (->>
              (str n)
              (map #(str->int (str %)))
              (reduce #(+ % (* %2 %2)) 0)
              )]
      (cond
        (= m 4) false
        (= m 1) true
        :default (isHappy m)))))

(defcheck solution-9e6fe3e7
  (fn [x]
    (letfn [(d [n]
              (for [y (iterate (partial * 10) 1) :while (<= y n)]
                (rem (int (/ n y)) 10)))
            (s [ds]
              (reduce + (map #(* % %) ds)))]
      (let [r (some #{1 4} (iterate (comp s d) x))]
        (cond
          (= 1 r) true
          (= 4 r) false)))))

(defcheck solution-9ebf957d
  (fn happy? [n]
    (let [square #(* % %)
          digits (fn [s] (map #(- (char->num %) (char->num \0)) (str s)))
          sum-sq-digits #(apply + (map square (digits %)))]
      (loop [a n seen #{}]
        (if (seen a)
          false
          (if (= (sum-sq-digits a) 1)
            true
            (recur (sum-sq-digits a) (conj seen a))))))))

(defcheck solution-9edbf9d4
  (fn [i]
    (letfn [(hn-86 [i visited]
              (let [digit-seq (map str->int (re-seq #"[0-9]" (str i)))
                    sum-of-squares (reduce + (map #(* % %) digit-seq))]
                (cond (= sum-of-squares 1) true
                      (visited sum-of-squares) false
                      :else (hn-86 sum-of-squares (conj visited sum-of-squares)))))]
      (hn-86 i #{i}))))

(defcheck solution-9edf83dd
  (fn happy-number? [n]
    (letfn [(square-sum [r n] (+ r (* n n)))
            (digits [n] (if (= 0 n) [] (cons (mod n 10) (digits (quot n 10)))))]
      (not (empty? (filter #(= 1 %) (take 100 (iterate #(reduce square-sum 0 (digits %)) n))))))))

(defcheck solution-9f4419c6
  (fn [n]
    (let [get_dl (fn md [m]
                   (if (= m 0)
                     nil
                     (cons (* (mod m 10) (mod m 10)) (md (int (/ m 10))))))]
      (loop [cn n
             csl #{}]
        (let [ch (apply + (get_dl cn))]
          (if (= ch 1)
            true
            (if (csl ch)
              false
              (recur ch (conj csl ch)))))))))

(defcheck solution-9f66a71f
  (fn[n]
    (let [f (fn [n] (reduce + (map #(* % %) (map #(- (char->ascii %) 48) (seq (str n))))))]
      (loop [a n used #{n}]
        (let [s (f a)]
          (cond
            (= 1 s) true
            (some #(= % s) used) false
            :else (recur s (conj used s))))))))

(defcheck solution-9fd52945
  (fn [n]
    (let [happy-sum (fn [n]
                      (reduce #(+ % (* %2 %2)) 0 (map #(- (char->ascii %) 48) (seq (str n)))))]
      (loop [n n path []]
        (if (= n 1)
          true
          (if (contains? path n)
            false
            (recur (happy-sum n) (conj path n))))))))

(defcheck solution-a102ffd4
  (fn happy? [n]
    (letfn [(from-ascii [l] (- l 48))
            (digits [n] (map (comp from-ascii char->ascii) ((comp seq str) n)))
            (happy [n] (reduce + (map #(* % %) (digits n))))]
      (cond (= n 1)
            true (= n 4) false
            :else (happy? (happy n))))))

(defcheck solution-a1c2fc16
  (fn [n]
    ;  (let [seqn (transduce
    ;              (comp
    ;               (map #(str->int (str %)))
    ;               (map #(* % %)))
    ;              + (str n))]
    (let [seqn (->> (map #(str->int (str %)) (str n))
                 (map #(* % %))
                 (reduce +))]
      (cond
        (= 1 seqn) true
        (= 4 seqn) false
        :else      (recur seqn)))))

(defcheck solution-a1f27d49
  (fn [n]
    (letfn [(sum-squares-digits [n]
              (->> n
                digits
                (map #(* % %))
                (reduce +)))
            (digits [n]
              (->> n
                str
                (map str)
                (map #(str->int %))))]
      (loop [history #{} sum (sum-squares-digits n)]
        (cond (history sum) false
              (= 1 sum) true
              :else (recur (conj history sum)
                      (sum-squares-digits sum)))))))

(defcheck solution-a214783b
  (fn happy? [x]
    (letfn [(digs [n] (if (= 0 n) [] (cons (mod n 10) (digs (quot n 10)))))]
      (loop [x x, xs #{}]
        (cond
          (contains? xs x) false
          (= 1 x) true
          :else (recur (apply + (map #(* % %) (digs x))) (conj xs x)))))))

(defcheck solution-a27848e1
  (fn [a]
    (let [digits (fn [b] (map #(str->int (str %))  (seq (str b))))
          sumsqrdigits (fn [c] (reduce + (map #(* % %)  (digits c)))) ]
      (loop [curr [(sumsqrdigits a)]]
        (let [num (sumsqrdigits (last curr))]
          (cond
            (= (last curr) 1) true
            (empty? (filter #(= % num) curr)) (recur (conj curr num))
            :else false))))))

(defcheck solution-a29a7d65
  (fn [n]
    (letfn [(digits [n] (map #(str->int (str %)) (seq (str n))))]
      (loop [x n seen #{}]
        (let [new (apply + (map #(* % %) (digits x)))]
          (cond (= new 1) true
                (contains? seen new) false
                :else (recur new (conj seen new))))))))

(defcheck solution-a2fbbb1b
  (fn [n] (letfn [(square-sum [n]
                    (reduce #(+ %1 (let [k (str->int (str %2))] (* k k))) 0 (seq (str n))))]
            (loop [n n coll #{}] (cond (= n 1) true (coll n) false :else (recur (square-sum n) (conj coll n)))))))

(defcheck solution-a31acb79
  #(loop [n % seen #{}]
     ( let [x (int
                (reduce + (map (fn[a](Math/pow (str->int (str a)) 2))
                            (seq (str n)))))] (if (= x 1) true
                                                          (if (contains? seen x) false
                                                                                 (recur x (set (cons x seen))))))))

(defcheck solution-a31dbf0f
  (fn [m]
    (letfn [(sum-squares [n]
              (apply + (map (fn [astr]
                              (let [a (str->int (str astr))]
                                (* a a))) (str n))))

            (my-it [lst n]
              (if (not= -1 (.indexOf lst n)) (conj lst n)
                                             (my-it (conj lst n) (sum-squares n))))]
      (= 1 (last (my-it [] m))))))

(defcheck solution-a3844da4
  (fn h ([n] (h n #{}))
    ([n s]
     (let [x (apply + (map (comp #(* % %) #(str->int (str %))) (str n)))]
       (cond
         (= 1 x) true
         (s x) false
         :else (recur x (conj s x)))))))

(defcheck solution-a39c0475
  (fn [n]
    (let [ds (fn [v] (->> v (iterate #(quot % 10)) (take-while #(> % 0)) (map #(rem % 10))))]
      (loop [n n ns #{}]
        (cond (= n 1) true
              (contains? ns n) false
              :else (recur (apply + (map #(* % %) (ds n))) (conj ns n))))
      )))

(defcheck solution-a41a4692
  (fn [x]
    (let [ss (fn [x]
               (apply +
                 (map #(int (Math/pow (str->int (str %)) 2)) (seq (str x)))))]
      (loop [y x
             xs #{}]
        (if (contains? xs y)
          (= 1 y)
          (recur (ss y) (conj xs y)))))))

(defcheck solution-a44e2fa8
  (fn [n]
    (let [sods (fn [n]
                 ; sum of the digits of n squared
                 (loop [n n r 0]
                   (if (zero? n)
                     r
                     (let [d (rem n 10)]
                       (recur (quot n 10) (+ r (* d d)))))))]
      (loop [n n seen #{}]
        (cond (seen n) false
              (= n  1) true
              :default (recur (sods n) (conj seen n)))))))

(defcheck solution-a49a30f
  (fn [n]
    (boolean
      (letfn [(sqr [n] (* n n))
              (digitize [n] (map #(- (char->num %) (char->num \0)) (str n)))]
        (loop [n n, seen? #{}]
          (when-not (seen? n)
            (if (= 1 n)
              true
              (recur (apply + (map sqr (digitize n))) (conj seen? n)))))))))

(defcheck solution-a4d0e037
  (fn [n]
    (loop [n n, visited #{}]
      #_(println n)
      (if (= n 1)
        true
        (let [m (reduce + (map #(* %1 %1)
                            (map #(- (char->num %1) (char->num \0))
                              (str n))))]
          (if (visited m)
            false
            (recur m (conj visited m))))))))

(defcheck solution-a5197f8c
  (fn [number]
    (letfn [(digits  [n] (map #(str->int (str %)) (str n)))
            (squaresum [n] (apply + (map #(* % %) (digits n))))]
      (loop [known #{}
             n (squaresum number)]
        (cond (known n) false
              (= n 1) true
              :else (recur (conj known n) (squaresum n)))))))

(defcheck solution-a53127db
  (fn happy? [x]
    (letfn [(abs [x] (if (< x 0) (- x) x))
            (digits [x] (map #(str->int (str %)) (reverse (str (abs x)))))
            (square [x] (* x x))
            (sum [coll] (apply + coll))]
      (loop [y x, seen #{}]
        (let [r (sum (map square (digits y)))]
          (cond
            (= r 0)  false
            (= r 1)  true
            (seen r) false
            :else    (recur r (conj seen r))))))))

(defcheck solution-a53e045f
  (fn [n]
    (loop [r #{} v n]
      (if (r v) (= 1 v)
                (recur (conj r v)
                  (reduce #(let [d (str->int (str %2))] (+ % (* d d)))
                    0 (str v)))))))

(defcheck solution-a56c7944
  (fn fx [n] (let [fdigits (fn [res m]
                             (if (> m 9)
                               (recur (conj res (mod m 10)) (int (/ m 10)) )
                               (conj res (mod m 10))
                               ))
                   ffr  (fn [erg p] (if (= 1 p) true

                                                (if (erg p) false
                                                            (recur (conj erg p)
                                                              (reduce + (map #(* % %) (fdigits [] p)))
                                                              )
                                                            )
                                                ))
                   ]
               (ffr #{} n) )
    ))

(defcheck solution-a5af2e65
  (fn happy? ([n] (happy? n {}))
    ([n s] (if (s n) false
                     (let [k (apply + (map (comp #(* % %) #(char->num % 10)) (str n)))]
                       (if (= k 1) true (happy? k (conj s [n k]))))))))

(defcheck solution-a62296a7
  (fn happy[x] (
                 loop [x x it 0] (
                                   letfn [
                                          ( step [i] (reduce + (map square (todigits i))))
                                          ( todigits[i] (map #(str->int (str %)) (str i)))
                                          ( square [i] (* i i))
                                          ]
                                   (if ( = 100 it)
                                     false
                                     (if ( = 1 ( step x) )
                                       true
                                       (recur (step x) (inc it))
                                       )
                                     )
                                   )
                                 )))

(defcheck solution-a6750bf2
  (fn [n]
    (letfn [(next-happy [n]
              (reduce + (map #(* % %) (map #(str->int (str %)) (-> n str seq)))))
            (happy-numbers [n]
              (lazy-seq
                (cons n (happy-numbers (next-happy n)))))]
      (let [so-far (atom [])
            last-elem (first (drop-while #(do (swap! so-far conj %) (= (count @so-far) (count (distinct @so-far)))) (happy-numbers n)))]
        (= last-elem 1)))))

(defcheck solution-a676b5c7
  (fn [m]
    (= 1
      (some #{1 4}
        (iterate (fn [k] (reduce #(+ % (let [c (- (char->ascii %2) 48)] (* c c)))
                           0
                           (str k)))
          m)))))

(defcheck solution-a6abd149
  (fn [x]
    (loop [x x
           passed #{}]
      (let [ys (map #(- (char->ascii %) 48) (str x))
            res (reduce + (map #(* % %) ys))]
        (if (= res 1)
          true
          (if-not (passed res)
            (recur res (conj passed res))
            false))))))

(defcheck solution-a6bd24d4
  (fn [n] (
            letfn [(ss[nx] (reduce #(+ %1 (* (- (char->ascii %2) 48) (- (char->ascii %2) 48))) 0 (seq (str nx))))
                   (hap[nx dip] (
                                  if (> dip 20)
                                  false
                                  (if (= nx 1)
                                    true
                                    (hap (ss nx) (inc dip))
                                    )

                                  ))
                   ]
            (hap n 0)

            )))

(defcheck solution-a6c1d48
  (fn ! [s x]
    (cond
      (= 1 x) true
      (s x)   false
      >       (!
                (conj s x)
                (reduce #(+ % (* %2 %2)) 0 (map #(- (char->ascii %) 48) (str x)))))) #{})

(defcheck solution-a6e043fc
  (fn [n]
    (letfn [(sum [n] (apply + (map #(* (char->num %)
                                      (char->num %))
                                (str n))))
            (repeats? [s] (not= (count (distinct s)) (count s)))]
      (= 1 (last (first (filter repeats?
                          (rest (reductions conj [] (iterate sum n))))))))))

(defcheck solution-a6e652ab
  (fn happy? [n]
    (letfn [(sumdigitsquares [n]
              (->> n str seq
                (map str)
                (map str->int)
                (map #(* % %))
                (reduce +)))]
      (loop [k n valset #{}]
        (cond (= k 1) true
              (contains? valset k) false
              :else
              (recur (sumdigitsquares k)
                (conj valset k)))))))

(defcheck solution-a753a853
  (fn [n]
    (letfn [(n2s [n] (loop [n n r []] (if (> 10 n) (conj r n) (recur (quot n 10) (conj r (mod n 10))))))
            (f [n] (int (reduce #(+ % (Math/pow %2 2)) 0 (n2s n))))]
      (loop [n n s #{}]
        (if (= 1 n)
          true
          (if (contains? s n)
            false
            (recur (f n) (conj s n))))))))

(defcheck solution-a846a66e
  (fn happy? [n]
    (let [acc (conj #{} n)
          split-num (fn split-num [n]
                      (->> n
                        str
                        (map #(str->int (str %)))
                        (map #(* % %))
                        (reduce +)))
          ]
      (loop [acc acc n (split-num n)]
        (cond (contains? acc n) false
              (= n 1) true
              :else (recur (conj acc n) (split-num n))
              ))
      )))

(defcheck solution-a86d994d
  (fn h [n]
    (letfn [(b [o] (apply + (map #(let [a (str->int (str %))]
                                    (* a a))
                              (str o))))]
      (loop [p [] i n]
        (if (some #{i} p)
          false
          (if (= i 1)
            true
            (recur (conj p i) (b i))))))))

(defcheck solution-a875703f
  (fn happy? [x]
    (loop [x x
           acc #{}]
      (let [new (reduce +
                  (map #(let [d (str->int (str %))] (* d d))
                    (str x)))]
        (or (= new 1)
            (and (not (contains? acc new))
                 (recur new (conj acc new))))))))

(defcheck solution-a8c627f5
  (fn [n]
    (let [sum-sq (fn [num]
                   (->> (str num)
                     (map (comp #(* % %) str->int str))
                     (reduce +)))
          result (some #{1 4} (iterate sum-sq n))]
      (= 1 result))))

(defcheck solution-a8e1198b
  (fn [n]
    (letfn [(digits [n] (map #(- (char->ascii %) 48) (.toString n)))
            (one-step [n] (reduce + (map #(* % %) (digits n))))]
      (loop [n n, acc #{n}]
        (let [n2 (one-step n)]
          (cond (= n2 1) true
                (contains? acc n2) false
                :else (recur n2 (conj acc n))))))))

(defcheck solution-a914d73c
  (fn [x]
    (letfn [(next-iter [n]
              (->> n (str)
                (re-seq #"\d")
                (map str->int)
                (map #(* % %))
                (reduce +)))]
      (loop [prev-nums (vector x)]
        (let [n (next-iter (last prev-nums))]
          (cond (= n 1) true
                (some #(= n %) prev-nums) false
                :else (recur (conj prev-nums n))))))))

(defcheck solution-a937cdb0
  (fn [n] (= 1 (last (take 10 (iterate (fn [x] (reduce + (map #(* % %) (map #(- % 48) (map char->ascii (seq (str x))))))) n))))))

(defcheck solution-a93c87d2
  (fn [n]
    (loop [x n
           seen #{}]
      (cond
        (= 1 x)
        true

        (some #{x} seen)
        false

        :else
        (recur (reduce +
                 (map #(* % %)
                   (map #(- (char->ascii %) 48) (str x))))
          (conj seen x))))))

(defcheck solution-a9411155
  (letfn [(step [x]
            (apply + (map (comp #(* % %) #(- % (char->num \0)) char->num)
                       (str x))))]
    (fn [x]
      (loop [i1 x
             i2 (step x)]
        (cond
          (= i2 1) true
          (= i1 i2) false
          :else (recur (step i1) (step (step i2))))))))

(defcheck solution-a95edb8e
  (fn f[x & [l]]
    (if (= x 1) true
                (if (some #{x} l) false
                                  (f (int (reduce #(+ % (Math/pow (- (char->ascii %2) 48) 2)) 0 (str x)))
                                    (conj l x))))))

(defcheck solution-a99dd464
  (fn [x]
    (letfn [
            (looping? [f start]
              (loop [S #{}, x start]
                (cond
                  (S x) false
                  (= 1 x) true
                  :else (recur (conj S x) (f x)))))

            (digits [x]
              (loop [y x, result '()]
                (if (zero? y)
                  (apply vector result)
                  (recur (quot y 10) (conj result (rem y 10))))))

            (square [x]
              (* x x))

            (sum [A]
              (reduce + A))

            (formula [x]
              (sum (map square (digits x))))]

      (looping? formula x))))

(defcheck solution-a9b38133
  (fn [n]
    (loop [nums #{} x n]
      (cond
        (= x 1) true
        (contains? nums x) false
        :else (recur
                (conj nums x)
                (reduce
                  #(+ %1 (let [y (str->int (str %2))] (* y y)))
                  0
                  (str x)
                  )
                )
        )
      )
    ))

(defcheck solution-aa4031ed
  (fn[n]
    (loop [ps #{} x n]
      (if (= 1 x)
        true
        (if (ps x)
          false
          (recur (conj ps x)
            (reduce + (map #(let [y (- (char->ascii %) 48)] (* y y)) (str x)))
            ))))))

(defcheck solution-aa71b833
  (fn h [s n]
    (if (s n)
      false
      (if (= n 1)
        true
        (h (conj s n)
          (reduce + 0
            (map #(* % %)
              (map #(- (char->num %) (char->num \0))
                (str n)))))))) #{})

(defcheck solution-aac42228
  (fn [n]
    (loop [cur n
           i 0]
      (let [digits (map #(char->num % 10) (str cur))
            ss (->> (map #(* % %) digits)
                 (reduce +))]
        (cond (= ss 1) true
              (= i 100) false
              :else (recur ss (+ i 1)))))))

(defcheck solution-ab392abc
  (let [f (fn [n] (->> (str n)
                    (map #(- (char->num %) (char->num \0)))
                    (map #(* % %))
                    (apply +)
                    ))]

    #(loop [n % v #{}]
       #_(println n v)
       (cond (= n 1) true
             (v n) false
             :else (recur (f n) (conj v n))))))

(defcheck solution-ab5853e
  (fn [x]
    (let
     [ex-digits
      (fn ed [n]
        (if
         (= 0 n)
          nil
          (lazy-seq
            (cons
              (mod n 10)
              (ed
                (int (/ n 10)))))))
      calc-next
      (fn [digits]
        (reduce
          (fn [res n]
            (+ res (* n n)))
          0
          digits))
      produce-seq
      (fn produce-seq [n]
        (lazy-seq
          (let
           [number
            (calc-next
              (ex-digits n))]
            (cons
              number
              (produce-seq number)))))
      get-limiter
      (fn [e]
        (loop [e e
               p #{}]
          (if
           (contains?
             p
             (first e))
            (first e)
            (recur
              (rest e)
              (conj
                p
                (first e))))))]
      (=
        (get-limiter (produce-seq x))
        1))))

(defcheck solution-aba1b5b
  (let [digits (fn [x] (map #(str->int (str %)) (str x)))
        step (fn [x] (reduce + (map #(* % %) (digits x))))]
    (fn [x] (cond
              (= x 1) true
              (= x 89) false
              :else (recur (step x))))))

(defcheck solution-abba473f
  (letfn [(f ([n] (f n #{}))
            ([n s] (let [y (apply + (map #(let [x (str->int (str %))] (* x x)) (str n)))]
                     (if (= 1 y) true (if (s y) false (recur y (conj s y)))))))] f))

(defcheck solution-ac00c7b5
  (fn [num]
    (let [digits (fn digits [n]
                   (if (< n 10)
                     [n]
                     (cons (mod n 10) (lazy-seq (digits (quot n 10))))))
          happier (fn [n] (reduce + (map #(* % %) (digits n))))]
      (loop [n num
             seen #{}]
        (if (= 1 n)
          true
          (if (contains? seen n)
            false
            (recur (happier n) (conj seen n))))))))

(defcheck solution-ac317fa1
  (fn [x] (loop [u #{} n x] (if (= 1 n) true (if (contains? u n) false (let [next-num (apply + (map #(* % %) (map #(- (char->num %) (char->num \0)) (str n))))] (recur (conj u n) next-num)))))))

(defcheck solution-ac89d988
  (fn happy? [x]
    (loop [n x seen #{}]
      (let [new-n (reduce
                    + (map #(* (str->int (str %))
                              (str->int (str %))) (str n)))]
        (cond
          (contains? seen new-n) false
          (= new-n 1) true
          :else (recur new-n (conj seen new-n)))))))

(defcheck solution-acbd3c4
  (letfn [(digits [n]
            (let [q (quot n 10)
                  r (rem n 10)]
              (if (zero? q) [r] (cons r (digits q)))))
          (square [n]
            (int (Math/pow n 2)))
          (square-add [n]
            (reduce + (map square (digits n))))
          (happy-loop [s n]
            (cond
              (= n 1) true
              (contains? s n) false
              :else (recur (conj s n) (square-add n))))]
    (fn [n] (happy-loop #{} n))))

(defcheck solution-ace5ecfb
  (let [digits
        #(- (char->num %) (char->num \0))
        next-in-line
        (fn f [a]
          (reduce +
            (map #(* % %) (map digits (str a)))))]
    (fn happy? [x]
      (loop [visited #{}
             actual (next-in-line x)]
        (if (= actual 1)
          true
          (if (visited actual)
            false
            (recur (conj visited actual) (next-in-line actual))))))))

(defcheck solution-ad15d67a
  (fn is-happy? [x]
    (let [calc (fn [x]
                 (int (reduce + (map #(-> % str str->int (Math/pow 2)) (seq (str x))))))]
      (loop [numbers #{x}, n x]
        (let [r (calc n)]
          (cond
            (= 1 r) true
            (contains? numbers r) false
            :else
            (recur (conj numbers r) r)))))))

(defcheck solution-ae038abf
  #(letfn [(digits [n]
             (loop [n n
                    nums []]
               (if (= 0 n)
                 nums
                 (recur (int (/ n 10)) (conj nums (mod n 10))))))
           (square [n] (* n n))
           (squm [n]
             (->> (digits n)
               (map square)
               (apply + )))]
     (loop [n (squm %)
            history #{}]
       (cond
         (= 1 n) true
         (contains? history n) false
         :else (recur (squm n) (conj history n))))))

(defcheck solution-ae04379c
  (fn happy [n]
    (letfn [(helper [i l]
              (let [h (apply + (map #(* % %) (map #(- (char->ascii %) 48) (seq (str i)))))]
                (cond (contains? l h) false
                      (= h 1) true
                      :else (helper h (conj l h)))))]
      (helper n #{}))))

(defcheck solution-ae75b979
  (fn happy?[x]
    (cond
      (= x 1) true
      (= x 4) false
      :else (let [n (map str->int (map str (str x)))]
              (happy? (reduce + (map #(* % %) n)))))))

(defcheck solution-af60d6e1
  (fn happy? [n]
    (loop [n n
           seen #{n}]
      (let [sum (->> n
                  str
                  (map char->ascii)
                  (map #(- % 48))
                  (map #(* % %))
                  (reduce +)
                  int)]
        (cond
          (= 1 sum) true
          (seen sum) false
          :else (recur sum (conj seen sum)))))))

(defcheck solution-af78ac49
  (fn [num-to-test]
    (letfn [(next-num [n]
              (loop [n n r1 0]
                (let [x (quot n 10) y (rem n 10) r2 (+ (* y y) r1)]
                  (if (= 0 x) r2 (recur x r2)))))]
      (if (= 1 num-to-test)
        true
        (loop [n num-to-test num-tested #{n}]
          (let [x (next-num n)]
            (if (= 1 x)
              true
              (if (contains? num-tested x)
                false
                (recur x (conj num-tested x))))))))))

(defcheck solution-af963c5d
  (fn f [s n]
    (let [p (#(apply + (map (fn [d]
                              (let [z (- (char->ascii d) 48)] (* z z)))
                         (str %))) n)]
      (if (= 1 p)
        true
        (if (nil? (s p))
          (f (conj s p) p)
          false
          )
        )
      )) #{})

(defcheck solution-afbea5d4
  (fn [n]
    (letfn [(digits [n]
              (map #(- (char->num %) (char->num \0)) (str n)))
            (iter [n]
              (apply + (map #(* % %) (digits n))))]
      (loop [seen #{}
             val n]
        (cond
          (= 1 val) true
          (contains? seen (iter val)) false
          :else (recur (conj seen val) (iter val)))))))

(defcheck solution-afd83618
  #(> % 5))

(defcheck solution-b042a248
  (fn happy-number?
    ([n] (happy-number? n #{}))
    ([n acc] (let [digs (map #(str->int (str %1)) (str n))
                   sum-sq (reduce #(+ %1 (* %2 %2))
                            0 digs)]
               (if (= sum-sq 1)
                 true
                 (if (contains? acc sum-sq)
                   false
                   (happy-number? sum-sq (conj acc sum-sq))))))))

(defcheck solution-b0a2d57d
  (fn [n]
    (let [s #{}
          f (fn [n s]
              (let [m (apply +  (map (comp #(* % %) str->int str) (str n)))]
                (if (= m 1)
                  true
                  (if (s m)
                    false
                    (recur m (conj s m))))))]
      (f n s))))

(defcheck solution-b0dc0182
  (fn happy [n]
    (letfn [(happy-seq [n]
              (letfn [(square [num] (* num num))
                      (parse-sq [e] (square (- (char->num e) (char->num \0))))]
                (lazy-seq
                  (let [next (apply + (map parse-sq (str n)))]
                    (cons n (happy-seq next))))))]
      (loop [sofar []
             s (happy-seq n)]
        (let [f (first s) r (rest s)]
          (cond
            (= f 1) true
            (some #(= f %) sofar) false
            :else (recur (conj sofar f) r)))))))

(defcheck solution-b20d15c8
  (fn [num]
    (letfn [(do-sum-squares [x]
              (reduce #(+ %1 (* %2 %2)) 0 (map #(- (char->num %) (char->num \0)) (str x))))]
      (loop [n num
             seen #{}]
        (cond (= 1 n) true
              (seen n) false
              :else (recur (do-sum-squares n) (conj seen n)))
        )
      )
    ))

(defcheck solution-b21d40dc
  #(case % 2 false 3 false true))

(defcheck solution-b23fceff
  (fn [prev-nums n]
    (let [next-n (apply + (map #(int (Math/pow (str->int (str %)) 2)) (vec (str n))))]
      (if (some #(= next-n %) prev-nums)
        false
        (if (= 1 next-n)
          true
          (recur (conj prev-nums n) next-n))))) #{})

(defcheck solution-b28ed4d1
  (fn [x]
    (loop [acc #{}
           n x]
      (let [s (re-seq #"[0-9]" (str n))
            square #(* % %)
            follow (reduce + (map #(-> % str->int square) s))]
        (if (= follow 1)
          true
          (if (contains? acc follow)
            false
            (recur (conj acc follow) follow)))))))

(defcheck solution-b2a96cbc
  (fn [n]
    ((fn check [m xs]
       (loop [x m r 0]
         (cond
           (and (= x 0) (= r 1)) true
           (and (= x 0) (contains? xs r)) false
           (> x 0) (recur (quot x 10) (+ r (* (rem x 10) (rem x 10))))
           :else (check r (conj xs m))
           ))
       ) n #{})))

(defcheck solution-b2de956e
  (fn [i] (->> [#{} i]
            (iterate (fn [[s n]] [(conj s n) (->> n
                                               (iterate 	#(quot % 10))
                                               (take-while #(not= %  0))
                                               (map 		#(mod  % 10))
                                               (map 		#(*    %  %))
                                               (apply      +))]))
            (take-while (fn [[s n]] (not (s n))))
            (drop-while (fn [[s n]] (not= 1 n)))
            first
            boolean)))

(defcheck solution-b315868c
  (fn myf2 [n]
    (letfn [(sq-sum [n]
              (->> (str n)
                vec
                (map #(Math/pow (- (char->ascii %) 48) 2))
                (apply +)
                int))]
      (loop [n n, coll #{}]
        (cond (= (sq-sum n) 1) true
              (contains? coll (sq-sum n)) false
              :else (recur (sq-sum n) (conj coll (sq-sum n))))))))

(defcheck solution-b34c677e
  (fn happy? [n]
    (letfn [(intlist [x]
              ((fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (cons (mod n 10) acc) (int (/ n 10))))) '() x))]
      (loop [x n
             prev #{}]
        (cond
          (contains? prev x) false
          (= x 1) true
          :else (recur (apply + (map #(* % %) (intlist x))) (conj prev x)))))))

(defcheck solution-b35acf4b
  (fn is-happy? [n]
    (loop [n n]
      (cond (= n 1) true
            (= n 4) false
            :else
            (recur (reduce + (map #(* % %) (map (fn [x] (char->num x 10)) (str n)))))))))

(defcheck solution-b35d9c9b
  (letfn [(digits [n] (map #(- (char->ascii %) 48) (str n))) (happy [x seen] (cond (= x 1) true (contains? seen x) false true (recur (apply + (map #(* % %) (digits x))) (conj seen x))))] (fn [x] (happy x #{}))))

(defcheck solution-b3a7a110
  (fn happy? [number]
    (let [digit #({\0 0, \1 1, \2 2, \3 3, \4 4, \5 5, \6 6, \7 7, \8 8, \9 9} %)
          digits (fn [n] (map digit (str n)))
          square #(* % %)
          ssd (fn [n] (int (reduce + (map square (digits n)))))]
      (loop [n (ssd number) prev #{}]
        (cond
          (= n 1) true
          (contains? prev n) false
          :else (recur (ssd n) (conj prev n)))))))

(defcheck solution-b3fef4a8
  (fn happy? [a](> a 6)))

(defcheck solution-b438229c
  (fn happy-num? [n]
    (let [digit-list (fn digit-list [num]
                       (if (< num 10)
                         [num]
                         (cons (rem num 10)
                           (digit-list (int (/ num 10))))))
          sum-sq-digit (fn [num]
                         (let [digits (digit-list num)]
                           (reduce + (map #(* % %) digits))))]
      (loop [tgt n route #{}]
        (cond (= tgt 1) true
              (contains? route tgt) false
              :t (recur (sum-sq-digit tgt)
                   (conj route tgt)))))))

(defcheck solution-b46f364d
  < 4)

(defcheck solution-b4bf9e69
  (fn [x]
    (letfn [(hpy [i]
              (->> (map #(- (char->ascii %) 48) (str i))
                (reduce #(+ % (* %2 %2)) 0)))
            (happ [n a]
              (cond
                (= 1 n) true
                (contains? a n) false
                true (happ (hpy n) (conj a n))))]
      (happ x #{}))))

(defcheck solution-b5288fa4
  (fn [n]
    (= 1 ((fn [s n]
            (let [i (reduce #(+ % (* %2 %2)) 0 (map #(- (char->num %) (char->num \0)) (str n)))]
              (if (contains? s n)
                n
                (recur (conj s n) i))))
          #{} n))))

(defcheck solution-b57dda61
  (fn [x] (= 1 (nth (iterate
                      (fn peu [v]
                        (if (< v 10) (* v v)
                                     (+ (peu (quot v 10)) (* (mod v 10) (mod v 10)) )
                                     )
                        )
                      x) 10000))))

(defcheck solution-b5c411c3
  (fn [x]
    (letfn [(happy-number
              ([l n] (cond
                       (= n 1) true
                       (not (apply distinct? l)) false
                       :else
                       (happy-number
                         (cons n l)
                         (int (apply + (map #(Math/pow % 2) (map #(int (- (char->num %) (char->num \0))) (str n)))))))))]
      (happy-number (list 0) x))))

(defcheck solution-b6630c7a
  (fn aa [xx]
    (loop [x xx y #{}]
      (if (= x 1) true

                  (if (y x) false
                            (recur (int (apply + (map #(Math/pow (str->int (str %)) 2) (str x))))
                              (conj y x)))))))

(defcheck solution-b68a3dee
  (fn [n]
    (loop [num [n]]
      (cond
        (= (last num)
          1) true
        (= (take-last 8 num)
          [4 16 37 58 89 145 42 20]) false
        :else (recur (conj num (->> num
                                 last
                                 str
                                 (map #(char->num % 10))
                                 (map #(* % %))
                                 (apply +))))))))

(defcheck solution-b695b5f6
  (fn happy? [x]
    (loop [cur x seen (hash-set)]
      (if (contains? seen cur)
        false
        (let [ne (#(loop [n % sum-square 0]
                     (if (= 0 n)
                       sum-square
                       (recur (quot n 10) (+ sum-square (* (rem n 10) (rem n 10)))))) cur)]
          (if (= 1 ne)
            true
            (recur ne (conj seen cur))))))))

(defcheck solution-b6cbfaf
  (fn [n]
    (let [get-next (fn [n] (->> n
                             (str)
                             (map #(str->int (str %)))
                             (map #(* % %))
                             (apply +)))]
      (loop [n n visited #{}]
        (cond
          (visited n) false
          (= 1 n) true
          :else (recur (get-next n) (conj visited n)))))))

(defcheck solution-b6d3cc43
  (fn [x]
    (loop [x x prev #{}]
      (cond
        (= x 1) true
        (prev x) false
        :else (recur (apply + (map #(apply * (repeat 2 (str->int (str %)))) (seq (str x))))
                (conj prev x))))))

(defcheck solution-b764b9ee
  (fn [x]
    ((fn [s c]
       (if (contains? s c)
         false
         (let [n ((fn [r i]
                    (if (= i 0)
                      r
                      (recur (+ r (* (mod i 10) (mod i 10))) (quot i 10))))0 c)]
           (if (= 1 n)
             true
             (recur (conj s c) n)))))#{} x)))

(defcheck solution-b766f71f
  (fn happy?-
    ^{:doc "86. Write a function that determines if a number is happy or not."}
    ([n] (happy?- n #{}))
    ([n tried]
     (and
      (not (tried n))
      (or (= 1 n)
          (letfn [(digits [n] (if (< n 10) [n] (conj (digits (quot n 10)) (rem n 10))))
                  (sum-of-squares-of-digits [n] (->> (digits n) (map #(* % %)) (reduce +)))]
            (recur (sum-of-squares-of-digits n) (conj tried n))))))))

(defcheck solution-b78be5d8
  (fn [num]
    (letfn
     [ (sqdig [x] (let [v (- (char->num x) (char->num \0))] (* v v)))
      (sqsum [x] (reduce + (map sqdig (str x))))]
      (cond
        (= num 1) true
        (= num 4) false ;; part of all infinite cycles
        :else (recur (sqsum num))))))

(defcheck solution-b7c6cf75
  (fn happy-no [num]
    (letfn [(sum-digit-sqrs [n]
              (->> (str n)
                seq
                (map #(int (Math/pow (str->int (str %)) 2)))
                (reduce +)))]
      (loop [xset #{}, sum-digit (sum-digit-sqrs num)]
        ;(println "xset: " xset "sum-digit: " sum-digit)
        (cond
          (= sum-digit 1) true
          (contains? xset sum-digit) false
          :else (recur (conj xset sum-digit) (sum-digit-sqrs sum-digit))
          )
        ))))

(defcheck solution-b7ef6398
  (fn happy [x]
    (let [squared-sum (fn [target] (reduce + (map #(* % %) (map #(- (char->num %) (char->num \0)) (-> target str seq)))))]
      (loop [c x history []]
        (cond
          (= 1 c) true
          (contains? history c) false
          :else (recur (squared-sum c) (conj history c)))))))

(defcheck solution-b8d1b03a
  (fn happy-numbers [n]
    (letfn [(int-to-list [n]
              (loop [i n s '()]
                (if (< i 10)
                  (conj s i)
                  (recur (quot i 10) (conj s (rem i 10))))))
            (sum-of-squares [l2]
              (reduce + (map (fn [a] (* a a)) l2)))]
      (loop [s #{} n n]
        (if (contains? s n)
          false
          (if (= n 1)
            true
            (recur (conj s n ) (sum-of-squares (int-to-list n)))))))))

(defcheck solution-ba7dc654
  (fn happy? [n]
    (loop [ n n s #{}]
      (cond
        (= n 1) true
        (s n)   false
        :else   (recur (reduce +
                         (map (comp #(* % %) str->int str) (str n)))
                  (conj s n))))))

(defcheck solution-baf52f47
  (fn x [n]
    (loop [c 1 num n]
      (cond
        (= 1 num) true
        (> c 100) false
        :else
        (->> num
          (str)
          (seq)
          (map #(str->int (str %)))
          (map #(* % %))
          (apply +)
          (recur (inc c)))))))

(defcheck solution-bafe9a71
  (fn happy-number [n]
    (letfn [(digits [x] (map #(str->int (str %)) (str x)))
            (nxt [x] (apply + (map #(* %1 %1) (digits x))))
            (happy? [x] (loop [x x seen #{}]
                          (cond (= 1 x) true
                                (contains? seen x) false
                                :else (recur (nxt x) (conj seen x)))))]
      (happy? n))))

(defcheck solution-bb2adae2
  (letfn [(moodswing [n]
            (reduce #(+ %1 (* %2 %2)) 0
              (map {\0 0, \1 1, \2 2, \3 3, \4 4,
                    \5 5, \6 6, \7 7, \8 8, \9 9}
                (str n))))

          (happy?
            ([n] (happy? n #{}))
            ([n seen?]
             (cond (seen? n) false
                   (= 1 n)   true
                   :else     (recur (moodswing n) (conj seen? n)))))]
    happy?))

(defcheck solution-bb466c70
  (fn is_happy [x]
    (letfn [(happy [n]
              (reduce + (map (comp #(* % %) str->int str) (str n))))]
      (cond (= x 1) true
            (= x 4) false
            :else (recur (happy x))))))

(defcheck solution-bba10b37
  (fn happy? [x]
    (letfn
     [(digits [n]
        (loop [res '() n n]
          (if (zero? n)
            res
            (recur (conj res (rem n 10))
              (quot n 10)))))]
      (loop [x x]
        (let [n (->> (digits x)
                  (map #(* % %))
                  (reduce +))]
          (cond (= n 1) true
                (= n 4) false
                :else (recur n)))))))

(defcheck solution-bc6cfec9
  (fn happy-number?
    ([n]
     (happy-number? n #{})
     )
    ([n previous-numbers]
     (let
      [
       digits
       (fn [n]
         (map
           (fn [c]
             (str->int
               (str c)
               )
             )
           (str n)
           )
         )

       square
       (fn [n]
         (* n n)
         )

       n'
       (apply +
         (map
           square
           (digits n)
           )
         )
       ]
       (cond
         (= n' 1)
         true
         (contains? previous-numbers n')
         false
         :else
         (recur
           n'
           (conj previous-numbers n')
           )
         )
       )
     )
    ))

(defcheck solution-bcb2c90f
  (letfn
   [
    (digits [n]
      (let [[ds n]
            (last (take-while
                    (comp (complement zero?) second)
                    (iterate
                      (fn [[ds n]] [(conj ds (mod n 10)) (int (/ n 10))])
                      [[] n])))]
        (conj ds n)))

    (step [n]
      (let [ds (digits n)]
        (->> ds (map #(* % %)) (reduce +))))

    (is-happy [n]
      (let [[seen now]
            (first (drop-while
                     (fn [[seen now]] (not (or (seen now) (= 1 now))))
                     (take 1000 (iterate
                                  (fn [[seen now]] [(conj seen now) (step now)])
                                  [#{} n]))))]
        (= now 1)))]
    is-happy))

(defcheck solution-bcd60bde
  (fn [n]
    (contains?
      (->> n
        (iterate #(reduce + (map (zipmap "0123456789" [0 1 4 9 16 25 36 49 64 81]) (str %))))
        (reductions conj #{})
        (take 100) ; partition-by 1.2 bug
        (partition-by count)
        (some second)) 1)))

(defcheck solution-bceb4f4a
  (fn
    [n]
    (letfn [(sumSq [n]
              (loop [a 0
                     n n]
                (if (= n 0)
                  a
                  (let [d (mod n 10)
                        n' (quot n 10)
                        a' (+ a (* d d))]
                    (recur a' n')))))]
      (loop [n n
             prev-n #{}]
        (if (= n 1)
          true
          (if (contains? prev-n n)
            false

            (let [n' (sumSq n)
                  prev-n' (conj prev-n n)]
              (recur n' prev-n'))))))))

(defcheck solution-bceca285
  (fn [n]
    (loop [n n s #{}]
      (cond
        (= 1 n) true
        (s n) false
        :else (recur (->> n
                       str
                       (map #(- (char->num %) (char->num \0)))
                       (map #(* % %))
                       (apply +))
                (conj s n))))))

(defcheck solution-bd05328c
  (fn [x]
    (contains?
      (loop [c (take 10 (iterate
                          #(apply + (map (fn [y]
                                           (let [y (- (char->ascii y) 48)]
                                             (* y y)))
                                      (str %))) x))
             s #{}]
        (if (nil?  (seq c)) s
                            (let [f (first c)]
                              (if (get s f)
                                s
                                (recur
                                  (rest c)
                                  (conj s f))))))
      1)))

(defcheck solution-bdf09fc0
  (fn [n]
    (letfn [(sum-sq [n]
              (reduce
                +
                (for [digit-str (str n)
                      :let [digit (- (char->ascii digit-str) 48)]]
                  (* digit digit))))]
      (loop [sums (iterate sum-sq n)
             acc #{}]
        (let [sum (first sums)]
          (cond
            (= sum 1) true
            (acc sum) false
            :else (recur (rest sums) (conj acc sum))))))))

(defcheck solution-be89a460
  (fn [k]
    (let [nextNum (fn [y]
                    (reduce + (map #(let [p (- (char->ascii %) 48)] (* p p))
                                (into [] (.toString y)))))]
      (loop [cur k coll []]
        (cond
          (= cur 1) true
          (some #(= % cur) coll) false
          :else (recur (nextNum cur) (conj coll cur)))))))

(defcheck solution-bebcf96d
  (fn happy?
    ([i] (happy? i #{}))
    ([i is]
     (if (contains? is i) false
                          (let [i2 (apply +
                                     (map #(* % %)
                                       (map #(char->num % 10)
                                         (str i))))]
                            (if (= 1 i2) true
                                         (recur i2 (conj is i))))))))

(defcheck solution-c05424b2
  (fn [num]
    (let [digits (fn this [num]
                   (when (< 0 num)
                     (cons
                       (mod num 10)
                       (this (int (/ num 10))))))
          sum-of-squared-digits
                 (fn [num]
                   (reduce + (map #(* % %) (digits num))))]
      (loop [coll (iterate sum-of-squared-digits num)
             seen #{}]
        (if (= 1 (first coll))
          true
          (if (seen (first coll))
            false
            (recur (rest coll) (conj seen (first coll)))))))))

(defcheck solution-c0742ff4
  (fn [s]
    (loop [s (str s)
           m #{}]
      (let [v  (str (int (reduce #(+ % (Math/pow (- (char->ascii %2) 48) 2)) 0 s)))]
        (cond
          (= v "1") true
          (m (set v)) false
          :else (recur v (conj m (set v))))))))

(defcheck solution-c0bafc58
  (fn happy-num? [x]
    (loop [keys #{} n x]
      (let [m (reduce + (map (fn [x] (#(* % %) ( - (char->ascii x) 48))) (seq (str n))))]
        (if (keys m) (= m 1)
                     (recur (conj keys m) m))))))

(defcheck solution-c119ebbd
  (fn f ([n] (f #{} n))
    ([acc n] (cond (= n 1) true
                   (acc n) false
                   :else (recur (conj acc n)
                           (apply + (map #(let [n (char->num %)] (* n n)) (seq (str n)))))))))

(defcheck solution-c19582e1
  (fn h [num & {:keys [seen] :or {seen #{}}}]
    (let [new (->> (str num)
                (map #(- (char->ascii %) 48))
                (map #(* % %))
                (apply +))]
      (cond (= new 1) true
            (seen new) false
            :else (h new :seen (conj seen new))))))

(defcheck solution-c1b28090
  (fn happy? [n]
    (letfn [(digits0 [x b]
              (loop [x x, d '()]
                (if (zero? x) (cons 0 d)
                              (recur (int (/ x b))
                                (cons (mod x b) d)))))]
      (loop [n n, seen #{}]
        (cond (= n 1)  true
              (seen n) false
              :else    (recur (reduce + (map #(* % %) (digits0 n 10))) (conj seen n)))))))

(defcheck solution-c1c7438c
  (fn [n]
    (let [get-digits
                         (fn [num]
                           (loop [current num results []]
                             (if (= current 0) results
                                               (recur (quot current 10) (conj results (rem current 10))))))
          get-new-number (fn [num] (apply + (map #(* % %) (get-digits num))))]
      (loop [current n trail #{}]
        (cond
          (= current 1) true
          (contains? trail current) false
          :else (recur (get-new-number current) (conj trail current)))))))

(defcheck solution-c1ead9ec
  (fn happy-numbers [n]
    (letfn [(sum-square [x]
              (loop [a x b 0]
                (if (not (zero? a))
                  (recur (quot a 10) (+ b (#(* % %)(rem a 10))))
                  b)))]
      (loop [a n acc #{} n' (sum-square n)]
        (if (= 1 n')
          true
          (if (contains? acc n')
            false
            (recur n' (conj acc n') (sum-square n'))))))))

(defcheck solution-c20bd578
  (letfn [(S [n]
            (loop [n n s 0]
              (if (zero? n) s
                            (recur (quot n 10) (+ s (* (rem n 10) (rem n 10)))))))

          (H? [n]
            (loop [n n seen #{}]
              (cond (= n 1) true
                    (seen n) false
                    :else (recur (S n) (conj seen n)))))]
    H?))

(defcheck solution-c20dc843
  (fn is-happy? [n]
    (letfn [(to-digits [n] (map #(- (char->num %) (char->num \0)) (str n)))
            (sum-of-squares [coll] (reduce + (map #(* % %) coll)))]
      (loop [curr n seen #{}]
        (cond
          (= 1 curr) true
          (seen curr) false
          :else (recur (sum-of-squares (to-digits curr)) (conj seen curr)))))))

(defcheck solution-c21045b9
  (letfn [
          (loc-d-seq [acc d]
            (if (> 10 d)
              (cons d acc)
              (loc-d-seq
                (cons (rem d 10) acc)
                (quot d 10))))
          (d-seq [n]
            (loc-d-seq '() n))
          (d-seq-but0 [n]
            (filter pos? (d-seq n)))
          (sum-sq [xs]
            (apply + (map (fn [a] (* a a)) xs)))
          (loc-happy? [iters d]
            (if (= 1 d) true
                        (if (iters d) false
                                      (loc-happy? (conj iters d) (sum-sq (d-seq-but0 d))))))
          (happy? [a] (loc-happy? #{} a))]
    happy? ))

(defcheck solution-c250ea0
  (fn f [s n]
    (or
     (= n 1)
     (if (s n)
       false
       (f (conj s n) (apply + (map (zipmap "0123456789" [0 1 4 9 16 25 36 49 64 81]) (str n))))))) #{})

(defcheck solution-c2c8ee3a
  (fn [n]
    (loop [x n l []]
      (let [r (apply + (map #(* % %) (map #(str->int (str %)) (str x))))]
        (if (= 1 r) true
                    (if (some #(= % r) l) false
                                          (recur r (conj l r))))))))

(defcheck solution-c2e3873e
  (fn [n]
    (loop [i n coll #{}]
      (cond (= i 1) true
            (contains? coll i) false
            :else (recur (->> (iterate (fn [[x _]] (let [r (mod x 10)] [(/ (- x r) 10) r])) [i 0])
                           (drop 1)
                           (take-while #(or (> (first %) 0) (> (second %) 0)))
                           reverse
                           (map #(second %))
                           (map #(* % %))
                           (apply + )) (conj coll i))))))

(defcheck solution-c4389e7f
  (fn h [n]
    (loop [seen #{}, n n]
      (cond (seen n) false
            (= 1 n) true
            :else (recur
                    (conj seen n)
                    (reduce #(+ %1 (* %2 %2)) 0 (map (comp str->int str) (str n)) )  )))))

(defcheck solution-c47f3a4c
  (fn [d]
    (letfn [(digs [n] (map #(- (char->ascii %) 48) (str n)))
            (sqdig [n] (apply + (map #(* % %) (digs n))))]
      (loop [n d, idx 0]
        (let [s (sqdig n)]
          (cond
            (= 1 s) true
            (= 100 idx) false
            :default (recur s (inc idx))))))))

(defcheck solution-c4aa2b51
  (fn is-happy?
    [n]
    (letfn [(num->digits
              [num]
              (loop [n num res []]
                (if (zero? n)
                  res
                  (recur (long (/ n 10)) (cons (mod n 10) res)))))]
      (loop [s #{} nr n]
        (cond
          (= nr 1) true
          (contains? s nr) false
          :else (let [i (apply + (map #(* % %) (num->digits nr)))]
                  (recur (conj s nr) i)))))))

(defcheck solution-c4b1bb36
  (fn [number]
    (loop [n (str number)
           acc #{}]
      (let [squared-sum (apply + (map #(* (- (char->num %) (char->num \0)) (- (char->num %) (char->num \0))) n))]
        (cond
          (= 1 squared-sum) true
          (acc squared-sum) false
          :else (recur (str squared-sum) (conj acc squared-sum)))))))

(defcheck solution-c4dde5c6
  (fn happy? [number]
    (case number
      1 true
      4 false
      (recur (->> (str number)
               (map #(- (char->ascii %) 48))
               (map #(* % %))
               (reduce +))))))

(defcheck solution-c5b0d82b
  (fn [n]
    (letfn [(cmpt [[& nums]] (reduce + (map * nums nums)))
            (splt [integer] (map #(str->int (str %)) (str integer)))]
      (loop [parm n
             cache []]
        (if (some #{parm} cache)
          (= parm 1)
          (recur (cmpt (splt parm)) (conj cache parm)))))))

(defcheck solution-c64d748c
  (fn [n]
    (= (last
         (take 1000
           (iterate (fn [x]
                      (apply + (map (comp #(* % %) str->int str) (str x))))
             n)))
      1)))

(defcheck solution-c658e317
  (fn happy?[x]
    (let [digits (fn [x] (map (zipmap "0123456789" (range 10)) (str x)))
          sq #(* % %)
          next-x (fn [x] (reduce + (map sq (digits x))))
          happy-seq (fn [x] (iterate next-x x))
          proc (fn [[l xs] x]
                 (cond (= 1 x) [:happy  xs]
                       (xs  x) [:sad    xs]
                       :else   [l (conj xs x)]))]
      (= :happy
        (ffirst (drop-while #(= :? (first %)) (reductions proc [:? #{}] (happy-seq x))))))))

(defcheck solution-c6baca08
  (fn happy-number2 [n]
    (letfn [(digits [n]
              (loop [n n
                     r '()]
                (if (= n 0)
                  r
                  (let [rem (mod n 10)
                        div (quot n 10)]
                    (recur div (conj r rem))))))

            (next-seq [n]
              (let [sq (map #(* % %) (digits n))]
                (apply + sq)))
            ]
      (loop [n n
             seq #{n}]
        (let [n2 (next-seq n)]
          (if (= 1 n2)
            true
            (if (contains? seq n2)
              false
              (recur n2 (conj seq n2)))))))))

(defcheck solution-c6d400ab
  (fn [x] (let [sum (fn [y] (->> (str y)
                              (map #(- (char->ascii %) 48))
                              (map #(Math/pow % 2))
                              (apply + )
                              int))]
            (loop [z (sum x) s #{}]
              (cond (= 1 z) true
                    (s z) false
                    :else (recur (sum z) (conj s z)))))))

(defcheck solution-c7464c82
  (fn [x] (= 1 (nth (iterate #(reduce (fn [y z] (+ y (let [t (- (char->ascii z) 48)] (* t t)))) 0 (str %)) x) 10))))

(defcheck solution-c764227c
  (let [->digits (fn [n]
                   (->> (iterate (fn [[q r]] [(quot q 10) (rem q 10)]) [n])
                     (take-while (fn [vs] (some (complement zero?) vs)))
                     (map second)
                     rest
                     reverse
                     (into [])))
        sumsq-of-digits (fn [n]
                          (->> n
                            ->digits
                            (map (fn [x] (* x x)))
                            (apply +)))
        f (fn [n]
            (loop [x n, seen #{}]
              (if (seen x)
                false
                (let [x' (sumsq-of-digits x)]
                  (if (== 1 x')
                    true
                    (recur x' (conj seen x)))))))]
    f))

(defcheck solution-c7c2d1da
  (fn happynum [n]
    (letfn [(sqdsum [n]
              (reduce (fn [ret this] (+ ret (* this this))) 0 (for [d (str n)] (- (char->ascii d) 48))))]
      (loop [n n seen #{}]
        (if (= 1 n)
          true
          (if (contains? seen n)
            false
            (recur (sqdsum n) (conj seen n))))))))

(defcheck solution-c7c5f9c8
  (fn [n]
    (let [f (fn [n]
              (let [digits (map #(- (char->num %) (char->num \0)) (str n))]
                (reduce + (map #(* % %) digits))))]
      (loop [seen #{}
             xs   (iterate f n)]
        (let [x (first xs)]
          #_(println x)
          (cond
            (= 1 x) true
            (seen x) false
            :else (recur (conj seen x) (rest xs))))))))

(defcheck solution-c8abad1d
  (fn[x]
    (= 1
      (nth
        (iterate
          (fn f[x](apply +
                    (map #(* % %)
                      (loop [r [] v x]
                        (if (zero? v) r
                                      (recur (conj r (mod v 10)) (quot v 10)))))))
          x)
        9))))

(defcheck solution-c8b10946
  (fn [x]
    (let [digits
          (fn [rs n]
            (if (< n 10) (cons n rs)
                         (recur (cons (rem n 10) rs) (quot n 10))))]
      (loop [n x seen #{}]
        (let [sm (apply + (map #(* %1 %1) (digits [] n)))]
          ;(prn n sm)
          (cond
            (= 1 sm) true
            (contains? seen sm) false
            :else (recur sm (conj seen sm)))))
      )
    ))

(defcheck solution-c8d1ad10
  (fn hn? [nn]
    (let [step (fn [n] (->> n
                         str
                         (map #(- (char->num %) (char->num \0)))
                         (map #(* % %))
                         (apply +)))]
      (loop [i nn done #{}]
        (let [i2 (step i)]
          (cond (= 1 i2) true
                (done i2) false
                :else (recur i2 (conj done i2))))))))

(defcheck solution-c8d3520
  (fn [n]
    (contains? ((fn happy-group [x s]
                  (let [new-x (apply +
                                (map #(int (Math/pow (str->int (str %)) 2))
                                  (str x)))]
                    (if (s new-x)
                      s
                      (happy-group new-x (conj s new-x)))))
                n #{n}) 1)))

(defcheck solution-c8f4998f
  (fn is-happy? [n]
    (let [square-sum (int (reduce #(+ %1 (Math/pow (str->int (str %2)) 2)) 0 (str (int n))))]
      (if (= 1 square-sum)
        true
        (if (= 4 square-sum)
          false
          (is-happy? square-sum))))))

(defcheck solution-c9a49409
  (fn [n]
    (loop [n n
           s #{n}]
      (let [v (->> n
                str
                (map #(- (char->ascii %) 48))
                (map #(* % %))
                (apply +))]
        (if (s v)
          false
          (if (= v 1)
            true
            (recur v (conj s v))))))))

(defcheck solution-ca709ee4
  (fn h [n]
    (let [m map
          r (->> n str (m #(- (char->ascii %) 48)) (m #(* % %)) (apply +))]
      (and (not= r 4) (or (= 1 r) (h r))))))

(defcheck solution-ca974a43
  (fn [start] (= true (some #(= 1 %) (take 100 (iterate (fn [x] (reduce + (map #(let [n (- (char->ascii %) 48)] (* n n)) (str x)))) start))))))

(defcheck solution-cb671d43
  (fn [n]
    ((fn happy [xs x]
       (cond
         (= x 1) true
         (xs x) false
         :else (let [x2
                     (reduce + (map #(* % %) (map #(- (char->num %) (char->num \0)) (str x))))]
                 (recur (conj xs x) x2))))
     #{} n)))

(defcheck solution-cba9ecfe
  (fn [x]
    (letfn [(digitsum [x n]
              (if (zero? x)
                n
                (recur (quot x 10) (+ n (#(* % %) (rem x 10))))))]
      (if (< x 10) (val (find {1 true 2 false 3 false 4 false 5 false
                               6 false 7 true 8 false 9 false} x))
                   (recur (digitsum x 0))))))

(defcheck solution-cbdd452f
  (fn [num]
    (let [next-happy-number (fn [num] (->> (str num)
                                        (seq)
                                        (map #(str->int (str %)))
                                        (map #(* % %))
                                        (reduce +)
                                        ))
          number-sequence (take 100 (iterate next-happy-number num))]
      (loop [number-sequence number-sequence]
        (if (empty? number-sequence)
          false
          (if (= (first number-sequence) 1)
            true
            (recur (rest number-sequence))))))))

(defcheck solution-cc220bd0
  (fn happy [n]
    (let [doit (fn [i]
                 (let [#_#__ (println i)
                       predigits (seq (str i))
                       #_#__ (println predigits)
                       digits (map #(- (char->num %) (char->num \0)) predigits)
                       #_#__ (println digits)
                       sqdigits (map #(* % %) digits)
                       #_#__ (println sqdigits)
                       sumsq (reduce + sqdigits)
                       #_#__ (println sumsq)]
                   sumsq))]
      (loop [seen #{}
             i n]
        (cond
          (= i 1) true
          (contains? seen i) false
          :else (recur (conj seen i) (doit i)))))))

(defcheck solution-cc7aa91
  (fn [num]
    (loop [mem #{}, n num]
      (cond (= 1 n) true
            (mem n) false
            :else (recur (conj mem n)
                    (->> (str n)
                      (map (comp #(* % %) #(char->num % 10)))
                      (apply +)))))))

(defcheck solution-ccacaeba
  #(letfn
    [ (sq [c]
        (let [v (- (char->num c) (char->num \0))] (* v v)))]

     (loop [h #{} x %1 ]
       (cond (= 1 x) true
             (h x) false
             :else
             (let [x1 (apply + (map sq (str x)))]
               (recur (conj h x) x1))))))

(defcheck solution-ccc328fa
  (fn [x]
    (let [
          iterate* (fn [f x]
                     (let [impl (fn impl [hist f x]
                                  (cond
                                    (== 1 x) (cons 1 '())
                                    (contains? hist x) (cons x '())
                                    :else (lazy-seq
                                            (cons x (impl (conj hist x) f (f x))))))]
                       (impl #{} f x)))
          calc (fn [x]
                 (let [square (fn [x] (* x x))
                       sum (fn [xs] (reduce + 0 xs))]
                   (->> x
                     (str)
                     (map #(str->int (str %)))
                     (map square)
                     (sum))))
          ]
      (->> x (iterate* calc) (last) (== 1)))))

(defcheck solution-ccc628a9
  (fn [x]
    ((fn h [xs]
       (letfn [
               (ss [x]  (if (zero? x)  0  (let [r (rem x 10) q (quot x 10)]  (+ (* r r) (ss q))     )   ) )
               ]
         (cond
           (= 1 (first xs)) true
           (some #{(first xs)} (rest xs)) false
           :else (h (concat [(ss (first xs))] xs))
           )
         )
       ) [x])
    ))

(defcheck solution-cd5c3d9e
  (fn [h]
    (loop [seen #{} cur h]
      (if (= 1 cur) true
                    (if (contains? seen cur) false
                                             (recur
                                               (conj seen cur)
                                               (reduce + (map #(* % %) (loop [n cur d []]
                                                                         (if (zero? n) d
                                                                                       (recur (quot n 10) (conj d (mod n 10)))))))))))))

(defcheck solution-cd89bd00
  (fn [n]
    (letfn [(digits [n] (map #(str->int (str %)) (str n)))]
      (loop [m n seen #{}]
        (let [sum (apply + (map #(* % %) (digits m)))]
          (cond
            (= sum 1) true
            (seen sum) false
            :else (recur sum (conj seen sum))))))))

(defcheck solution-cdbde45e
  (fn [n]
    (let [sq (fn [k] (* k k))
          div (fn [k] (quot k 10))
          rmd (fn [k] (rem k 10))
          sum-sq-digits (fn [k]
                          (loop [nm k sum 0]
                            (if (zero? nm)
                              sum
                              (recur
                                (div nm)
                                (+ sum (sq (rmd nm)))))))]
      (loop [next-sq-num n found #{}]
        (if (= 1 next-sq-num)
          true
          (if (contains? found next-sq-num)
            false
            (recur (sum-sq-digits next-sq-num) (conj found next-sq-num))))))))

(defcheck solution-ce262c0e
  (fn [n]
    (letfn [(happy [nn]
              (apply + (map (comp #(* % %) str->int str) (str nn))))]
      (cond
        (= 1 (happy n)) true
        (= 4 (happy n)) false
        :else (recur (happy n))))))

(defcheck solution-ce62c9e9
  (fn [x]
    (loop [res #{} n x]
      (if (= n 1)
        true
        (
          let [
               sqr (fn [i] (* i i))
               sqrsum (reduce #(+ %1 (sqr (- (char->ascii %2) 48))) 0 (str n))
               ]
          (if (res sqrsum)
            false
            (recur (conj res sqrsum) sqrsum))
          )
        )
      )
    ))

(defcheck solution-ce65b175
  (fn solve [n]
    (let [digits (fn [s] (map #(- (char->num %) (char->num \0)) (str s)))
          sum-squares (fn [d] (reduce (fn [sum x] (+ sum (* x x))) 0 d))
          check (fn [x seen]
                  (let [ss (sum-squares (digits x))]
                    (cond (= ss 1) true
                          (seen ss) false
                          :else (recur ss (conj seen x)))))]
      (check n #{}))))

(defcheck solution-ce83dddb
  (fn happy?
    ([n]
     (happy? n #{}))
    ([n s]
     (let [n' (reduce + (map (comp #(* % %) str->int str) (seq (str n))))]
       (cond
         (= 1 n') true
         (contains? s n') false
         :else (happy? n' (conj s n')))))))

(defcheck solution-cec2e5de
  (fn [n]
    (loop [n n p #{}]
      (if (p n)
        (= n 1)
        (recur (apply + (map #(* % %)
                          (map #(str->int (str %)) (str n))))
          (conj p n))))))

(defcheck solution-cec9f4c9
  (fn happy ([n] (happy n #{n}))
    ([n seen] (let [next (apply + (map (comp #(* % %) #(str->int %) str) (flatten (partition 1 (str n)))))]
                (if (= next 1)
                  true
                  (if (contains? seen next)
                    false
                    (happy next (conj seen next))))))))

(defcheck solution-ceecc340
  (letfn [(dig [n]
            (if (> 10 n) [n]
                         (let [x (mod n 10)]
                           (conj (dig (-> n (- x) (/ 10))) x))))
          (*+ [x] (->> x dig  (map #(* % %)) (apply +)))]
    (fn [x]
      (loop [acc [] n x]
        (if (= 1 n) true
                    (if (contains? acc n) false
                                          (recur (conj acc n) (*+ n))))))))

(defcheck solution-cef4e34f
  (fn happy? [n]
    (letfn [(k [m] (reduce #(+ (* %2 %2) %) 0 (map #(- (char->ascii %) 48) (str m))))]
      (loop [p n s [n]]
        (let [l (k p)]
          (cond
            (= 1 l) true
            (some #{l} s) false
            :else (recur l (cons l s))))))))

(defcheck solution-cf38ae96
  (fn ishappy? [start]
    (letfn [(num->digits [n]
              (loop [num n
                     digits []]
                (if (zero? num)
                  digits
                  (recur (long (/ num 10)) (cons (mod num 10) digits)))))]
      (loop [num start history []]
        (cond (contains? history num) false
              (= num 1) true
              :else (recur (apply + (map #(* % %) (num->digits num))) (set (conj history num))))
        ))))

(defcheck solution-cf479148
  (fn happy? [x]
    (loop [n x
           seen #{}]
      (let [digits (map #(str->int (str %)) (str n))
            result (reduce (fn [acc d] (+ acc (* d d))) 0 digits)]
        (cond
          (= 1 result) true
          (contains? seen n) false
          :else (recur result (conj seen n)))))))

(defcheck solution-cf4e8fc3
  (fn happy? [x]
    (letfn [(sum-sq-digit [x]
              (->>
                (str x)
                (map #(- (char->num %) (char->num \0)))
                (map #(* % %))
                (apply +)
                str))]
      (loop [x x
             seen #{}]
        (let [sx (sum-sq-digit x)]
          (cond
            (= "1" sx) true
            (seen sx) false
            :else (recur sx (conj seen sx))))))))

(defcheck solution-cfd65102
  (fn a
    ([n] (a #{} n))

    ([res n]
     (cond

       (= 1 n) true

       (contains? res n) false

       :default (recur
                  (conj res n)
                  (reduce +
                    (map #(* % %)
                      (map #(str->int (str %)) (str n))))
                  )
       )
     )
    ))

(defcheck solution-cfe69140
  (fn [n]
    (loop [x n
           v #{}]
      (let [s (apply + (map #(* % %) (map #(- (char->ascii %) 48) (str x))))]
        (cond
          (= 1 s) true
          (v s)   false
          1       (recur s (conj v s)))))))

(defcheck solution-cfee63f1
  #(loop [c %1 seen? #{}]
     (cond
       (= c 1) true
       (seen? c) false
       :else (recur
               (apply + (map (fn [a] (* a a))
                          (loop [result [] x c]
                            (if (zero? x)
                              result
                              (recur (conj result (mod x 10))
                                (int (/ x 10)))))))
               (conj seen? c)))))

(defcheck solution-d13249bb
  (fn f [s n]
    (if (s n)
      (= n 1)
      (f (conj s n) (apply + (map #(* % %) (map #(- (char->ascii %) 48) (str n))))))) #{})

(defcheck solution-d157f379
  (fn [n]
    (letfn [(sum-square-parts [n] (->> (str n)
                                    (map (comp #(* % %) str->int str))
                                    (apply +)))]
      (loop [n n hist #{}]
        (let [s (sum-square-parts n)]
          (cond
            (hist n) false
            (= 1 s) true
            :else (recur s (conj hist n))))))))

(defcheck solution-d1df46bd
  (fn [s n]
    (cond
      (= n 1) true
      (s n) false
      :else (recur (conj s n)
              (apply + (map (comp #(* % %) str->int str) (str n)))))) #{})

(defcheck solution-d2710de1
  (fn [n]
    (letfn [(step [n]
              (reduce + (map #(* % %) (digits n))))
            (digits [n]
              (loop [n n
                     acc ()]
                (if (< n 10)
                  (cons n acc)
                  (recur (quot n 10) (cons (rem n 10) acc)))))]
      (loop [n n
             steps #{}]
        (if (= n 1)
          true
          (if (contains? steps n)
            false
            (recur (step n) (conj steps n))))))))

(defcheck solution-d29e883a
  (fn happy? [x]
    (loop [current x repeats #{}]
      (let [digits (map #(str->int (str %)) (str current))
            sum-of-squares (reduce + (map #(* % %) digits))]
        (cond (= 1 sum-of-squares) true
              (contains? repeats sum-of-squares) false
              :else (recur sum-of-squares (conj repeats sum-of-squares)))))))

(defcheck solution-d2fc115c
  (fn [i]
    ((fn veri [n r]
       (let [nr (apply + (map #(* % %) (map #(str->int (str %)) (str r))))]
         (if (= 1 nr)
           true
           (if (= n 200)
             false
             (veri (inc n) nr))))) 1 i )))

(defcheck solution-d3318f2d
  (fn [n]
    (letfn [(square [x] (* x x))
            (digits [x]
              (lazy-seq
                (when (pos? x)
                  (cons (mod x 10) (digits (quot x 10))))))]
      (loop [x n, visited #{}]
        (cond
          (= 1 x) true
          (visited x) false
          :else (recur (apply + (map square (digits x)))
                  (conj visited x)))))))

(defcheck solution-d362ce29
  (fn [x]
    (letfn [(next-happy [y]
              (->> y
                (str)
                (seq)
                (map str)
                (map #(str->int %))
                (map #(* % %))
                (reduce +)))]
      (loop [x x s #{}]
        #_(prn x s)
        (if (= x 1)
          true
          (let [new-x (next-happy x)]
            #_(prn new-x)
            (if (contains? s new-x)
              false
              (recur new-x (conj s new-x)))))))))

(defcheck solution-d396622f
  (fn [n]
    (letfn [(digits [x]
              (loop [n x
                     ds '()]
                (if (zero? n)
                  ds
                  (let [d (mod n 10)
                        r (quot n 10)]
                    (recur r (cons d ds))))))

            (digit-square-sum [x]
              (->> (digits x)
                (map #(* % %))
                (reduce +)))]
      (loop [x n
             seen #{}]
        (cond (seen x) false
              (= 1 x) true
              :else (recur (digit-square-sum x)
                      (conj seen x)))))))

(defcheck solution-d3a56d02
  (fn [n]
    (let [next-step (fn [n]
                      (let [digits (map #(str->int (str %)) (str n))]
                        (apply + (map #(* % %) digits))))]
      (loop [i n]
        (case i
          1 true
          4 false
          (recur (next-step i)))))))

(defcheck solution-d4272bd9
  (fn [n]
    (letfn [
            (digs[num]
              (loop [num num res []]
                (if (< 0 num)
                  (recur (int (/ num 10)) (cons (mod num 10) res))
                  res)))
            (sumsq[n]
              (reduce + (map #(* % %) (digs n))))]
      (loop [x n seen #{}]
        (if (= 1 x)
          true
          (let [v (sumsq x)]
            (if (get seen v)
              false
              (recur v (conj seen x)))))))))

(defcheck solution-d4d9f6a
  (fn happy?
    ([n]
     (happy? n #{}))
    ([n tried]
     (cond
       (= 1 n) true
       (contains? tried n) false
       :else (let
              [x (reduce
                   +
                   (map
                     #(* % %)
                     ((fn bd [a] (if (<= a 0) '() (cons (rem a 10) (bd (quot a 10))))) n)))]
               (happy? x (conj tried n)))))))

(defcheck solution-d4f06b8e
  (fn [x] (letfn [(digits [n] (map #(- (char->ascii %) 48) (str n)))
                  (sos [s] (reduce + (map #(* % %) s)))]
            (-> (iterate (comp sos digits) x)
              (nth 20)
              (= 1)))))

(defcheck solution-d53907aa
  (fn [n]
    (= 1 (some #{1 4}
           (iterate
             (fn [x]
               (apply + (map #((fn [x] (* x x)) (- (char->ascii %) 48)) (str x))))
             n)))))

(defcheck solution-d5c8857d
  (fn [n]
    (letfn [(splitnumber [i]
              (map #(- (char->ascii %) 48) (str i)))
            (proc [i]
              (reduce +
                (map
                  #(* % %)
                  (splitnumber i))
                ))]
      (loop [x n r #{}]
        (let [xx (proc x)]
          (cond
            (= xx 1) true
            (r xx) false
            :else (recur xx (conj r xx))))
        ))
    ))

(defcheck solution-d5c99119
  (fn happy?
    ([n] (happy? n #{}))
    ([n s] (if (= 1 n) true (if (s n) false (recur ((fn [m] (apply + (map #(* % %) (map #(- (char->num %) (char->num \0)) (str m))))) n) (conj s n)))))))

(defcheck solution-d6773d12
  (fn [x]
    (loop [n x
           a #{}]
      (cond
        (= n 1) true
        (contains? a n) false
        1 (recur
            (->>
              (map #(- (char->num %) (char->num \0)) (str n))
              (map #(* % %))
              (apply +))
            (conj a n))))))

(defcheck solution-d74e1843
  (let [iter (fn i [n]
               (if (< n 10)
                 (* n n)
                 (+ (i (mod n 10)) (i (quot n 10)))))
        iters (fn [n] (iterate iter n))
        some-cycle (fn [s]
                     (some #(when (= (first %) (last %)) (first %))
                       (map vector (drop 1 s) (mapcat (partial repeat 2) s))))
        ]
    (comp (partial = 1) some-cycle iters)))

(defcheck solution-d777f0ba
  (fn [orig]
    (let [ss (fn [n] (->> n str seq (map str) (map #(str->int %)) (map #(* % %)) (reduce +)))]
      (loop [n 0 nums (iterate ss orig) hist #{}]
        (cond
          (= n 1) true
          (contains? hist n) false
          :else (recur (first nums) (drop 1 nums) (conj hist n))))
      )))

(defcheck solution-d77a9a28
  (fn happy?
    [n]
    (letfn [(digits [n]
              (lazy-seq
                (if (< n 10)
                  (list n)
                  (cons (rem n 10) (digits (quot n 10))))))
            (sum-squared-digits [n]
              (apply + (map #(* % %) (digits n))))]
      (loop [i n
             seen #{}]
        (cond
          (= i 1) true
          (seen i) false
          :else (recur (sum-squared-digits i) (conj seen i)))))))

(defcheck solution-d7a5017
  (fn [n]
    (let [f (fn [n]  (reduce #(+ % (* (- (char->ascii %2) 48) (- (char->ascii %2) 48))) 0 (str n)))
          h (fn h [n s]
              (let [a (= 1 n) b (s n)]
                (if a
                  a
                  (if b
                    (not b)
                    (h (f n) (conj s n))))))]
      (h n #{}))))

(defcheck solution-d7adbec7
  (fn [n]
    (loop [t n a {}]
      (cond
        (= 1 t) true
        (contains? a t) false
        true
        (let [new (apply + (map #(* % %) (map #(- (char->num %) (char->num \0)) (str t))))]
          (recur new (assoc a t 1)))))))

(defcheck solution-d7c95dd6
  (fn [n]
    (loop [n n, seen #{}]
      (let [digits
                (fn [i]
                  (map
                    #(- (char->ascii %) 48)
                    (str i)))
            sos
                (fn [ds]
                  (reduce + (map #(* % %) ds)))
            sum (sos (digits n))]
        (or (= sum 1)
            (if (seen sum)
              false
              (recur sum (conj seen sum))))))))

(defcheck solution-d7ff0799
  (fn [n]
    (= 1 ((fn [s n]
            (let [i (fn [n] (reduce #(+ % (* %2 %2)) 0 (map #(- (char->num %) (char->num \0)) (str n))))]
              (if (contains? s n)
                n
                (recur (conj s n) (i n))))) #{} n))))

(defcheck solution-d81109b3
  (fn [n]
    (let [trans (fn [x]
                  (apply + (map #(* % %) (map (comp #(str->int %) str) (seq (str x))))))
          f (fn [xs] (conj xs (trans (last xs))))]
      (= 1 (last (last (take-while #(= % (distinct %)) (iterate f [n]))))))))

(defcheck solution-d83322f7
  (fn [n]
    (letfn [(happy-seq [n]
              (let [n' (map #(char->num %) (str n))
                    n'' (map #(* % %) n')
                    s (reduce + n'')]
                (lazy-seq
                  (cons n (happy-seq s)))))
            (is-happy-number? [ns hs]
              (if (= 1 (first hs))
                true
                (if (contains? ns (first hs))
                  false
                  (recur (conj ns (first hs)) (rest hs)))))]
      (is-happy-number? #{} (happy-seq n)))))

(defcheck solution-d85c2d77
  (fn happy?
    [x]
    (let [get-digits (fn [n]
                       (map #(- (char->ascii %) 48) (str n)))
          sum-squares (fn [n]
                        (reduce + (map #(* % %) n)))]

      (loop [x x
             tried #{}]
        (cond
          (= x 1) true
          (some #{x} tried) false
          :else (recur (sum-squares (get-digits x)) (conj tried x)))))))

(defcheck solution-d992e0fc
  (fn f
    ([n] (f n #{}))
    ([i r]
     (let [e (apply + (map #(* % %) (map #(str->int %)
                                      (re-seq #"\d" (str i)))))]
       (if (= e 1) true (if (r e) false (f e (conj r e))))))))

(defcheck solution-d9d3a5ee
  (partial (fn happy? [seen x] (cond
                                 (= 1 x) true
                                 (seen x) false
                                 :else (let [y (apply + (map (comp #(* % %) str->int) (re-seq #"\d{1}" (str x))))] (recur (conj seen x) y))
                                 )) #{}))

(defcheck solution-d9de94e9
  (fn happy-number? [n]
    (let [square (fn [x] (let [y (str->int (.toString x))] (* y y)))
          happy  (fn [x] (->> x (str) (seq) (map square) (apply +)))]
      (loop [seen #{}, number (happy n)]
        (if (and (not (seen number)) (> number 1))
          (recur (conj seen number) (happy number))
          (= 1 number))))))

(defcheck solution-da2d369f
  (fn f
    ([n] (f [] n))
    ([res n] (if (some #(= n %) res)
               false
               (if (= n 1)
                 true
                 (let [se (map (comp str->int str) (seq (str n)))
                       m (reduce #(+ % (* %2 %2)) 0 se)]
                   (f (conj res n) m)))))))

(defcheck solution-da655c5
  (fn [m]
    (loop [n m rec []]
      (if (= 1 n)
        true
        (if (some #(= % n) rec)
          false
          (recur (->>
                   (str n)
                   (re-seq #"\d")
                   (map #(str->int (str %)))
                   (map #(* % %))
                   (reduce +)) (cons n rec)))))))

(defcheck solution-dac716f8
  (fn [n]
    (let [sqrt (fn [x] (* x x))
          sq-sum (fn [sum x]
                   (if-not (> x 0)
                     sum
                     (recur (+ sum (sqrt (mod x 10)))
                       (int (/ x 10)))))
          loop-x (fn [s x]
                   (if (s x)
                     x
                     (recur (conj s x) (sq-sum 0 x))))]
      (= 1 (loop-x #{} n)))))

(defcheck solution-daea9ce7
  (fn happy?
    ([n] (happy? n #{}))
    ([n seen]
     (letfn [(digits [n]
               (when (pos? n)
                 (cons (rem n 10)
                   (digits (quot n 10)))))
             (dig-sq-sum [n]
               (reduce + (map (fn [d] (* d d))
                           (digits n))))]
       (cond
         (= 1 n) true
         (seen n) false
         :else (happy? (dig-sq-sum n) (conj seen n)))))))

(defcheck solution-daf14c75
  (fn happy-number? [n]
    (letfn [(get-digit [n]
              (if (pos? n)
                (cons (rem n 10) (get-digit (quot n 10)))
                ()))
            (square-digit [n]
              (apply + (map #(* % %) (get-digit n))))
            (number-seq [n]
              (lazy-seq
                (cons n (number-seq (square-digit n)))))
            (happy? [coll index]
              (let [z (nth coll index)]
                (cond (= 1 z) true
                      (some #{z} (take index coll)) false
                      :else (happy? coll (inc index)))))]
      (happy? (number-seq n) 0))))

(defcheck solution-dc89c146
  (fn h
    [num]
    (letfn [(get-digits
              ([num result]
               (if (> num 0)
                 (conj (get-digits (int (/ num 10)) result) (mod num 10))
                 result))
              ([num]
               (get-digits num [])))
            (sum-of-squares
              [coll]
              (reduce #(+ %1 (* %2 %2)) 0 coll))]
      (= 1 (last (take 10 (iterate (comp sum-of-squares get-digits) num)))))))

(defcheck solution-dca75031
  (fn happy?
    [n]
    (let [crunch (fn [s] (str (apply + (map #(* % %) (map #(str->int (str %)) s)))))
          happy-helper (fn happy-helper
                         [s already-seen-set]
                         (if (= s "1")
                           true
                           (if (contains? already-seen-set s)
                             false
                             (happy-helper (crunch s) (conj already-seen-set s)))))]
      (happy-helper (str n) #{}))))

(defcheck solution-ddfc4554
  (fn [x]
    (letfn [(square [x] (* x x))
            (char-op [c] (-> c char->ascii (- 48) square))
            (iter [n] (->> n str (map char-op) (apply +)))]
      (not= nil (some #{1} (take 123 (iterate iter x)))))))

(defcheck solution-ddfdbe46
  (fn ishappy? [n]
    (letfn [(split-num [n]  (seq (map #(str->int (str %)) (seq (str n)))))
            (square [n] (* n n))
            (happy [n]  (apply + (map square  (split-num n))))]
      (cond
        (= n 1) true
        (= n 4) false
        :otherwise (ishappy? (happy n))
        )
      )))

(defcheck solution-de08671c
  (fn [num]
    (let [_sq_sum  (fn [n] (reduce + (map #(* % %) (for [sq (str n)](str->int (str sq))))))]
      (loop [history [] n num]
        (if (= 1 n) true
                    (if (contains? history n) false
                                              (recur (conj history (_sq_sum n)) (_sq_sum n))
                                              )
                    )
        ))
    ))

(defcheck solution-de40e2f6
  (fn [n]
    (loop [n n, seen #{}]
      (let [hap (reduce #(+ % (* %2 %2)) 0 (map #(- (char->ascii %) 48) (str n)))]
        (cond
          (= 1 hap) true
          (seen hap) false
          :else (recur hap (conj seen hap)))))))

(defcheck solution-de926cc5
  (fn [s n]
    (or (= n 1)
        (if (s n)
          false
          (recur (conj s n)
            (apply + (map (comp #(* % %) #(char->num % 10))
                       (str n))))))) #{})

(defcheck solution-dea426f6
  (fn [n]
    (letfn [(dsum [n] (reduce + (map #(* % %) (map #(- (char->ascii %) 48) (str n)))))]
      (loop [n n d #{}]
        (cond
          (= n 1) true
          (contains? d n) false
          :else (recur (dsum n) (conj d n)))))))

(defcheck solution-deb9fd7f
  (fn happy?
    ([n] (happy? n #{}))
    ([n cache]
     (let [happy-sum (fn happy-sum [n]
                       (if (= n 0) 0
                                   (+ (* (rem n 10) (rem n 10)) (happy-sum (quot n 10)))))]
       (if (= n 1)
         true
         (if (cache n) false
                       (happy? (happy-sum n) (conj cache n))))))))

(defcheck solution-def9a3dc
  (fn happy-number?
    ([x] (happy-number? x []))
    ([x init]
     (letfn [(pow-of-two [x] (reduce * [x x]))
             (to-int [c] (- (char->num c) (char->num \0)))
             (pow-and-sum
               [digits]
               (reduce +
                 (map (comp pow-of-two to-int) digits)))]
       (let [curr (pow-and-sum (str x))]
         (if-let [happy (= 1 curr)]
           happy
           (if (and ((comp not nil?) (last init)) (> curr (pow-of-two (last init))))
             false
             (recur (pow-and-sum (str curr)) (conj init curr)))))))))

(defcheck solution-df2d6b87
  (fn [n]
    (letfn [(step [n]
              (->> (str n)
                (map (comp #(* % %) str->int str))
                (apply +)))]
      (loop [seen #{}
             num n]
        (cond
          (= num 1) true
          (contains? seen num) false
          :else (recur (conj seen num) (step num)))))))

(defcheck solution-df6e4acc
  (fn [n]
    (let [f (fn f [n]
              (let [next (reduce (fn [s i] (+ s (* i i))) 0
                           (map #(str->int (str %)) (str n)))]
                (lazy-seq (cons next (f next)))))
          rf (fn rf
               ([sq] (rf sq '()))
               ([[fst & rs] acc]
                (cond
                  (= 1 fst) true
                  (seq (filter (partial = fst) acc)) false
                  :default (rf rs (cons fst acc)))))]
      (rf (f n)))))

(defcheck solution-dfa2ca3c
  (fn [n]
    (letfn [(next-num [x] (apply + (map #(* % %) (map #(- (char->ascii %) 48) (str x)))))]
      (loop [ seen #{ n } cur n ]
        (let [nn (next-num cur)]
          (if (= nn 1)
            true
            (if (contains? seen nn)
              false
              (recur (conj seen nn) nn))))))))

(defcheck solution-e094cdc7
  (fn hn? [x]
    (if (not= nil (some #(= 1 %)
                    (take 10 (iterate
                               (fn s [n]
                                 (reduce #(+ %1 (* %2 %2)) 0
                                   (map #(str->int %) (re-seq #"\d" (str n)))
                                   )
                                 )
                               x
                               ))
                    )) true false)
    ))

(defcheck solution-e0ceeaa3
  (fn [num]
    (let
     [ sqr #(* % %)
      sum-squares (fn sum-squares [num]
                    (if (< num 10)
                      (sqr num)
                      (+ (sqr (mod num 10))
                         (sum-squares (quot num 10)))))]
      (loop [num num num-tries 100]
        (cond
          (= num 1) true
          (zero? num-tries) false
          :else (recur (sum-squares num) (dec num-tries)))))))

(defcheck solution-e1267ca9
  (fn top ([n acc]
           (let [digits (map #(char->num % 10) (str n))
                 sumsquares (reduce + (map (fn[x] (* x x)) digits))
                 happy? (= 1 sumsquares)
                 nothappy? (contains? acc sumsquares)]
             (cond (true? happy?) true
                   (true? nothappy?) false
                   :else (top sumsquares (conj acc sumsquares)))))
    ([n] (top n #{}))))

(defcheck solution-e1d719e1
  (fn happy? [n]
    (loop [n n path #{}]
      (if (= 1 n)
        true
        (let [sum
              (reduce
                +
                (map
                  #(let [x (str->int (str %))] (* x x))
                  (str n)))]
          (if (path sum) false (recur sum (conj path sum))))))))

(defcheck solution-e20aa968
  (fn
    f
    ([number] (f number 0))
    ([number counter]
     (let
      [x
       (reduce
         +
         (map
           (comp
            (fn* [p1__2852#] (* p1__2852# p1__2852#))
            (fn* [p1__2853#] (str->int p1__2853#))
            str)
           (seq (str number))))]
       (if (= x 1) true (if (= counter 10000) false (recur x (inc counter))))))))

(defcheck solution-e23b9064
  (fn happy [y]
    (letfn [(gs [y] (->> y
                      (str)
                      (into [])
                      (map #(- (char->ascii %) 48))
                      (reduce #(+ (* %2 %2) %1) 0) ))
            ]
      (loop [sum y seen #{y}]
        (let [r (gs sum)]
          #_(print r)
          (cond
            (seen r) false
            (= r 1) true
            :else (recur r (conj seen r))))))))

(defcheck solution-e2aa8876
  (fn happy?
    ([n]
     (happy? n #{}))
    ([n s]
     (let [d (map #(str->int (str %)) (str n))
           it (apply + (map #(* % %) d))]
       (cond
         (= 1 it) true
         (s it) false
         :else (happy? it (into s #{it})))))))

(defcheck solution-e2d98f9d
  (fn happy [n]
    (letfn [(digits [n]
              (loop [c n res ()]
                (if (< c 10)
                  (conj res c)
                  (recur (quot c 10) (conj res (rem c 10))))))]
      (loop [c n seen #{n}]
        (let [next-step (reduce + (map #(* % %) (digits c)))]
          (cond
            (= next-step 1) true
            (seen next-step) false
            :else (recur next-step (conj seen next-step))))))))

(defcheck solution-e3273f2a
  (fn [n]
    (loop [x n, s #{}]
      (cond
        (= x 1) true
        (get s x) false
        :else (recur (apply + (map #(let [i (str->int (str %))] (* i i)) (str x ))) (conj s x))))))

(defcheck solution-e3ff494d
  (letfn [( digit [n]
            (apply + (map #(* % % )(map #(- (char->num %) (char->num \0))  (seq (str n )))))
            )]
    (fn [x]
      (loop [current x results #{}]
        (if (= current 1)
          true
          (let [y (digit current)]
            (if (contains? results y)
              false
              (recur y (conj results y)))))))))

(defcheck solution-e407c068
  (fn myHappyNumbers
    [num]
    (loop [numbers #{} last num]
      (cond
        (contains? numbers last) false
        (= last 1) true
        :else (recur (conj numbers last)
                (->> last
                  (str)
                  (map #(char->num % 10))
                  (map #(* % %))
                  (reduce +)))))))

(defcheck solution-e42edb9f
  (fn hn [n]
    (letfn
     [(digits [n]
        (if (< n 10) (list n)
                     (cons (rem n 10) (digits (quot n 10)))))

      (sum-of-digits [n]
        (reduce + (map #(* %1 %1) (digits n))))]
      (loop [i (sum-of-digits n), sums #{}]
        (cond (sums i) false
              (= i 1) true
              :else (recur (sum-of-digits i) (conj sums i)))))))

(defcheck solution-e4555641
  (fn [n]
    (letfn [(digits [k] (->> (iterate #(quot % 10) k) (take-while #(> % 0)) (map #(mod % 10))))
            (squared-sum [s] (->> (map #(* % %) s) (reduce +)))]
      (loop [k n visited-numbers #{}]
        (cond (= k 1) true (visited-numbers k) false
              :else (recur (->> (digits k) squared-sum) (conj visited-numbers k)))))))

(defcheck solution-e4b3fb6c
  (fn happy
    ([n] (happy n #{}))
    ([n seen]
     (let [digits (map #(char->num %) (str n))
           square #(* % %)
           new-num (apply + (map square digits))]
       (cond
         (= new-num 1) true
         (get seen new-num) false
         :else (recur new-num (conj seen new-num)))))))

(defcheck solution-e4daae2a
  (fn [n]
    (letfn [(f [n] (reduce + (map (comp #(* % %) str->int str) (str n))))
            (h? [n seen]
              (if (= n 1)
                true
                (if (seen n)
                  false
                  (recur (f n) (conj seen n)))))]
      (h? n #{}))))

(defcheck solution-e523ad02
  (fn happy [n]
    (let [digits-fn (fn [n] (->> n str seq (map #(str->int (str %)))))
          sum-sq-digits-fn (fn [n] (reduce + (map #(* % %) (digits-fn n))))]
      (loop [seen-nums #{}
             x n]
        (if (contains? seen-nums x)
          false
          (if (= 1 x)
            true
            (recur (conj seen-nums x) (sum-sq-digits-fn x))))))))

(defcheck solution-e5563bee
  (fn happy? [n]
    (loop [c n, seen #{}]
      (or
       (= c 1)
       (and
        (not (seen c))
        (let [digits ((fn ds [x] (if (zero? x) () (cons (mod x 10) (ds (quot x 10))))) c)
              d-sum-sq (reduce #(+ %1 (* %2 %2)) 0 digits)]
          (recur d-sum-sq (conj seen c))))))))

(defcheck solution-e557079e
  (fn [n]
    (letfn [(calc [n]
              (apply + (map #(let [a (char->num %)] (* a a)) (.toString n))))]
      (loop [visited #{n} curr (calc n)]
        (cond (= 1 curr) true
              (contains? visited curr) false
              :else (recur
                      (conj visited curr)
                      (calc curr)))))))

(defcheck solution-e57b7cbf
  (fn happy? [i]
    (let [int->digits (fn [i] (map #(- (char->ascii %) 48 ) (str i)))
          sum (fn [is] (apply + is))
          square (fn [i] (* i i))
          summed-squares (fn [i]
                           (->>
                             i
                             int->digits
                             (map #(square %))
                             sum))]
      (loop [i i
             previous #{i}]
        (let [next (summed-squares i)]
          (cond
            (= 1 next) true
            (contains? previous next) false
            :else (recur next (conj previous next))))))))

(defcheck solution-e5aaf11f
  (fn happy?
    ([x] (happy? x #{}))
    ([x prev]
     (let [xs   (map #(- (char->num %) (char->num \0)) (seq (str x)))
           sqrs (map #(* % %) xs)
           sum  (reduce + sqrs)]
       (or (= 1 sum)
           (and (nil? (prev sum))
                (recur sum (conj prev sum))))))))

(defcheck solution-e5bf2b35
  (fn happy-number?
    ([x] (happy-number? x 0 100))
    ([x current max]
     (letfn [(digits [x]
               (let [[q r] [(quot x 10) (rem x 10)]]
                 (if (zero? q) [r] (conj (digits q) r))))
             (square [x] (* x x))]
       (let [sum-of-squares (reduce + (map square (digits x)))]
         (cond
           (= 1 sum-of-squares) true
           (> current max) false
           :else (happy-number? sum-of-squares (inc current) max)))))))

(defcheck solution-e5d27a9
  (fn [n]
    (letfn [(square [x] (* x x))
            (digits [x]
              (->> x str (map #(-> % str str->int))))
            (sum-of-squares [x]
              (->> x digits (map square) (reduce +)))]
      (loop [n    n
             seen #{}]
        (cond (= n 1)  true
              (seen n) false
              :else    (recur (sum-of-squares n)
                         (conj seen n)))))))

(defcheck solution-e5e9e81c
  (fn [s n]
    (let [m (reduce + (map #(* % %)
                        (map (comp str->int str)
                          (str n))))]
      (cond (s m) false
            (= m 1) true
            true (recur (conj s n) m)))) #{})

(defcheck solution-e5f4fa95
  (fn
    [num]
    (letfn [(sum-squares-of-digits
              [x]
              (apply + (map #(* (- (char->num %) (char->num \0))
                               (- (char->num %) (char->num \0))) (str x))))
            (is-happy?
              [st x]
              (if (st x)
                false
                (let [y (sum-squares-of-digits x)]
                  (if (= 1 y)
                    true (is-happy? (conj st x) y)))))]
      (is-happy? #{} num))))

(defcheck solution-e63d7605
  (fn [n]

    (loop [ n n
           lst [] ]

      (let [x (reduce #(+ % (Math/pow (- (char->ascii %2) 48) 2)) 0 (str (int n)))]
        (cond
          (= x 1.0) true
          (some #(= % x) lst) false
          :else (recur x (conj lst x)))))))

(defcheck solution-e79374cf
  (fn [x]
    (cond
      (= 1 x) true
      ; All non-happy numbers follow sequences that reach the cycle:
      ;   4, 16, 37, 58, 89, 145, 42, 20, 4, ...
      (= 4 x) false
      :else (letfn [(sq [y]
                      (if (< y 10)
                        (* y y)
                        (+ (* (rem y 10) (rem y 10)) (sq (quot y 10)))))]
              (recur (sq x)))
      )
    ))

(defcheck solution-e8268c27
  (fn [n]
    (let [max-cnt 100000
          square-sum (fn square-sum [n]
                       (->> (map #(- (char->ascii %) 48) (seq (str n)))
                         (map #(* % %))
                         (apply +)))
          is-happy (fn [n cnt]
                     (if (= cnt max-cnt)
                       false
                       (let [sqr-sum (square-sum n)]
                         (if (= n sqr-sum)
                           true
                           (recur sqr-sum (inc cnt))))))]
      (is-happy n 1))))

(defcheck solution-e830bad3
  (fn [n]
    (letfn [(dig [n]
              (loop [d '() n n]
                (if (zero? n) d
                              (recur (cons (rem n 10) d) (quot n 10)))))
            (f [n]
              (apply + (map (fn [x] (* x x)) (dig n))))]
      (loop [n n v #{}]
        (cond
          (= n 1) true
          (v n) false
          :else (recur (f n) (conj v n)))))))

(defcheck solution-e834738b
  (fn f
    ([n] (f n #{}))
    ([n s]
     (let [r (apply + (map (comp #(* % %) str->int str) (str n)))]
       (cond
         (= r 1) true
         (s r) false
         1 (f r (conj s r)))))))

(defcheck solution-e85295de
  (fn [number]
    (letfn [(digits [n] (map #(char->num % 10) (str n)))
            (square [n] (* n n))]
      (loop [n number
             seen #{}]
        (if (seen n) false
                     (let [newNum (->> n (digits) (map square) (apply +))]
                       (if (= 1 newNum) true
                                        (recur newNum (conj seen n) )

                                        )
                       )
                     )
        )
      )
    ))

(defcheck solution-e893d42e
  (fn happy-number [x]
    (cond
      (= 1 x) true
      (= 4 x) false
      :else (happy-number (reduce (fn[a b](+ a ((comp #(* % %)  #(- % 48) char->ascii) b) ))  0 (str x))))))

(defcheck solution-e9ab2d11
  (fn happy
    [n & visited]
    (let [digits (fn [n]
                   (loop [digits [] pool n]
                     (if (= pool 0)
                       digits
                       (recur (conj digits (rem pool 10)) (quot pool 10)))))
          next-iter (fn [n] (apply + (map #(* % %) (digits n))))
          visited (or visited #{})
          n' (next-iter n)]
      (cond
        (= n' 1) true
        (get visited n') false
        :default (recur n' (conj visited n'))))))

(defcheck solution-ea7d55bd
  (fn happy? [n]
    (letfn [(sqrsum [n]
              (reduce
                #(let [d (- (char->ascii %2) 48)]
                   (+ % (* d d)))
                0
                (seq (str n))))]
      (loop [n n acc #{}]
        (if (= n 1)
          true
          (if (nil? (acc n))
            (recur (sqrsum n) (conj acc n))
            false))))))

(defcheck solution-eaa82109
  (fn [coll n]
    (let [next-n
          (reduce + (map #(* % %) (map #(str->int (str %)) (str n))))]
      (cond
        (= 1 next-n) true
        (contains? coll next-n) false
        :else (recur (conj coll next-n)  next-n)))) #{})

(defcheck solution-eaab1e6c
  (fn [n]
    (loop [prev #{} [f & remaining] (iterate (fn [x] (reduce #(+ % (* %2 %2)) 0 (map #(str->int (str %)) (str x)))) n)]
      (if (= 1 f)
        true
        (if (prev f)
          false
          (recur (conj prev f) remaining))))))

(defcheck solution-ebd160e5
  (fn [x]
    (letfn [(ds [z]
              (loop [out [] in z]
                (if (= in 0)
                  out
                  (recur (conj out (rem in 10)) (quot in 10)))))
            (ssd [y]
              (reduce #(+ (* %2 %2) %1) 0 (ds y)))]
      (loop [out #{} in x]
        (cond
          (= in 1) true
          (contains? out in) false
          :else (recur (conj out in) (ssd in)))))))

(defcheck solution-ec8710d4
  (fn f
    ([n] (f n #{}))
    ([n c]
     (cond (= 1 n) true
           (c n) false
           :else
           (f (reduce #(+ % (* %2 %2)) 0
                (map (comp str->int str)
                  (str n)))
             (conj c n))))))

(defcheck solution-eca394c9
  (letfn [(formula [x]
            (->> (str x)
              (map #(- (char->num %) (char->num \0)))
              (map #(* % %))
              (apply +)))]
    (fn happy?
      ([n] (happy? n (formula n) #{}))
      ([n x seen]
       (cond
         (seen x) false
         (= x 1) true
         :else (recur n (formula x) (conj seen x)))))))

(defcheck solution-ece09ae7
  (fn [n]
    (loop [n n seen #{}]
      (cond (= n 1) true
            (seen n) false
            :else (recur (->> (str n)
                           (map str)
                           (map #(str->int %))
                           (map #(* % %))
                           (apply +))
                    (conj seen n))))))

(defcheck solution-ed0be2d8
  (fn [n] (let [break (fn break [n] (if (> n 0)
                                      (cons (mod n 10) (lazy-seq (break (quot n 10))))
                                      nil))
                step (fn [n] (apply + (map #(* % %) (break n))))]
            (loop [n n seen #{}]
              (if (= n 1)
                true
                (if (contains? seen n)
                  false
                  (recur (step n) (conj seen n))))))))

(defcheck solution-ed96ea0d
  (fn [n]
    (letfn [(split-digit [x]
              (loop [res-seq []
                     left x]
                (if (< left 10)
                  (conj res-seq left)
                  (recur (conj res-seq (mod left 10))
                    (int (/ left 10))))))]
      (loop [cache #{}
             x n]
        (cond
          (= x 1) true
          (contains? cache x) false
          :else (recur (conj cache x)
                  (reduce #(+ %1 (* %2 %2))
                    0
                    (split-digit x))))))))

(defcheck solution-edaec555
  (fn happy? [n]
    (loop [n n, seen #{}]
      (let [happy-sum (reduce + 0 (map #(* (- (char->num %) (char->num \0))
                                          (- (char->num %) (char->num \0))) (seq (str n))))]
        (cond
          (= happy-sum 1) true
          (seen happy-sum) false
          :else (recur happy-sum, (conj seen n)))))))

(defcheck solution-edc0911e
  (fn [n]
    (letfn [(get-digits [n]
              (->> (str n)
                (map str)
                (map str->int)))]
      (loop [past [n]]
        (let [new (->> (get-digits (last past))
                    (map #(* % %))
                    (apply +))]
          (cond
            (= new 1) true
            ((set past) new) false
            :else (recur (conj past new))))))))

(defcheck solution-edfd5386
  (fn is-happy? [x]
    (loop [n x previous #{x}]
      (let [sum (->> (str n)
                  (map #(let [digit (char->num % 10)]
                          (* digit digit)),)
                  (apply + ))]
        (cond (== sum 1)   true
              (previous sum) false
              :else        (recur sum (conj previous sum)))))))

(defcheck solution-ee101c8a
  (fn hb [n]
    (loop [x n s #{n}]
      (let [a (->> (seq (str x))
                (map #(* (str->int (str %)) (str->int (str %))))
                (reduce +)

                )]
        (cond (= 1 a) true
              (contains? s a) false
              :else (recur a (conj s a)))))
    ))

(defcheck solution-eebf0aa6
  (fn [n] (loop [seen #{}
                 [s & rst] (iterate (fn [s] (apply + (map (comp #(* % %) str->int)
                                                       (re-seq #"." (str s)))))
                             n)]
            (cond (= s 1) true
                  (seen s) false
                  :else (recur (conj seen s) rst)))))

(defcheck solution-eec5bbe4
  (fn [num]
    (let [digits (fn [n]
                   (map second
                     (rest
                       (take-while #(not= % [0 0])
                         (iterate
                           (fn [[q r]] [(quot q 10) (rem q 10)])
                           [n 0])))))
          s-d (fn [n] (apply + (map #(* % %) (digits n))))]
      (loop [num num
             seen #{}]
        (cond
          (seen num) false
          (= num 1) true
          :else (recur (s-d num) (conj seen num)))))))

(defcheck solution-eed08a1f
  (letfn [(sumdigits [x] (apply + (map #(Math/pow (- (char->ascii %) 48) 2) (vec (str (int x))))))]
    (fn hn[c] (if (= 1 (int (last (take 50 (iterate sumdigits c))))) true false))))

(defcheck solution-eed25d7c
  (fn [n]
    (cond
      (zero? n) false
      (= n 1) true
      (and (>= n 2)
           (<= n 5)) false
      :else (recur (->> n
                     str
                     (map #(- (char->num %) (char->num \0)))
                     (map #(* % %))
                     (apply +)
                     )))))

(defcheck solution-ef0ea0dd
  (fn [n]
    (loop [tmpn n seen #{}]
      (if (= 1 tmpn)
        true
        (if (contains? seen tmpn)
          false
          (recur (reduce + (map #(* % %) (map str->int (map str (apply vector (str tmpn)))))) (conj seen tmpn)))))))

(defcheck solution-ef1ac8e9
  (letfn [(ss [n] (reduce + (map #(* % %) (map #(str->int (str %)) (seq (str n))))))]
    (fn happy?
      ([n] (happy? #{} n))
      ([seen n]
       (cond
         (= n 1) true
         (contains? seen n) false
         :else (recur (conj seen n) (ss n)))))))

(defcheck solution-ef582cf8
  (fn happy? [x]
    (let [digits  (fn [x] (map #(char->num % 10) (str x)))
          sumsq   (fn [x] (reduce + (map #(* % %) (digits x))))]
      (loop [n x, seen #{}]
        (if (= n 1) true
                    (if (seen n) false
                                 (recur (sumsq n) (conj seen n))))))))

(defcheck solution-ef772a4d
  #((fn g [x s]
      (or (= x 1)
          (and (not (s x))
               (g ((fn f [n] (if (= n 0) 0
                                         (+ (* (rem n 10) (rem n 10)) (f (quot n 10))))) x)
                 (conj s x)))))
    % #{}))

(defcheck solution-f0019bd3
  (fn [n] (if (= 1 (some
                     #(if (or (= % 1) (= % 4)) %)
                     (iterate (fn [p]
                                (->> p str (map #(str->int (str %))) (map #(* % %)) (apply +) )) n))) true false)))

(defcheck solution-f07d8583
  (fn happy
    ([x] (happy #{} x))
    ([coll x]
     (cond
       (= x 1) true
       (nil? (get coll x))
       (happy (conj coll x)
         (apply + (map (fn [y] (* y y))
                    (map second
                      (take-while #(> (first %) 0)
                        (iterate (fn [[a b]] (vector (quot a 10) (mod (quot a 10) 10))) [x (mod x 10)]))))))
       true false))))

(defcheck solution-f0807c84
  (let [dsqsum (fn [nn] (let [ digits (loop [n nn d '()] (if (= n 0) d (recur (quot n 10) (cons (mod n 10) d))))] (reduce + (map #(* % %) digits))))]
    (fn happy [l n]
      (let [dss (dsqsum n)]
        (if (= 1 dss)
          true
          (if (some #(= dss %) l)
            false
            (recur (cons dss l) dss)))))) '())

(defcheck solution-f0a30206
  (fn g
    ([m l]
     (let  [n (apply + (map #(int (Math/pow (mod % 10) 2))
                         (take-while #(not (= % 0)) (iterate #(int (/ % 10)) m))))]
       (if (= 1 n) true (if (some #(= n %) l) false (g n (cons n l))))
       )
     )
    ([m] (g m []))))

(defcheck solution-f0cb8ae3
  (fn [n]
    (let [f (fn [x]
              (->> x
                str
                (map #(str->int (str %)))
                (map #(* % %))
                (apply +)))
          s (iterate f n)]
      (loop [seen #{}
             [x & xs] s]
        (cond
          (= x 1) true
          (seen x) false
          :else (recur (conj seen x) xs))))))

(defcheck solution-f1121fe5
  (fn happy? [o]
    (let [ssq (fn ssq [n] (if (< n 10) (* n n)
                                       (+ (* (mod n 10) (mod n 10)) (ssq (quot n 10)))))]
      (loop [n o seen {}]
        (if (seen n)
          false
          (let [q (ssq n)]
            (or (== q 1) (recur q (assoc seen n true)))))))))

(defcheck solution-f1552425
  (fn [n]
    (let [digsqr (fn [nn] (reduce + (map #(let [a (- (char->num %) (char->num \0))] (* a a)) (str nn))))]
      (loop [nn n seen #{}]
        (cond
          (seen nn) false
          (= nn 1) true
          :else (recur (digsqr nn) (conj seen nn)))))))

(defcheck solution-f21afcb4
  (fn [x]
    (let [digits     (fn [i] (map #(- (char->ascii %) 48) (str i)))
          sum-square (fn [x] (apply + (map #(* % %) (digits x))))
          iter       (iterate sum-square x)
          last-one   (last (take 100 iter))]
      (= last-one 1))))

(defcheck solution-f25dc204
  (fn [s x]
    (or (= x 1)
        (if (s x) false
                  (recur (conj s x) (apply + (map #(* (- (char->ascii %) 48) (- (char->ascii %) 48)) (str x))))))) #{})

(defcheck solution-f2ae94e2
  (fn [n]
    (let [happy-seq (fn [n]
                      (iterate (fn [i] (reduce (fn [s j]
                                                 (let [d (str->int (str j))]
                                                   (+ s (* d d))))
                                         0 (str i))) n))
          h? (fn [hs ps]
               (let [e (first hs)] (cond (= 1 e) true
                                         (ps e) false
                                         :else (recur (next hs) (conj ps e)))))]
      (h? (happy-seq n) #{}))))

(defcheck solution-f2c7ca5e
  (fn [n]
    (let [to-int #(- (char->ascii %) 48)
          sqr #(* % %)
          f #(->>
               %
               str
               (map to-int)
               (map sqr)
               (apply +))]
      (loop [n n
             mem #{n}]
        (let [new (f n)]
          (cond
            (= 1 new) true
            (mem new) false
            :else (recur new (conj mem new))))))))

(defcheck solution-f2dde154
  (fn happy-number[x]
    (loop [next x
           previous []]
      #_(println next)
      (if (every? #(not= % next) previous)
        (recur (apply + (map #(* % %) (map #(str->int (str %)) (str next)))) (conj previous next))
        (= next 1)))))

(defcheck solution-f2f1eb35
  (fn [n]
    (= 1 (nth (iterate (fn [x] (reduce #(+ % (* %2 %2)) 0 (map #(- (char->ascii %) 48) (str x)))) n) 100))))

(defcheck solution-f2f53536
  (fn [i]
    (loop [s #{}
           [x & r] (iterate
                     (fn [a]
                       (->> a str (map str) (map str->int) (map #(* % %)) (apply +)))

                     i)]
      (if (s x)
        false
        (or (= 1 x)
            (recur (conj s x) r))))))

(defcheck solution-f3318458
  (fn happy? [candidate]
    (let [determine-happiness (fn determine-happiness [candidate seen]
                                (if (contains? seen candidate) false
                                                               (let [next-candidate (reduce + (map #(* % %)
                                                                                                (map #(char->num %)
                                                                                                  (str candidate))))]
                                                                 (if (= next-candidate 1)
                                                                   true
                                                                   (determine-happiness next-candidate (conj seen candidate))))))]
      (determine-happiness candidate #{}))))

(defcheck solution-f338c514
  (fn [v]
    (let
     [ n
      (fn [v]
        (apply +
          (map #(* (mod % 10) (mod % 10))
            (take-while #(> % 0)
              (iterate #(quot % 10) v)))))
      h
      (fn h
        ([v] (h v #{}))
        ([v a]
         (if
          (a v) false
                (or
                 (= 1 v)
                 #(h (n v) (conj a v))))))]
      (trampoline h v))))

(defcheck solution-f385d86
  (fn [n]
    ((fn f [n c]
       (let [m (reduce
                 #(+ %1 (* %2 %2))
                 0
                 (map
                   #(mod % 10)
                   (take-while pos?
                     (iterate #(int (/ % 10)) n))))]
         (if (= m 1) true
                     (if (zero? c) false (recur m (dec c))))))
     n 1000)))

(defcheck solution-f44ffee1
  (fn happy[n]
    (loop [s (str n) r #{}]
      (if (contains? r s)
        (do
          false)
        (let [sum (str (reduce #(+ %1 (* (str->int (str %2)) (str->int (str %2)))) 0 s))]
          (if (= 1 (str->int sum))
            true
            (recur (str sum) (conj r s))))))))

(defcheck solution-f46419d8
  (fn [n] (loop [x n]
            (if (some #(= x %) #{1 4})
              (= x 1)
              (let [s (map #(str->int (str %)) (str x))]
                (recur (apply + (map #(int (Math/pow % 2)) s))))))))

(defcheck solution-f572ed49
  (fn happy? [n]
    (loop [history #{} n n]
      (let [digits (->> n str (map (comp str->int str)))
            squared-sum (reduce #(+ %1 (* %2 %2)) 0 digits)]
        (cond
          (= squared-sum 1) true
          (contains? history squared-sum) false
          :else
          (recur (conj history squared-sum) squared-sum)
          )
        )
      )
    ))

(defcheck solution-f59e0c4b
  (fn [n]
    (loop [seen #{} n n]
      (if (seen n)
        false
        (if (= 1 n)
          true
          (let [sum (reduce #(+ % (* %2 %2)) 0 (map #(- (char->num %) (char->num \0)) (str n)))]
            (recur (conj seen n) sum)))))))

(defcheck solution-f5a1e41f
  (fn happy-number [n]
    (loop [n n mem #{}]
      (cond
        (= n 1) true
        (mem n) false
        true (recur
               (reduce
                 +
                 (map #(let [tmp (str->int (str %))]
                         (* tmp tmp))
                   (str n)))
               (conj mem n))))))

(defcheck solution-f5fdb3e
  #(letfn [
           (square [coll]
             "sum the squares of all elements in the collection"
             (reduce + (for[x coll](* x x)))
             )

           (num-to-coll[N]
             "transform n into a collection of digits"
             (loop [n N c ()]
               (if (= n 0)
                 c
                 (recur (int (/ n 10)) (conj c (mod n 10)))
                 )
               )
             )

           (happy? [N]
             "for every unhappy number the process ends in the circle 4 16 ... For the happy ones it terminates with 1"
             (loop [n N]
               (if (= n 1)
                 true
                 (if (contains? #{4, 16, 37, 58, 89, 145, 42, 20} n)
                   false
                   (recur (square (num-to-coll n)))
                   )
                 )
               )
             )
           ]


     (happy? %)
     ))

(defcheck solution-f690f8ab
  (fn [n]
    (let [succ (fn [n] (reduce + (map #(let [d (str->int (str %))] (* d d)) (str n))))]
      (loop [seen #{n} n (succ n)]
        (if (= n 1)
          true
          (if (contains? seen n)
            false
            (recur (conj seen n) (succ n))))))))

(defcheck solution-f6b4f434
  (fn hn
    ([x] (hn x #{}))
    ([x seen]
     (if (= 1 x) true
                 (letfn [(digits [n]
                           (when (not= n 0)
                             (conj (digits (quot n 10)) (rem n 10))))]
                   (if (seen x) false
                                (recur
                                  (apply + (map #(* % %) (digits x)))
                                  (conj seen x))))))))

(defcheck solution-f6c89131
  (fn [x]
    (case x
      1 true
      4 false
      (recur (reduce (fn [acc y]
                       (+ acc (* y y)))
               0
               (map (comp str->int str)
                 (seq (str x))))))))

(defcheck solution-f717c1c5
  (fn [n]
    (= 1 (some #{1 4} (iterate (fn [x]
                                 (reduce #(+ %1 (apply * ((juxt identity identity) (- (char->ascii %2) 48))))
                                   0
                                   (str x)))
                        n)))))

(defcheck solution-f8bce8ff
  (fn happy? [n]
    (loop [i n seen? #{}]
      (let [nxt (reduce + (map (comp #(* % %) str->int str) (str i)))]
        (cond
          (= nxt 1)   true
          (seen? nxt) false
          :else       (recur nxt (conj seen? i)))))))

(defcheck solution-f90d4f90
  (letfn [(digits [n]
            (map #(char->num % 10) (str n)))
          (square [n] (* n n))]
    (fn happy?
      ([n] (happy? n #{}))
      ([n seen]
       (let [x (reduce + (map square (digits n)))]
         (cond
           (= 1 x)  true
           (seen x) false
           :else    (recur x (conj seen x))))))))

(defcheck solution-f90d6dfd
  (fn f [n]
    (loop [i n, p []]
      (let [s (reduce + (map (comp #(* % %) str->int str) (str i)))]
        (cond
          (= 1 s) true
          (some #{s} p) false
          :default (recur s (conj p s)))))))

(defcheck solution-f94288ef
  (fn happy-number
    [n]
    (let [digits (fn [n] (map #(str->int (str %)) (str n)))
          sum-squares (fn [n] (reduce #(+ %1 (* %2 %2)) 0 (digits n)))]
      (loop [current n
             i 0]
        (cond
          (= i 1000) false
          (= (sum-squares current) 1) true
          :else (recur (sum-squares current) (inc i)))))))

(defcheck solution-f97bb0a9
  (fn [n]
    (let [ns (atom #{})
          next-num (fn [n] (->> (map str->int (re-seq #"\d" (str n)))
                             (map #(* %1 %1))
                             (reduce +)))]
      (loop [nx n]
        (if (= 1 (next-num nx))
          true
          (if (contains? @ns (next-num nx))
            false
            (do
              (swap! ns conj nx)
              (recur (next-num nx)))))))))

(defcheck solution-fa4683dc
  (fn iter
    ([n] (iter n #{}))
    ([n tested]
     (letfn [ (digits [s]
                (map #(str->int %)
                  (re-seq #"[0-9]" (str s))))]
       (cond (= n 1) true
             (tested n) false
             :else (recur (reduce + (map #(* % %) (digits n)))
                     (conj tested n)))))))

(defcheck solution-fa88edea
  (fn happy

    ([x]

     (happy x 1))

    ([x cnt]

     (if (= x 1)

       true

       (if (> cnt 100)

         false

         (happy (reduce + (map #(* % %) (map str->int (re-seq #"\w" (str x))))) (inc cnt)))))))

(defcheck solution-fac22d7e
  (fn [n]
    (letfn [(squ-sum [a]
              "square every digit, and sum it"
              (loop [sum 0
                     a a]
                (if (zero? a)
                  sum
                  (recur (+ sum (* (mod a 10)
                                  (mod a 10)))
                    (quot a 10)))))]
      (loop [item n
             sitem #{}]
        (let [item (squ-sum item)]
          (if (= 1 item)
            true
            (if (sitem item)
              false
              (recur item (set(cons item sitem))))))))))

(defcheck solution-fb2f2e84
  (fn happyNum
    [n]
    (let [ssd (fn [x]
                (reduce
                  #(+ %1 (* %2 %2))
                  0
                  (map #(str->int (str %)) (str x))))]
      (loop [currNum n
             alreadySeen #{}]
        (cond
          (contains? alreadySeen (ssd currNum)) false
          (= (ssd currNum) 1) true
          :else (recur (ssd currNum) (conj alreadySeen currNum)))))))

(defcheck solution-fb2f3ab5
  (fn [n]
    (loop [it 1000 n n]
      (let [ns (map (fn [i] (str->int (str i))) (str n))
            new-n (reduce (fn [acc e] (+ acc (* e e))) 0 ns)]
        (if (= 0 it)
          false
          (if (= 1 new-n)
            true
            (recur (dec it) new-n)))))))

(defcheck solution-fb566ac
  #(letfn[(sqDigit [d]
            (let [dVal (- (char->num d) (char->num \0))]
              (* dVal dVal)))
          (nextNumber[x]
            (reduce + (map sqDigit (seq (str x)))))]
     (loop[seen #{}, number %]
       (cond
         (seen number) false
         (= number 1) true
         :else (recur (conj seen number) (nextNumber number))))))

(defcheck solution-fb6093b5
  (fn happy? [n]
    (case n
      1 true
      4 false
      (recur (->> n
               (iterate #(quot % 10))
               (take-while pos?)
               (map #(let[d (mod % 10)] (* d d)))
               (reduce +))))))

(defcheck solution-fb647ab2
  (fn [x]
    (loop [a #{} x x]
      (or (= 1 x)
          (if (a x)
            false (recur (conj a x)
                    (apply + (map (comp #(* % %) #(- (char->ascii %) 48)) (str x)))
                    ))))))

(defcheck solution-fb74a98e
  (fn [x]

    (loop [n x, sums #{}]

      (let [csum (reduce #(+ (* %2 %2) %1) 0 (map #(- (char->ascii %1) 48) (seq (str n))))]
        (cond
          (= 1 csum) true
          (sums csum)  false
          :else (recur csum (conj sums csum)))))))

(defcheck solution-fbdec990
  (fn happy?
    ([x] (happy? #{} x))
    ([seens x] (cond
                 (= 1 x) true
                 (contains? seens x) false
                 :else (happy? (conj seens x) (reduce #(+ %1 (* %2 %2)) 0 (map #(str->int (str %)) (str x))))
                 )
     )
    ))

(defcheck solution-fc92e109
  (fn happy?
    ([num] (happy? num #{}))
    ([num priors]
     #_(println num priors)
     (let [digits (fn [n out]
                    (if (= n 0)
                      out
                      (let [d (rem n 10)
                            r (quot n 10)]
                        (recur r (cons d out)))))
           result (apply + (map #(* % %) (digits num '())))]
       (if (= result 1)
         true
         (if (priors result)
           false
           (happy? result (conj priors num))))))))

(defcheck solution-fccebaa4
  (fn [x]
    (let [ssqd   (fn [a r]
                   (let [m  (mod r 10)
                         q  (quot r 10)
                         a' (+ a (* m m))]
                     (if (zero? q) a' (recur a' q))))
          seqssq (fn ssq [x]
                   (let [sx (ssqd 0 x)]
                     (cons sx (lazy-seq (ssq sx)))))
          cycles (fn [a [x & xs]]
                   (if (a x)
                     x
                     (recur (conj a x) xs)))]
      (->> x seqssq (cycles #{}) (= 1)))))

(defcheck solution-fd60fc82
  (fn [x]
    (letfn [(sumsq [y] (reduce + (map #(* % %) (map #(- (char->ascii %) 48) (str y)))))]
      (loop [z (sumsq x)
             seen #{}]
        (cond
          (seen z) false
          (= 1 z) true
          :else (recur (sumsq z) (conj seen z)))))))

(defcheck solution-fd7803f3
  (fn happy? [n]
    (let [digits
              (fn digits [n] (lazy-seq
                               (when (pos? n)
                                 (cons (rem n 10) (digits (quot n 10))))))
          sqr #(* % %)
          ]
      (loop [n n, seen #{}]
        (cond
          (= n 1) true
          (seen n) false
          :else (recur (apply + (map sqr (digits n))) (conj seen n))
          )))))

(defcheck solution-fd84716b
  (fn happy?
    ([n] (happy? n #{}))
    ([n prev]
     (let [next (reduce + (map (comp #(* % %) #(- (char->num %) (char->num \0))) (str n)))]
       (cond
         (= 1 next) true
         (prev n)   false
         :else (happy? next (conj prev n)))))))

(defcheck solution-fdaab340
  (fn happy?
    ([x] (happy? x #{}))
    ([x v]
     (let
      [y ;let y = sum of sq digs (x)
       (reduce
         + ((fn sqdigs [n] (if (zero? n) '() (cons (* (mod n 10) (mod n 10)) (sqdigs (quot n 10))))) x)
         )
       ]
       (or
        (= y 1)
        (if (v y)
          false
          (recur y (conj v y))
          )
        )
       )
     )
    ))

(defcheck solution-fdad668b
  (fn is-happy? [n]
    (let [sqr #(* % %)
          to-int #(- (char->num %) (char->num \0))
          sqr-as-int (comp sqr to-int)]
      (loop [n n, seen #{}]
        (if (seen n) false
                     (let [n2 (apply + (map sqr-as-int (str n)))]
                       (if (= 1 n2) true
                                    (recur n2 (conj seen n)))))))))

(defcheck solution-fe12c1b5
  (fn [n]
    (letfn [(sq-sum [coll]
              (apply + (map #(* %1 %1) coll)))
            (individual-digit [n]
              (reduce #(conj %1 %2)
                []
                (map #(str->int %1)
                  (map str (vec (str n))))))]
      (loop [times 0 result n]
        (if (> times 10)
          false
          (if (= result 1) true (recur (inc times) (sq-sum (individual-digit result)))))))))

(defcheck solution-ff8b4f39
  (fn [n]
    (let [ch2int #(- (char->num %) (char->num \0))
          sqr #(* % %)
          sqri (fn [x] (reduce + (map #(sqr (ch2int %)) (str x))))]
      (loop [res []
             cur n]
        (if (= 1 cur)
          true
          (if (nil? (some #{cur} res))
            (recur (conj res cur) (sqri cur))
            false))))))

(defcheck solution-ff9ea2b9
  (fn [n]
    (let [get-digits (fn get-digits [n]
                       (if (< n 10)
                         [n]
                         (conj (get-digits (quot n 10)) (mod n 10))))
          to-happy (fn [n](reduce + (map #(* % %) (get-digits n))))
          ]
      (= 1 (first (drop-while #(and (not= % 1) (not= % 4)) (iterate to-happy n))) )
      )))

(defcheck solution-ffa20bc
  (fn happy? [n]
    ((fn subhappy [n seen]
       (let [digits ((fn dig [n]
                       (if (= 0 n) []
                                   (conj (dig (quot n 10)) (mod n 10)))) n)
             newn (reduce + (map #(* % %) digits))]
         (cond
           (= 1 newn) true
           (contains? seen newn) false
           true (subhappy newn (conj seen newn))))) n #{})))
