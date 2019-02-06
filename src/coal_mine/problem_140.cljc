(ns coal-mine.problem-140
  (:require [coal-mine.checks :refer [defcheck-140] :rename {defcheck-140 defcheck}]
            [clojure.test]
            [clojure.string]
            [clojure.set]))

(defcheck solution-1072618c
  (fn run [circuit]
    (letfn [(combinations [n items]
              (cond
                (= n 0) '(())
                (empty? items) '()
                :else (concat
                        (map #(cons (first items) %)
                          (combinations (dec n) (rest items)))
                        (combinations n (rest items)))))
            (combinationPair [n items]
              (cond
                (= n 0) '(())
                (empty? items) '()
                :else (concat
                        (map #(cons (first (first items)) %)
                          (combinationPair (dec n) (rest items)))
                        (map #(cons (second (first items)) %)
                          (combinationPair (dec n) (rest items)))
                        (combinationPair n (rest items)))))
            (solution? [circuit solutions]
              (= circuit
                 (set (reduce (fn [result solution] (clojure.set/union result
                                                                       (set
                                                                        (matches circuit solution))))
                       #{}
                       solutions))))
            (matches [circuit solution]
              (doall (filter (fn [row] (every? (fn [elem] (contains? row elem)) solution)) circuit)))
            (collectElements [circuit]
              (vals (group-by #(clojure.string/upper-case (str %))
                      (reduce (fn [result elem] (clojure.set/union result elem))
                        #{}
                        circuit))))
            (generateAll [circuit]
              (let [elements (collectElements circuit)]
                (sort-by count (filter #(not-empty %) (mapcat #(combinationPair % elements) (range (inc (count elements))))))))
            (makeSmaller
              ([circuit solutions] (makeSmaller circuit solutions 1))
              ([circuit solutions n]
               (if (= n (count solutions))
                 solutions
                 (let [soln (filter #(and (not-empty %)
                                          (solution? circuit %))
                              (combinations n solutions))]
                   (if (not-empty soln)
                     (apply min-key #(count (flatten %)) soln)
                     (recur circuit solutions (inc n)))))))
            (findSolution [circuit]
              (reduce (fn [result elem]
                        (do
                          (let [cntElements (count (collectElements circuit))]
                            (if (= (first result) circuit)
                              result
                              (let [matches (matches circuit elem)
                                    wrongMatch (not= (count matches) (int (Math/pow 2 (- cntElements (count elem)))))]
                                (if (or wrongMatch
                                        (empty? matches))
                                  result
                                  [(clojure.set/union (first result) (set matches))
                                   (conj (second result) elem)]))))))
                [#{} #{}]
                (generateAll circuit)))]
      (let [[resultSet solution] (findSolution circuit)]
        (set (map set (makeSmaller circuit solution)))))))

(defcheck solution-18616f8b
  (fn veitch [baf]
    (let [simp (fn [m1 m2]
                 (let [s1 (first m1) s2 (first m2) c1 (count s1)]
                   (when (= c1 (count s2))
                     (let [i (set (keep s1 s2))]
                       (when (= (count i) (dec c1))
                         (let [d1 (filter (complement i) s1)
                               d2 (filter (complement i) s2)]
                           (when (and (= 1 (count d1) (count d2))
                                      (= (clojure.string/lower-case (str (first d1)))
                                        (clojure.string/lower-case (str (first d2)))))
                             [i (into (second m1) (second m2))])))))))
          iter (fn [baf]
                 (reduce (fn [m [k v]] (assoc m k v)) {}
                   (for [s1 baf s2 baf :when (not= s1 s2)
                         :let [i (simp s1 s2)] :when i]
                     i)))
          mitm (->> baf (reduce #(assoc % %2 (hash-set %2)) {})
                 (iterate iter) (take-while seq) (apply merge))
          mits (sort-by count (map key mitm))
          sols (fn sols [comb]
                 (lazy-cat comb (sols (for [ps comb mt mits] (conj ps mt)))))
          vald (fn [sol]
                 (= baf (reduce #(into % (mitm %2)) #{} sol)))]
      (first (filter vald (sols (map #(hash-set %) mits)))))))

(defcheck solution-19dd3320
  (fn veitch [rules]
    (let [dict (fn [rs] (apply clojure.set/union rs))
          negation {'a 'A, 'A 'a, 'b 'B, 'B 'b, 'c 'C, 'C 'c, 'd 'D, 'D 'd}
          numOfSyms (count (first rules))
          gen-comb (fn [n syms]
                     (loop [i 1 r #{#{}}]
                       (let[nr (set (reduce concat (for[acc r s syms :when (and (not (contains? acc s))
                                                                                (not (contains? acc (negation s))))]
                                                     [(conj acc s)])))]
                         (if (= i n) nr (recur (inc i) nr)))))
          find-solution (fn [solutions n]
                          (loop[s solutions result #{}]
                            (if(seq s)
                              (let[newlyMatched (filter #(clojure.set/subset? (first s) %) rules)]
                                (if (= (count newlyMatched) n) (recur (rest s) (conj result (first s)))
                                                               (recur (rest s) result)))
                              result)))]
      (loop[i 1 solutions #{} matched #{} remainingRules rules]
        (cond
          (= matched rules) solutions
          (= i numOfSyms) (clojure.set/union solutions (clojure.set/difference rules matched))
          :else (let[n (int (Math/pow 2 (- numOfSyms i)))
                     potSol (gen-comb i (dict remainingRules))
                     sol (find-solution potSol n)
                     newlyMatched (set (apply concat (for[s sol] (filter #(clojure.set/subset? s %) rules))))
                     refinedSolutions (set (loop[s sol reSol solutions]
                                             (if(seq s) (recur (rest s) (filter #(empty? (clojure.set/intersection (first s) %)) reSol))
                                                        reSol)))]
                  (if (seq sol) (recur (inc i) (clojure.set/union refinedSolutions sol) (clojure.set/union matched newlyMatched) (clojure.set/difference remainingRules newlyMatched))
                                (recur (inc i) solutions matched remainingRules))))))))

(defcheck solution-236f9aad
  (fn [algebra]
    (letfn
     [(combination [sets]
        (loop [ss sets acc []]
          (if (empty? ss)
            acc
            (let [s (first ss) rss (rest ss)]
              (recur rss (concat acc (map #(vector s %) rss)))))))
      (s-complement [s1 s2]
        (let [xs1 (clojure.set/difference s1 s2)
              xs2 (clojure.set/difference s2 s1)
              xs (clojure.set/union xs1 xs2)]
          (when (= 1 (count (set (map (comp clojure.string/capitalize str) xs))))
            [(clojure.set/intersection s1 s2) s1 s2])))
      (simplify [sets acc]
        (let [comps (keep #(apply s-complement %) (combination sets))
              ss (set (map first comps))
              more (clojure.set/difference sets (set (mapcat rest comps)))]
          (if (empty? sets)
            acc
            (recur ss (clojure.set/union acc more)))))
      (s-item [s simps]
        (let [ss (filter #(clojure.set/subset? % s) simps)]
          (when (= 1 (count ss)) (first ss))))]
      (let [simps (simplify algebra #{})]
        (set (keep #(s-item % simps) algebra))))))

(defcheck solution-264fdcf0
  (fn prob140 [baf]
    (let [symbol-difference (fn [a b]
                              (clojure.set/union (clojure.set/difference a b) (clojure.set/difference b a)))
          complementary? (fn [a b] (= 1 (count (distinct (map #(clojure.string/capitalize (str %)) (symbol-difference a b))))))
          simplify (fn [baf extra] (let [simplification (for [a baf b (disj baf a) :when (complementary? a b)]
                                                          [(clojure.set/intersection a b) a b])
                                         to-remove (set (mapcat rest simplification))
                                         more-extra (clojure.set/difference baf to-remove)
                                         to-simplify (set (map first simplification))]
                                     (if (= baf to-simplify) extra
                                                             (recur to-simplify (clojure.set/union extra more-extra)))))
          reapplied (fn [baf] (map (fn [s] (filter #(clojure.set/subset? % s) (simplify baf #{}))) baf))
          ]
      (set (map first (filter #(= (count %) 1) (reapplied baf))))
      )
    ))

(defcheck solution-26f0737a
  (fn [sets]
    (let [
          converge (fn [f x]
                     (loop [x0 x x1 (f x)]
                       (if (= x0 x1) x1 (recur x1 (f x1)))))
          opposite (fn [a b] (and (not= a b) (= (clojure.string/upper-case (str a)) (clojure.string/upper-case (str b)))))
          adjacent (fn [s0 s1] (let [d0 (apply disj s0 s1) d1 (apply disj s1 s0)] (and (= 1 (count d0)) (= 1 (count d1)) (opposite (first d0) (first d1)))))
          combine (fn [s0 s1] (disj s0 (first (apply disj s0 s1))))
          covers (fn [ss] (every? (fn [s] (some #(empty? (apply disj % s)) ss)) sets))
          remove-redundency (fn [ss] (let [options (for [s ss :let [new-ss (disj ss s)] :when (covers new-ss)] new-ss)] (if (empty? options) ss (first options))))
          simplify (fn [sets]
                     (let [segments (for [
                                          s0 sets s1 sets
                                          :when (= (count s0) (count s1))
                                          :when (adjacent s0 s1)]
                                      [s0 s1 (combine s0 s1)])]
                       (reduce (fn [sets [s0 s1 sc]]
                                 #_(println [s0 s1 sc])
                                 (disj (conj sets sc) s0)) sets segments)))]
      (remove-redundency (converge simplify sets)))))

(defcheck solution-30b18ec1
  (letfn [
          (one-bit-diff? [s0 s1]
            (let [d0 (clojure.set/difference s0 s1)
                  d1 (clojure.set/difference s1 s0)]
              (and (= 1 (count d0))
                   (= (clojure.string/upper-case (str d0)) (clojure.string/upper-case (str d1))))))
          (simplify-once [s]
            (->> (for [x s y (rest (drop-while (partial not= x) s))] [x y])
              (keep (fn [[x y]] (if (one-bit-diff? x y) [(clojure.set/intersection x y) x y])))
              (reduce (fn [s [n x y]] (conj (disj s x y) n)) s)))
          (remove-redundant-rules [s]
            (reduce
              (fn [s ss]
                (let [s' (disj s ss)
                      s'-flat (->> s' (map vec) flatten set)]
                  (if (or (not= (count ss) 2) (some nil? (map #(s'-flat %) ss))) s s')))
              s s))
          (simplify [s]
            (let [s' (simplify-once s)]
              (if (= s s')
                (remove-redundant-rules s)
                (recur s'))))]
    simplify))

(defcheck solution-41252b88
  (fn p1 [exps]
    (let [c (fn c [[s1 s2]]
              (let [x (clojure.set/difference s1 s2)
                    y (clojure.set/difference s2 s1)]
                (if (and (= (count x) 1)
                         (= (clojure.string/upper-case (str x))
                           (clojure.string/upper-case (str y))))
                  [(clojure.set/intersection s1 s2) s1 s2])))
          s (fn s [[exps ys]]
              (let [r (keep c (loop [exps exps r []]
                                (if (empty? exps) r
                                                  (recur (rest exps)
                                                    (concat r
                                                      (map #(list (first exps) %)
                                                        (rest exps)))))))
                    x (set (map first r))
                    y (reduce #(disj % (second %2) (nth %2 2)) exps r)]
                [x (clojure.set/union ys y)]))]
      (let [ts (apply clojure.set/union (last (take-while #(not (empty? (first %))) (iterate s [exps #{}]))))]
        (set (map first (filter #(= (count %) 1) (map (fn [x] (filter #(clojure.set/subset? % x) ts)) exps))))))))

(defcheck solution-4e3a6c99
  (fn [i]
    (let [u clojure.set/union
          f filter s set
          b (fn [[x m] t]
              (s (f identity (map
                               (fn [[y n]]
                                 (let [i (clojure.set/intersection x y)]
                                   (when (= (inc (count i)) (count (s (map (comp clojure.string/upper-case str) (u x y)))))
                                     [i (into m n)])))
                               t))))
          t (loop [x #{} y (map #(vector % #{%}) i)]
              (if (empty? y)
                x
                (recur
                  (into x (s (f #(empty? (b % y)) y)))
                  (apply u (f #(not (empty? %)) (map #(b % y) y))))))
          p #(clojure.set/subset? (second %) (apply u (map second (disj t %))))]
      (loop [[x y] (map s [(f p t) (remove p t)])]
        (if (= i (apply u (map second y)))
          (s (map first y))
          (let [z (first x)]
            (recur [(disj x z) (conj y z)])))))))

(defcheck solution-63a4df5f
  (fn [ss]
    (let [sets
          (->> [ss #{}]
            (iterate (fn [[sets acc]]
                       (->> (for [a sets, b (disj sets a)] [a b])
                         (map (fn [[a b]]
                                (->> (clojure.set/union
                                       (clojure.set/difference a b)
                                       (clojure.set/difference b a))
                                  (map (comp clojure.string/lower-case str))
                                  ((fn [[x & xs :as all]]
                                     (and (seq all)
                                          (apply = x xs)
                                          [(clojure.set/intersection a b) a b]))))))
                         (filter identity)
                         ((juxt
                            #(->> % (map first) set)
                            #(->> %
                               (reduce (fn [z x] (disj z (second x) (last x))) sets)
                               (clojure.set/union acc)))))))
            (take-while #(seq (first %)))
            last
            (apply clojure.set/union))]
      (->> ss
        (map (fn [x] (filter #(clojure.set/subset? % x) sets)))
        (filter #(= 1 (count %)))
        (map first)
        set))))

(defcheck solution-676dbca1
  (fn [formula]
    (letfn [(diff-by-one-complement? [s1 s2]
              (let [d1 (clojure.set/difference s1 s2)
                    d2 (clojure.set/difference s2 s1)]
                (and (= 1
                       (count d1)
                       (count d2)
                       (count (set
                                (map (comp clojure.string/lower-case str)
                                  (concat d1 d2))))))))
            (minimize [[mins formula]]
              (let [pairs (for [s1 formula
                                s2 (disj formula s1)]
                            (list s1 s2))
                    [minified unmin]
                    (reduce (fn [[minified unmin :as acc] [s1 s2]]
                              (if (diff-by-one-complement? s1 s2)
                                [(conj minified (clojure.set/intersection s1 s2))
                                 (disj unmin s1 s2)]
                                acc))
                      [#{} formula]
                      pairs)]
                [(clojure.set/union mins unmin) minified]))
            (eliminate [minified formula]
              (reduce (fn [acc s]
                        (let [subs (filter (partial clojure.set/superset? s)
                                     minified)]
                          (if (= 1 (count subs))
                            (conj acc (first subs))
                            acc)))
                #{}
                formula))]
      (let [minified (first
                       (first
                         (drop-while #(seq (last %))
                           (iterate minimize [#{} formula]))))]
        (eliminate minified formula)))))

(defcheck solution-80ab42c1
  (fn [s]
    (letfn [(diff-by-one [s1 s2]
              (->>
                (clojure.set/union (clojure.set/difference s1 s2) (clojure.set/difference s2 s1))
                (mapv str)
                (mapv clojure.string/lower-case)
                (reduce =)))
            (is-extra [s1 s]
              (if (= 4 (count s1))
                false
                (let [simple-s (disj s s1)
                      reduced-s (reduce clojure.set/union (vec simple-s))]
                  (->> s1
                    (map #(contains? reduced-s %))
                    (reduce  #(and (true? %2) %1))))))
            (remove-extras [s]
              (reduce #(if
                        (is-extra %2 s) %1 (conj %1 %2))
                #{} (vec s)))
            (solv [s]
              (->> s
                (map (fn [x]
                       (let [result
                             (set (for [y (vec s)]
                                    (let [i (clojure.set/intersection x y)]
                                      (if (and (= (count i) (dec (count x))) (diff-by-one x y))
                                        i
                                        x))))]
                         (if (= 1 (count result))
                           result
                           (disj result x)))))
                (reduce clojure.set/union)
                (set)))]
      (-> s (solv) (solv) (remove-extras)))))

(defcheck solution-82d554ea
  (fn [input]
    (letfn[(combinacao [pivot n]
             (set
               (cond
                 (= n 1) (for [x pivot] ; 1 elemento
                           #{x})
                 (= n 2) (for [x pivot ; 2 elementos
                               y (disj pivot x)]
                           (hash-set x y))
                 (= n 3) (for [x pivot] ; 3 elementos
                           (disj pivot x))
                 (= n 4) #{pivot})))
           (simplifica-n [pivot n]
             (filter (fn [expressao-simplificada]
                       (->> (map (fn [expressao-original]
                                   (clojure.set/subset? expressao-simplificada expressao-original))
                              input)
                         (filter identity)
                         count
                         (= (/ (-> input first count dec (#(apply * (repeat % 2))))
                              (apply * (repeat (dec n) 2))))))
               (combinacao pivot n)))
           (simplifica [pivot]
             (some identity (map #(let [exp (simplifica-n pivot %)]
                                    (when (seq exp)
                                      exp))
                              (range 1 5 1))))
           (gera-mapa-possibilidades []
             (reduce (fn [resposta pivot]
                       (assoc resposta pivot (simplifica pivot)))
               {}
               input))
           (simplifica-mapa-possibilidades []
             (let [mapa-possibilidades (gera-mapa-possibilidades)]
               (map (fn [map-entry-pivot]
                      (let [outras-possibilidades-pivot (filter (fn [expressao-simplificada-corrente]
                                                                  (when (and (> (count expressao-simplificada-corrente)
                                                                               (count (first (val map-entry-pivot))))
                                                                             (clojure.set/subset? expressao-simplificada-corrente (key map-entry-pivot)))
                                                                    expressao-simplificada-corrente))
                                                          (mapcat val (dissoc mapa-possibilidades (key map-entry-pivot))))]
                        (if (seq outras-possibilidades-pivot)
                          outras-possibilidades-pivot
                          (-> map-entry-pivot val))))
                 mapa-possibilidades)))
           (gera-possibilidades []
             (letfn [(gera-possibilidades-it [[primeiras-possibilidades & outras-possibilidades]]
                       (if (seq outras-possibilidades)
                         (for [possibilidade-corrente primeiras-possibilidades
                               outras-possibilidades-correntes (gera-possibilidades-it outras-possibilidades)]
                           (conj outras-possibilidades-correntes possibilidade-corrente))
                         (map hash-set primeiras-possibilidades)))]
               (set (gera-possibilidades-it (simplifica-mapa-possibilidades)))))]
      (reduce (fn [expressao-mais-simples expressao-corrente]
                (if (< (count expressao-mais-simples) (count expressao-corrente))
                  expressao-mais-simples
                  expressao-corrente))
        (gera-possibilidades)))))

(defcheck solution-8bd51f07
  (fn p140 [minterms] ; https://en.wikipedia.org/wiki/Quine%E2%80%93McCluskey_algorithm
    (letfn [(combine [x y]
              (let [s (clojure.set/difference x y)
                    t (clojure.set/difference y x)]
                (when (and (= 1 (count s))
                           (= 1 (count t))
                           (= (clojure.string/upper-case (str (first s)))
                             (clojure.string/upper-case (str (first t)))))
                  (clojure.set/intersection x y))))
            (find-primes [candidates prime-implicants]
              (if (empty? candidates) prime-implicants
                                      (loop [xs candidates
                                             implicants #{}
                                             ps prime-implicants]
                                        (if (empty? xs) (find-primes implicants ps)
                                                        (let [x (first xs)]
                                                          (let [zs (remove empty?
                                                                     (for [y candidates :when (not= x y)]
                                                                       (combine x y)))]
                                                            (if (empty? zs)
                                                              (recur (rest xs) implicants (conj ps x))
                                                              (recur (rest xs) (into implicants zs) ps))))))))
            (covers? [prime minterm] (= prime (clojure.set/intersection prime minterm)))
            (essential? [prime primes minterms]
              (true? (some true?
                       (map (fn [minterm]
                              (= 1 (count (filter #(covers? % minterm) primes))))
                         (filter #(covers? prime %) minterms)))))
            (minimize-primes [not-covered primes used-primes]
              (if (empty? not-covered) used-primes
                                       (let [essential (first (filter #(essential? % primes not-covered) primes))]
                                         (recur (remove #(covers? essential %) not-covered)
                                           (disj primes essential)
                                           (conj used-primes essential)))))]
      (minimize-primes minterms (find-primes minterms #{}) #{}))))

(defcheck solution-94fb6260
  (fn [algebra]
    (let [
          ; Lifted from clojure.set
          intersection (fn intersection
                         ([s1] s1)
                         ([s1 s2]
                          (if (< (count s2) (count s1))
                            (recur s2 s1)
                            (reduce (fn [result item]
                                      (if (contains? s2 item)
                                        result
                                        (disj result item)))
                              s1 s1)))
                         ([s1 s2 & sets]
                          (apply intersection (cons (intersection s1 s2) sets))))
          ; Lifted from clojure.set
          union (fn
                  ([] #{})
                  ([s1] s1)
                  ([s1 s2]
                   (if (< (count s1) (count s2))
                     (reduce conj s2 s1)
                     (reduce conj s1 s2))))
          ; Lifted from clojure.set
          difference (fn difference
                       ([s1] s1)
                       ([s1 s2]
                        (if (< (count s1) (count s2))
                          (reduce (fn [result item]
                                    (if (contains? s2 item)
                                      (disj result item)
                                      result))
                            s1 s1)
                          (reduce disj s1 s2)))
                       ([s1 s2 & sets]
                        (reduce difference s1 (conj sets s2))))
          as-implicants (fn [algebra]
                          (map #(hash-map :signature % :minterms #{%}) algebra))
          combinable? (fn [{x-sig :signature} {y-sig :signature}]
                        (let [target (- (count x-sig) 1)]
                          (->> (intersection x-sig y-sig) count (== target))))
          combine (fn [{x-sig :signature, x-minterms :minterms}
                       {y-sig :signature, y-minterms :minterms}]
                    { :signature (->> (difference x-sig y-sig)
                                   first
                                   str
                                   clojure.string/lower-case
                                   (conj (intersection x-sig y-sig)))
                     :minterms (union x-minterms y-minterms) })
          clean-primes (fn [{:keys [primes combos]}]
                         { :primes (->> primes
                                     (map (fn [{:keys [signature minterms]}]
                                            { :signature (->> signature
                                                           (filter symbol?)
                                                           set)
                                             :minterms minterms }))
                                     set)
                          :combos combos })
          sift (fn [xss]
                 (let [
                       accum-combos-with (fn [xs]
                                           (fn [acc ys]
                                             (if (combinable? xs ys)
                                               (conj acc (combine xs ys))
                                               acc)))
                       ]
                   (->> xss
                     (reduce (fn [acc xs]
                               (let [combos (reduce (accum-combos-with xs) #{} xss)]
                                 (if (empty? combos)
                                   (assoc acc :primes (-> (acc :primes) (conj xs)))
                                   (assoc acc :combos (-> (acc :combos) (union combos))))))
                       { :primes #{}, :combos #{} })
                     clean-primes)))
          find-prime-implicants (fn [algebra]
                                  (let [f (fn [{the-rest :combos}]
                                            (sift the-rest))]
                                    (->> { :primes #{}, :combos (as-implicants algebra) }
                                      (iterate f)
                                      (take-while #(or (seq (% :primes))
                                                       (seq (% :combos))))
                                      (map #(% :primes))
                                      (reduce union #{})
                                      set)))
          essential-prime-implicants (fn [prime-implicants]
                                       (let [contains (fn [minterm]
                                                        (fn [{pi-minterms :minterms}]
                                                          (pi-minterms minterm)))
                                             pivotal? (fn [minterm]
                                                        (->> prime-implicants
                                                          (filter (contains minterm))
                                                          count
                                                          (== 1)))
                                             pivotal-minterms (filter pivotal? algebra)
                                             essential? (fn [{pi-minterms :minterms}]
                                                          (some #(pi-minterms %) pivotal-minterms))]
                                         (filter essential? prime-implicants)))
          minimize (fn [prime-implicants]
                     ; In fact, this solution is incomplete.  One or more of the nonessential
                     ; prime implicants may be needed in the solution.  However, none of the
                     ; test cases currently exposes this, and I don't have access to append
                     ; additional test cases.
                     (->> prime-implicants
                       essential-prime-implicants
                       (map #(% :signature))
                       set))
          ]
      (->> algebra
        find-prime-implicants
        minimize))))

(defcheck solution-9709d5ac
  (fn [F]
    (letfn [(sym-diff [s t] (into (apply disj s t) (apply disj t s)))
            (grey [s t]
              (let [d (sym-diff s t)]
                (if (and (= 2 (count d)) (apply = (map (comp clojure.string/lower-case str) d)))
                  {:a s :b t :d d})))

            (pairs [C] (if-not (empty? C) (lazy-cat (map (partial vector (first C)) (rest C)) (pairs (rest C)))))

            (primes [C]
              (if-not (empty? C)
                (let [P (keep (partial apply grey) (pairs C))]
                  (into (reduce (fn [R {a :a b :b}] (disj R a b)) C P) (primes (set (map (fn [{a :a d :d}] (apply disj a d)) P)))))))

            (measure [C] (reduce (fn [R c] (+ R (count c))) 0 C))

            (R [C I]
              (if (empty? I) [C] (mapcat (fn [i] (R (conj C i) (rest I))) (first I))))]

      (let [P (primes F)
            I (map (fn [t] (filter (partial clojure.set/superset? t) P)) F)]
        (first (sort-by measure (R #{} I)))))))

(defcheck solution-9b0f99c9
  (fn [minterms]
    (letfn [(sort-ignore-case [xs]
              (sort (clojure.string/lower-case (apply str xs))))
            (match? [& terms]
              (apply = (map sort-ignore-case terms)))
            (combinable? [term1 term2]
              (and (not= term1 term2)
                   (match? term1 term2)
                   (= 1 (count (clojure.set/difference term1 term2)))))
            (reduce-terms [terms]
              (let [prime-terms (atom terms)
                    implicants (for [term1 terms
                                     term2 terms
                                     :when (combinable? term1 term2)]
                                 (do (swap! prime-terms disj term1)
                                     (clojure.set/intersection term1 term2)))]
                {:implicants (set implicants)
                 :prime-terms @prime-terms}))]
      (let [prime-terms
            (loop [acc ()
                   terms minterms]
              (if (empty? terms)
                (->> (map :prime-terms acc)
                  (apply clojure.set/union))
                (let [reduced-terms (reduce-terms terms)]
                  (recur (cons reduced-terms acc)
                    (:implicants reduced-terms)))))]
        (-> (for [minterm minterms
                  :let [xs (->> prime-terms
                             (filter #(clojure.set/superset? minterm %)))]
                  :when (= 1 (count xs))]
              (first xs))
          set)))))

(defcheck solution-9cdfe0b6
  (fn [xs]
    (letfn [(c [[s1 s2]]
              (let [x (clojure.set/difference s1 s2)]
                (if (and (= (count x) 1)
                         (= (clojure.string/upper-case (str x))
                           (clojure.string/upper-case (str (clojure.set/difference s2 s1)))))
                  [(clojure.set/intersection s1 s2) s1 s2])))
            (s [[xs ys]]
              (let [r (keep c (loop [xs xs r []]
                                (if (empty? xs) r
                                                (recur (rest xs) (concat r (map #(list (first xs) %) (rest xs)))))))
                    x (set (map first r))
                    y (reduce #(disj % (second %2) (nth %2 2)) xs r)]
                [x (clojure.set/union ys y)]))]
      (let [ts (apply clojure.set/union (last (take-while #(not (empty? (first %))) (iterate s [xs #{}]))))]
        (set (map first (filter #(= (count %) 1) (map (fn [x] (filter #(clojure.set/subset? % x) ts)) xs))))))))

(defcheck solution-9e29dfc9
  (fn [xs]
    (letfn
     [(qmdiffaux [xf yf]
        (let [x (first xf)
              y (first yf)
              d (clojure.set/difference x y)]
          (when (and (= (count x) (count y)) (= 1 (count d))
                     (= (clojure.string/upper-case (str d)) (clojure.string/upper-case (str (clojure.set/difference y x)))))
            [(clojure.set/intersection x y) (into (second xf) (second yf))])))
      (qmdiff [x xs]
        (let [d (keep #(qmdiffaux x %) xs)]
          (if (empty? d)
            #{x}
            (into #{} d))))
      (qmstage [xs]
        (into #{} (mapcat #(qmdiff %1 xs) xs)))
      (addextra [xs]
        (map (fn [x] [x #{x}]) xs))
      (removeoverlap [xs]
        (into #{} (map first (remove (fn [x]
                                       (let [removedx (remove #(= x %) xs)
                                             others (reduce #(into %1 (second %2)) #{} removedx)]
                                         (clojure.set/subset? (second x) others))) xs))))]
      (let [n (apply max (map count xs))]
        (removeoverlap (qmstage (qmstage (addextra xs))))))))

(defcheck solution-a3064736
  (fn [coll]
    (let [power-set (fn [s] (reduce #(into % (for [x %] (conj x %2)))
                              #{#{}} s))
          indexed (->> coll
                    (map #(zipmap (disj (power-set %) #{}) (repeat #{%})))
                    (apply merge-with into))]
      (->> indexed
        (sort-by (comp count first))
        (reduce (fn [acc [k v]]
                  (if (and (= (count v)
                             (->> (- (count (first v)) (count k))
                               (Math/pow 2)
                               long))
                           (not-any? (partial clojure.set/superset? k) acc))
                    (conj acc k)
                    acc))
          #{})
        (repeat 2)
        (apply reduce (fn [result x]
                        (if (= (count coll) (->> (disj result x) (mapcat indexed) set count))
                          (disj result x)
                          result)))))))

(defcheck solution-a6cdc89c
  ;using Quine&ndash;McCluskey algorithm
  (fn [s]
    (let [remerge (fn [m] (if (empty? m) {} (reduce #(merge-with (comp set concat) %1 %2) m)))
          gen (fn [ss] (remerge (for [x ss]
                                  (let [inte (fn [y] (clojure.set/intersection x y))
                                        diff (fn [y] (clojure.set/difference x y))
                                        upper (fn [i] (clojure.string/upper-case (str i)))]
                                    (remerge (map
                                               #(if (and (= (dec (count x)) (count (inte %)))
                                                         (= (set (map upper %)) (set (map upper x))))
                                                  (hash-map (inte %) (conj #{} x %)) {} )
                                               (disj ss x)))))))
          dist (fn [m] (set (keys (filter #(not= (val %)
                                             (clojure.set/intersection (val %) (set (reduce concat (vals (dissoc m (key %)))))))
                                    m))))]
      (loop [p s m {} his {} step 0]
        (cond
          (and (= 0 step) (empty? (gen p))) s
          (empty? (gen p)) (dist (remerge (map #(hash-map % (his %)) p)))
          :else (recur
                  (reduce conj (set (keys m)) (clojure.set/difference p (set (reduce concat (vals m)))))
                  (gen p)
                  (merge-with (comp set concat) his (if (empty? m) {}
                                                                   (remerge (for [x m]
                                                                              (if (not (empty? (his (first (val x)))))
                                                                                (hash-map (key x) (set (reduce concat (map his (val x)))))
                                                                                (hash-map (key x) (val x)))))))
                  (inc step)))))))

(defcheck solution-a9885476
  (fn my-veitch-please
    [func]
    (letfn [(compare-elem [bool-el to-simplify]
              (let [diff (clojure.set/difference bool-el to-simplify)
                    diff-other (clojure.set/difference to-simplify bool-el)]
                (when (and (= 1 (count diff))
                           (= (clojure.string/lower-case (str (first diff)))
                             (clojure.string/lower-case (str (first diff-other)))))
                  (disj bool-el (first diff)))))
            (simplify-elem [bool-el bool-func]
              (keep #(compare-elem bool-el %) bool-func))
            (simplify-all [[bool-func result]]
              (let [after-simplification (map #(vector % (simplify-elem % bool-func)) bool-func)
                    simplified (get-simplified after-simplification)
                    not-simplified (concat result (get-not-simplified after-simplification))]
                [simplified not-simplified]))
            (get-not-simplified [simplified]
              (into #{} (keep #(when (empty? (second %))
                                 (first %)) simplified)))
            (get-simplified [simplified]
              (into #{} (reduce concat #{} (keep #(when (not (empty? (second %)))
                                                    (second %)) simplified))))
            (from-subsets [result]
              (map #(filter (fn [current-res] (clojure.set/subset? current-res %)) result) func))]
      (loop [after-simplification [func #{}]]
        (if (empty? (first after-simplification))
          (into #{} (mapcat identity (filter #(= 1 (count %)) (from-subsets (second after-simplification)))))
          (recur
            (simplify-all after-simplification)))))))

(defcheck solution-bb6f96f5
  (fn [arr]
    (let [
          pow2 (fn [n] (int (/ (Math/log n) (Math/log 2))))
          c (count (first arr))
          clr (fn [i]
                (let [j (sort-by count (into [] i))]
                  (reduce (fn [lst n]
                            (if (seq (clojure.set/difference n (reduce into #{}
                                                                 (disj  lst n))))
                              lst (disj lst n))) i j)))
          grp (fn [lst cnt]
                (set
                  (for [i lst j lst
                        :let [ic  (count (apply clojure.set/intersection
                                           (into i j)))]
                        :when (and (= cnt  ic)
                                   (= (- c (pow2 (count (into i j)))) ic)
                                   (empty? (clojure.set/intersection i j))
                                   (#{2 4 8 16} (count (into i j))))]

                    (into i j))))
          z   (set (map #(conj #{} %) arr))]

      (->> (loop [q z cnt (dec (count (first (first z)))) lg [] ]
             (let [n (grp q cnt)]
               (if (empty? n) (clr q)
                              (recur  (into q n) (dec cnt) (cons [cnt n [:q q]]  lg)))))
        (map #(apply clojure.set/intersection %))
        set))))

(defcheck solution-c334f7f7
  (fn [fs]
    (let [first-pass (fn [s]
                       (let [lc         (comp clojure.string/lower-case str)
                             sorter (fn [s] (sort-by #(lc (str %)) s))
                             reducer (fn [x y] (let [l (map vector (sorter x) (sorter y))]
                                                 (set (map first (remove (fn [[a b]] (not= a b)) l)))))
                             neighbors? (fn [x y] (let [nl (reducer x y)]
                                                    (and (= (count x) (count y)) (= (map (comp lc str) (sorter x)) (map (comp lc str) (sorter y))) (= (count nl) (dec (count x))))))]
                         (loop [current s
                                removed #{}]
                           (let [ns (for [x current y current :when (neighbors? x y)] [x y])
                                 new-rem (clojure.set/union removed (set (apply concat ns)))
                                 new (set (map (fn [[a b]] (reducer a b)) ns))
                                 iter-result (clojure.set/difference (clojure.set/union current new) new-rem)]
                             (if (empty? new) iter-result
                                              (recur iter-result new-rem))))))
          power-set (fn [sets]
                      (loop [s sets r (conj #{sets} #{})]
                        (if (empty? s) r
                                       (recur (rest s) (into r (set (map #(conj % (first s)) r)))))))
          super-neighbors (fn [s] (set (mapcat (fn [e] (filter #(clojure.set/superset? % e) fs)) s)))]
      (first (sort-by count
               (for [s (power-set (first-pass fs)) :when (= (super-neighbors s) fs)] s))))))

(defcheck solution-c54b7083
  (fn karn [sets]
    (let [; If s1 and s2 are boolean complements (they differ by only 1 flipped var)
          ; return the common values and the original sets.
          bool-complement (fn [[s1 s2]]
                            (let [x12 (clojure.set/difference s1 s2)
                                  x21 (clojure.set/difference s2 s1)]
                              (if (and (= (count (clojure.set/difference s1 s2)) 1)
                                       (= (clojure.string/upper-case (str x12))
                                         (clojure.string/upper-case (str x21))))
                                [(clojure.set/intersection s1 s2) s1 s2]
                                )
                              )
                            )
          ; Return the common values of every boolean complement of pairing in sets,
          ; and sets with those removed.
          bool-reduce (fn [[sets acc]]
                        (let [; Filter the boolean complements for every pairing in sets.
                              comps (keep bool-complement (for [a sets, b (disj sets a)] [a b]))
                              ; Get the reduced boolean algebra.
                              x (set (map first comps))
                              ; Remove the complements from sets.
                              y (reduce #(disj % (second %2) (last %2)) sets comps)]
                          [x (clojure.set/union acc y)]
                          )
                        )
          ; Iteratively apply bool-reduce until no more reductions can be made.
          ; Return the last set generated.
          reduced-set (apply clojure.set/union
                        (last (take-while #(not (empty? (first %)))
                                (iterate bool-reduce [sets #{}]))))
          ; Apply the reduced-set to the original sets.
          sets* (map (fn [x] (filter #(clojure.set/subset? % x) reduced-set)) sets)
          ]
      ; Return the set of boolean algebra that are uniquely required to satisfy sets.
      (set (map first (filter #(= (count %) 1) sets*)))
      )
    ))

(defcheck solution-ce2dd207
  (fn quine [mins]
    (let [mins (set (map #(set(map str %)) mins))
          one-different (fn [s1 s2] (let [d1 (clojure.set/difference s1 s2)
                                          d2 (clojure.set/difference s2 s1)]
                                      (if (and (= 1 (count d1))
                                               (= (clojure.string/lower-case (str (first d1)))
                                                 (clojure.string/lower-case (str (first d2)))))
                                        (clojure.set/intersection s1 s2) false)))
          delete-other (fn [primes sets] (reduce #(if (some (fn [x] (clojure.set/subset? x %2)) primes) %1 (conj %1 %2)) #{} sets))
          get-primes (fn pri [sets]
                       (let [new-primes (reduce (fn [x y]
                                                  (clojure.set/union x (reduce #(if (one-different y %2)
                                                                                  (conj %1 (one-different y %2))
                                                                                  %1) #{} sets))) #{} sets)
                             ]
                         (if (empty? new-primes) sets (pri (clojure.set/union (delete-other new-primes sets) new-primes)))))
          primes (get-primes mins)
          dominate-s (fn [primes sets]
                       (apply clojure.set/union
                         (let [ss (set (map #(reduce (fn [x y] (if (clojure.set/subset? y %) (conj x y) x )) #{} primes) sets))]
                           (reduce #(if (some (partial clojure.set/superset? %2) (disj ss %2)) %1 (conj %1 %2)) #{} ss))))
          dominate-z (fn [primes sets] ; Too lazy, test does not need it
                       ())]
      (set (map #(set (map symbol %)) (dominate-s primes mins))))))

(defcheck solution-d8cd94ea
  (fn final-bool [sets]
    (letfn [(cmpr [s1 s2]
              (let [d1 (clojure.set/difference s1 s2)
                    d2 (clojure.set/difference s2 s1)
                    c1 (count d1)
                    c2 (count d2)
                    i (clojure.set/intersection s1 s2)]
                (if (and (not= s1 s2)
                         (<= (+ c1 c2) 2)
                         (or
                          (= (* c1 c2) 0)
                          (= (clojure.string/lower-case (str (first d1)))
                            (clojure.string/lower-case (str (first d2))))))
                  i)))
            (rpl [s seq-sets]
              (or (not-empty (keep identity
                               (map #(cmpr s %) seq-sets)))
                  (list s)))
            (rdc [seq-sets]
              (distinct (mapcat #(rpl % seq-sets) seq-sets)))
            (fnl [sets]
              (let [s (seq sets)
                    r (rdc s)]
                (if (= s r)
                  (set r)
                  (fnl r))))
            (overlap [sets]
              (first (or (not-empty
                           (filter identity (for [s sets
                                                  :let [r-sets (apply clojure.set/union (disj sets s))]]
                                              (if (= (clojure.set/intersection s r-sets) s)
                                                (disj sets s)))))
                         [sets])))]
      (let [f (fnl sets)]
        (if (not= (count (first sets)) (count (first f)))
          (overlap f)
          f)))))

(defcheck solution-de6e5cf6
  (fn [ss]
    (let [simplifiable? (fn [x y] (and
                                   (= (set (map #(clojure.string/upper-case (str %)) x))
                                     (set (map #(clojure.string/upper-case (str %)) y)))
                                   (= 1 (count (clojure.set/difference x y)))))

          simplifications (fn [ss] (set (for [x ss y ss :when (simplifiable? x y)] (clojure.set/intersection x y))))

          generates? (fn [ss mins]
                       (every? (fn [s] (some #(clojure.set/subset? % s) mins)) ss))]

      (->> (iterate simplifications ss)
        (take-while (partial not= #{}))
        reverse
        (reduce (fn [x y] (clojure.set/union x (set (remove #(generates? #{%} x) y)))))
        ((fn [s] (remove #(generates? ss (disj s %)) s)))
        set))))

(defcheck solution-e78ee594
  (fn [sats]
    (let [one-diff (fn [a b] (= 1 (count (clojure.set/difference a b))))
          neighbor? (fn [a b]
                      (and (empty? (clojure.set/intersection a b))
                           (every? #(some (partial one-diff %) b) a)))
          ones (map (partial conj #{}) sats)
          pair-up (fn [items]
                    (->> (iterate next items)
                      (take-while identity)
                      (mapcat (fn [[b & bs]]
                                (keep #(when (neighbor? b %)
                                         (into b %)) bs)))))
          twos (pair-up ones)
          fours (pair-up twos)
          eights (pair-up fours)
          sixteens (pair-up eights)
          big-to-small (concat sixteens eights fours twos ones)
          boxes (loop [coverage (zipmap sats (map #(for [b big-to-small
                                                         :when (some #{%} b)]
                                                     b)
                                               sats))
                       boxes []]
                  (if (empty? coverage)
                    boxes
                    (let [[min-cover [largest-box]] (apply min-key (comp count val) coverage)]
                      (recur (reduce dissoc coverage largest-box)
                        (conj boxes largest-box))))
                  )
          ]
      (set (map (partial apply clojure.set/intersection) boxes)))))

(defcheck solution-eb8b3061
  (fn k-map [s]
    (letfn [(is-eq-symbol? [a b] (->> [a b] (map #(clojure.string/upper-case (str %))) set count (= 1)))
            (impl [s1 s2]
              (let [c1 (clojure.set/difference s1 s2)
                    c2 (clojure.set/difference s2 s1)]
                (if (and (= 1 (count c1) (count c2))
                         (is-eq-symbol? (first c1) (first c2)))
                  (clojure.set/intersection s1 s2)
                  #{})))]
      (let [cds1 (-> s first count dec)
            step1 (set (for [si s sj s
                             :let [u (impl si sj)]
                             :when (= (count u) cds1)]
                         u))
            cds2 (dec cds1)
            [ts1 ts2] (->> (for [si step1 sj step1
                                 :let [u (impl si sj)]
                                 :when (= (count u) cds2)]
                             [u #{si sj}])
                        set
                        (reduce (fn [[a1 a2] [k v]]
                                  [(conj a1 k) (reduce conj a2 v)])
                          [#{} #{}]))
            step2 (clojure.set/union ts1 (clojure.set/difference step1 ts2))
            tab (->> step2
                  (reduce (fn [a st]
                            (reduce (fn [a i]
                                      (if (clojure.set/subset? st i)
                                        (update-in a [st] #(conj % i)) a))
                              a s))
                    (zipmap step2 (repeat (count step2) #{})))
                  vec)
            tab (filter (fn [[x y]]
                          (->> tab
                            (filter #(-> % first (not= x)))
                            (map second)
                            (into [y])
                            (apply clojure.set/difference)
                            empty?
                            not)) tab)
            ignored (->> tab
                      (reduce #(-> %2 second (clojure.set/union %1)) #{})
                      (clojure.set/difference s))
            ]
        #_(println ignored)
        (loop [a #{} tab tab]
          (let [[[x sy] & r] (sort-by (comp count second) > tab)
                a (conj a x)]
            (if (empty? r) (disj (clojure.set/union a ignored) nil)
                           (recur a (map (fn [[x y]] [x (clojure.set/difference y sy)]) r))
                           )))
        ))))

(defcheck solution-f0ea3e04
  (fn [mt]
    (letfn [(convert [t i] {:g #{i} :t (sort-by #(-> % name clojure.string/lower-case) t)})
            (count-diff [x y] (->> (map vector x y) (filter (partial apply not= )) count))
            (combine [x y]
              (when (= (count-diff (x :t) (y :t)) 1)
                [{:g (into (x :g) (y :g))
                  :t (map #(if (not= %1 %2) '- %1) (x :t) (y :t))}]))
            (combinations [t ts]
              (let [cs (mapcat #(combine t %) ts)]
                [cs (when (empty? cs) [t])]))
            (reduze [terms]
              (let [r  (map #(combinations % terms) terms)
                    c  (set (mapcat first r))
                    p  (mapcat second r)]
                [c p]))
            (primes [t]
              (loop [t (map-indexed #(convert %2 %) t)
                     p []]
                (let [[a b] (reduze t)]
                  (if (empty? a)
                    (into p b)
                    (recur a (into p b))))))
            (coverage [t ts] (reduce #(remove (%2 :g) %) (t :g) (remove #{t} ts)))]
      (let [p    (primes mt)
            cov  (filter #(not-empty (coverage % p)) p)
            r    (set (map #(->> % :t (remove #{'-}) set) cov))]
        r
        )
      )
    ))

(defcheck solution-fb01139e
  (fn veitch [xs]
    (letfn [
            (strip-product [xs k]
              (letfn
               [(powerline [x xs] (reduce #(conj %1 (conj x %2)) #{} xs))
                (powerup [c xs result]
                  (if (= c k)
                    result
                    (recur (inc c)
                      xs
                      (apply clojure.set/union
                        (conj (map #(powerline % xs) (filter #(= (count %) c) result))
                          result)))))]
                (filter #(= (count %) k)
                  (powerup 0 (apply clojure.set/union xs) #{#{}}))))
            (merge-v [xs k]
              (reduce (fn [acc nx]       ;nx for next x
                        (let [nxv (reduce #(if (clojure.set/superset? %2 nx)
                                             (conj % %2)
                                             %)
                                    #{} xs)]
                          (if (empty? nxv)
                            acc
                            (assoc acc nx nxv))))
                {}
                (strip-product xs k)))
            (go-veitch
              [xs]
              (loop [ks (range 1 (inc (count (first xs))))
                     ksv #{}]
                (if (empty? ks)
                  ksv
                  (recur (rest ks)
                    (reduce (fn [acc e]
                              (if (= (count (second e))
                                    (int (Math/pow 2 (- (count (first xs))
                                                        (first ks)))))
                                (conj acc (first e))
                                acc))
                      ksv
                      (merge-v xs (first ks)))
                    ))
                ))
            (simplify [xs]
              (let [maxcount (apply max (map count xs))]
                (loop [xs (sort-by count xs)
                       result {}]
                  (if (empty? xs) result ; shouldn't execute here
                                  (recur (rest xs)
                                    (#(if (> (count %) 0)
                                        (assoc result #{(first xs)} %)
                                        result
                                        )
                                     (reduce
                                       (fn [acc e] (if (and (= (count e) maxcount)
                                                            (clojure.set/superset? e (first xs)))
                                                     (conj acc e)
                                                     acc))
                                       #{} xs))))
                  )
                ))
            (merge-ss [fss ss]
              (reduce
                (fn [acc e]
                  (merge
                    acc
                    (reduce
                      #(if (clojure.set/superset? (first %2) (first e))
                         %
                         (assoc % (clojure.set/union (first e)
                                    (first %2))
                                  (clojure.set/union (second e)
                                    (second %2))))
                      {}
                      (dissoc ss (first e)))
                    ))
                {}
                fss))
            (elimate [xs fss ss]             ;use least ss key to satisfy xs
              (loop [rss ss
                     result []]
                (cond (= (second (first rss)) xs) (recur (rest rss) (conj result (ffirst rss)))
                      (and (empty? rss) (empty? result)) (elimate xs fss (merge-ss fss ss))
                      (empty? rss) result
                      :else (recur (rest rss) result))
                )
              )
            ]

      (apply min-key #(apply + (map count %)) (#(elimate xs % %) (simplify (go-veitch xs))))
      )))

(defcheck solution-fdc6111f
  (fn veitch [bafs]
    (let [sym-diff (fn [a b]
                     (clojure.set/union
                       (clojure.set/difference a b)
                       (clojure.set/difference b a)))
          complementary? (fn [a b]
                           (->> (sym-diff a b)
                             (map #(clojure.string/capitalize (str %)))
                             distinct
                             count
                             (= 1)))
          simplify (fn [bafs residue]
                     (let [simplification (for [a bafs b (disj bafs a) :when (complementary? a b)]
                                            [(clojure.set/intersection a b) a b])
                           to-remove (set (mapcat rest simplification))
                           more-residue (clojure.set/difference bafs to-remove)
                           to-simplify (set (map first simplification))]
                       (if (= bafs to-simplify) residue
                                                (recur to-simplify (clojure.set/union residue more-residue)))))
          simpflified (simplify bafs #{})
          reapplied (map (fn [s] (filter #(clojure.set/subset? % s) simpflified)) bafs)
          ]
      (set (map first (filter #(= (count %) 1) reapplied))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-140))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))


