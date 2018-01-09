(ns coal-mine.problem-73
  (:require [coal-mine.checks :refer [defcheck-73] :rename {defcheck-73 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-102a59f9
  #(last (some #{[:o :o :o] [:x :x :x]}
           (partition 3
             (map
               (vec (flatten %))
               [0 1 2 3 4 5 6 7 8
                0 3 6 1 4 7 2 5 8
                0 4 8 2 4 6])))))

(defcheck solution-107e6f6f
  (fn [board]
    (let [rows (concat
                board
                (apply map vector board)
                (list
                  (map-indexed (fn [i r] (nth r i)) board)
                  (map-indexed (fn [i r] (nth r i)) (reverse board))))]
      (some #{:x :o}
        (map #(when (apply = %) (first %))
          rows)))))

(defcheck solution-10ca3972
  (fn [board]
    (ffirst
      (filter
        #(and (apply = %)
              (not= :e (first %)))
        (map
          (fn [line] (map #(get-in board %) line))
          [[[0 0] [0 1] [0 2]]
           [[1 0] [1 1] [1 2]]
           [[2 0] [2 1] [2 2]]
           [[0 0] [1 0] [2 0]]
           [[0 1] [1 1] [2 1]]
           [[0 2] [1 2] [2 2]]
           [[0 0] [1 1] [2 2]]
           [[2 0] [1 1] [0 2]]])))))

(defcheck solution-1201d821
  (fn winning-game [game]
    (let [check-trio (fn [game xs ys] (if (apply = (map #(nth (nth game %2) %1) xs ys))
                                        (nth (nth game (first ys)) (first xs))
                                        nil))
          winning-vectors (concat
                           (list (list (range 3) (range 3)))
                           (list (list (reverse (range 3)) (range 3)))
                           (map #(list (range 3) (repeat 3 %)) (range 3))
                           (map #(list (repeat 3 %) (range 3)) (range 3)))
          result (remove #(= % :e) (filter identity (map #(check-trio game (first %) (second %)) winning-vectors)))]
      (if (empty? result)
        nil
        (first result)))))

(defcheck solution-1201eaf5
  (fn [board]
    (letfn [(lines [rows]
              (concat rows
                      (for [col [0 1 2]]
                        (map #(get % col) rows))
                      [(for [[x y] [[0 0] [1 1] [2 2]]]
                         (get (get rows y) x))]
                      [(for [[x y] [[0 2] [1 1] [2 0]]]
                         (get (get rows y) x))]))]
      (->> (lines board)
        (map #(into #{} %))
        (remove #(= #{:e} %))
        (filter #(= 1 (count %)))
        (ffirst)))))

(defcheck solution-127a3a11
  (fn [board]
    (let [ get (fn [[y x]] (nth (nth board y) x))
          xwin (repeat 3 :x)
          owin (repeat 3 :o)
          win (fn [line]
                (let [sym (map get line)]
                  (or (= sym xwin) (= sym owin))))
          coords [['(0 0) '(0 1) '(0 2)]
                  ['(0 0) '(1 1) '(2 2)]
                  ['(0 0) '(1 0) '(2 0)]
                  ['(0 1) '(1 1) '(2 1)]
                  ['(0 2) '(1 2) '(2 2)]
                  ['(1 0) '(1 1) '(1 2)]
                  ['(2 0) '(1 1) '(0 2)]
                  ['(2 0) '(2 1) '(2 2)]]
          ]
      (loop [input (seq coords) line (first input) ]
        (if (= input '())
          nil
          (if (win line)
            (get (first line))
            (recur (rest input) (first (rest input)))))))))

(defcheck solution-12899e7c
  (fn tictactoe [rows]
    (let [columns (partition 3 (apply interleave rows))
          diagonal1 [[(get (first rows) 0) (get (second rows) 1) (get (get rows 2) 2)]]
          diagonal2 [[(get (first rows) 2) (get (second rows) 1) (get (get rows 2) 0)]]
          all-sets (concat rows columns diagonal1 diagonal2)]
      (if (some #(every? #{:x} %) all-sets)
        :x
        (if (some #(every? #{:o} %) all-sets)
          :o
          nil)))))

(defcheck solution-128f879a
  (fn [b]
    (let
     [chk #(let [s (into #{} %)] (if (and (not (contains? s :e)) (= (count s) 1)) (first %) nil))
      d [0 1 2]]

      (some #(when (not (nil? %)) %) (map chk
                                       (concat
                                        [(map #(get (get b %) %) d)
                                         (map #(get (get b %) (- 2 %)) d)]
                                        (map (fn [i] (map #(get % i) b)) d)
                                        b))))))

(defcheck solution-1291f5
  #(some {[:x :x :x] :x [:o :o :o] :o}
     (concat % (apply map list %)
             (for [d [[[0 0] [1 1] [2 2]] [[2 0] [1 1] [0 2]]]]
               (for [[x y] d] ((% x) y))))))

(defcheck solution-12d33a3
  (fn any-winner [board]
    (letfn [(winner [v]
              (cond
                (every? #(= :x %) v)
                :x

                (every? #(= :o %) v)
                :o

                :else
                nil))
            (diag-winner [board]
              (map winner
                [(map-indexed (fn [i b] (nth b i)) board)
                 (map-indexed (fn [i b] (nth b (- 2 i))) board)]))
            (rows-winner [board]
              (map winner board))
            (cols-winner [board]
              (rows-winner (apply map vector board)))]
      (first (filter identity (mapcat #(% board) [rows-winner cols-winner diag-winner]))))))

(defcheck solution-132055c8
  (fn ttt [board]
    (let [ series (conj board (map first board) (map second board) (map last board)
                    [(first (first board)) (second (second board)) (last (last board))]
                    [(last (first board)) (second (second board)) (first (last board))])
          ]
      (cond
        (some true? (map (fn [x] (every? #(= :x %) x)) series)) :x
        (some true? (map (fn [x] (every? #(= :o %) x)) series)) :o
        :else nil
        ))
    ))

(defcheck solution-1370cebf
  (fn [b]
    (let [possibilities [[[0 0] [0 1] [0 2]]
                         [[1 0] [1 1] [1 2]]
                         [[2 0] [2 1] [2 2]]
                         [[0 0] [1 0] [2 0]]
                         [[0 1] [1 1] [2 1]]
                         [[0 2] [1 2] [2 2]]
                         [[0 0] [1 1] [2 2]]
                         [[0 2] [1 1] [2 0]]]
          read-board (fn [possibility]
                       (for [[i j] possibility] ((b i) j)))
          winner (fn [p]
                   (and (apply = p)
                        (not= (first p) :e)))
          the-wins (filter winner (map read-board possibilities))]
      (if (seq the-wins)
        (ffirst the-wins)
        nil))))

(defcheck solution-13f53a2d
  (fn [[[x1 _ x2][_ o _][y1 _ y2] :as c]]
    (->> (map (fn [n] (map #(nth % n) c)) (range 3))
      (concat c [[x1 o y2] [x2 o y1]])
      (some {[:x :x :x] :x [:o :o :o] :o}))))

(defcheck solution-13f98f20
  (fn score [board]
    (letfn [(to-matrix [board]
              (mapv
                (fn [row]
                  (mapv #(case %
                           :e 0
                           :x 1
                           :o -1)
                    row))
                board))

            (major [matrix]
              (mapv
                #(nth (nth matrix %) %)
                (range (count matrix))))

            (minor [matrix]
              (mapv
                #(nth (nth matrix %) (- 2 %))
                (range (count matrix))))

            (diagonals [matrix]
              [(major matrix) (minor matrix)])

            (columns [matrix]
              (apply mapv vector matrix))

            (solutions [matrix]
              (reduce into [matrix
                            (diagonals matrix)
                            (columns matrix)]))]
      (case (->> board
              to-matrix
              solutions
              (map (partial reduce +))
              (filter #{3 -3})
              first)
        nil nil
        3 :x
        -3 :o))))

(defcheck solution-13ff8739
  (fn [rows]
    (let
     [cols    (apply map vector rows)
      diags   (map #(map get rows %) [[0 1 2][2 1 0]])
      all     (concat rows cols diags)
      equals  (map #(if (apply = %) (first %)) all)
      ]
      (some #{:x :o} equals))))

(defcheck solution-1408064f
  (fn [board]
    (let [win? (comp #(and % (first %)) #{[:x :x :x] [:o :o :o]})
          board' (apply mapv vector board)
          p (fn [i j] (get-in board [i j]))]
      (or
       (some win? board)
       (some win? board')
       (win? [(p 0 0) (p 1 1) (p 2 2)])
       (win? [(p 2 0) (p 1 1) (p 0 2)])))))

(defcheck solution-1434616
  (fn [coll]
    (->> coll
      (concat (apply map vector coll))
      (cons (list (first (first coll)) (second (second coll)) (last (last coll))))
      (cons (list (first (last coll)) (second (second coll)) (last (first coll))))
      (filter #(or (= [:x :x :x] %) (= [:o :o :o] %)))
      ffirst)))

(defcheck solution-14477ad0
  (fn [x]
    (some #(when (and (= (.indexOf % :e) -1) (apply = %)) (first %)) (let [c (second (second x))]
                                                                       (concat (apply map vector x)
                                                                               (conj x (vector (ffirst x) c (nth (nth x 2) 2))
                                                                                 (vector (nth (nth x 0) 2) c (nth (nth x 2) 0))))))))

(defcheck solution-148d792b
  (fn [rows]
    (let [cols (apply map vector rows)
          diag (fn [x] (map-indexed #(nth %2 %) x))
          diags [(diag rows) (diag (map reverse rows))]
          lines (concat rows cols diags)
          line-sets (map set lines)]
      (first (some #{#{:x} #{:o}} line-sets)))))

(defcheck solution-14a5ffca
  (fn at [b]
    (letfn [(hori [b]
              ((comp first first)
               (filter #(or (= % [:x 3]) (= % [:o 3]))
                 (mapcat #(vec (frequencies %)) b))))
            (vert [b]
              (let [new-b (apply map vector b)]
                (hori new-b)))
            (diag [b]
              (let [fb (flatten b)
                    dl [(nth fb 0) (nth fb 4) (nth fb 8)]
                    dr [(nth fb 2) (nth fb 4) (nth fb 6)]]
                (hori [dl dr])))]
      (some identity  [(hori b) (vert b) (diag b)]))))

(defcheck solution-15423673
  (fn [x]
    (first (filter
             (fn [p]
               (some (fn [l] (every? #(= p (nth (flatten x) %)) l))
                 [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]))
             [:x :o]))))

(defcheck solution-1571a6b5
  (fn [m]
    (let [positions [[[0 0] [0 1] [0 2]] [[1 0] [1 1] [1 2]] [[2 0] [2 1] [2 2]] [[0 0] [1 0] [2 0]] [[0 1] [1 1] [2 1]] [[0 2] [1 2] [2 2]] [[0 0] [1 1] [2 2]] [[0 2] [1 1] [0 2]]]]
      (some (fn [l]
              (let [result (map #(get-in m %) l)]
                (cond
                  (every? #(= :o %) result) :o
                  (every? #(= :x %) result) :x)
                )) positions))))

(defcheck solution-15dfa787
  (fn f [xo]
    (let [d #(for [i (range 0 3)] (get-in (vec %) [i i]))
          ox (apply mapv vector xo)]
      (->> (concat xo ox [(d xo) (d (reverse xo))])
        (map set)
        (some #{#{:x} #{:o}})
        (first)))))

(defcheck solution-15e84b2d
  (fn [board]
    (cond
      (some #(= [:x :x :x] %) board) :x
      (some #(= [:x :x :x] %) (apply map (fn [x y z] [x y z]) board)) :x
      (= (first (first board)) (second (second board)) (last (last board)) :x) :x
      (= (last (first board)) (second (second board)) (first (last board)) :x) :x
      (some #(= [:o :o :o] %) board) :o
      (some #(= [:o :o :o] %) (apply map (fn [x y z] [x y z]) board)) :o
      (= (first (first board)) (second (second board)) (last (last board)) :o) :o
      (= (last (first board)) (second (second board)) (first (last board)) :o) :o
      :else nil)))

(defcheck solution-168f6701
  (fn [game]
    (letfn [(extract [c] (map nth game c))
            (won? [s] (cond (= s [:x :x :x]) :x
                            (= s [:o :o :o]) :o
                            :else nil))]
      (->> (concat game (map extract [[0 0 0] [1 1 1] [2 2 2] [0 1 2] [2 1 0]]))
        (filter won?)
        first first))))

(defcheck solution-16b6ea76
  (fn [[a b c]]
    (letfn [
            (check-diag-win[cand a b c]
              (if (= cand (second b))
                (or
                 (and (= cand (first a))
                      (= cand (last c)))
                 (and (= cand (last a))
                      (= cand (first c))))
                nil))

            (check-col-win[cand a b c col]
              (and
               (= (nth a col) cand)
               (= (nth b col) cand)
               (= (nth c col) cand)))

            (check-row-win[cand a b c]
              (or (every? #(= cand %) a)
                  (every? #(= cand %) b)
                  (every? #(= cand %) c)))

            (check-cand[cand a b c]
              (if (or
                   (check-col-win cand a b c 0)
                   (check-col-win cand a b c 1)
                   (check-col-win cand a b c 2)
                   (check-row-win cand a b c)
                   (check-diag-win cand a b c))
                cand
                nil))
            ]
      (or (check-cand :x a b c)
          (check-cand :o a b c)))))

(defcheck solution-170b487e
  (fn [board]
    (let [row (fn [r] (board r))
          col (fn [c] (map #(% c) board))
          diag (fn [coord] (map #(get-in board %) coord))
          d1 (diag [[0 0] [1 1] [2 2]])
          d2 (diag [[0 2] [1 1] [2 0]])
          r3 (range 3)
          lines (concat
                 (map row r3)
                 (map col r3)
                 [d1 d2])
          win? (fn [[x & xs]] (if (and
                                   (not= x :e)
                                   (every? #(= x %) xs))
                                x))]
      (->> lines
        (map win?)
        (filter (complement nil?))
        first))))

(defcheck solution-1769f3a7
  (fn [b] (let [p [[[0 0] [1 1] [2 2]]
                   [[0 2] [1 1] [2 0]]
                   [[0 0] [1 0] [2 0]]
                   [[0 1] [1 1] [2 1]]
                   [[0 2] [1 2] [2 2]]]]
            (some {[:x :x :x] :x [:o :o :o] :o} (concat (map #(map (fn [[x y]] ((b x) y)) %) p) b)))))

(defcheck solution-176b73a8
  (fn [board]
    (cond
      (seq (filter #(every? #{:x} %) board)) :x
      (seq (filter #(every? #{:o} %) board)) :o

      (->> (map first board) (every? #{:x})) :x
      (->> (map first board) (every? #{:o})) :o

      (->> (map last board) (every? #{:x})) :x
      (->> (map last board) (every? #{:o})) :o

      (->> (map (comp first rest) board) (every? #{:x})) :x
      (->> (map (comp first rest) board) (every? #{:o})) :o

      (every? #{:x} ((juxt (comp first first) (comp second second) (comp last last)) board)) :x
      (every? #{:o} ((juxt (comp first first) (comp second second) (comp last last)) board)) :o

      (every? #{:o} ((juxt (comp last first) (comp second second) (comp first last)) board)) :o
      (every? #{:x} ((juxt (comp last first) (comp second second) (comp first last)) board)) :x

      :else nil)))

(defcheck solution-176bf51f
  (fn ttt [board]
    (let  [rows board
           columns (apply map vector board)
           diags [(map-indexed (fn [i j] (get-in board [i j])) [0 1 2]) (map-indexed (fn [i j] (get-in board [i j])) [2 1 0])]
           all (concat rows columns diags)
           winner (some (fn [t] (if (every? (partial = (first t)) t) (first t))) all)]
      (if (#{:x :o} winner) winner))))

(defcheck solution-182c9bbb
  (fn [board]
    (let [horizontal (map identity board)
          vertical (partition 3 (for [x (range (count board)) c board] (nth c x)))
          diag1 (for [x (range (count board))] (nth (nth board x) x))
          diag2 (for [x (range (count board))] (nth (reverse (nth board x)) x))
          triples (concat horizontal vertical [diag1] [diag2])]
      (if-let [v
               (first (filter
                        #(and (every? (fn [n] (= n (first %)) ) %)(not= (first %) :e))
                        triples))]
        (first  v)
        )
      )))

(defcheck solution-18913b3c
  (fn tictactoe [board] (
                          let
                         [
                          starting-board (apply hash-map(interleave (range)(flatten board)))
                          rows (partition 3 (range 10))
                          cols (apply map list rows)
                          diags '((0 4 8)(2 4 6))
                          winposition (flatten(concat rows cols diags))
                          winvalues  (partition 3 (map #(get starting-board  %) winposition))
                          haswin (fn [player] (seq (filter #(= (repeat 3 player) %) winvalues)))
                          ]
                          (if (haswin :x) :x (if (haswin :o) :o nil) )

                          )))

(defcheck solution-18a5a395
  (fn [col]
    (letfn [(check [posl]
              (reduce (fn [ret c] (let [[x y z] c]
                                    (if (and (= x y z)
                                             (or (= x :x )
                                                 (= x :o ))
                                             )
                                      x ret)))
                nil posl))
            (intlv [col]
              (partition 3 (apply interleave col)))

            (diag [col]
              (for [x [0 1 2]] (nth (nth col x) x)))
            (rdiag [col]
              (for [x [0 1 2]] (nth (nth col x) (- 2 x))))
            ]
      (if-let [ret (check col)]
        ret
        (if-let [ret (check (intlv col))]
          ret
          (if-let [ret (check (vector (diag col)))]
            ret
            (if-let [ret (check (vector (rdiag col)))]
              ret
              nil)))))))

(defcheck solution-190a4d08
  (fn [board]
    (some (fn [player]
            (if (or (some (partial every? (partial = player))
                      board)
                    (some (fn [col]
                            (every? #(= player (% col))
                              board))
                      [0 1 2])
                    (and (= (get-in board [1 1]) player)
                         (or (= (get-in board [0 2])
                               (get-in board [2 0])
                               player)
                             (= (get-in board [0 0])
                               (get-in board [2 2])
                               player))))
              player))
      [:o :x])))

(defcheck solution-1a1c22d7
  (fn winner [[one two three :as rows]]
    (let [cols (apply map list rows)
          bdiag (for [i (range 3)]
                  (get-in rows [i i]))
          fdiag (for [i (range 3)]
                  (get-in rows [i (- 2 i)]))
          triplets (concat rows cols [bdiag fdiag])]
      (cond
        (some #{(repeat 3 :x)} triplets) :x
        (some #{(repeat 3 :o)} triplets) :o
        :else nil))))

(defcheck solution-1affcbbf
  (fn gen-winner
    [m]
    (letfn [(win?
              [player xs]
              (some #(apply = player %) xs))
            (winner?
              [player]
              (when (or
                     (win? player m)
                     (win? player (apply map #(vec %&) m))
                     (win? player (let [sz (count m)]
                                    [(for [i (range sz)]
                                       ((m i) i))
                                     (for [i (range sz)]
                                       ((m i) (- sz i 1)))])))
                player))]
      (or (winner? :x)
          (winner? :o)))))

(defcheck solution-1b443037
  (fn [board]
    (let [r (range 3)
          hor board
          ver (map
                (fn [idx]
                  (reduce
                    #(conj %1 (nth %2 idx))
                    []
                    board))
                r)
          dia (map
                (fn [idxv]
                  (reduce
                    conj
                    []
                    (map
                      nth
                      board
                      idxv)))
                [r (reverse r)])
          all (concat hor ver dia)
          direct?
          (fn [e]
            (some
              (fn [vec]
                (every? #(= % e) vec))
              all))]
      (cond (direct? :o) :o
            (direct? :x) :x
            :else nil)
      )
    ))

(defcheck solution-1c2c67f9
  (fn winner
    [board]
    (letfn [(pos [[i j]] ((board i) j))
            (same [ps]
              (let [vs (map pos ps)]
                (cond
                  (apply = :x vs) :x
                  (apply = :o vs) :o
                  :else nil)))]
      (some same [[[0 0] [0 1] [0 2]]
                  [[1 0] [1 1] [1 2]]
                  [[2 0] [2 1] [2 2]]
                  [[0 0] [1 0] [2 0]]
                  [[0 1] [1 1] [2 1]]
                  [[0 2] [1 2] [2 2]]
                  [[0 0] [1 1] [2 2]]
                  [[2 0] [1 1] [0 2]]]))))

(defcheck solution-1ce63aac
  (fn [board]
    (let [row-win (fn [board]
                    (let [rows (map distinct board)]
                      (reduce
                        (fn [winner row]
                          (if-not (nil? winner)
                            winner
                            (if (and (= (count row) 1) (not= (first row) :e))
                              (first row)
                              nil
                              )
                            )
                          )
                        nil
                        rows
                        )
                      )
                    )
          col-win (fn [board]
                    (let [board-90 [(map first board)
                                    (map second board)
                                    (map last board)]]
                      (row-win board-90)
                      )
                    )
          diag-win (fn [board]
                     (if (= (second (second board)) :e)
                       nil
                       (if (= (first (first board))
                             (second (second board))
                             (last (last board)))
                         (first (first board))
                         (if (= (last (first board))
                               (second (second board))
                               (first (last board)))
                           (last (first board))
                           nil
                           )
                         )
                       )
                     )
          ]
      (if-let [row (row-win board)]
        row
        (if-let [col (col-win board)]
          col
          (if-let [diag (diag-win board)]
            diag
            nil
            )
          )
        )
      )
    ))

(defcheck solution-1dce0eb4
  (fn [x]
    (let [rows (map #(distinct %) x)
          cols (map #(distinct %) (apply map vector x))
          tlbr (distinct (map-indexed #(nth %2 %1) x))
          trbl (distinct (map-indexed #(nth %2 (- (count %2) %1 1)) x))
          all (conj (concat rows cols) tlbr trbl)]
      (cond
        (some #(= [:x] %) all) :x
        (some #(= [:o] %) all) :o
        :else nil))))

(defcheck solution-1de29712
  (fn [s]
    (let [dag (fn [s dir]
                (let [[start end f f2]
                      (if (= dir :left-right)
                        [0 (count s) inc #(identity %)]
                        [(dec (count s)) -1 dec  #(- (dec (count s)) %)])]
                  (loop [cnt start ret []]
                    (if (= cnt end)
                      (vec ret)
                      (recur (f cnt) (conj ret (nth (nth s cnt) (f2 cnt) )))))))
          col (fn [s i]
                (vec (map #(nth % i) s)))
          cols (fn [s]
                 (vec (map #(col s %) (range (count s)))))
          expand-board (fn [s]
                         (concat s (cols s) [(dag s :left-right)] [(dag s :right-left)]))]
      (let [res (->> s expand-board (map distinct) (filter #(= (count %) 1)) (filter #(not= (first %) :e)) flatten first)]
        (if (not= :e res)
          res
          nil)))))

(defcheck solution-1dedc21
  (fn [ttt] (let [c (concat ttt (map (fn [index] (map #(% %2) ttt index)) [[0 0 0] [1 1 1] [2 2 2] [0 1 2] [2 1 0]]))
                  t (fn [m item] (some identity (map #(apply = (conj % item)) m)))]
              (cond (t c :o) :o
                    (t c :x) :x
                    :else nil))))

(defcheck solution-1e1a6fd2
  (fn [b]
    (let [hh b
          vv (for [v [0 1 2]](map #(% v) b))
          x1 (map #(get-in b [% %]) [0 1 2])
          x2 (map #(get-in b [% (- 2 %)]) [0 1 2])]
      (->> (conj (concat hh vv) x1 x2)
        (map set)
        (filter #(= 1(count %)))
        (some #(or (:x %) (:o %)))
        )
      )))

(defcheck solution-1ec7da83
  (fn __ [ [ [a b c] [d e f] [g h i]] ]
    (let [ grid [[a b c][d e f][g h i]
                 [a d g][b e h][c f i]
                 [a e i][g e c]]
          answer (first (first (filter (fn [[x y z]](= x y z)) grid ))) ]
      (some #{answer} [:x :o]))))

(defcheck solution-1f0fd9d7
  (fn [g]
    (let [di1 #(map get % (range))
          di2 #(di1 (reverse %))
          d1 (di1 g) d2 (di2 g)
          g' (apply map vector g)
          win? #(some (partial every? (partial = %)) %2)]
      (condp win? (list* d1 d2 (concat g g'))
        :x :x
        :o :o
        nil))))

(defcheck solution-1f260c5f
  (fn [v]
    (->> (-> (apply map (fn [x1 x2 x3] [x1 x2 x3]) v)
           (#(into % v))
           vec
           (conj (first (reduce (fn [[a i] x] [(conj a (nth x i)) (inc i) ]) [[] 0] v)))
           (conj (first (reduce (fn [[a i] x] [(conj a (nth x i)) (dec i) ]) [[] 2] v)))
           )

      (filter #(apply = %))
      first
      first
      (#(if (= % :e) nil %))
      )))

(defcheck solution-1fc3be39
  (fn [b]
    (letfn [(t [i j]
              (nth (nth b i) j))
            (w [c]
              (or (some #(= % [c c c]) b)
                  (= c (t 0 0) (t 1 1) (t 2 2))
                  (= c (t 0 2) (t 1 1) (t 2 0))
                  (some #(= c (t 0 %) (t 1 %) (t 2 %)) (range 3))))]
      (if (w :o)
        :o
        (if (w :x)
          :x
          nil)))))

(defcheck solution-20e8e1e4
  (fn [t]
    (some (fn [r]
            (let [f (first r)]
              (and
               (and (not= f :e)
                    (every? #(= f %) r))
               f)))
      (concat t
              (partition (count t) (apply interleave t))
              (map
                #(take-nth (inc (count t)) (flatten %))
                [t (map reverse t)])))))

(defcheck solution-2167c236
  (fn [s]
    (let [f #(when (and (apply = %) (not= (first %) :e)) (first %))
          g #(apply (partial map (fn [& x] (f x)) ) %)
          o (fn [a b] `[~(a b) ~(a (- (count a) (inc b)))])]
      (some identity (concat (keep f s) (g s) (g (map o s (range))))))))

(defcheck solution-2174b9ec
  (fn ttt [b]
    (let [g [(map first b)
             (map second b)
             (map last b)
             (first b)
             (second b)
             (last b)
             [(first (first b)) (second (second b)) (last (last b))]
             [(first (last b)) (second (second b)) (last (first b))]]]
      (if (some #(= [:x :x :x] %) g)
        :x
        (if (some #(= [:o :o :o] %) g)
          :o
          nil)))))

(defcheck solution-21a547b1
  (fn [game]
    (let [n    (count game)
          rows (seq game)
          cols (apply map vector game)
          diag1 (map #(get (get game %1) %2) (range n) (range n))
          diag2 (map #(get (get (vec (rseq game)) %1) %2) (range n) (range n))
          sols (conj (concat rows cols) diag1 diag2)
          checkln #(if (apply = %2)
                     (if (= (first %2) :e) %1 (first %2))
                     %1)]
      (reduce checkln nil sols))))

(defcheck solution-21ba83ae
  (fn
    [board]
    (->> (-> []
           (conj (into #{} (first board)))
           (conj (into #{} (second board)))
           (conj (into #{} (last board)))
           (conj (into #{} (map #(nth % 0 nil) board)))
           (conj (into #{} (map #(nth % 1 nil) board)))
           (conj (into #{} (map #(nth % 2 nil) board)))
           (conj (into #{} (map #(nth (flatten board) % nil) (filter #(zero? (rem % 4)) (range (count (flatten board)))))))
           (conj (into #{} (map #(nth (flatten board) % nil) [2 4 6]))))
      (sort-by count)
      (some #(and (= (count %) 1) (or (% :x) (% :o)))))))

(defcheck solution-21c54059
  (fn [b]
    (let [rows (map identity b)
          cols (apply mapv vector b)
          diags (conj [] (map-indexed #(nth %2 %1) b)
                  (map-indexed #(nth %2 %1) (reverse b)))
          freqs (map frequencies (concat rows cols diags))
          xwon (some #(= (:x %) 3) freqs)
          owon (some #(= (:o %) 3) freqs)]
      (cond (true? xwon) :x
            (true? owon) :o
            :else nil)
      )
    ))

(defcheck solution-21d6068f
  (fn p73 [b]
    (->> b
      ;; extract sequence of triples of rows, columns and diagonals
      (concat
       (map (partial get b) [0 1 2]) ;; rows
       (apply map vector b) ;; col
       (vector (map get b [0 1 2])) ;; diagonal
       (vector (map get b [2 1 0]))) ;; diagonal
      ;; only keep triples of a single type
      (map distinct)
      (filter #(= 1 (count %)))
      (filter #(not= '(:e) %))
      (ffirst))))

(defcheck solution-2280319d
  (fn [board]
    (let [cols (apply mapv vector board)
          d (range (count board))
          diag-idxs [(map vector d d) (map vector d (reverse d))]
          diags (map #(map (partial get-in board) %) diag-idxs)
          all (concat board cols diags)
          row-wins? (fn [piece row] (every? (partial = piece) row))
          wins? (fn [piece] (some (partial row-wins? piece) all))]
      (cond (wins? :x) :x
            (wins? :o) :o
            :else nil))))

(defcheck solution-22c86f32
  (fn [r] (let [c (group-by identity (concat r
                                             (for [i [0 1 2]] (map #( % i) r))
                                             [[((r 0) 0) ((r 1) 1) ((r 2) 2)]]
                                             [[((r 0) 2) ((r 1) 1) ((r 2) 0)]]))]
            (ffirst (c [:x :x :x] (c [:o :o :o]))))))

(defcheck solution-232631f9
  (fn [b]
    (let [f (fn [l] (reduce #(if (and
                                  (=  %1 %2)
                                  (not= %2 :e))
                               %2 nil)
                      l))
          f1 (fn [m] (map f m))
          f2 (fn [m] (apply map list m))
          f3 (fn [m] (map-indexed #(nth %2 %1) m))
          f4 (fn [m] (map-indexed #(nth %2 (- 2 %1)) m))]
      (reduce
        #(if (not (nil? %2)) %2 %1)
        nil
        (flatten [(f1 b) (f1 (f2 b)) (f (f3 b)) (f (f4 b))])))))

(defcheck solution-238c957d
  (fn [b]
    (some (fn [p]
            (if (or
                 ;; diag:
                 (every? #{p}
                   (for [i (range 3)]
                     ((b i) i)))
                 ;; anti-diag:
                 (every? #{p}
                   (for [i (range 3)]
                     ((b i) (- 2 i))))
                 ;; across:
                 (some (fn [i]
                         (every? #{p} (for [j (range 3)] ((b i) j))))
                   (range 3))
                 ;; down:
                 (some (fn [i]
                         (every? #{p} (for [j (range 3)] ((b j) i))))
                   (range 3)))
              p))
      [:x :o])))

(defcheck solution-238e6198
  (fn [g] (let [win (fn [v] (if (and (apply = v) (not= (v 0) :e)) (v 0)))
                r (range 3)]
            (some win (concat g
                              (apply map vector g)
                              [(mapv #((g %) %) r)
                               (mapv #((g %1) %2) r (reverse r))])))))

(defcheck solution-2391fd57
  (fn [_]
    (let [e #(apply = % %2)
          a [0 1 2]
          b [2 1 0]
          s [:x :o]
          d (fn [x y] (map #(get (_ %) %2) x y))]
      (first (keep min
               (concat
                (for [c s
                      f [(fn [i] (map #(% i) _))
                         #(_ %)]
                      x a
                      :when (e c (f x))]
                  c)
                (for [c s
                      :when (or (e c (d a a))
                                (e c (d b a)))]
                  c)))))))

(defcheck solution-23ab9a00
  (fn [[[a1 a2 a3] [b1 b2 b3] [c1 c2 c3]]]
    (first (drop-while nil? (map
                              (fn [[x y z]] (cond (= x y z :x) :x (= x y z :o) :o))
                              [[a1 a2 a3] [b1 b2 b3] [c1 c2 c3] [a1 b1 c1] [a2 b2 c2] [a3 b3 c3] [a1 b2 c3] [a3 b2 c1]])))))

(defcheck solution-24058fde
  (fn [board] (condp #(some #{%1} %2) (map first (filter #(apply = %)
                                                   (concat board (apply map vector board)
                                                           (vector (vec (map #(nth %1 %2) board (range 3))) ;first diag
                                                             (vec (map #(nth %1 %2) board (reverse (range 3))))))))
                :x :x
                :o :o
                nil)))

(defcheck solution-242e90bb
  (fn analyze-ttt [board]
    (letfn [(columns [board]
              (partition (count (first board)) (apply interleave board)))
            (not-all-empty? [xs] (not-every? #{:e} xs))
            (diag1 [board]
              (loop [i 0 res []]
                (if (>= i (count board))
                  res
                  (recur (inc i) (conj res ((board i) i))))))
            (diag2 [board]
              (loop [i 0 res []]
                (if (>= i (count board))
                  res
                  (recur (inc i) (conj res ((board i) (- (count board) i 1)))))))]
      (first (first
               (filter #(apply = %)
                 (filter not-all-empty?
                   (concat board (columns board) [(diag1 board) (diag2 board)]))))))))

(defcheck solution-24667296
  (fn f [m]
    (let [lines (concat
                 (for [i [0 1 2]] (for [j [0 1 2]] [i j]))
                 (for [i [0 1 2]] (for [j [0 1 2]] [j i]))
                 [(for [i [0 1 2]] [i i])]
                 [(for [i [0 1 2]] [i (- 2 i)])])
          w (fn [line] (reduce
                         #(if (= %1 %2) %1 nil)
                         (map #((m (% 0)) (% 1)) line)))]
      (->>(map w lines)
        (filter #(and (not (nil? %)) (not= :e %)))
        first))))

(defcheck solution-24c4fc06
  #(some {[:x :x :x] :x [:o :o :o] :o}
     (concat
      % (apply map vector %)
      (for [d [[[0 2] [1 1] [2 0]] [[0 0] [1 1] [2 2]]]]
        (for [[y x] d]
          ((% y) x))))))

(defcheck solution-24efea28
  (fn [board] (let [rfn (partial map (partial apply list))
                    cfn (partial apply map list)
                    pfn (fn [r1 r2 board] (list (map (fn [[i1 i2]] (-> board (nth i1) (nth i2)))
                                                  (apply map vector (list r1 r2)))))
                    enil (fn [k] (if (= :e k) nil k))]
                (enil (ffirst (drop-while (partial apply not=)
                                (concat (rfn board) (cfn board)
                                        (pfn (range 3) (range 3) board)
                                        (pfn (range 3) (range 2 -1 -1) board))))))))

(defcheck solution-25299440
  (fn tic-tac-toe1 [sq]
    (let [size (count sq)
          diag1 (for [a (range size) b (range size)
                      :when (= (dec size) (+ a b))]
                  (get-in sq [a b]))
          diag2 (map #(get-in sq  [% %]) (range size))
          lines (-> sq (concat (apply map vector sq)) (conj diag1 diag2))
          winner (fn[coll] (if (and (#{:o :x} (first coll)) (apply = coll))
                             (first coll)))]
      (some winner lines))))

(defcheck solution-252fcb96
  (fn [board]
    (letfn [(win?
              [coll]
              (let [[h & t] coll]
                (when (and (not= h :e) (every? (fn [e] (= e h)) t))
                  h)))]
      (some win?
        (concat board
                (apply map vector board)
                (list (map (fn [r i] (nth r i)) board (range 3)))
                (list (map (fn [r i] (nth r i)) board (reverse (range 3)))))))))

(defcheck solution-25c64f1d
  (fn f [board]
    (cond
      (= (get-in board [0 0])
        (get-in board [1 1])
        (get-in board [2 2]))
      (#{:o :x} (get-in board [0 0]))
      (= (get-in board [0 2])
        (get-in board [1 1])
        (get-in board [2 0]))
      (#{:o :x} (get-in board [0 2]))
      true
      (let [full-row (->> board
                       (filter #(= 1 (count (set %))))
                       first first)
            full-col (->> board
                       (apply mapv vector)
                       (filter #(= 1 (count (set %))))
                       first first)]
        (or full-row full-col nil)))))

(defcheck solution-2624026a
  (fn [board] (let
               [lines [[0 1 2] [3 4 5] [6 7 8] ; horizontal
                       [0 3 6] [1 4 7] [2 5 8] ; vertical
                       [0 4 8] [2 4 6]]        ; diagonal
                flat-board  (flatten board)
                board-lines (map (fn [x] (map #(nth flat-board %) x)) lines)]
                (letfn
                 [(win-line? [side] (fn [line] (every? (partial = side) line)))
                  (winner?   [side] (some (win-line? side) board-lines))]
                  (some #(and (winner? %) %) [:x :o])))))

(defcheck solution-278c1bec
  (fn [b]
    (some (fn [l]
            (let [trinca (map #(get-in b %) l)]
              (cond
                (apply = :o trinca) :o
                (apply = :x trinca) :x
                :else nil)))
      (conj
        (concat
         (map #(map (partial vector %)
                 (range 0 3))
           (range 0 3))
         (map #(map (fn [l]
                      (vector l %))
                 (range 0 3))
           (range 0 3)))
        (list [0 0] [1 1] [2 2])
        (list [0 2] [1 1] [2 0])))))

(defcheck solution-279eacba
  (fn tic-tac-toe
    [board]
    (let [eq-and-value (fn [args]
                         (if (apply = args)
                           (if (= (first args) :e)
                             nil
                             (first args))))]
      (let [col1 (eq-and-value (map first board))
            col2 (eq-and-value (map #(nth % 1) board))
            col3 (eq-and-value (map last board))]
        (if-let [retv (or col1 col2 col3)]
          retv
          (let [row1 (eq-and-value (first board))
                row2 (eq-and-value (nth board 1))
                row3 (eq-and-value (last board))]
            (if-let [retv (or row1 row2 row3)]
              retv
              (let [diag1 (eq-and-value [(first (first board))
                                         (nth (nth board 1) 1)
                                         (last (last board))])
                    diag2 (eq-and-value [(last (first board))
                                         (nth (nth board 1) 1)
                                         (first (last board))])]
                (if-let [retv (or diag1 diag2)]
                  retv
                  nil)))))))))

(defcheck solution-27f7b99b
  (fn ttt [board]
    (let [board-vector (flatten board)
          check-lines [[0 3 6]
                       [1 4 7]
                       [2 5 8]
                       [0 1 2]
                       [3 4 5]
                       [6 7 8]
                       [0 4 8]
                       [2 4 6]]]
      (->> check-lines
        (map (fn [line] (map #(nth board-vector %) line)))
        (filter #(= 1 (count (distinct %))))
        (filter #(not (= :e (first %))))
        (#(if (empty? %)
            nil
            (first (first %))))))))

(defcheck solution-29c3057
  (fn [a]
    (let [both     (reduce #(conj %1 %2) a (vec (for [x (range 3)]
                                                  (vec (for [y (range 3)]
                                                         (nth (nth a y) x))))))

          diags  [(vec (for [xy (range 3)] (nth (nth a xy) xy)))
                  (vec (for [xy (reverse (range 3))] (nth (nth a xy) (- 2 xy))))]

          fil-n (fn [lol]
                  (first (remove nil?
                           (for [idx (range (count lol))]
                             (reduce
                               #(cond
                                  (= :x %1 %2) :x
                                  (= :o %1 %2) :o
                                  :else nil) (nth lol idx))))))
          both-res (fil-n both)
          diags-res (fil-n diags)]
      (first (remove nil? [both-res diags-res])))))

(defcheck solution-29d0c616
  (fn [board]
    (let [solutions [[0 1 2] [3 4 5] [6 7 8]
                     [0 3 6] [1 4 7] [2 5 8]
                     [0 4 8] [2 4 6]]
          flat (vec (mapcat identity board))
          all? #(reduce (fn [x y] (if (= x y) x)) %)]
      (->> solutions
        (map (comp vals (partial select-keys flat)))
        (some (comp #{:x :o} all?))))))

(defcheck solution-29ee3614
  (fn [s]
    (let [wins '((0 1 2) (3 4 5) (6 7 8)
                 (0 3 6) (1 4 7) (2 5 8)
                 (0 4 8) (2 4 6))
          p (mapcat identity s)
          q (filter #(= 1 (count %))
              (map distinct
                (map (fn [x](reduce #(conj % (nth p %2)) [] x))
                  wins)))
          ]
      (if (or (= 0 (count q)) (= 8 (count q)))
        nil
        (first (flatten q))))))

(defcheck solution-29f87536
  (fn [board]
    (let [triples
          (fn [board]
            (map #(map (partial get-in board) %)
              (concat
               (for [x [0 1 2]]
                 [[x 0] [x 1] [x 2]])
               (for [y [0 1 2]]
                 [[0 y] [1 y] [2 y]])
               [[[0 0] [1 1] [2 2]]
                [[0 2] [1 1] [2 0]]]
               )))


          ]
      (ffirst (filter #(or (= [:o :o :o] %)
                           (= [:x :x :x] %))
                (triples board)))
      )
    ))

(defcheck solution-2a0ac203
  (fn [board]
    (letfn [(has-winner [coll] (some #(if (not (or (nil? %) (= :e %))) % false) coll))
            (row-winner [r] (if (apply = r) (first r) nil))
            (transpose [b] (apply mapv vector b))
            (winner-by-row [b] (has-winner (map row-winner b)))
            (winner-by-col [b] (winner-by-row (transpose b)))
            (winner-by-diag [b] (has-winner (list (if (or (= (get (get b 0) 0) (get (get b 1) 1) (get (get b 2) 2))
                                                          (= (get (get b 0) 2) (get (get b 1) 1) (get (get b 2) 0)))
                                                    (get (get b 1) 1) nil))))]
      (some identity [(winner-by-row board) (winner-by-col board) (winner-by-diag board)]))))

(defcheck solution-2a66fbab
  (fn f[x]
    (loop [l x, n 0]
      (if (= n 3)
        (let [f1 (fn [l] (reduce #(conj %1 (nth %2 (count %1))) [] l))
              all (conj (conj l (f1 x)) (f1 (reverse x)))
              chk (fn [l] (let [r (set l)]
                            (if (and (= 1 (count r)) (not= :e (first r)))
                              (first r)
                              nil)))
              r (remove nil? (map chk all))]
          (if (> (count r) 0)
            (first r)
            nil))
        (recur (conj l (map #(nth % n) x)) (inc n))))))

(defcheck solution-2a77f27d
  (fn [board]
    (let [[[x _ a] [_ y _] [b _ z]] board
          diag1 [x y z]
          diag2 [a y b]
          swapped-board (apply map list board)
          win? (fn [r]
                 (cond
                   (every? #(= :x %) r) :x
                   (every? #(= :o %) r) :o))]
      (if-let
       [w (win? diag1)] w
                        (if-let
                         [w (win? diag2)] w
                                          (if-let
                                           [w (some #{:x :o} (for [row board] (win? row)))] w
                                                                                            (if-let
                                                                                             [w (some #{:x :o} (for [row swapped-board] (win? row)))] w
                                                                                                                                                      nil)))))))

(defcheck solution-2a981525
  (fn [sit]
    (letfn
     [(hor-win [player sit]
        (some (partial = (repeat 3 player)) sit))
      (vert-win [player sit]
        (and (seq (first sit))
             (or (= (repeat 3 player) (map first sit)) (recur player (map rest sit)))))
      (diag-win [player [line1 line2 line3]]
        (or (= (repeat 3 player) (list (nth line1 0) (nth line2 1) (nth line3 2)))
            (= (repeat 3 player) (list (nth line1 2) (nth line2 1) (nth line3 0)))))]
      (first (filter #(or (hor-win % sit) (vert-win % sit) (diag-win % sit)) [:x :o])))))

(defcheck solution-2a98e335
  (fn tic-tac-toe-winner [board]
    (letfn [(line-winner [line]
              (if (apply = line) (first line) nil))]
      (let [rows (map line-winner board)
            cols (map line-winner (apply map vector board))
            diag-1 (line-winner (map get board (range 3)))
            diag-2 (line-winner (map get board (range 2 -1 -1)))]
        (some #{:x :o} (concat rows cols [diag-1] [diag-2]))))))

(defcheck solution-2c5746e2
  (fn  [board]
    (let [tboard [(map first board)
                  (map second board)
                  (map #(nth % 2) board)]
          cross [(map #(get-in board %) [[0 0] [1 1] [2 2]])
                 (map #(get-in board %) [[0 2] [1 1] [2 0]])]
          all (concat board tboard cross)]
      (cond
        (some #{[:x :x :x]} all) :x
        (some #{[:o :o :o]} all) :o
        :else nil))))

(defcheck solution-2c7ace65
  (letfn
   [(all-eq? [v x] (every? #(= x %) v))
    (transpose [v] (vec (apply map vector v)))
    (win? [brd side]
      (or
       (some #(all-eq? % side) brd)
       (some #(all-eq? % side) (transpose brd))
       (all-eq? [((brd 0)0) ((brd 1)1) ((brd 2)2)] side)
       (all-eq? [((brd 0)2) ((brd 1)1) ((brd 2)0)] side)))]
    (fn [brd]
      (cond
        (win? brd :x) :x
        (win? brd :o) :o
        :else nil))))

(defcheck solution-2d086a15
  (fn tic-tac-toe [board]
    (let [same? (fn [sec] (if (apply = sec) (first sec) nil))
          rows (map same? board)
          cols (map same? (apply map vector board))
          diag1 (same? (map get board [0 1 2]))
          diag2 (same? (map get board [2 1 0]))]
      (some #{:x :o} (concat rows cols [diag1] [diag2])))))

(defcheck solution-2daea4ed
  (fn [board]
    (let [flat (concat board
                       (apply map vector board)
                       [(vec (map ((comp vec flatten) board) [2 4 6]))]
                       [(vec (map ((comp vec flatten) board) [0 4 8]))])
          winner (fn [xo flat]
                   (if (some true? (map #(every? #{xo} %) flat)) xo))]
      (or (winner :x flat)
          (winner :o flat)))))

(defcheck solution-2e335e9a
  (fn [board]
    (letfn [(row [k] (get board k))
            (col [k] (map #(get % k) board))
            (diag [k] (map #(get (get board %) (if (= k 1) % (- 2 %))) (range 3)))
            (abstract-win [x sym] (apply = (conj x sym)))
            (wins [x] (cond (abstract-win x :x) :x (abstract-win x :o) :o))]
      (let [all (concat [(diag 0) (diag 1)] (apply concat (map #(vector (col %) (row %)) (range 3))))
            winners (filter (complement nil?) (map wins all))]
        (if (seq winners)
          (first winners)
          nil)))))

(defcheck solution-2f2104fc
  (fn [x]
    (let [check-win (fn [[a :as xs]]
                      (if (and
                           (not= :e a)
                           (apply = xs)) xs []))]
      (first
        (first
          (filter seq
            [(check-win (map first x))
             (check-win (map second x))
             (check-win (map last x))
             (check-win (first x))
             (check-win (second x))
             (check-win (last x))
             (check-win (map #(%1 %2) [first second last] x))
             (check-win (map #(%1 %2) [last second first] x))]))))))

(defcheck solution-2f31e472
  (fn [rows]
    (let [cols (apply map vector rows)
          diags [(map #(get-in rows %) [[0 0] [1 1] [2 2]])
                 (map #(get-in rows %) [[2 0] [1 1] [0 2]])]]
      (->> (concat rows cols diags)
        (filter #(apply = %))
        first
        (some #{:x :o})))))

(defcheck solution-2f535ca4
  (fn tic-tac [a]
    (let [xy [[[0 0] [1 0] [2 0]]
              [[0 1] [1 1] [2 1]]
              [[0 2] [1 2] [2 2]]
              [[0 0] [0 1] [0 2]]
              [[1 0] [1 1] [1 2]]
              [[2 0] [2 1] [2 2]]
              [[0 0] [1 1] [2 2]]
              [[2 0] [1 1] [0 2]]]
          xs [:x :x :x]
          os [:o :o :o]
          path (fn [a p] (map #(let [[x y] %] ((a x) y)) p))]
      (cond
        (some #(= xs (path a %)) xy) :x
        (some #(= os (path a %)) xy) :o
        :else nil))))

(defcheck solution-2fcb1059
  (fn [x]
    (let [rows x
          cols (apply map vector x)
          diags [(take-nth 4 (flatten x))
                 (take-nth 4 (flatten (map reverse x)))]
          all (concat rows cols diags)
          won? (fn [s] (some #(every? (partial = s) %) all))]
      (cond (won? :x) :x
            (won? :o) :o
            :else nil))))

(defcheck solution-30068c11
  (fn prob73 [m]
    ;; return first path that contains all :x or :o
    ;; if none return nil
    (letfn [(triples [m]
              (concat (rows m) (cols m) (diags m)))
            (rows [m]
              (map #(row m %) (range 3)))
            (row [m n] (nth m n))
            (cols [m]
              (map #(col m %) (range 3)))
            (col [m n] (vec (map #(nth % n) m)))
            (diags [m]
              (list [(last (col m 0)) (second (col m 1)) (first (col m 2))]
                [(first (col m 0)) (second (col m 1)) (last (col m 2))]))]
      (let [tups (triples m)]
        #_(print tups)
        #_(print (count (filter #(= [:x :x :x] %) tups)))
        (if (> (count (filter #(= [:x :x :x] %) tups)) 0)
          :x
          (if (> (count (filter #(= [:o :o :o] %) tups)) 0)
            :o
            nil
            ))))))

(defcheck solution-3159fe27
  (fn[b]
    (let [m (concat b (apply map vector b) [[((b 0) 0) ((b 1) 1) ((b 2) 2)] [((b 2) 0) ((b 1) 1) ((b 0) 2)]])
          w (fn[p] (some #(every? (fn[a] (= a p)) %) m))]
      (cond (w :x) :x (w :o) :o :else nil))))

(defcheck solution-316f4138
  (fn [vs] (let [x (some #(if (apply = %) (first %))
                     (concat vs  (apply map vector vs) [[(first (first vs)) (second (second vs)) (last (last vs))] [(last (first vs)) (second (second vs)) (first (last vs))]]))]
             (cond
               (= x :e) nil
               (= x :o) :o
               (= x :x) :x)
             )))

(defcheck solution-319f8aa9
  (fn [D B] ( ->>
              `(~@B ~@(apply map list B) ~(D B) ~(-> B reverse D))
              (some #{[:x :x :x] [:o :o :o]})
              first)
    ) (partial map-indexed #(%2 %)))

(defcheck solution-325f0fc3
  (fn [b]
    (some
      (comp first #{#{:o} #{:x}} set)
      (concat b
              (map #(map nth b [% % %]) [0 1 2])
              (map #(map nth b %) [[0 1 2] [2 1 0]])))))

(defcheck solution-32ac2d21
  (fn [rows]
    (let [line #(let [f (first %)] (when (and (apply = %) (#{:x :o} f)) f))
          vd (fn [i] (line (map #(% %2) rows i)))
          gw #(first (keep % %2))]
      (some #(if % %) [(gw line rows)
                       (gw vd
                         (let [r [0 1 2]]
                           (concat [r (reverse r)]
                                   (map #(repeat 3 %) r))))]))))

(defcheck solution-32b72a18
  (fn tic-tac-toe
    [board]
    (let [x :x o :o]
      (letfn [(horizontal-check
                [board]
                (filter
                  #(not (nil? %))
                  (map #(cond (every? (partial = x) %)
                              x
                              (every? (partial = o) %)
                              o) board)))
              (invert-board
                [board]
                (if (seq (flatten board))
                  (cons (map first board) (invert-board (map rest board)))))
              (diagonal
                [board]
                (if (seq (flatten board))
                  (cons (ffirst board) (diagonal (rest (map rest board))))))]
        (first (drop-while nil? (flatten
                                  ((juxt
                                     horizontal-check ;; check for horizontal win
                                     (comp horizontal-check invert-board) ;; check for vertical win
                                     (comp horizontal-check vector diagonal) ;; check for bottom-left-to-top-right win
                                     (comp horizontal-check vector diagonal #(map reverse %))) board))))))))

(defcheck solution-332e3f50
  (fn winner
    [board]
    (let [won? (fn [k]
                 (let [w [k k k]]
                   (seq
                     (filter
                       #{w}
                       (concat
                        board
                        ((partial apply map vector) board)
                        (map #(map (partial get-in board) %)
                          [[[0 0] [1 1] [2 2]] [[2 0] [1 1] [0 2]]]))))))]
      (cond (won? :x) :x
            (won? :o) :o))))

(defcheck solution-33488290
  (fn analyze[board](let[[[a1 b1 c1] [a2 b2 c2] [a3 b3 c3]] board
                         posib (conj board [a1 a2 a3] [b1 b2 b3] [c1 c2 c3] [a1 b2 c3] [a3 b2 c1])
                         q (fn[k v](count (filter #(= k %) v)))]
                      (if (some #{3} (map #(q :x %) posib)) :x
                                                            (when (some #{3} (map #(q :o %) posib)) :o)))))

(defcheck solution-349668f7
  (fn [brd]
    (some #(if (or (every? #{:x} %)
                   (every? #{:o} %))
             (first %)
             nil)
      (conj
        (concat brd (apply map vector brd))
        (map-indexed (fn [i e] (first (drop i e))) brd)
        (map-indexed (fn [i e] (last (drop-last i e))) brd)))))

(defcheck solution-353f84a1
  (fn [b]
    (letfn [(win [ts xo] (if (some identity (map #(every? #{xo} %) ts)) xo))
            (diagonal [b] (vector (flatten (partition 1 4 (flatten b)))))]
      (let [rs (concat b (apply map list b) (diagonal b) (diagonal (map reverse b)))]
        (some identity (map #(win rs %) [:x :o]))))))

(defcheck solution-35699491
  (fn [m]
    (let [diagonal (fn [m] (reduce #(conj %1 (%2 (count %1))) [] m))
          transpose (fn [m] (apply (partial map vector) (reverse m)))
          candidates (fn [m] (conj m (diagonal m)))
          all-candidates (concat (candidates m) (candidates (transpose m)))]
      (cond
        (some (fn [c] (every? #(= % :x) c)) all-candidates) :x
        (some (fn [c] (every? #(= % :o) c)) all-candidates) :o
        :else nil))))

(defcheck solution-35eb2b56
  (fn [y] (letfn
           [
            (rd [x] [(take-nth 4 (flatten x))])
            (ld [x] [(rest (drop-last (take-nth 2 (flatten x))))])
            (v [x]   (apply mapv vector x))
            (buildOptions [x] (concat (v x) x (rd x) (ld x)))
            (success? [x] (cond
                            (some #(= % [:x :x :x]) x) :x
                            (some #(= % [:o :o :o]) x) :o
                            :else nil
                            ))
            ]
            (success? (into [] (buildOptions y)))
            )))

(defcheck solution-362ba2e2
  (fn [b]
    (first (some #{[:x :x :x] [:o :o :o]}
             (concat b (apply map list b)
                     (map #(map (fn [a i] (a i)) b %) [[0 1 2] [2 1 0]]))))))

(defcheck solution-36515cfb
  (fn [b]
    (let [[[p1 p2 p3] [p4 p5 p6] [p7 p8 p9]] b]
      (reduce #(or %1 %2)
        (map (fn [[x y z]] (if (and (= x y) (= y z) (not= x :e)) x nil))
          [(b 0) (b 1) (b 2)
           [p1 p4 p7]
           [p2 p5 p8]
           [p3 p6 p9]
           [p1 p5 p9]
           [p3 p5 p7]])))))

(defcheck solution-3654c32d
  (fn [[[a b c] [d e f] [g h i] :as A]]
    (some
      (fn [[x y z]]
        (and (not= x :e) (= x y z) x))
      (conj A [a d g] [b e h] [c f i] [a e i] [c e g]))))

(defcheck solution-36e08fd3
  (fn [matrix] (let [r3 (range 3)
                     col (fn [n] (for [i r3] [i n]))
                     row (fn [n] (for [i r3] [n i]))
                     diag0 (for [i r3] [i i])
                     diag1 (for [i r3] [i (- 2 i)])
                     win? (fn [coll symb] (every? #(= % symb) (map #(get-in matrix %) coll)))
                     win-col (fn [symb] (or (win? (col 0) symb)
                                            (win? (col 1) symb)
                                            (win? (col 2) symb)))
                     win-row (fn [symb]  (or (win? (row 0) symb)
                                             (win? (row 1) symb)
                                             (win? (row 2) symb)))
                     win-diag (fn [symb] (or (win? diag0 symb) (win? diag1 symb)))
                     win-symb? (fn [symb] (or (win-col symb) (win-row symb) (win-diag symb)))]
                 (cond
                   (win-symb? :x) :x
                   (win-symb? :o) :o
                   :else nil))))

(defcheck solution-3714f7a
  (fn [[[a b c] [d e f] [g h i]]]
    (let [cases [[a b c] [d e f] [g h i] [a e i]
                 [c e g] [a d g] [b e h] [c f i]]

          player-win-case?
                (fn [pkey case]
                  (every? #(= % pkey) case))

          player-win-game?
                (fn [pkey]
                  (some #(player-win-case? pkey %) cases))]
      (cond (player-win-game? :x) :x
            (player-win-game? :o) :o))))

(defcheck solution-376c93c3
  (fn [x]
    (ffirst
      (filter
        #(and (apply = %) (not= (first %) :e))
        (partition 3 (map
                       (vec (flatten x))
                       '(0 1 2 3 4 5 6 7 8 0 3 6 1 4 7 2 5 8 0 4 8 2 4 6)))))))

(defcheck solution-379971e
  (fn [[[a b c] [d e f] [g h i]]]
    (let [rows [[a b c] [d e f] [g h i],
                [a d g] [b e h] [c f i],
                [a e i] [c e g]],
          check-row (fn [row]
                      (cond (every? #(= :x %) row) :x
                            (every? #(= :o %) row) :o
                            :else nil))]
      (first (remove nil? (map check-row rows))))))

(defcheck solution-379cc6df
  (fn [b]
    (let [m (fn [r] (map #(%1 %2) b r))
          c (fn [] [(m (range 2 -1 -1))
                    (m (range))
                    (m (repeat 0))
                    (m (repeat 1))
                    (m (repeat 2))])
          l (concat b (c))
          x? #(= :x %)
          o? #(= :o %)]
      (cond (some #(= 3 (count (filter x? %))) l) :x
            (some #(= 3 (count (filter o? %))) l) :o
            :else nil))))

(defcheck solution-37f40d8f
  (fn tic-tac-toe [board]
    (let
     [[top-row mid-row bot-row] board
      [left-col mid-col right-col]
      (map (fn [col] (map (fn [row-vec] (row-vec col)) board)) (range 3))
      left-diag [ (get top-row 0) (get mid-row 1) (get bot-row 2)]
      right-diag [ (get top-row 2) (get mid-row 1) (get bot-row 0) ]
      all-streaks [top-row mid-row bot-row
                   left-col mid-col right-col
                   left-diag right-diag]
      winner? (fn [streak]
                (cond
                  (every? #(= % :x) streak) :x
                  (every? #(= % :o) streak) :o))
      winners (filter #(not (nil? %)) (map winner? all-streaks))]
      (if (seq winners)
        (first winners)))))

(defcheck solution-38140cc3
  (fn winner [board]
    (let [x (filter
              (fn [e]
                (and (apply = e) (every? #(not= % :e) e)))
              (concat board
                      (apply map list board)
                      (reduce (fn [a b]
                                (let [[l r] a
                                      n (count l)
                                      c (count b)]
                                  [(conj l (nth b n))
                                   (conj r (nth b (- c n 1)))]))
                        [[] []]
                        board)))]
      (if (empty? x) nil (first (first x))))))

(defcheck solution-383c0dbe
  (fn [board]
    (letfn [(get-inds [sym]
              (letfn [(get-sym [row col] (nth (nth board row) col))
                      (gen-ind [row col] (+ (* 10 row) col))
                      (get-inds* [row col acc]
                        (if (= row 3)
                          acc
                          (if (= col 3)
                            (recur (inc row) 0 acc)
                            (if (= sym (get-sym row col))
                              (recur row (inc col) (conj acc (gen-ind row col)))
                              (recur row (inc col) acc)))))]
                (get-inds* 0 0 [])))
            (won? [inds]
              (letfn [(has-seq?
                        ([start step repeats]
                         (if (= 1 repeats)
                           true
                           (let [new-s (+ start step)]
                             (if (some #{new-s} inds)
                               (recur new-s step (dec repeats))
                               false))))
                        ([step repeats]
                         (some #{true} (map #(has-seq? % step repeats) inds))))]
                (or (has-seq? 1 3) (has-seq? 9 3) (has-seq? 10 3) (has-seq? 11 3))))]
      (cond
        (won? (get-inds :x)) :x
        (won? (get-inds :o)) :o
        :else nil))))

(defcheck solution-38d99d9e
  (fn goo [tic]
    (let
     [combos (conj
               tic
               (map #(% 0) tic)
               (map #(% 1) tic)
               (map #(% 2) tic)
               (map-indexed (fn [index item] (item index)) tic)
               (map-indexed (fn [index item] (item index)) (reverse tic)))]

      (ffirst (filter (fn [xs] (and (not-any? #(= :e %) xs)
                                    (apply = xs)))
                combos)))))

(defcheck solution-395a3c68
  (fn [field]
    (letfn [(rows [] field)
            (cols [] (apply map vector field))
            (crosses [] [[(get-in field [0 0]) (get-in field [1 1]) (get-in field [2 2])]
                         [(get-in field [0 2]) (get-in field [1 1]) (get-in field [2 0])]])
            (full [sym] (some (fn [seq] (every? #(= % sym) seq)) (concat (rows) (cols) (crosses))))]
      (cond (full :x) :x
            (full :o) :o))))

(defcheck solution-395f5008
  (fn get-winner [board]
    (let [flat-board (flatten board)
          possible-sets [[0 1 2][3 4 5][6 7 8]
                         [0 3 6][1 4 7][2 5 8]
                         [0 4 8][2 4 6]]
          winning-set? (fn [coll] (or (every? #(= % :x) coll)
                                      (every? #(= % :o) coll)))
          all-sets (reduce (fn[rs ps] (conj rs (map #(nth flat-board %) ps))) [] possible-sets)
          winner (filter winning-set? all-sets)]
      (if (empty? winner) nil (ffirst winner)))))

(defcheck solution-3964eba4
  (fn [l]
    (let [pos [[[0 0] [0 1] [0 2]]
               [[1 0] [1 1] [1 2]]
               [[2 0] [2 1] [2 2]]
               [[0 0] [1 0] [2 0]]
               [[0 1] [1 1] [2 1]]
               [[0 2] [1 2] [2 2]]
               [[0 0] [1 1] [2 2]]
               [[2 0] [1 1] [0 2]] ]]
      (cond
        (some #(every? identity (map (fn [[x y]] (= :x (nth (nth l x) y ))) %)) pos) :x
        (some #(every? identity (map (fn [[x y]] (= :o (nth (nth l x) y ))) %)) pos) :o))))

(defcheck solution-39a28c3d
  (fn [board]
    (letfn [(test-horizontal [e]
              (some
                (fn [b]
                  (some
                    #(apply = e %)
                    b))
                [board (apply map vector board)])
              )
            (test-diag [e]
              (let [diag (fn [b] (map-indexed #(nth %2 %) b))]
                (some
                  #(apply = e (diag %))
                  [board (reverse board)])))
            (test [e]
              (some
                #(% e)
                [test-horizontal test-diag]))]
      (first (filter test '(:x :o))))))

(defcheck solution-39b750c5
  (fn [board]
    (letfn [(row-is? [sym row_num]
              (apply = sym (nth board row_num)))
            (col-is? [sym col_num]
              (apply = sym (map #(nth % col_num) board)))
            (diag-1-is? [sym]
              (apply = sym [(ffirst board) (second (second board)) (last (last board))]))
            (diag-2-is? [sym]
              (apply = sym [(last (first board)) (second (second board)) (first (last board))]))
            (win-for? [sym]
              (some true? (concat [(diag-1-is? sym) (diag-2-is? sym)]
                                  (for [fun [(partial row-is? sym) (partial col-is? sym)]
                                        rc [0 1 2]]
                                    (fun rc)))))]
      (cond (win-for? :x) :x
            (win-for? :o) :o))))

(defcheck solution-39ce8573
  (fn [b]
    (let [rows [[[0 0] [1 0] [2 0]]
                [[0 1] [1 1] [2 1]]
                [[0 2] [1 2] [2 2]]]
          cols [[[0 0] [0 1] [0 2]]
                [[1 0] [1 1] [1 2]]
                [[2 0] [2 1] [2 2]]]
          dias [[[0 0] [1 1] [2 2]]
                [[2 0] [1 1] [0 2]]]
          all (concat rows cols dias)
          val-fn (fn [grouping] (map #(get-in b %) grouping))]
      (reduce (fn [found? [v0 v1 v2]]
                (if (and (not found?) (not= v0 :e) (= v0 v1 v2))
                  v0
                  found?))
        nil
        (map val-fn all)))))

(defcheck solution-3a087be2
  (fn [b]
    (let [win-row? (fn [p row] (every? #(= p %) row))
          win? (fn [p]
                 (or
                  (win-row? p (first b))
                  (win-row? p (second b))
                  (win-row? p (nth b 2))
                  (win-row? p (map first b))
                  (win-row? p (map second b))
                  (win-row? p (map #(nth % 2) b))
                  (win-row? p [(get-in b [0 0]) (get-in b [1 1]) (get-in b [2 2])])
                  (win-row? p [(get-in b [2 0]) (get-in b [1 1]) (get-in b [0 2])])
                  ))]
      (if (win? :x)
        :x
        (if (win? :o)
          :o)))))

(defcheck solution-3a6e83db
  (fn [board]
    (let [coords (vec (for [y (range 3) x (range 3)] [x y]))
          h (partition 3 coords)
          v (apply map vector h)
          d1 (map coords [0 4 8])
          d2 (map coords [2 4 6])
          lines (concat h v (vector d1 d2))
          runs (filter #(apply = %) (map #(map (partial get-in board) %) lines))
          results (remove (partial = :e) (map first runs))]
      (if (= 1 (count results)) (first results) nil))))

(defcheck solution-3aaed9a
  (fn [b]
    (let [l (count b)
          x (range l)
          y (reverse x)
          rs (-> (into b (apply map vector b))
               (conj (vec (mapcat #(take 1 (drop %1 (cycle %2))) x b)))
               (conj (vec (mapcat #(take 1 (drop %1 (cycle %2))) y b))))]
      (->> (filter #(apply = %) rs)
        (map first)
        (some #{:o :x})))))

(defcheck solution-3b2438b5
  (fn [board] (let [o-wins [:o :o :o]
                    x-wins [:x :x :x]
                    flip-board (->> board (apply map list ))
                    elem       (fn [row col] (-> board (nth row) (nth col)))
                    diag1      (map #(elem % %) (range 3))
                    diag2      (map #(elem % (- 2 %)) (range 3))]
                #_(println diag1)
                #_(println diag2)
                (cond
                  (some #(= o-wins %) board) :o
                  (some #(= o-wins %) flip-board) :o
                  (some #(= x-wins %) board) :x
                  (some #(= x-wins %) flip-board) :x
                  (= x-wins diag1) :x
                  (= x-wins diag2) :x
                  (= o-wins diag1) :o
                  (= o-wins diag2) :o
                  :else                      nil))))

(defcheck solution-3b2bde7b
  (fn [board]
    (let [diagonals (vector
                      (map get board (range 3))
                      (map get board (reverse (range 3))))
          rows board
          columns (apply mapv vector board)]
      (some {[:x :x :x] :x, [:o :o :o ] :o} (concat diagonals rows columns)))))

(defcheck solution-3b4ed465
  (fn [[x y z :as grid-by-lines]]
    (let [column (fn [f] (map f grid-by-lines))
          grid-by-columns (map column (list first second last))
          diagonal (fn [grid] (map #(%1 %2) (list first second last) grid))
          diagonals (map diagonal (list grid-by-lines (reverse grid-by-lines)))
          is-every (fn [symbol a] (every? #(= symbol %) a))
          is-winner (fn [user] (some #(is-every user %) (concat grid-by-lines grid-by-columns diagonals)))]
      (cond
        (is-winner :x) :x
        (is-winner :o) :o
        :else nil
        )
      )
    ))

(defcheck solution-3b9839f0
  (fn tic-tac-toe [board]
    (let [rotate-board (fn [board]
                         (apply mapv vector
                           (map rseq board)))
          r-board (rotate-board board)
          check-row (fn [[a b c]]
                      (if (and (= a b) (= b c)) (if (not= :e a) a nil) nil))
          check-rows (fn [[r1 r2 r3]]
                       (cond (not= nil (check-row r1)) (check-row r1)
                             (not= nil (check-row r2)) (check-row r2)
                             (not= nil (check-row r3)) (check-row r3)
                             :else nil))
          check-diagonal (fn [[[a _ _][_ b _][_ _ c]]]
                           (if (and (= a b) (= b c)) (if (not= :e a) a nil) nil))]
      (cond (not= nil (check-diagonal board)) (check-diagonal board)
            (not= nil (check-diagonal r-board)) (check-diagonal r-board)
            (not= nil (check-rows board)) (check-rows board)
            (not= nil (check-rows r-board)) (check-rows r-board)
            :else nil))))

(defcheck solution-3c19d050
  (fn checker [bord]
    (let [flat-bord (reduce conj [] (flatten bord))]
      (letfn [(_g [inx] (get flat-bord inx))
              (winner [coll]
                (let [x (-> coll (distinct))]
                  (cond
                    (= 1 (count x)) (first x)
                    :else nil)))]
        (loop [_target (map #(conj [] (_g (first %)) (_g (second %)) (_g (last %)))
                         [[0 1 2] [3 4 5] [6 7 8]
                          [0 3 6] [1 4 7] [2 5 8]
                          [0 4 8] [2 4 6]])]
          (when-not (empty? _target)
            (let [answer (winner (first _target))]
              (if (or (nil? answer) (= :e answer))
                (recur (rest _target))
                answer)))
          )))))

(defcheck solution-3c98709e
  #(let [p (concat % (apply map list %)
                   [(map nth % (range)) (map nth (map reverse %) (range))])]
     (first (some #{[:x :x :x] [:o :o :o]} p))))

(defcheck solution-3cbf37c6
  (fn [matrix]
    (loop [[k & ks] (flatten matrix) [v & vs] '(8 1 6 3 5 7 4 9 2) r {:x 0 :o 0 :e 0}]
      (if k
        (recur ks vs (merge-with + r {k v}))
        (cond
          (= 15 (:x r)) :x
          (= 15 (:o r)) :o
          :else nil)))))

(defcheck solution-3cecc01c
  (fn [[l1 l2 l3]]
    (first
      (filter
        (partial contains? #{:o :x})
        (map
          (partial reduce #(if (and (= %1 %2) (not= %1 :e)) %1 nil))
          (concat
           [l1 l2 l3]
           [[(first l1) (second l2) (last l3)]
            [(last l1) (second l2) (first l3)]]
           (apply map list [l1 l2 l3])))))))

(defcheck solution-3d37ee96
  (fn [board]
    (let [b (to-array-2d board)
          dims (range 3)
          get-row (fn [r] (nth board r))
          get-col (fn [c] (map #(aget b % c) dims ))
          get-diag-1 (map #(aget b % %) dims)
          get-diag-2 (map #(aget b (- 2 %) %) dims)
          all (concat (map get-row dims) (map get-col dims) [get-diag-1] [get-diag-2])
          s (map set all)
          singles (set (map first (filter #(= 1 (count %)) s)))
          ]
      (cond
        (contains? singles :o) :o
        (contains? singles :x) :x
        :else nil
        )
      )
    ))

(defcheck solution-3e34ca3
  (fn __ [[[a b c]
           [d e f]
           [g h i] :as board]]
    (let [B (set board)
          Bt (set (apply map list board))
          test (fn [x]
                 (let [x3 (list x x x)]
                   (or
                    (B x3)
                    (Bt x3)
                    (= x3 [a e i])
                    (= x3 [c e g]))))]
      (cond
        (test :x) :x
        (test :o) :o
        :else nil))))

(defcheck solution-3f1c4ee3
  (fn [b]
    (cond
      (every? #(= :x %) (first b)) :x
      (every? #(= :x %) (second b)) :x
      (every? #(= :x %) (last b)) :x
      (every? #(= :x %) (map first [(first b) (second b) (last b)])) :x
      (every? #(= :x %) (map second [(first b) (second b) (last b)])) :x
      (every? #(= :x %) (map last [(first b) (second b) (last b)])) :x
      (every? #(= :x %) ((juxt #(first (first %)) #(second (second %)) #(last (last %))) b)):x
      (every? #(= :x %) ((juxt #(first (last %)) #(second (second %)) #(last (first %))) b)):x
      (every? #(= :o %) (first b)) :o
      (every? #(= :o %) (second b)) :o
      (every? #(= :o %) (last b)) :o
      (every? #(= :o %) (map first [(first b) (second b) (last b)])) :o
      (every? #(= :o %) (map second [(first b) (second b) (last b)])) :o
      (every? #(= :o %) (map last [(first b) (second b) (last b)])) :o
      (every? #(= :o %) ((juxt #(first (first %)) #(second (second %)) #(last (last %))) b)):o
      (every? #(= :o %) ((juxt #(first (last %)) #(second (second %)) #(last (first %))) b)):o
      :else nil)))

(defcheck solution-3f270c65
  (fn [board]
    (let [check-triple (fn [l] (if (= (count (set l)) 1)
                                 (first l)
                                 nil))
          flip (apply mapv vector board)
          slash (mapv #(get-in board [% %]) (range 3))
          back-slash (mapv #(get-in board [%1 %2]) (range 3) (reverse (range 3)))]
      (->> (concat board flip [slash back-slash])
        (map check-triple ,,)
        (keep #{:x :o} ,,)
        (distinct ,,)
        (first ,,)))))

(defcheck solution-3f492454
  (fn [rows]
    (let [cols (apply map vector rows)
          diags (map #(map % (range 3)) [#((rows %) %) #((rows %) (- 2 %))])
          lines (concat rows cols diags)]
      (first (some (comp #{#{:x} #{:o}} set) lines)))))

(defcheck solution-3fccd81c
  (fn [m]
    (first
      (some #{[:x :x :x]
              [:o :o :o]}
        (map #(map (vec (flatten m)) %)
          (partition 3 [0 1 2, 0 3 6, 0 4 8
                        3 4 5, 1 4 7, 2 4 6
                        6 7 8, 2 5 8]))))))

(defcheck solution-40043580
  (fn [m]
    (let [combs (concat m (map #(map % m) [first second last]) [(map #((m %) %) [0 1 2])] [(map #((m (- 2%)) %) [0 1 2])])
          win (some #{[:x :x :x] [:o :o :o]} combs)]
      (if (empty? win) nil (first win)))))

(defcheck solution-410fada0
  (fn __ [board]
    (let [bt (apply map vector board)
          cross [(map #(-> board (nth %) (nth %)) (range 3))
                 (map #(-> board (nth (- 2 %)) (nth %)) (range 3))]]
      (cond
        (some #(= (distinct %) [:x]) board) :x
        (some #(= (distinct %) [:x]) bt) :x
        (some #(= (distinct %) [:x]) cross) :x
        (some #(= (distinct %) [:o]) board) :o
        (some #(= (distinct %) [:o]) bt) :o
        (some #(= (distinct %) [:o]) cross) :o
        :else nil)
      )))

(defcheck solution-41323ba
  (fn [board]
    (let [rows (concat board
                       (apply map list board)
                       (for [pos-seq [[0 0 1 1 2 2]
                                      [0 2 1 1 2 0]]]
                         (for [pos (partition 2 pos-seq)]
                           (get-in board pos))))]
      (first (for [[who :as row] rows
                   :when (and (not= :e who)
                              (apply = row))]
               who)))))

(defcheck solution-41766df6
  (fn winner [board]
    (let [triplet-same-fn (fn [row] (when (apply = row) (first row)))
          col-fn (fn [col-num] (map #(nth % col-num) board))
          diag-1 (for [x (range 3)]
                   (nth (nth board x) x))
          diag-2 (for [x (range 3)]
                   (nth (nth board x) (- 2 x)))
          rows board
          cols (map col-fn (range 3))
          all-triplets (conj (concat rows cols) diag-1 diag-2)
          winner-seq (->> all-triplets
                       (map triplet-same-fn)
                       (remove #(or (nil? %) (#{:e} %))))]
      (when (seq winner-seq)
        (first winner-seq)))))

(defcheck solution-41c73dc1
  #(some {[:x :x :x] :x [:o :o :o] :o}
     (map (partial map (fn [k] (get-in % k)))
       [[[0 0][0 1][0 2]][[1 0][1 1][1 2]][[2 0][2 1][2 2]]
        [[0 0][1 0][2 0]][[0 1][1 1][2 1]][[0 2][1 2][2 2]]
        [[0 0][1 1][2 2]][[0 2][1 1][2 0]]])))

(defcheck solution-41eeedaa
  (fn [[[a b c]
        [d e f]
        [g h i]
        :as m]]
    (->>
      (conj m [a d g] [b e h] [c f i] [a e i] [c e g])
      (filter #(apply = %))
      (remove #(some #{:e} %))
      ffirst)))

(defcheck solution-4260f7e7
  (fn winner [board]
    (let [lines #{#{[0 0] [0 1] [0 2]}
                  #{[1 0] [1 1] [1 2]}
                  #{[2 0] [2 1] [2 2]}
                  #{[0 0] [1 0] [2 0]}
                  #{[0 1] [1 1] [2 1]}
                  #{[0 2] [1 2] [2 2]}
                  #{[0 0] [1 1] [2 2]}
                  #{[0 2] [1 1] [2 0]}}
          occupied-cells (fn [board piece]
                           (set
                             (for [row (range 3)
                                   column (range 3)
                                   :when (= piece (get-in board [row column]))]
                               [row column])))
          winner? (fn [board piece]
                    (let [cells (occupied-cells board piece)]
                      (loop [l lines]
                        (cond
                          (empty? l) false
                          (clojure.set/subset? (first l) cells) true
                          :else (recur (rest l))))))]
      (cond
        (winner? board :x) :x
        (winner? board :o) :o
        :else nil))))

(defcheck solution-4377a599
  (fn [[[tl _ tr] [_ m] [bl _ br] :as v]]
    (let [inv (apply map vector v)
          diags [[tl m br] [bl m tr]]
          all (concat v inv diags)
          match (some #(identity %) (map #(if (apply = %) (first %)) all))]
      (if (= :e match) nil match))))

(defcheck solution-43963bf7
  (fn [rows]
    (let [cols (apply mapv vector rows)
          diags (map (fn [r] (map-indexed #(first (drop %1 %2)) r))
                  [rows (reverse rows)])
          candidates (map set (concat rows cols diags))]
      (->> candidates
        (filter (comp (partial = 1) count))
        (remove #(contains? % :e))
        first
        first))))

(defcheck solution-43aa338f
  (fn analyze [table]
    (letfn [(winner? [ [head & tail] ]
              (when (and (not= head :e) (every? #{head} tail)) head))
            (map-cols-from [[xs ys zs]]
              (map #(apply vector [% %2 %3]) xs ys zs))
            (map-diags-from [[[fhead _ ftail]
                              [_ mid _]
                              [lhead _ ltail]]]
              (apply vector [[fhead mid ltail] [lhead mid ftail]]))
            (combine [table]
              (reduce conj table (lazy-cat (map-cols-from table)
                                   (map-diags-from table))))]
      (when-let [line (seq (filter winner? (combine table)))]
        ((comp first last) line)))))

(defcheck solution-43ff6d07
  (fn [[r1 r2 r3]]
    (let [f (fn [r c]
              (condp = r
                0 (nth r1 c)
                1 (nth r2 c)
                2 (nth r3 c)))
          rows (map (fn [x] (set (map #(f % x) (range 3)))) (range 3))
          cols (map (fn [x] (set (map #(f x %) (range 3)))) (range 3))
          diag1 (set (map #(f % %) (range 3)))
          diag2 (set (list (f 2 0) (f 1 1) (f 0 2)))
          all (concat rows cols [diag1] [diag2])
          res (filter #(and (= 1 (count %))
                            (or (:x %) (:o %)))
                all)]
      (if (seq res)
        (first (first res))
        nil))))

(defcheck solution-4447ac8d
  (fn [board]
    (let [win? (fn [combos player] (not (nil? (some (fn [c] (every? (fn [cell] (= cell player)) c)) combos))))
          win-by-row? (fn [rows player] (win? rows player))
          win-by-column? (fn [[r1 r2 r3] player] (let [columns (map vector r1 r2 r3)] (win? columns player)))
          win-by-diagonal? (fn [[[d11 _ d21] [_ dc _] [d22 _ d12]] player] (win? [[d11 dc d12] [d21 dc d22]] player))
          win-player? (fn [player]
                        (not (nil? (some true? (map (fn [win-fn] (win-fn board player)) [win-by-row? win-by-column? win-by-diagonal?])))))]
      (cond
        (win-player? :x) :x
        (win-player? :o) :o
        :else nil))))

(defcheck solution-4449af1d
  (fn ttt [board]
    (let [rows  (partition 3 (for [r (range 3), c (range 3)] [r c]))
          cols  (partition 3 (for [c (range 3), r (range 3)] [r c]))
          diags '(([0 0] [1 1] [2 2]) ([0 2] [1 1] [2 0]))
          winning-pos (concat (concat rows cols) diags)
          winner? (fn [player] (some identity
                                 (map #(every? (fn [pos] (= (get-in board pos) player))
                                         %)
                                   winning-pos)))]
      (cond
        (winner? :x) :x
        (winner? :o) :o))))

(defcheck solution-4476e9e8
  (fn [n]
    (let [check (fn [j] (if (= 1 (count (distinct j))) (first j) :e))
          lines (map check n)
          columns (map #(check ( map (fn [x] (nth x %)) n)) (range 0 3))
          diag (check (map (fn [x] (nth (nth n x) x)  ) (range 0 3) ))
          odiag (check (map (fn [x] (nth (nth n x) (- 2  x))  ) (range 0 3) ))
          final (flatten [lines columns diag odiag]) ]


      (first (filter  #(not= :e %) final) ) ) ))

(defcheck solution-44db4a1b
  (fn [matrix]
    (let [[[r1c1 r1c2 r1c3]
           [r2c1 r2c2 r2c3]
           [r3c1 r3c2 r3c3]] matrix]
      (first
        (first
          (filter #(= (count %) 1)
            (filter #(not (contains? % :e))
              (map set [[r1c1 r1c2 r1c3]
                        [r2c1 r2c2 r2c3]
                        [r3c1 r3c2 r3c3]
                        [r1c1 r2c1 r3c1]
                        [r1c2 r2c2 r3c2]
                        [r1c3 r2c3 r3c3]
                        [r1c1 r2c2 r3c3]
                        [r1c3 r2c2 r3c1]]))))))))

(defcheck solution-44ed8916
  ; General function for nxn row-col game
  (fn [rows]
    (let [n (count rows)]
      (->>
        (concat rows
                ;; verticals
                (apply map vector rows)
                ;; diagonals
                (map (partial map (partial get-in rows))
                  [(for [x (range n)] [x x])
                   #_[[0 0][1 1][2 2]]
                   (for [x (range n)] [x (- (dec n) x)])
                   #_[[0 2][1 1][2 0]]]))
        (some (set (map (partial repeat n) [:x :o])))
        first))))

(defcheck solution-44fd9fdb
  (letfn [(row-match [sym row]
            (every? #(= sym %) row))
          (diag1 [b]
            [((b 0) 0) ((b 1) 1) ((b 2) 2)])
          (diag2 [b]
            [((b 0) 2) ((b 1) 1) ((b 2) 0)])
          (transpose [b]
            (vec (apply map vector b)))]
    (fn [b]
      (cond
        (some identity (map #(row-match :x %) b)) :x
        (some identity (map #(row-match :o %) b)) :o
        (some identity (map #(row-match :x %) (transpose b))) :x
        (some identity (map #(row-match :o %) (transpose b))) :o
        (row-match :x (diag1 b)) :x
        (row-match :o (diag1 b)) :o
        (row-match :x (diag2 b)) :x
        (row-match :o (diag2 b)) :o
        :else nil))))

(defcheck solution-45689d66
  (fn [b]
    (let [all (concat
               b
               (apply map vector b)
               [(map get b [0 1 2])]
               [(map get b [2 1 0])])
          win (filter (fn [c] (or (every? #(= % :o) c) (every? #(= % :x) c)))
                all)]
      (if (empty? win)
        nil
        (ffirst win)
        ))
    ))

(defcheck solution-458586a0
  (fn [[[a b c]
        [e f g]
        [h i j]]]
    (some #(let [dist (distinct %)] (if (or (second dist) (= (first dist) :e)) nil (first dist))) [ [a b c] [e f g] [h i j] [a e h] [b f i] [c g j] [a f j] [c f h] ])))

(defcheck solution-45a44281
  (fn ttt [board]
    (let [transpose (fn [v] (apply (partial map vector) v))
          tboard (transpose board)
          getin (fn [row i] (row i))
          diagonal (fn [b is] [(mapv getin b is)])
          all-positions (concat board
                                tboard
                                (diagonal board (range))
                                (diagonal tboard (range 2 -1 -1)))]
      (cond
        (some #{[:o :o :o]} all-positions) :o
        (some #{[:x :x :x]} all-positions) :x
        :default nil))))

(defcheck solution-45daa2cd
  (fn [[[a b c]
        [d e f]
        [g h i]]]
    (nth (some #{[:x :x :x]
                 [:o :o :o]}
           (partition 3 [a b c, d e f, g h i,
                         a d g, b e h, c f i,
                         a e i, c e g]))
      0)))

(defcheck solution-464e2cd
  (fn[board]
    (let [row (fn[n] (board n))
          column (fn [n](vec(map #((board %) n) (range 3))))
          diag (vec (map #((board %) %) (range 3)))
          adiag (vec (map #((board %) (- 2 %)) (range 3)))
          lines (concat (map row (range 3) ) (map column (range 3))   [diag adiag ])
          winner (fn [line] (if (= (line 0)(line 1)(line 2)) (line 0) nil ))
          find-winner (fn [L] (if (or (empty? L)(first L)) (first L) (recur (rest L))))]

      (some #{:x,:o}(map winner lines))

      )
    ))

(defcheck solution-474d3bc4
  (fn [board]
    (letfn [(winner? [board player]
              (let [winners [[7 0 0] [0 7 0] [0 0 7]
                             [4 4 4] [2 2 2] [1 1 1]
                             [4 2 1] [1 2 4]]
                    binified (partition 3 (map #(if (= % player) 1 0)
                                            (flatten board)))
                    multiplied (map #(map * [4 2 1] %) binified)
                    summarized (map #(reduce + %) multiplied)]
                (not (empty?
                       (for [winner winners :when
                             (= winner (map bit-and summarized winner))]
                         true)))))]
      (cond
        (winner? board :x) :x
        (winner? board :o) :o))))

(defcheck solution-477dab3c
  (fn [b]
    (some {[:o :o :o] :o [:x :x :x] :x}
      (concat b
              (apply map list b)
              (map
                #(for [n [0 1 2]] (nth (nth % n) n))
                [b (rseq b)])))))

(defcheck solution-47b73179
  (fn ttt [board] (first (remove #(= % nil) (map (fn [x] (reduce #(if (or (= :e %1) (not= %1 %2)) nil %1) x)) (vec (apply concat
                                                                                                                     (vector board
                                                                                                                       (vec (map vec (partition 3 (apply interleave board))))
                                                                                                                       (vector
                                                                                                                         (apply vector (map nth board (range 3)))
                                                                                                                         (apply vector (map nth board (reverse (range 3))))
                                                                                                                         )
                                                                                                                       ))))))))

(defcheck solution-47ed68ab
  (fn who-wins [board]
    (some
      (fn wins? [e]
        (let [e3 (repeat 3 e)]
          (when
           (or (some #(= % e3) board)
               (some #(= % e3) (apply map vector board))
               (every? #(= (nth (nth board %) %) e) (range 3))
               (every? #(= (nth (nth board %) (- 2 %)) e) (range 3)))
            e)))
      [:x :o])))

(defcheck solution-48586406
  (fn [xss]
    (let [x-count (count xss)
          directions [[0 -1] [-1 -1] [-1 0] [-1 1]]
          lookup-keys #(map (fn [d] [(map + % d) d]) directions)
          fold-key #(fn [agg [l-key r-key]]
                      (if (= (get-in xss l-key) %2)
                        (assoc agg r-key (inc (or (get-in %1 [l-key r-key]) 1)))
                        agg))
          indexes (for [x (range x-count)
                        y (range x-count)
                        :when (not= :e (get-in xss [x y]))]
                    [x y])
          m-fold (fn [data key]
                   (let [l-keys (lookup-keys key)
                         c-player (get-in xss key)
                         next-data (reduce (fold-key data c-player) {} l-keys)]
                     (if (some #(= % x-count) (vals next-data))
                       [c-player nil]
                       [nil (assoc data key next-data)])))
          winner-g (fn [[winner data] key]
                     (if winner [winner] (m-fold data key)))
          [winner] (reduce winner-g [nil {}] indexes)]
      winner)))

(defcheck solution-48970516
  (letfn [(rows [b] b)
          (cols [b] (apply map list b))
          (diags [b] [[(get-in b [0 0]) (get-in b [1 1]) (get-in b [2 2])]
                      [(get-in b [0 2]) (get-in b [1 1]) (get-in b [2 0])]])
          (wins? [p b] (some #(every? #{p} %) (concat (rows b) (cols b) (diags b))))]
    (fn [board] (cond
                  (wins? :x board) :x
                  (wins? :o board) :o))))

(defcheck solution-49253fde
  (fn [ttt]
    (ffirst
      (filter
        #(and (= 1 (count %)) (not= :e (first %)))
        (map
          set
          (conj
            ttt
            (map first ttt)
            (map second ttt)
            (map last ttt)
            (map-indexed #(%2 %1) ttt)
            (map-indexed #(%2 (- 2 %1)) ttt)
            )
          )
        )
      )
    ))

(defcheck solution-49402945
  (fn [b]
    (letfn [(horz [b] b)
            (vert [b] (map #(map % b) [first second last]))
            (diag [b] (map (partial map (partial get-in b))
                        [[[0 0] [1 1] [2 2]]
                         [[2 0] [1 1] [0 2]]]))
            (played? [x] (not (= :e x)))
            (winner [[x & _ :as s]] (when (and (apply = s) (played? x)) x))]
      (some winner (mapcat #(% b) [horz vert diag])))))

(defcheck solution-49cfa01c
  (fn [board]
    (letfn [(win [[[a b c] [d e f] [g h i]] p]
              (or (= p a b c)
                  (= p d e f)
                  (= p g h i)
                  (= p a d g)
                  (= p b e h)
                  (= p c f i)
                  (= p a e i)
                  (= p c e g)))]
      (cond
        (win board :x) :x
        (win board :o) :o
        :else nil))))

(defcheck solution-49d6a7cb
  (fn [board]
    (let [three-in-a-row?
               (fn [row]
                 (if (and (= (first row) (second row))
                          (= (first row) (last row)))
                   (if (not= (first row) :e)
                     (first row)
                     nil)
                   nil))
          rows (concat (map identity board)
                       (for [x [0 1 2]] (map #(get % x) board))
                       (list (vec (map #((get board %) %) [0 1 2])))
                       (list (vec (map #((get (vec (reverse board)) %) %) [2 1 0]))))]
      (first (filter #(identity %) (map three-in-a-row? rows))))))

(defcheck solution-4b43850e
  (fn [v]
    (let [cols (apply map vector v)
          diag1 (map #(nth %1 %2) v (range 3))
          diag2 (map #(nth %1 %2) v (reverse (range 3)))
          all-rows (concat v cols [diag1 diag2])]
      (->> all-rows
        (map #(into #{} %))
        (filter #(= 1 (count %)))
        (apply concat)
        (some #{:x :o})))))

(defcheck solution-4befc6c5
  (fn [v] (let [x (fn [a] (let [q (filter (into [] (map #(= a %) (flatten v))) (range  9))] (if (#{'(3 4 5) '(0 3 6) '(0 4 8) '(2 4 6)} q) a nil)))] (or (x :o) (x :x)))))

(defcheck solution-4c40fe61
  (fn [b]
    (some {[:o :o :o] :o [:x :x :x] :x}
      (concat b (partition 3 (apply interleave b))
              (for [i [[0 4 8][2 4 6]]]
                (map #(nth (flatten b) %) i))))))

(defcheck solution-4c91a5a
  (fn [[a b c]]
    (let [q (into [a b c
                   [(first a)(second b)(last c)]
                   [(first c)(second b)(last a)]] (map vector a b c))
          won? (fn [[x & r]] (when (and (apply = x r) (not= :e x)) x))]
      (first (remove nil? (map won? q))))))

(defcheck solution-4d1a06c5
  (fn [[a b c]]
    (let [q (into [a b c
                   [(first a)(second b)(last c)]
                   [(first c)(second b)(last a)]] (map vector a b c))
          won? (fn [[x & r]] (when (and (apply = x r) (not= :e x)) x))]
      (first (remove nil? (map won? q))))))

(defcheck solution-4d758a4d
  (fn [[r1 r2 r3]]
    (letfn
     [(sl [n] [(nth r1 n) (nth r2 n) (nth r3 n)])
      (allAre [x c] (every? #(= x %) c))
      (awin [x]
        (or (allAre x r1)
            (allAre x r2)
            (allAre x r3)
            (allAre x (sl 0))
            (allAre x (sl 1))
            (allAre x (sl 2))
            (allAre x [(first r1) (second r2) (last r3)])
            (allAre x [(first r3) (second r2) (last r1)])))]
      (if (awin :x) :x (if (awin :o) :o nil)))))

(defcheck solution-4dc74014
  #(some
     (fn [v] (and (apply = v) ({:o :o :x :x :e nil} (first v))))
     (for [d [[0 1 2] [3 4 5] [6 7 8] [0 3 6]
              [1 4 7] [2 5 8] [0 4 8] [2 4 6]]]
       (map (partial nth (flatten %)) d))))

(defcheck solution-4dd6df70
  (fn [matrix]
    (let [
          col1 (fn [letra matriz]
                 (every? #(= letra %) (for [i (range 3)] ((matriz i) 0) )))
          col2 (fn [letra matriz]
                 (every? #(= letra %) (for [i (range 3)] ((matriz i) 1) )))
          col3 (fn [letra matriz]
                 (every? #(= letra %) (for [i (range 3)] ((matriz i) 2) )))
          fil1 (fn [letra matriz]
                 (every? #(= letra %) (for [i (range 3)] ((matriz 0) i) )))
          fil2 (fn [letra matriz]
                 (every? #(= letra %) (for [i (range 3)] ((matriz 1) i) )))
          fil3 (fn [letra matriz]
                 (every? #(= letra %) (for [i (range 3)] ((matriz 2) i) )))
          diag1 (fn [letra matriz]
                  (every? #(= letra %) (for [i (range 3)] ((matriz i) i) )))
          diag2 (fn [letra matriz]
                  (every? #(= letra %) (for [i (range 3)] ((matriz (- 2 i) ) i) )))
          funcs [col1 col2 col3 fil1 fil2 fil3 diag1 diag2]

          x ( some identity (for [f funcs] (f :x matrix)) )
          o ( some identity (for [f funcs] (f :o matrix)) )
          ]
      (cond x :x
            o :o
            :else nil)

      )))

(defcheck solution-4de1684c
  (fn [board]
    (let [marks #{:o :x}
          lines (concat board
                        (map (fn [index] (map #(% index) board))
                          (range (count (first board))))
                        (reduce (fn [lines index]
                                  [(conj (first lines)
                                     ((board index) index))
                                   (conj (second lines)
                                     ((nth (reverse board) index) index))])
                          [[] []]
                          (range (count board))))]
      (letfn [(same-marks? [line]
                (and (marks (first line))
                     (apply = line)
                     (first line)))]
        (let [win-line (first (filter same-marks? lines))]
          (if win-line (first win-line) nil))))))

(defcheck solution-4e05b419
  (fn [b]
    (letfn [(winner [s]
              (if (= (count (distinct s)) 1)
                (first (distinct s))
                nil))
            (check-winner [w]
              (if (and w (not (= w :e)))
                w
                nil))]
      (check-winner (or (winner (map first b))
                        (winner (map second b))
                        (winner (map last b))
                        (winner (first b))
                        (winner (second b))
                        (winner (last b))
                        (winner (map nth b (range 3)))
                        (winner (map nth b (reverse (range 3))))
                        )
        )
      )
    ))

(defcheck solution-4e120df0
  (fn ttt [b]
    (let [diagonal
          (fn [b]
            (let [one (first b)
                  two (second b)
                  three (last b)]
              (if
               (and
                (not= :e (first one))
                (= (first one) (second two) (last three))) (first one)
                                                           nil)))
          horizontal
          (fn [b]
            (let [winner (filter #(= 1 (count (set %))) b)]
              (if (and (not (empty? winner)) (not= :e (ffirst winner)))
                (ffirst winner)
                nil)))
          flip-board
          (fn [b]
            [(map first b)
             (map second b)
             (map last b)])]
      (first (filter #(not (nil? %)) [(diagonal b)
                                      (diagonal (map reverse b))
                                      (horizontal b)
                                      (horizontal (flip-board b))])))))

(defcheck solution-4e31408b
  (fn check-ttt [b]
    (letfn [(hwin [b]
              (loop [b b]
                (if (and b)
                  (let [player (first (first b))]
                    (if (and (not= player :e) (= (count (set (first b))) 1))
                      player
                      (recur (next b)))))))

            (vwin [b]
              (loop [i 0]
                (if (> i 2)
                  nil
                  (let [col (apply concat (partition 1 3 (drop i (apply concat b))))]
                    (if (and (not= (first col) :e) (= (count (set col)) 1))
                      (first col)
                      (recur (inc i)))))))

            (dwin [[[x1 __ x2]
                    [__ x3 __]
                    [x4 __ x5]]]
              (cond
                (and (not= :e x1) (= x1 x3 x5)) x1
                (and (not= :e x2) (= x2 x3 x4)) x2
                :else nil))
            ]
      (or (vwin b)
          (hwin b)
          (dwin b)))))

(defcheck solution-4e36de1f
  (fn [m]
    (first (first
             (filter #(and (apply = %) (not= :e (first %)))
               (concat m
                       (apply mapv vector m)
                       (let [d (fn [m] (map (fn [x] (get-in m x)) (map #(list % %) (range 3))))] [(d m) (d ((comp vec reverse) m))])))))))

(defcheck solution-4e564fa2
  (fn [s]
    (letfn [(win? [board k]
              (some #(every? (conj #{} k) %)
                (concat board
                        (apply map vector board)
                        (conj []
                          (for [idx (range 3)]
                            (get-in board [idx idx]))
                          (for [idx (range 3)]
                            (get-in board [idx (- 2 idx)]))))))]
      (cond
        (win? s :x) :x
        (win? s :o) :o
        :else nil))))

(defcheck solution-4e96114d
  ; This is HORRIFYING
  (fn check-lines [coll]
    (loop [rng (take 3 (range))]
      (let [three-in-row (fn three-in-row [coll]
                           (let [result (reduce #(if (= %1 %2) %1 nil) (first coll) coll)]
                             (if (or (= result :o) (= result :x))
                               result
                               nil)))]
        ; This handles diagonals
        (if (= '() rng)
          (let [r-v (three-in-row [(nth (nth coll 0) 0) (nth (nth coll 1) 1) (nth (nth coll 2) 2)])
                l-v (three-in-row [(nth (nth coll 0) 2) (nth (nth coll 1) 1) (nth (nth coll 2) 0)])]
            (if r-v
              r-v
              (if l-v
                l-v
                nil)))
          ; This handles horizontal/verticals
          (let [n (first rng)
                v-val (three-in-row (nth coll n))
                h-val (three-in-row (reduce #(conj %1 (nth %2 n)) [] coll))]
            (if v-val
              v-val
              (if h-val
                h-val
                (recur (rest rng))))))))))

(defcheck solution-4eadc04f
  (fn [b]
    (let [r (range 3)
          w
            (concat
             (for [x r]
               (for [y r] [x y]))
             (for [x r]
               (for [y r] [y x]))
             [[[0 0] [1 1] [2 2]]]
             [[[2 0] [1 1] [0 2]]])
          p (fn [[x y]] (nth (nth b x) y))
          m #(every? (partial = %) (map p %2))
          v #(some true? (map (partial m %) w))]
      (cond
        (v :x) :x
        (v :o) :o))))

(defcheck solution-4f1cd6e7
  (fn [board]
    (letfn [(check-horizonal [board]
              (first
                (flatten
                  (filter #(every? #{:x :o} %)
                    (filter #(= 1 (count (set %))) board)))))]
      (let [h (check-horizonal board)]
        (if h
          h
          (let [v (check-horizonal (apply map vector board))]
            (if v
              v
              (let [fd (check-horizonal [[(first (first board)) (second (second board)) (last (last board))]])]
                (if fd
                  fd
                  (check-horizonal [[(first (last board)) (second (second board)) (last (first board))]]))))))))))

(defcheck solution-4f77526b
  (fn [x]
    (->> (concat x (apply (partial map vector) x)
                 (map (fn diag [m] (reduce #(conj %1 (%2 (count %1))) [] m)) [x (reverse x)]))
      (filter (partial apply =)) (map first) (filter #{:x, :o}) first)))

(defcheck solution-4f9039ce
  (fn [board]
    (let [columns (for [x (range 3)] (map #(nth % x) board))
          diag1 (for [x (range 3)] (nth (board x) x))
          diag2 (for [x (range 3)] (nth (board x) (- 2 x)))
          allposs (concat board columns [diag1] [diag2])]
      (#(cond
          (empty? %) nil
          (= (first %) [:x :x :x]) :x
          (= (first %) [:o :o :o]) :o
          :else (recur (rest %))) allposs))))

(defcheck solution-4fcdfe50
  (fn [b]
    (let [d (flatten b)
          g (fn [c] (vector (map #(nth d %) c)))
          r (apply concat [b (map #(map % b) [first second last])
                           (g [0 4 8]) (g [2 4 6])])
          w (filter #(some #{:x :o} #{%}) (map first (filter #(apply = %) r)))]
      (if (empty? w) nil (first w)))))

(defcheck solution-4ffe8392
  (fn f [coll]
    (let [row1  (coll 0)
          row2  (coll 1)
          row3  (coll 2)
          col1  (map #(% 0) coll)
          col2  (map #(% 1) coll)
          col3  (map #(% 2) coll)
          dia1  [(get-in coll [0 0]) (get-in coll [1 1]) (get-in coll [2 2])]
          dia2  [(get-in coll [0 2]) (get-in coll [1 1]) (get-in coll [2 0])]
          all   [row1 row2 row3 col1 col2 col3 dia1 dia2]
          check (fn [e] (some #(apply = (conj % e)) all))]
      (cond (check :x) :x
            (check :o) :o
            :else       nil))))

(defcheck solution-5012f07a
  (fn [g]
    (let [cols (map (fn [col] (map #(nth % col) g)) (range 3))
          diag [[(first (first g)) (second (second g)) (last (last g))]
                [(last (first g)) (second (second g)) (first (last g))]]
          lines (concat g cols diag)]
      (cond
        (some #(= % [:x :x :x]) lines) :x
        (some #(= % [:o :o :o]) lines) :o
        true nil))))

(defcheck solution-50706089
  (fn an [l]
    (let [rotated ((juxt (partial map first) (partial map second) (partial map last)) l)
          d1 (map (partial get-in l) [[0 0] [1 1] [2 2]])
          d2 (map (partial get-in l) [[0 2] [1 1] [2 0]])
          f #(when (apply = %) (first %))
          all (concat l rotated [d1 d2])]
      (->> (map f all)
        (filter #(not= :e %))
        sort
        last))))

(defcheck solution-507740c6
  (fn [[l1 l2 l3]]
    (let [cols   (partition 3 (interleave l1 l2 l3))
          cross1 (list (first l1) (second l2) (last l3))
          cross2 (list (last l1)  (second l2) (first l3))]
      (->> (concat cols [l1 l2 l3 cross1 cross2])
        (map #(remove #{:e} %))
        (some (fn [[a b c]]
                (if (and a (= a b c))
                  a))))
      )
    ))

(defcheck solution-507b3ac5
  (fn [board]
    (let [horizontal-winner (fn [board player]
                              (some identity (map #(every? (partial = player) %) board)))
          vertical-winner (fn [board player]
                            (horizontal-winner (apply map vector board) player))
          ul-diag-winner (fn [board player]
                           (= (get-in board [0 0])
                             (get-in board [1 1])
                             (get-in board [2 2])
                             player))
          diag-winner (fn [board player]
                        (or (ul-diag-winner board player)
                            (ul-diag-winner (vec (map (comp vec reverse) board)) player)))
          winner? (fn [player]
                    (or (horizontal-winner board player)
                        (vertical-winner board player)
                        (diag-winner board player)))]
      (cond
        (winner? :x) :x
        (winner? :o) :o
        ))))

(defcheck solution-50bd4262
  (fn winner [m]
    (let [
          rotate-matrix (fn [m] (apply map list m))
          lines (fn [m] (concat m
                                (rotate-matrix m)
                                [(map-indexed #(nth %2 %1) m)
                                 (map-indexed #(nth %2 (- 2 %1)) m)]))]
      (->> (lines m)
        (map distinct)
        (filter #(= 1 (count %)))
        (filter #(not (= :e (first %))))
        (first)
        (first)))))

(defcheck solution-514b6fa6
  (fn [[[a b c]
        [d e f]
        [g h i]]]

    (let [rcd [[a b c][d e f][g h i] ;; rows
               [a d g][b e h][c f i] ;; cols
               [a e i][c e g]]]      ;; diags
      (reduce
        #(let [ft (first %2)]
           (if (and (apply = %2)(not= :e ft)) ft %1))
        nil
        rcd))))

(defcheck solution-516147f0
  (fn [horizontal-lines]
    (let [vertical-lines (for [x (range 3)]
                           (for [y (range 3)]
                             ((vec (horizontal-lines y)) x)))
          diagonal-lines [(for [i (range 3)]
                            ((vec (horizontal-lines i)) i))
                          (for [i (range 3)]
                            ((vec (horizontal-lines i)) (- (dec 3) i)))]
          all-lines (concat horizontal-lines vertical-lines diagonal-lines)
          count-items-in-line (fn [line item]
                                (count (filter #(= item %) line)))
          count-max-items-in-lines (fn [item]
                                     (apply max (map #(count-items-in-line % item) all-lines)))]
      (cond
        (= 3 (count-max-items-in-lines :x)) :x
        (= 3 (count-max-items-in-lines :o)) :o
        :else nil))))

(defcheck solution-51aca45d
  (fn [b]
    (first
      (remove
        #(= :e %)
        (for [t (concat
                 [[[0 0] [1 1] [2 2]]
                  [[0 2] [1 1] [2 0]]]
                 (for [r [0 1 2]]
                   [[r 0] [r 1] [r 2]])
                 (for [c [0 1 2]]
                   [[0 c] [1 c] [2 c]]))]
          (let [v (for [[r c] t]
                    ((b r) c))]
            (if (apply = v)
              (first v)
              :e)))))))

(defcheck solution-5278598a
  (fn [s]
    (let [positions (concat (map #(repeat 3 %) (range 3)) [[0 1 2] [2 1 0]])
          get-line (fn [tr] (map nth s tr))
          rcd (concat s (map get-line positions))]
      (some {[:x :x :x] :x [:o :o :o] :o} rcd))))

(defcheck solution-52aff4da
  (fn analyze-a-tic-tac-toe-board [board]
    (letfn [(win? [who]
              (some true?
                (for [x (range 3)
                      y (range 3)
                      [dx dy] [[1 0] [0 1] [1 1] [1 -1]]]
                  (every? true? (for [i (range 3)]
                                  (= (get-in board [(+ (* dx i) x)
                                                    (+ (* dy i) y)])
                                    who))))))]
      (cond (win? :x) :x
            (win? :o) :o
            :else nil))))

(defcheck solution-52eb7e2c
  (fn [t]
    (let [lines [[0 1 2] [3 4 5] [6 7 8]
                 [0 3 6] [1 4 7] [2 5 8]
                 [0 4 8] [6 4 2]]
          t (flatten t)
          getp #(nth t %)
          answer? (fn [k line] (every? #(= k (getp %)) line))]
      (cond
        (some true? (map #(answer? :x %) lines)) :x
        (some true? (map #(answer? :o %) lines)) :o
        ))))

(defcheck solution-531af0dd
  (fn aa [[[a b c] [d e f] [g h i]]]

    (cond
      (and (= a b c) (not= a :e)) a
      (and (= d e f) (not= d :e)) d
      (and (= g h i) (not= g :e)) g
      (and (= a d g) (not= a :e)) a
      (and (= b e h) (not= b :e)) b
      (and (= c f i) (not= c :e)) c
      (and (= a e i) (not= a :e)) a
      (and (= c e g) (not= c :e)) c
      :else nil
      )
    ))

(defcheck solution-532c7b2a
  (fn check-winner
    [board]
    (letfn [(get-diagonal [board fun n] (if board
                                          (cons (nth (first board) n) (get-diagonal (next board) fun (fun n)))
                                          []))
            (ttl [board] (into (into (#(into %1 (apply map vector %1)) board) (vector (get-diagonal board dec 2))) (vector (get-diagonal board inc 0))))]
      (get (reduce into {} (map (fn [val]
                                  {(some #(= (repeat 3 val) %) (ttl board)) val}) [:x :o])) true nil))))

(defcheck solution-533aa202
  (fn winner [board]
    (let [interpose-matrix
                #(for [i (range (count (first %)))]
                   (for [row %] (nth row i))
                   )
          rows board
          cols (interpose-matrix board)
          d1 [(nth (nth board 0) 0)
              (nth (nth board 1) 1)
              (nth (nth board 2) 2)]
          d2 [(nth (nth board 0) 2)
              (nth (nth board 1) 1)
              (nth (nth board 2) 0)]
          lines (concat rows cols [d1] [d2])
          ]
      (cond
        (some (partial = [:o :o :o]) lines) :o
        (some (partial = [:x :x :x]) lines) :x
        )
      )
    ))

(defcheck solution-539260e7
  (fn [b]
    (some (comp #{:o :x} first)
      (filter (partial apply =)
        (conj b
          [(get-in b [0 0]) (get-in b [1 1]) (get-in b [2 2])]
          [(get-in b [0 2]) (get-in b [1 1]) (get-in b [2 0])]
          (map first b)
          (map second b)
          (map #(nth % 2) b))))))

(defcheck solution-53d92337
  (fn [b]
    (let [lines
          (concat
           (for [x [0 1 2]] (for [y [0 1 2]] [x y]))
           (for [y [0 1 2]] (for [x [0 1 2]] [x y]))
           '(([0 0] [1 1] [2 2]) ([0 2] [1 1] [2 0])))
          collapsed
          (zipmap
            (for [x [0 1 2] y [0 1 2]] [x y])
            (flatten b))]
      (some {#{:x} :x #{:o} :o}
        (map #(set (map collapsed %)) lines)))))

(defcheck solution-54401559
  (fn [b]
    (letfn [
            (triplets [b]
              (lazy-cat
                b
                (for [i [0 1 2]] (for [j [0 1 2]] (get-in b [j i])))
                (list (for [i [0 1 2]] (get-in b [i i])))
                (list (for [i [0 1 2]] (get-in b [i (- 2 i)])))))]
      (loop [t (triplets b)]
        (cond
          (nil? t) nil
          (apply = :x (first t)) :x
          (apply = :o (first t)) :o
          :else (recur (next t)))))))

(defcheck solution-560cf4c9
  (fn [board]
    (let [v (fn [row col] (nth (nth board row) col))
          triples (concat board
                          (apply map list board)
                          [ [(v 0 0) (v 1 1) (v 2 2)]
                           [(v 0 2) (v 1 1) (v 2 0)] ])]
      (if-let [t (first (filter #(let [p (partition-by identity %)]
                                   (and (= 1 (count p))
                                        (not= :e (ffirst p))))
                          triples))]
        (first t)))))

(defcheck solution-574026f5
  (fn [board]
    (let [
          transposed (apply (partial map vector) board)
          flipped (reverse board)
          row-of      (fn [b s] (when (some (fn [row] (every? #(= s %) row)) b) s))
          ldiag-of    (fn [b s] (when (every? #(= s %) (map nth b (range))) s))
          won         (fn [s] (some identity [(row-of board s) (row-of transposed s) (ldiag-of board s) (ldiag-of flipped s)]))]
      (some won [:x :o])
      )))

(defcheck solution-57869fe3
  (fn [rows]
    (let [won?
          (fn [player]
            (if (some
                  (fn [row] (every? #(= player %) row))
                  (concat
                   rows
                   (partition 3 (apply interleave rows))
                   [(map nth rows (range 3))
                    (map nth rows (reverse (range 3)))]))
              player))]
      (or (won? :x) (won? :o)))))

(defcheck solution-57a5e83c
  (fn [board]
    (->> (concat board (apply map vector board))
      (cons (map-indexed #(nth %2 %1) board))
      (cons (map-indexed #(nth (reverse %2) %1) board))
      (filter #{[:x :x :x] [:o :o :o]})
      ffirst)))

(defcheck solution-58265481
  (fn [board]
    (letfn [(won? [b x]
              (cond (= (b 0) [x x x]) true
                    (= (b 1) [x x x]) true
                    (= (b 2) [x x x]) true
                    (= x ((b 0) 0) ((b 1) 1) ((b 2) 2)) true
                    (= x ((b 0) 2) ((b 1) 1) ((b 2) 0)) true
                    (= x ((b 0) 0) ((b 1) 0) ((b 2) 0)) true
                    (= x ((b 0) 1) ((b 1) 1) ((b 2) 1)) true
                    (= x ((b 0) 2) ((b 1) 2) ((b 2) 2)) true
                    :else false))]
      (cond (won? board :x) :x
            (won? board :o) :o
            :else nil))))

(defcheck solution-582f9a0b
  (fn who-won? [board]
    (let [fns [first second last]
          all-equal? (fn [s] (reduce #(if (= %1 %2) %1 nil) s))
          lines (concat board ; rows
                        (map #(map % board) fns) ; columns
                        [(map #(% %2) fns board) (map #(% %2) (reverse fns) board)]) ; diagonals
          result (some all-equal? lines)]
      (if (#{:e} result) nil result))))

(defcheck solution-58366fc5
  (fn [z] (let [dir [first second last]]
            (letfn [(same? [x l] (every? #(= x %) l))
                    (trav [r] (map  (fn [f z] (into [] (reduce #(conj % (f %2)) [] z))) dir (repeat 3 r)))
                    (l->r [z] [(map #(% %2) dir z)])
                    (r->l [z] [(map #(% %2) (reverse dir) z)])
                    (win? [e c]  (or (some #(same? e %) c)
                                     (some #(same? e %) (trav c))
                                     (some #(same? e %) (l->r c))
                                     (some #(same? e %) (r->l c))))]
              (cond
                (win? :x z) :x
                (win? :o z) :o
                :else nil)))))

(defcheck solution-58f4432c
  (fn [[u v w :as x]]
    (let [t (apply map vector x)
          l (concat x t [[(u 0) (v 1) (w 2)] [(u 2) (v 1) (w 0)]])]
      (if (some #(= [:x :x :x] %) l) :x
                                     (if (some #(= [:o :o :o] %) l) :o nil)))))

(defcheck solution-59137f4d
  #(let [[r1 r2 r3] %
         f (fn [a b c] (and (not= :e a) (= a b c)))]
     (cond
       (f (r1 0) (r1 1) (r1 2)) (r1 0)
       (f (r2 0) (r2 1) (r2 2)) (r2 0)
       (f (r3 0) (r3 1) (r3 2)) (r3 0)
       (f (r1 0) (r2 0) (r3 0)) (r1 0)
       (f (r1 1) (r2 1) (r3 1)) (r1 1)
       (f (r1 2) (r2 2) (r3 2)) (r1 2)
       (f (r1 0) (r2 1) (r3 2)) (r1 0)
       (f (r1 2) (r2 1) (r3 0)) (r1 2)
       :else nil)))

(defcheck solution-592ce0f0
  (fn [g]
    (some
      #(when (or (= % [:x :x :x])
                 (= % [:o :o :o]))
         (first %)) (map (fn [x]
                           (map #(get-in g %) x))
                      [[[0 0] [0 1] [0 2]]
                       [[1 0] [1 1] [0 2]]
                       [[2 0] [2 1] [2 2]]
                       [[0 0] [1 1] [2 2]]
                       [[0 2] [1 1] [2 0]]
                       [[0 0] [1 0] [2 0]]
                       [[1 0] [1 1] [1 2]]
                       [[2 0] [2 1] [2 2]]]))))

(defcheck solution-5aa20e67
  (fn [xs]
    (letfn [(w ([a b c] (when (and (not= :e a) (= a b c)) a))
              ([[a b c]] (w a b c)))]
      (some #(and (not= :e %) %)
        (conj
          (map w xs)
          (w (first (first xs)) (first (second xs)) (first (last xs)))
          (w (second (first xs)) (second (second xs)) (second (last xs)))
          (w (last (first xs)) (last (second xs)) (last (last xs)))
          (w (first (first xs)) (second (second xs)) (last (last xs)))
          (w (last (first xs)) (second (second xs)) (first (last xs))))))))

(defcheck solution-5b30584c
  (fn [board]
    (let [row-res (fn [[sq1 sq2 sq3]] (if (and (not= sq1 :e) (= sq1 sq2 sq3))
                                        sq1
                                        nil))
          cols (apply map vector board)
          diag-idxs [[[0 0] [1 1] [2 2]] [[0 2] [1 1] [2 0]]]
          diags (map (partial map (partial get-in board)) diag-idxs)]
      (some (partial some row-res) [board cols diags]))))

(defcheck solution-5bc7fc
  (fn ttt [coll]
    (let [[a b c] (nth coll 0),
          [d e f] (nth coll 1),
          [g h i] (nth coll 2)]
      (if (= :e a b c d e f g h i)
        nil
        (cond
          (and (= a b c) (not= :e a)) a
          (and (= d e f) (not= :e d)) d
          (and (= a b c) (not= :e a)) a
          (and (= d e f) (not= :e d)) d
          (and (= g h i) (not= :e g)) g
          (and (= a d g) (not= :e a)) a
          (and (= b e h) (not= :e b)) b
          (and (= c f i) (not= :e c)) c
          (and (= a e i) (not= :e a)) a
          (and (= c e g) (not= :e c)) c
          :else nil)))))

(defcheck solution-5c25393b
  (fn [x]
    (let [lines (conj x
                  (map first x)
                  (map second x)
                  (map last x)
                  [(ffirst x) (second (second x)) (last (last x))]
                  [(last (first x)) (second (second x)) (first (last x))])]
      (some {[:x :x :x] :x, [:o :o :o] :o} lines))))

(defcheck solution-5c5dce49
  (fn f1 [c]
    (let [c (vec (flatten c))
          l [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]]
      (loop [[af & ar] l]
        (when-not (nil? af)
          (condp #(apply = %1 %2) (replace c af)
            :x :x
            :o :o
            (recur ar)))))))

(defcheck solution-5c749337
  (fn [board]
    (first (first (filter #(or (every? #{:x} %)
                               (every? #{:o} %))
                    (concat (map (fn [combo]
                                   (map #(get-in board %) combo))
                              [[[0 0] [1 1] [2 2]]
                               [[0 2] [1 1] [2 0]]
                               [[0 0] [1 0] [2 0]]
                               [[0 1] [1 1] [2 1]]
                               [[0 2] [1 2] [2 2]]
                               ])
                            board))))))

(defcheck solution-5c819785
  (fn [v] (let [ss (conj #{} (conj #{} ((v 0) 0) ((v 1) 1) ((v 2) 2))
                     (conj #{} ((v 0) 2) ((v 1) 1) ((v 2) 0))
                     (conj #{} ((v 0) 0) ((v 1) 0) ((v 2) 0))
                     (conj #{} ((v 0) 1) ((v 1) 1) ((v 2) 1))
                     (conj #{} ((v 0) 2) ((v 1) 2) ((v 2) 2))
                     (into #{} (v 0))
                     (into #{} (v 1))
                     (into #{} (v 2)))
                fss (filter #(and (= 1 (count %)) (not (contains? % :e))) ss)]
            (first (first fss)))))

(defcheck solution-5cb55818
  (fn winner? [rows]
    (let [[trow mrow brow] rows
          win? (fn [row]
                 (let [fst (first row)]
                   (if (and (not= fst :e) (apply = row))
                     fst
                     nil)))]
      (some win? (concat rows (map vector trow mrow brow) (vector (vector (first trow) (second mrow) (last brow))) (vector (vector (last trow) (second mrow) (first brow))))))))

(defcheck solution-5cce72ca
  (fn [board]
    (let [transpose #(apply map list %)
          winner? (partial apply =)
          rows board
          cols (transpose board)
          diagonals [(take-nth 4 (flatten board))
                     (take-nth 4 (flatten (map reverse board)))]
          remove-empty (partial remove (partial every? #(= :e %)))
          candidates (remove-empty (concat rows cols diagonals))]
      (ffirst (filter winner? candidates)))))

(defcheck solution-5ce895f7
  (fn winner [[[a b c :as top]
               [d e f :as middle]
               [g h i :as bottom]]]
    (let [left     [a d g]
          center   [b e h]
          right    [c f i]
          forward  [a e i]
          backward [c e g]]
      (when-let [winner (some #(when (or (every? #{:x} %) (every? #{:o} %)) %)
                          [top middle bottom
                           left center right
                           forward backward])]
        (first winner)))))

(defcheck solution-5ce8a123
  (fn __ [board]
    (let [revert-board (for [i (range 3)] (map #(nth % i) board))
          diag (fn [modifier] (for [i (range 3)] (-> board (nth (modifier i)) (nth i))))]
      (->> board
        (concat revert-board)
        (cons (diag identity))
        (cons (diag (partial - 2)))
        (filter (partial apply =))
        (filter #(not= (first %) :e))
        first first))))

(defcheck solution-5d565ed7
  (fn analyze-tic-tac-toe [board]
    (letfn [(row-filled? [board row value]
              (every? #(= value %) (nth board row))
              )
            (col-filled? [board col value]
              (every? #(= value %) (map #(nth % col) board))
              )
            (board-at [board x y]
              (nth (nth board x) y)
              )
            (diag-filled? [board value]
              (or (every? #(= value %) (map #(board-at board % %) (range 3)))
                  (every? #(= value %) (map #(board-at board % (- 2 %)) (range 3)))
                  )
              )
            (value-wins? [board value]
              (or (some #(row-filled? board % value) (range 3))
                  (some #(col-filled? board % value) (range 3))
                  (diag-filled? board value)
                  )
              )
            ]
      (first (filter #(value-wins? board %) '(:x :o)))
      )
    ))

(defcheck solution-5d8864bf
  (fn [[ [a b c] [d e f] [g h i] ]]
    (let [s [[a b c], [d e f], [ g h i], [a d g], [b e h ], [c f i ], [a e i] , [c e g]]
          t (fn [[x y z]]  (if (= x y z) x ))
          u (fn [v] (if (= :e v) nil v))]
      (u (first (filter #(not (nil? %)) (map t s)))))))

(defcheck solution-5d916b2f
  (fn [[r1 r2 r3 :as m]] (let [[c1 c2 c3] (apply map list m)
                               d1 (list (first c1) (second c2) (last c3))
                               d2 (list (last c1) (second c2) (first c3))
                               s (list r1 r2 r3 c1 c2 c3 d1 d2)]
                           (cond
                             (some #(= % [:x :x :x]) s) :x
                             (some #(= % [:o :o :o]) s) :o
                             :else nil))))

(defcheck solution-5d99cf4c
  (fn [xy]
    (some identity
      (map #(cond
              (every? #{:x} %) :x
              (every? #{:o} %) :o
              :else nil)
        (concat
         (for [r xy] r)
         (map #(for [r xy] (nth r %)) (range 3))
         [(map #(nth (nth xy %) %) (range 3))]
         [(map #(nth (nth xy (- 2 %)) %) (range 3))])))))

(defcheck solution-5dc5c467
  (fn [b]
    (let [l (set (map
                   #(set (for [[x y] %] ((b y) x)))
                   [[[0 0] [0 1] [0 2]]
                    [[1 0] [1 1] [1 2]]
                    [[2 0] [2 1] [2 2]]
                    [[0 0] [1 0] [2 0]]
                    [[0 1] [1 1] [2 1]]
                    [[0 2] [1 2] [2 2]]
                    [[0 0] [1 1] [2 2]]
                    [[0 2] [1 1] [2 0]]]))]
      (if (contains? l #{:x})
        :x (if (contains? l #{:o})
             :o nil)))))

(defcheck solution-5e3df32d
  (fn [board]
    (letfn [(win? [row]
              (if (and (apply = row) (not (= :e (first row))))
                (first row)
                nil))]
      (some identity
        (concat
         (map win? board)
         (for [i (range 3)] (win? (map #(nth % i) board)))
         [(win? (for [i (range 3)] (nth (nth board i) i)))]
         [(win? (for [i (range 3)] (nth (nth board i) (- 2 i))))])))))

(defcheck solution-5e403c7d
  #(ffirst
     (filter
       #{[:x :x :x] [:o :o :o]}
       (partition 3 (for [y [0 1 2 0 3 6 0 4 8 1 4 7 2 5 8 2 4 6 3 4 5 6 7 8]] ((% (quot y 3)) (rem y 3)))))))

(defcheck solution-5f2f8033
  (fn[t]
    (let [r [0 1 2]
          l (concat
             t
             (map (fn[i](map #(get % i) t)) r)
             [(map get t r)
              (map get t [2 1 0])])]
      (cond
        (some (fn [s] (every? #(= :x %) s)) l) :x
        (some (fn [s] (every? #(= :o %) s)) l) :o
        :else nil))))

(defcheck solution-5f4f5b51
  (fn [board]
    (let [w [
             [[0 0] [0 1] [0 2]]
             [[1 0] [1 1] [1 2]]
             [[2 0] [2 1] [2 2]]
             [[0 0] [1 0] [2 0]]
             [[0 1] [1 1] [2 1]]
             [[0 2] [1 2] [2 2]]
             [[0 0] [1 1] [2 2]]
             [[0 2] [1 1] [2 0]]]
          isMatch (fn [p]
                    (some (fn [l] (every? (fn [[r c]] (= p ((board c) r)) ) l)) w))]
      (cond
        (isMatch :x) :x
        (isMatch :o) :o
        :else nil))))

(defcheck solution-60124cac
  (fn [a m r b]
    (let [v [2 1 0]
          [r & _] (some
                    #(if (and (a not= :e %) (a = %)) %)
                    (concat b
                            (m
                              (fn [r] (m #(% %2) b r))
                              `[~(r) ~v ~@(m repeat v)])))]
      r)) apply map range)

(defcheck solution-601f38e4
  (fn who-win [score board]
    (cond
      (score :x board) :x
      (score :o board) :o
      :else            nil)) (fn score [who board]
                               (letfn [(nths [[i j]] (nth (nth board i) j))
                                       (win? [line] (every? #(= % who) line))]
                                 (some win?
                                   (concat
                                    board
                                    (apply map list board)
                                    (map #(map nths %) [[[0 0] [1 1] [2 2]] [[0 2] [1 1] [2 0]]]))))))

(defcheck solution-6026bd60
  (fn who-won? [[one two three]]
    (let [verts (map #(vector %1 %2 %3) one two three)
          diags (conj '() (vector (one 0) (two 1) (three 2)) (vector (one 2) (two 1) (three 0)))
          all (concat verts diags (list one two three))]
      (first (first (filter #(and (apply = %) (not= :e (first %))) all))))))

(defcheck solution-60742395
  (fn tic-tac-toe [t]
    (let [wins #{7 56 448 73 146 292 273 84}
          serie-of-bin (iterate (partial * 2) 1)
          flat (flatten t)
          mapper (fn [xo] #(if (= %1 xo) 1 0))
          reducer (fn [ss] (reduce #(+ %1 (apply * %2)) 0 (map vector ss serie-of-bin)))
          xs (map (mapper :x) flat)
          ys (map (mapper :o) flat)
          xres (reducer xs)
          yres (reducer ys)
          checker (fn [res] (some zero? (map #(bit-and-not %1 res) wins)))
          ]
      (if (checker xres)
        :x
        (if (checker yres)
          :o
          nil)
        )
      )))

(defcheck solution-60a004c6
  (fn ttt [[[ul um ur][ml mm mr][ll lm lr]]]
    (let [lines [[ul um ur][ml mm mr][ll lm lr][ul ml ll][um mm lm][ur mr lr][ul mm lr][ll mm ur]]]
      (some #{:x :o} (mapv (fn [[a b c]] (if (= a b c) a :e)) lines)))))

(defcheck solution-61df440a
  (fn [b]
    (let [p   (fn [[x y]] ((b x) y))
          all (concat b
                      (apply mapv vector b)
                      [(map p [[0 2] [1 1] [2 0]])]
                      [(map p [[0 0] [1 1] [2 2]])])
          f   (fn [s] (some true? (map (fn [c] (every? #(= % s) c)) all)))]
      (cond (f :x) :x
            (f :o) :o
            :else nil))))

(defcheck solution-62dc547c
  (fn [board]
    (letfn [(cols [board] (map (fn [a b c] [a b c]) (first board) (second board) (nth board 2)))
            (diag [board dir] [(map (fn [a] (nth (nth board (if (= dir :f) (- 2 a) a)) a)) (range 3))])]
      (let [all-runs (concat board (cols board) (diag board :f) (diag board :b))
            r (some #(if (apply = %) (first %)) all-runs)]
        (if (= r :e) nil r)))))

(defcheck solution-6316f243
  (letfn [(transpose
            [matrix]
            (apply mapv vector matrix))

          (diagonal'
            [matrix]
            (->> (range (count matrix))
              (mapv #(get-in matrix [% %]))))

          (diagonals
            [matrix]
            (mapv diagonal'
              [matrix
               (mapv (comp vec rseq)
                 matrix)]))


          (check
            [player board]
            (some (partial every? #{player})
              board))

          (wins?
            [player board]
            (some (partial check player)
              ((juxt identity
                 transpose
                 diagonals)
               board)))]

    (fn tic-tac-toe [board]
      (some #(when (wins? % board) %)
        [:x :o]))))

(defcheck solution-634390d9
  (fn tic-tac-toe [board]
    (let [apply-fs
                  (fn [fs point]
                    [((first fs) (first point))
                     ((second fs) (second point))])

          check-winner
                  (fn [fs board point]
                    (reduce #(if (and (identity %1) (not= :e %2) (= %2 %1)) %1 nil)
                      (map #(get-in board %)
                        (take 3
                          (iterate (partial apply-fs fs)
                            point)))))

          board-t (into [] (reverse (apply map vector board)))
          line [identity inc]
          diag [inc inc]
          edge (take 3 (map vector (range) (repeat 0)))]
      (some #{:x :o}
        (concat [(check-winner diag board   [0 0])
                 (check-winner diag board-t [0 0])]
                (map (partial check-winner line board)   edge)
                (map (partial check-winner line board-t) edge))))))

(defcheck solution-6380f6e
  (fn [board]
    (letfn [(rows [board] board)
            (cols [board] (apply map vector board))
            (diags [board]
              (let [coords [[[0 0] [1 1] [2 2]]
                            [[0 2] [1 1] [2 0]]]]
                (map #(map (fn [[r c]]
                             (nth (nth board r) c))
                        %)
                  coords)))
            (winner? [board]
              (or (every? (partial = :x) board)
                  (every? (partial = :o) board)))]
      (ffirst (filter winner? (concat (rows board)
                                      (cols board)
                                      (diags board)))))))

(defcheck solution-63ec1b8e
  (fn winner
    [board]
    (some (fn [line]
            (let [plays (set (map (partial get-in board) line))]
              (when-not (or (:e plays) (and (:x plays) (:o plays)))
                (first plays))))
      [[[0 0] [0 1] [0 2]]
       [[1 0] [1 1] [1 2]]
       [[2 0] [2 1] [2 2]]
       [[0 0] [1 0] [2 0]]
       [[0 1] [1 1] [2 1]]
       [[0 2] [1 2] [2 2]]
       [[0 0] [1 1] [2 2]]
       [[0 2] [1 1] [2 0]]])))

(defcheck solution-63f75555
  #(let [[_ & [_ & e :as d] :as c] (apply concat %)]
     (some (fn [[x y z]] (and (= x y z) (#{:o :x} x)))
       (concat % (map take-nth [3 3 3 4 2] [c d e c e])))))

(defcheck solution-64066c27
  (fn [m]
    ((fn victory[l]
       (let [res (filter
                   #(apply = %)
                   l
                   )]
         (if (empty? res)
           nil
           ( if (not= :e (first (first res)))
             (first (first res))
             nil
             ))))

     ((fn all3[[l1 l2 l3]]
        (let [s (concat l1 l2 l3)]
          (concat
           ;; lines
           (partition 3 s)
           ;; cols
           (map
             #(take-nth 3 (drop % s))
             (range 3))
           ;; \
           (list (take-nth 4 s))
           ;; /
           (list (->> s (take-nth 2) (drop 1) (drop-last)))

           ))) m))))

(defcheck solution-6438a242
  (fn [board]
    (let [vert (mapv vec board)
          transp (fn [board] (vec (apply map vector board)))
          hori (map vec (transp board))
          get-diag-l (fn [board] (reduce (fn [[acc pos] coll]
                                           [(conj acc
                                              (nth coll (inc pos)))
                                            (inc pos)])
                                   [[] -1] board))
          get-diag-r (fn [board] (reduce (fn [[acc pos] coll]
                                           [(conj acc
                                              (nth coll (dec pos)))
                                            (dec pos)])
                                   [[] 3] board))
          diag-ll (-> vert get-diag-l first)
          diag-r (-> vert get-diag-r first)
          comb-set (->
                     (apply merge
                       (into #{} vert)
                       (into #{} hori))
                     (conj diag-r diag-ll))]
      (cond (some #{[:x :x :x]} comb-set) :x
            (some #{[:o :o :o]} comb-set) :o
            :else nil))))

(defcheck solution-64860144
  (fn win? [b]
    (let [wins [[0 1 2] [3 4 5] [6 7 8]
                [0 3 6] [1 4 7] [2 5 8]
                [0 4 8] [2 4 6]]
          fb (vec (apply concat b))
          c (for [v wins] (map fb v))]
      (cond
        (some (partial = '(:x :x :x)) c) :x
        (some (partial = '(:o :o :o)) c) :o
        ))))

(defcheck solution-6494d998
  (fn [board]
    (let [x-row (repeat 3 :x)
          o-row (repeat 3 :o)
          straight #(or (some #{x-row} %)
                        (some #{o-row} %)
                        nil)]
      (or (first (straight board)) ; win by row
          (let [transposed (apply map list board)]
            (first (straight transposed))) ; win by column
          (let [diagonals [(map #(get-in board [% %])
                             [0 1 2])
                           (map #(get-in board [%1 %2])
                             [0 1 2] [2 1 0])]]
            (first (straight diagonals))) ; win by diagonal
          nil))))

(defcheck solution-64e7febc
  (let [winner-from
             (fn winner-from
               [three]
               (if (every? #(= :o %) three) :o
                                            (if (every? #(= :x %) three) :x nil)))
        winner-from-many
             (fn winner-from-many
               [many]
               (reduce (fn [accum next] (if (nil? next) accum next))
                 (map winner-from many)))
        rows (fn rows [board] board)
        cols (fn cols [board] [(map #(nth % 0) board)
                               (map #(nth % 1) board)
                               (map #(nth % 2) board)])
        digs (fn digs [board] [[(nth (nth board 0) 0)
                                (nth (nth board 1) 1)
                                (nth (nth board 2) 2)]
                               [(nth (nth board 0) 2)
                                (nth (nth board 1) 1)
                                (nth (nth board 2) 0)]])]
    (fn tic-tac-toe-winner
      [board]
      (winner-from-many (concat (rows board) (cols board) (digs board))))))

(defcheck solution-64f8cdd8
  (fn [ttt]
    (let [lines (->> (range 3)
                  (map
                    (fn [y]
                      (map #(vector [% y] [y %] [% %] [(- 2 %) %])
                        (range 3))))
                  (mapcat #(apply map vector %))
                  distinct
                  (map (fn [l] (map #(get-in ttt %) l))))
          pw (fn [p] (->> lines
                       (map (fn [l] (filter #(= p %) l)))
                       (map count)
                       (some #(= 3 %))))]
      (cond (pw :o) :o
            (pw :x) :x
            :else nil))))

(defcheck solution-65e86e07
  (fn [board]
    (let [zero-to-two (range 0 3)
          check (fn [x1 y1 x2 y2 x3 y3]
                  (let [pair1 ((board x1) y1)
                        pair2 ((board x2) y2)
                        pair3 ((board x3) y3)
                        all-equal #(= % pair1 pair2 pair3)]
                    (cond (all-equal :o) :o
                          (all-equal :x) :x
                          :else          nil)))
          tic-tac-toe (fn [t i]
                        (cond (= t :horiz) (check i 0 i 1 i 2)
                              (= t :vert)  (check 0 i 1 i 2 i)
                              :else   (if (== i 0)
                                        (check 0 0 1 1 2 2)
                                        (check 0 2 1 1 2 0))))]

      (if-let [win-row (some #(tic-tac-toe :horiz %) zero-to-two)]
        win-row
        (if-let [win-col (some #(tic-tac-toe :vert %) zero-to-two)]
          win-col
          (when-let [win-horiz (or (tic-tac-toe :diag 0) (tic-tac-toe :diag 2))]
            win-horiz))))))

(defcheck solution-664095bb
  (fn ttt [rows]
    (let [columns (apply map vector rows)
          diagonals (map (partial map get rows) [(range 3) (reverse (range 3))])
          longest (fn [player]
                    (->> (concat rows columns diagonals)
                      (map (comp count (partial filter #{player})))
                      (reduce max)))
          winner (filter (comp (partial = 3) val) {:x (longest :x) :o (longest :o)})]
      (if (seq winner) (first (first winner)) nil))))

(defcheck solution-66c8b2d0
  (fn [[r1 r2 r3]]
    (let [all-comb [r1 r2 r3 [(first r1) (first r2) (first r3)] [(second r1) (second r2) (second r3)] [(last r1) (last r2) (last r3)]
                    [(first r1) (second r2) (last r3)]  [(last r1) (second r2) (first r3)]
                    ]
          won (fn [s symb](every? #(= symb %) s))
          ]
      (or (first (first (filter #(won % :x) all-comb))) (first (first (filter #(won % :o) all-comb))))
      )
    ))

(defcheck solution-66e32b42
  (fn [b]
    (letfn [(cords-seq []
              (concat (map #(map vector (repeat %) (range 3)) (range 3))
                      (map #(map vector (range 3) (repeat %)) (range 3))
                      [[[0 0] [1 1] [2 2]]]
                      [[[0 2] [1 1] [2 0]]]))
            (won? [x]
              (some (fn [cords] (every? #(= x (get-in b %)) cords))
                (cords-seq)))]
      (cond
        (won? :x) :x
        (won? :o) :o
        true nil))))

(defcheck solution-66edc597
  (fn [board]
    (let [lines [(nth board 0) ; row 1
                 (nth board 1) ; row 2
                 (nth board 2) ; row 3
                 (map first board) ; column 1
                 (map second board) ; column 2
                 (map #(nth % 2) board) ;column 3
                 (map #(nth %1 %2) board (range)) ; upper left to lower right
                 (map #(nth %1 (- 2 %2)) board (range))]] ; lower left to upper right
      (cond (some identity (map #(apply = :x %) lines)) :x
            (some identity (map #(apply = :o %) lines)) :o
            :else nil))))

(defcheck solution-6754ed8
  (fn verify-ttt? [board]
    (let [transpose (fn [[x y]] [y x])
          shift (fn [q [x y]] [(+ x q) y])
          shifts (mapv #(partial shift %) (range 3))
          test-funs (for [tr [identity transpose] sh shifts]
                      (comp tr sh))
          get-in-t (fn [m transform coords]
                     (mapv #(get-in m (transform %)) coords))
          straights (set (map #(get-in-t board % [[0 0] [0 1] [0 2]]) test-funs))
          all (set (concat [(get-in-t board identity [[0 0] [1 1] [2 2]])]
                           [(get-in-t board identity [[0 2] [1 1] [2 0]])]
                           straights))]
      (cond (all [:x :x :x]) :x
            (all [:o :o :o]) :o))))

(defcheck solution-681dc474
  (fn tic-tac-toe [xs]
    (letfn [(win [xs]
              (cond
                (apply = :o xs) :o
                (apply = :x xs) :x
                ))]
      (->> (map #(map (fn [k] (get-in
                                xs k)) %)
             (concat
              (partition 3 (for [i (range 3) j (range 3)] [j i]))
              (partition 3 (for [i (range 3) j (range 3)] [i j]))
              (partition 3 (for [i (range 3)] [i i]))
              (partition 3 (for [i (range 3)] [i (- 2 i)]))
              ))
        (map win)
        (filter #(or (= :x %) (= :o %)))
        (first)

        ))
    ))

(defcheck solution-69317e84
  #(->> (concat % (apply map list %)
                [(for [x [0 1 2]] ((% x) x))
                 [((% 2) 0) ((% 1) 1) ((% 0) 2)]])
     (some {[:o :o :o] :o [:x :x :x] :x})))

(defcheck solution-69737272
  (fn [board]
    (letfn [(at2 [seq [x y]] (nth (nth seq y) x))
            (win? [seq] (let [s (first seq)]
                          (and (or (= s :o) (= s :x)) (every? #(= % s) seq))))]
      (ffirst (filter win?
                (map (fn [three] (map #(at2 board %) three))
                  (concat (map (fn [y] (vec (map (fn [x] [x,y]) (range 0 3)))) (range 0 3))
                          (map (fn [x] (vec (map (fn [y] [x,y]) (range 0 3)))) (range 0 3))
                          [[[0,0], [1,1], [2,2]]]
                          [[[0,2], [1,1], [2, 0]]]
                          )))))))

(defcheck solution-698fbbf3
  (fn [[row1 row2 row3 :as horizontals]]
    (let [verticals (map (fn [i] [(row1 i) (row2 i) (row3 i)]) (range 3))
          diagonal1 [(row1 0) (row2 1) (row3 2)]
          diagonal2 [(row1 2) (row2 1) (row3 0)]
          all-rows (concat [diagonal1 diagonal2] horizontals verticals)]
      (loop [[row & rows] all-rows]
        (cond
          (nil? row) nil
          (every? #{:x} row) :x
          (every? #{:o} row) :o
          :else (recur rows))))))

(defcheck solution-699c79ac
  (fn tic-tac-toe
    [board]
    (let [wins [[[0 0] [0 1] [0 2]]
                [[1 0] [1 1] [1 2]]
                [[2 0] [2 1] [2 2]]
                [[0 0] [1 0] [2 0]]
                [[0 1] [1 1] [2 1]]
                [[0 2] [1 2] [2 2]]
                [[0 0] [1 1] [2 2]]
                [[0 2] [1 1] [2 0]]]
          actuals (map (fn [row] (map #(get-in board %) row)) wins)
          x? (->> actuals (filter #(= % [:x :x :x])) (count) (pos?))
          o? (->> actuals (filter #(= % [:o :o :o])) (count) (pos?))]
      (cond
        x? :x
        o? :o
        :else nil))))

(defcheck solution-69c24272
  (fn [b]
    (letfn
     [(anarow
        [a b c]
        (if (and (= a b) (= b c))
          a nil))
      (akabd
        [b]
        (let [a1 (first b) a2 (second b) a3 (last b)
              a11 (first a1) a12 (second a1) a13 (last a1)
              a21 (first a2) a22 (second a2) a23 (last a2)
              a31 (first a3) a32 (second a3) a33 (last a3)]
          [(anarow a11 a12 a13) (anarow a21 a22 a23) (anarow a31 a32 a33)
           (anarow a11 a21 a31) (anarow a12 a22 a32) (anarow a13 a23 a33)
           (anarow a11 a22 a33) (anarow a13 a22 a31)]))
      (findnn
        [l]
        (some #(when (not= nil %) %) l)
        )
      (check
        [x]
        (if (or (= x :x) (= x :o)) x nil)
        )]
      (-> b akabd findnn check)
      )))

(defcheck solution-6bfd3068
  (fn [rows]
    (let [cols (apply map list rows)
          diags [[((rows 0) 0) ((rows 1) 1) ((rows 2) 2)]
                 [((rows 0) 2) ((rows 1) 1) ((rows 2) 0)]]
          lines (concat rows cols diags)]
      (if (some identity
            (map (fn [x] (every? #(= :x %) x)) lines)) :x
                                                       (if (some identity
                                                             (map (fn [x] (every? #(= :o %) x)) lines)) :o
                                                                                                        nil)))))

(defcheck solution-6c08a2a
  (fn [t]
    (let [[a b c] t
          [d e f] a
          [g h i] b
          [j k l] c
          q    #(if (= 1 (count %)) % false)
          row  (map set t)
          col  (map (comp set list) a b c)
          diag (map set [[d h l] [f h j]])
          res  (first (or (some q row) (some q col) (some q diag)))]
      (if (= :e res) nil res))))

(defcheck solution-6c4d4388
  (fn [board]
    (->> (conj (concat (map (fn [i] (map #(get-in board [% i]) [0 1 2])) [0 1 2])
                       board)
           [(get-in board [0 0]) (get-in board [1 1]) (get-in board [2 2])]
           [(get-in board [0 2]) (get-in board [1 1]) (get-in board [2 0])])
      (map #(dissoc (frequencies %) :e))
      (apply merge-with max)
      (filter #(= 3 (second %)))
      (ffirst))))

(defcheck solution-6c935f4b
  (fn find-wins
    [board]
    (let [generate-winners (fn [f] (partition 3 (for [x (range 0 3)
                                                      y (range 0 3)]
                                                  (f [x y]))))
          all-winning-squares (conj (into (generate-winners identity)
                                      (generate-winners (comp vec reverse)))
                                [[0 0] [1 1] [2 2]]
                                [[0 2] [1 1] [2 0]])]
      (first (filter (fn [r] (and r (not (= :e r))))
               (map #(let [line (map (fn[[x y]]
                                       (get (get board x) y)) %)]
                       (if (apply = line)
                         (first line)))
                 all-winning-squares))))))

(defcheck solution-6ce39e81
  (fn [grid]
    (let [mapped-grid (map (fn [x y] {x y}) (flatten grid) [8 1 6 3 5 7 4 9 2])
          valid-combinations (filter (fn [x]
                                       (== (reduce + (vals x))
                                         15)) (for [x (range 1 10)
                                                    y (range (inc x) 10)
                                                    z (range (inc y) 10)]
                                                {:x x :y y :z z}))
          xs (reduce (fn [a v]
                       (let [x (:x v)]
                         (if x (conj a x) a))) '() mapped-grid)
          os (reduce (fn [a v]
                       (let [o (:o v)]
                         (if o (conj a o) a))) '() mapped-grid)
          has-won (fn [vals] (reduce (fn [result combination]
                                       (or result
                                           (and (some #{(:x combination)} vals)
                                                (some #{(:y combination)} vals)
                                                (some #{(:z combination)} vals)
                                                true))) false valid-combinations))
          ]
      (cond (has-won xs) :x
            (has-won os) :o
            :else nil)
      )))

(defcheck solution-6ce735c8
  (fn [[[a1 a2 a3]
        [b1 b2 b3]
        [c1 c2 c3]]]
    (let [win? (fn [[x y z]] (when (and (not= x :e) (= x y z)) x))
          lines [[a1 a2 a3]
                 [b1 b2 b3]
                 [c1 c2 c3]
                 [a1 b1 c1]
                 [a2 b2 c2]
                 [a3 b3 c3]
                 [a1 b2 c3]
                 [a3 b2 c1]]]
      (some win? lines))))

(defcheck solution-6d669e96
  (fn [B]
    (letfn [(p [x y] (nth (nth B x) y))
            (w [& r] (let [R (reduce conj #{} r)] (cond (= #{:x} R) :x (= #{:o} R) :o)))]
      (or
       (w (p 0 0) (p 1 1) (p 2 2))
       (w (p 0 2) (p 1 1) (p 2 0))
       (some identity
         (for [i (range 3)]
           (or
            (w (p i 0) (p i 1) (p i 2))
            (w (p 0 i) (p 1 i) (p 2 i)))))))))

(defcheck solution-6d6ee662
  (fn [lst]
    (let [a (map #(apply str %) lst)
          b (map #(apply str %) (partition 3 (apply interleave lst)))
          c (map #(apply str %)
              [[(ffirst lst) (second (second lst)) (last (last lst))]
               [(first (last lst)) (second (second lst)) (last (first lst))]])
          d (concat a b c)]
      (cond
        (some #(= % ":x:x:x") d) :x
        (some #(= % ":o:o:o") d) :o
        :else nil))))

(defcheck solution-6df09550
  (fn [[a b c :as all]]
    (let [func (fn [x y z] (if (or (= x y z :x) (= x y z :o)) x nil))
          v (some identity (map func a b c))
          h (some identity (map (fn [[x y z]] (if (or (= x y z :x) (= x y z :o)) x nil)) all))
          d1 (apply func (map #(get-in all %) [[0 0] [1 1] [2 2]]) )
          d2 (apply func (map #(get-in all %) [[0 2] [1 1] [2 0]]) )]
      (some identity [v h d1 d2]))))

(defcheck solution-6e45797d
  (fn checktic [board]
    (let [winner? (fn [row]
                    (cond
                      (= row [:x :x :x]) :x
                      (= row [:o :o :o]) :o
                      :else nil))
          rotated (apply map vector board)
          pull-diag (fn [v] (map-indexed (fn [idx itm] (nth itm idx)) v))
          diags [(pull-diag board) (pull-diag (reverse board))]
          rows (concat board rotated diags)]
      (some identity (map winner? rows)))))

(defcheck solution-6e5a1087
  (fn winner-tic-tac-toe [rows]
    (letfn [
            (column [rows i]
              (let [n (count rows)]
                (map #(nth % i) rows)))

            (inverse [rows]
              (let [n (count rows)]
                (map #(column rows %) (range 0 n))))

            (diagonal1 [rows]
              (let [n (count rows)]
                (map
                  #((rows %) %)
                  (range 0 n))))

            (diagonal2 [rows]
              (let [n (count rows)]
                (map
                  #((rows (dec (- n %))) %)
                  (range 0 n))))

            (all-comb [rows]
              (into (into rows (inverse rows)) [(diagonal1 rows) (diagonal2 rows)]))

            (get-wining-comb [rows]
              (let [combs (all-comb rows)]
                (filter
                  (fn [row]
                    (or (every? #(= % :x) row) (every? #(= % :o) row)))
                  combs)))
            ]
      (let [winning-combs (get-wining-comb rows)]
        (cond
          (empty? winning-combs) nil
          :else (first (first winning-combs)))))))

(defcheck solution-6ede7c05
  (fn [[top mid bottom :as board]]
    (let [xss
          (concat board (map vector top mid bottom)
                  [[(top 0) (mid 1) (bottom 2)]
                   [(top 2) (mid 1) (bottom 0)]])]
      (cond (some true? (map #(every? #{:x} %) xss)) :x
            (some true? (map #(every? #{:o} %) xss)) :o
            :else nil))))

(defcheck solution-6f3d205a
  (fn tictac [[[a1 a2 a3] [b1 b2 b3] [c1 c2 c3]]]
    (letfn [(line? [x y z] (and (not= :e x) (= x y z)))]
      (cond
        (line? a1 a2 a3) a1
        (line? b1 b2 b3) b1
        (line? c1 c2 c3) c1
        (line? a1 b1 c1) a1
        (line? a2 b2 c2) a2
        (line? a3 b3 c3) a3
        (line? a1 b2 c3) a1
        (line? a3 b2 c1) a3
        :else nil))))

(defcheck solution-6f63b3a4
  (fn [board]
    (let [row-wins
                           (for [x (range 0 3)] (for [y (range 0 3)] [y x]))
          col-wins
                           (for [x (range 0 3)] (for [y (range 0 3)] [x y]))
          diagonal-wins
                           (vector
                             (for [x (range 0 3) y (range 0 3) :when (= x y)] [x y])
                             (for [x (range 0 3) y (range 0 3) :when (= (+ x y) 2)] [x y]))
          wins (concat row-wins col-wins diagonal-wins)
          mark (fn [x y] (get-in board [x y]))
          has-winning-line (fn [pl]
                             (some
                               (fn [win]
                                 (every? (fn [[x y]] (= (mark x y) pl)) win))
                               wins))]
      (cond
        (has-winning-line :x) :x
        (has-winning-line :o) :o
        true nil))))

(defcheck solution-6f863cc9
  (fn p73
    [coll]
    (let [hor (map (fn [x] (map #(list %1 %2) (cycle [x]) (range 3))) (range 3))
          ver (map (fn [x] (map #(list %1 %2) (range 3) (cycle [x]))) (range 3))
          dia1 (map #(list %1 %2) (range 3) (range 3))
          dia2 (map #(list %1 %2) (range 3) (reverse (range 3)))
          all (conj (concat hor ver) dia1 dia2)]
      (first
        (keep (fn [l]
                (let [end (map #(get-in coll %) l)]
                  (if (apply = end)
                    (if (or (= (first end) :x) (= (first end) :o))
                      (first end)
                      nil)))) all)))))

(defcheck solution-6fb9ac5b
  (fn [rows]
    (let
     [checkxo (fn [pred] (if (pred :x) :x (if (pred :o) :o nil)))
      alleq (fn [c coll] (apply = c coll))
      hasgroupeq (fn [c groups] (some (partial alleq c) groups))
      cols (apply map vector rows)
      diag (map-indexed #(nth %2 %1) rows)
      bdiag (map-indexed #(nth %2 (- (dec (count (first rows))) %1)) rows)]
      (checkxo #(hasgroupeq % (concat rows cols [diag bdiag]))))))

(defcheck solution-70287bad
  (fn solve-it [ls]
    (letfn [(get-cols [ls]
              [(vec (map first ls))
               (vec (map second ls))
               (vec (map last ls))])
            (get-diagonals [ls]
              (let [idx1 [[0 0] [1 1] [2 2]]
                    idx2 [[2 0] [1 1] [0 2]]]
                [(vec (map #(get-in ls %) idx1))
                 (vec (map #(get-in ls %) idx2))]))
            (get-cda [ls]
              (vec (concat ls (get-cols ls) (get-diagonals ls))))
            (all-e [e ls]
              (apply = e ls))]

      (let [lls (get-cda ls)]
        (cond
          (some (partial all-e :x) lls) :x
          (some (partial all-e :o) lls) :o
          :else nil)))))

(defcheck solution-7055a7b3
  (fn [rows]
    (let [cols (apply map vector rows)
          diag (fn [x] (map-indexed #(nth %2 %1) x))
          lines (concat rows cols [(diag rows) (diag (map reverse rows))])
          sets (map set lines)]
      (first (some #{#{:x} #{:o}} sets)))))

(defcheck solution-7099406c
  (fn testwin [rows]
    (let [rowwin #(if (and (nil? ((set %) :e))
                           (= 1 (count (set %))))
                    (first (set %)) nil)
          cols (vec (apply map vector rows))
          hor (some rowwin rows)
          vert (some rowwin cols)
          dia1 (rowwin (map-indexed #(%2 %) rows))
          dia2 (rowwin (map-indexed #(%2 %) (reverse rows)))]
      (some identity [hor vert dia1 dia2])
      )))

(defcheck solution-70d86dc0
  (fn [rows]
    (let [equal? (fn [s] (reduce (fn [memo item] (if (and (= memo item) (not= :e item)) item nil)) (first s) (rest s)))
          cols (apply map vector rows)
          row  (some equal? rows)
          col (some equal? cols)
          diag1 (equal? (map #(nth (nth rows %) %) (range 3)))
          diag2 (equal? (map #(nth (nth rows (second %)) (first %)) [[0 2][1 1][2 0]]))]
      (or col row diag1 diag2))))

(defcheck solution-70e3b582
  (fn [xs]
    (let [game (vec (flatten xs))]
      (first (some #{#{:x} #{:o}}
               (map #(set (map game %))
                 [[0 1 2] [3 4 5] [6 7 8] [0 3 6]
                  [1 4 7] [2 5 8] [0 4 8] [2 4 6]]))))))

(defcheck solution-7112e696
  (fn [xss]
    (letfn [(has-won? [player [[v11 _ v13] [_ v22 _] [v31 _ v33] :as xss]]
              (or
               (some #(every? (partial = player) %) xss)
               (every? #(= (first %) player) xss)
               (every? #(= (second %) player) xss)
               (every? #(= (nth % 2) player) xss)
               (= v11 v22 v33 player)
               (= v13 v22 v31 player)))]
      (cond (has-won? :x xss) :x
            (has-won? :o xss) :o
            :else nil))))

(defcheck solution-724d7735
  (fn [seq]
    (let [test (conj seq
                 (map first seq)
                 (map second seq)
                 (map last seq)
                 (map #(get %2 %) [0 1 2] seq)
                 (map #(get %2 %) [2 1 0] seq))]
      (first
        (first
          (filter
            #(apply = %)
            (filter (fn [s] (not(some #(= :e %) s))) test
              )))))))

(defcheck solution-72b23b83
  (fn [b]
    (let [h (seq (filter #(apply = %) b))
          v (seq (filter #(apply = %) (apply map #(list % %2 %3) b)))
          d (seq (filter #(apply = %) [(map #(get-in b [% %]) (range 3))
                                       (map #(get-in b [% (- 2 %)]) (range 3))]))
          w (into #{} (concat h v d))]
      (first (or (w [:x :x :x]) (w [:o :o :o]))))))

(defcheck solution-7308377b
  (fn [[a b c]]
    (let [q (into [a b c
                   [(first a)(second b)(last c)]
                   [(first c)(second b)(last a)]] (map vector a b c))
          won? (fn [[x & r]] (when (and (apply = x r) (not= :e x)) x))]
      (first (remove nil? (map won? q))))))

(defcheck solution-7357a7ed
  (fn [b]
    (some identity (map #(condp = (set %) #{:x} :x #{:o} :o nil)
                     (concat b (apply map vector b)
                             [(for [x (range 3)] ((b x) (- 2 x)))
                              (for [x (range 3)] ((b x) x))])))))

(defcheck solution-735a0464
  (fn
    [c]
    (let [b (flatten c)
          al (for [l [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]]
               (map #(nth b %) l))
          r (filter #(not= % :e) (map first (filter #(apply = %) al)))]
      (if (empty? r)
        nil
        (first r)))))

(defcheck solution-739883ad
  (fn [[[x1 x2 x3 :as row-x]
        [y1 y2 y3 :as row-y]
        [z1 z2 z3 :as row-z]]]
    (let [ rows [ row-x row-y row-z ]
          cols [[x1 y1 z1] [x2 y2 z2] [x3 y3 z3]]
          digs [[x1 y2 z3] [x3 y2 z1]]
          all  (concat rows cols digs) ]

      (letfn [ (full? [v s] (every? #(= % v) s))
              (check [v]   (some #(full? v %) all)) ]
        (cond
          (check :x) :x
          (check :o) :o)))))

(defcheck solution-73b72e16
  (fn [src]
    (let [cnt (count src)
          vert (partition-all cnt (for [i (range  cnt) v (map #(get % i) src)] v))
          diag1 [(for [i (range cnt)] (get-in src [i i]))]
          diag2 [(for [i (range cnt)] (get-in src [(- cnt i 1) i]))]
          ]
      (letfn [(chk [x] (distinct(filter #(and(=(count %)1)(not= :e (key(first %))))(map frequencies x))))]

        (reduce (fn [x y] y) nil (map #(key(first %))(chk(concat src vert diag1 diag2))))
        )
      )
    ))

(defcheck solution-74388a2d
  (fn [[[a b c][d e f][g h i]]]
    (let [game [a b c d e f g h i]]
      (first (some #{#{:x} #{:o}}
               (map #(set (map game %))
                 [[0 1 2] [3 4 5] [6 7 8] [0 3 6]
                  [1 4 7] [2 5 8] [0 4 8] [2 4 6]]))))))

(defcheck solution-74431c5d
  (fn [board]
    (let [winner? (fn [s] (if (apply = s) (first s) nil))
          row-winner? (map winner? board)
          col-winner? (map winner? (apply map vector board))
          lft-diag-winner? (winner? (map get board (range 3)))
          rgt-diag-winner? (winner? (map get board (range 2 -1 -1)))]
      (some #{:x :o} (or (concat row-winner? col-winner? [lft-diag-winner? rgt-diag-winner?]))))))

(defcheck solution-74a0210b
  (fn [t]
    (let
     [c1 (map first t)
      c2 (map second t)
      c3 (map last t)
      tra1 (map #(get-in t %) [[0 0] [1 1] [2 2]])
      tra2 (map #(get-in t %) [[0 2] [1 1] [2 0]])
      t (conj t c1 c2 c3 tra1 tra2)]
      (some #(if (or
                  (= :e (first %))
                  (< 1 (count (distinct %))))
               nil
               (first %)) t))))

(defcheck solution-7591cde9
  (fn [board]
    (let [rows board
          cols (apply map list board)
          diag1 (map nth board '(0 1 2))
          diag2 (map nth board '(2 1 0))
          sets (concat rows cols (list diag1 diag2))]
      (cond
        (some (partial every? #{:x}) sets) :x
        (some (partial every? #{:o}) sets) :o
        true nil))))

(defcheck solution-75abe25
  (fn prob-0073
    [bd]
    (let [lines [[[0 0] [0 1] [0 2]]
                 [[1 0] [1 1] [1 2]]
                 [[2 0] [2 1] [2 2]]
                 [[0 0] [1 0] [2 0]]
                 [[0 1] [1 1] [2 1]]
                 [[0 2] [1 2] [2 2]]
                 [[0 0] [1 1] [2 2]]
                 [[0 2] [1 1] [2 0]]]

          mat-get (fn mat-get
                    [mt ix]
                    ((mt (first ix)) (second ix)))

          ttt-win-check (fn ttt-win-check
                          [bd ln]
                          (if (apply = (map #(mat-get bd %) ln))
                            (mat-get bd (first ln))
                            nil))
          ]

      (loop [ls lines]
        (let [l      (first ls)
              l-rst  (rest  ls)
              winner (ttt-win-check bd l)]
          (cond
            (or (= :x winner) (= :o winner)) winner
            (empty? l-rst)                   nil
            :else                            (recur l-rst)))))))

(defcheck solution-75ae559
  (let [transpose (partial apply map vector)
        down-right-diag (fn [rows]
                          (map #(get-in rows [% %])
                            (range 3)))
        winner-in-triplet (fn [[head :as trip]]
                            (when (and (apply = trip)
                                       (#{:x :o} head))
                              head))]
    (fn [rows]
      (let [cols (-> rows transpose)
            diags [(-> rows down-right-diag)
                   (-> rows reverse vec down-right-diag)]]
        (some winner-in-triplet
          (concat rows cols diags))))))

(defcheck solution-75c45189
  (fn tictactoe [board]
    (letfn [(third [[a b c & rest]]
              c)
            (seq-winner [sq]
              (when-let [x (and (apply = sq) (first sq))]
                (if (= x :e)
                  nil
                  x)))
            (diags [mx]
              (list (map #(get-in mx %) [[0 0] [1 1] [2 2]])
                (map #(get-in mx %) [[0 2] [1 1] [2 0]])))
            (rows [mx]
              mx)
            (columns [mx]
              (map #(map % mx) [first second third]))]
      (some seq-winner (concat (diags board) (rows board) (columns board))))))

(defcheck solution-75d5bb1a
  (fn [[[x11 x12 x13] [x21 x22 x23] [x31 x32 x33] :as b]]
    (let [win (concat b (apply map vector b) [[x11 x22 x33] [x13 x22 x31]])]
      (first (some #{[:x :x :x] [:o :o :o]} win)))))

(defcheck solution-75df5d15
  (fn ttt [board]
    (let [
          eqOrNil (fn [lst]
                    (let [fst (first lst)]
                      (cond
                        (apply = lst) (cond
                                        (= :e fst) nil
                                        :else fst
                                        :else nil))))

          transpose (fn transpose [lst i]
                      (cond
                        (neg? i) '()
                        :else (cons (map #(nth % i) lst)
                                (transpose lst (dec i)))))

          check-cross (fn [board]
                        (let [
                              mid ((board 1) 1)
                              nw ((board 0) 0)
                              ne ((board 0) 2)
                              sw ((board 2) 0)
                              se ((board 2) 2) ]

                          (cond
                            (= mid :e) nil
                            (= mid nw se) mid
                            (= mid ne sw) mid
                            :else nil)))
          ]
      (first (filter #(not= nil %)
               (into
                 (cons
                   (check-cross board)
                   (map eqOrNil (transpose board 2)))
                 (map eqOrNil board)))))))

(defcheck solution-75f5834e
  (fn [ttt]
    (first
      (filter #(or (= % :x) (= % :o))
        (map #(first %)
          (filter #(apply = %)
            (concat (vector (ttt 0)) (vector (ttt 1)) (vector (ttt 2))
                    (for [x [[0 0 0] [1 1 1] [2 2 2] [0 1 2] [2 1 0]]]
                      (mapcat (fn [[f v]] (list (f v))) (partition 2 (interleave ttt x)))))))))))

(defcheck solution-763fc575
  (fn [[[a b c :as u] [d e f :as v] [g h i :as w]]]
    (let [p (fn [[x y z]] (if (and (not= x :e) (= x y z)) x))]
      (first (filter identity (map p [u v w [a d g] [b e h] [c f i] [a e i] [c e g]]))))))

(defcheck solution-765fff08
  (fn board-complete? [board]
    (let [n 3
          rows (for [y (range n)] (for [x (range n)] [x y]))
          cols (for [x (range n)] (for [y (range n)] [x y]))
          diags (list (for [x (range n)] [x x])
                  (for [x (range n)] [(- (- n 1) x) x]))
          lines (concat rows cols diags)
          positions (fn [board points]
                      (map (fn [[col row]] (-> board (nth row) (nth col))) points))
          all? (fn [xo board line]
                 (when (every? (partial = xo) (positions board line))
                   xo))]
      (some identity (concat (map (partial all? :x board) lines)
                             (map (partial all? :o board) lines))))))

(defcheck solution-7697119
  (fn
    [board]
    (let [key-to-indexes (->> (map-indexed
                                (fn [i row]
                                  (map-indexed (fn [j e] {:key e :index [i j]}) row))
                                board)
                           flatten
                           (group-by :key))
          win (fn [indexes]
                (and (> (count indexes) 2)
                     (or
                      (some #(> (count %) 2) (vals (group-by identity (map first indexes))))
                      (some #(> (count %) 2) (vals (group-by identity (map second indexes))))
                      (> (count (filter #{[0 0] [1 1] [2 2]} indexes)) 2)
                      (> (count (filter #{[0 2] [1 1] [2 0]} indexes)) 2))))]
      (or (when (win (map :index (:x key-to-indexes)))
            :x)
          (when (win (map :index (:o key-to-indexes)))
            :o)))))

(defcheck solution-76b3531
  (fn [board]
    (let [scores (->> [[4 9 2] [3 5 7] [8 1 6]]
                   (map #(map vector %1 %2) board)
                   (apply concat)
                   (group-by first))
          sum #(apply + (map second %))
          scores' (into {} (for [[k v] scores] [(sum v) k]))]
      (scores' 15))))

(defcheck solution-76d3d106
  (fn win [boarder]
    (letfn [
            (down-dir [[x y]] [(inc x) y])

            (right-dir [[x y]] [x (inc y)])

            (right-down-dir [[x y]] [(inc x) (inc y)])

            (left-down-dir [[x y]] [(inc x) (dec y)])

            (all-posiontions []
              (for [x (range 3)
                    y (range 3)]
                [x y]))

            (win-positions [f [x y]]
              (take 3 (iterate f [x y])))

            (who-win [[a b c]]
              (if (or (= a b c :x) (= a b c :o))
                a))
            (is-position-suitable? [[x y]]
              (and (> x -1) (> y -1) (< x 3) (< y 3)))
            (all-win-positions []
              (apply concat
                (for [dir-fn [down-dir right-dir right-down-dir left-down-dir]]
                  (filter #(every? is-position-suitable? %)
                    (map #(win-positions dir-fn %) (all-posiontions))))))]
      (first
        (remove nil?
          (map who-win
            (map #(for [xy-pos %] (get-in boarder xy-pos)) (all-win-positions))))))))

(defcheck solution-76f4e93a
  (fn tic-tac-toe [board]
    (let
     [
      horizontal-paths board

      vertical-paths
                       (map
                         (fn [x]
                           (map
                             (fn [row]
                               (nth row x)
                               )
                             board
                             )
                           )
                         (range 3)
                         )

      path-top-left
                       (map
                         (fn [i]
                           (nth
                             (nth board i)
                             i
                             )
                           )
                         (range 3)
                         )

      path-top-right
                       (map
                         (fn [i]
                           (nth
                             (nth board i)
                             (- 2 i)
                             )
                           )
                         (range 3)
                         )

      cross-paths
                       [path-top-left path-top-right]

      paths
                       (concat
                        horizontal-paths
                        vertical-paths
                        cross-paths
                        )

      wins?
                       (fn [player]
                         (some
                           (fn [p]
                             (every?
                               (fn [v]
                                 (= v player)
                                 )
                               p
                               )
                             )
                           paths
                           )
                         )
      ]
      (cond
        (wins? :x) :x
        (wins? :o) :o
        :else
        nil
        )
      )
    ))

(defcheck solution-7711544c
  (fn tic [coll]
    (letfn [(get-coll [idx0 idx1]
              (if (or (< idx0 0) (< idx1 0) (> idx0 2) (> idx1 2))
                nil
                ((coll idx0) idx1)))
            (map2 [position]
              {(some #(true? %)
                 (map (fn [coll2]
                        (every? #(true? %)
                          (for [x [position]
                                y coll2]
                            (let [result (get-coll (+ (x 0) (y 0)) (+ (x 1) (y 1)))]
                              (if (= result :e)
                                false
                                (= result (apply get-coll position)))))))
                   [[[-1 0] [1 0]]
                    [[0 1] [0 2]]
                    [[0 -1] [0 -2]]
                    [[0 -1] [0 1]]
                    [[-1 -1] [1 1]]
                    [[1 -1] [-1 1]]]))
               (apply get-coll position)
               })]
      ((apply merge (map (fn [position]
                           (map2 position))
                      [[1 0] [1 1] [1 2]])) true))))

(defcheck solution-771f567c
  (fn tic [t]
    (letfn [(check [v] (partial = v))
            (threerange [] (take 3 (range)))
            (checkrow [v i] (every? (check v) (nth t i)))
            (checkcol [v i] (every? (check v) (map #(nth % i) t)))
            (checkrowcol [v i] (or (checkrow v i) (checkcol v i)))
            (checkall [v] (some true? (map #(checkrowcol v %) (threerange))))
            (nnth [i j] (nth (nth t j) i))
            (checkdiag [v] (or (every? (check v) (map (fn [a] (nnth a a)) (threerange)))
                               (every? (check v) (map (fn [a] (nnth (- 2 a) a)) (threerange))))
              )]
      (cond
        (or (checkall :o) (checkdiag :o)) :o
        (or (checkall :x) (checkdiag :x)) :x
        :else nil
        )
      )
    ))

(defcheck solution-774a533c
  (fn check [board]
    (let [flt (flatten board)
          check-row (fn [r x] (every? #(= x %) r))
          get-rows (concat board
                           (partition 3 (apply interleave board))
                           [(take-nth 4 flt)
                            (take 3 (take-nth 2 (drop 2 flt)))])
          win? (fn [x] (some #(check-row % x) get-rows))]
      (cond
        (win? :x) :x
        (win? :o) :o
        :else nil))))

(defcheck solution-776b8bde
  (fn [[[a b c] [d e f] [g h i]]]
    (let [m {[:x :x :x] :x, [:o :o :o] :o}]
      (or (m [a b c]) (m [d e f]) (m [g h i]) (m [a d g]) (m [b e h]) (m [c f i]) (m [a e i]) (m [c e g])))))

(defcheck solution-77e02e24
  (fn [board]
    (let
     [flipped (apply map vector board)
      diagonal (map-indexed #(get %2 %1) board)
      diagonal2 (map-indexed #(get %2 (- 2 %1)) board)
      all (concat board flipped [diagonal diagonal2])]
      (cond
        (some (partial = [:x :x :x]) all) :x
        (some (partial = [:o :o :o]) all) :o
        :else nil
        ))))

(defcheck solution-78a97d70
  (fn
    [[x y z]]
    (let [f first
          s second
          t last]
      (some #(and (apply = %) (some #{:x :o} %))
        `(~x ~y ~z
          ~@(map #(map % [x y z]) [f s t])
          ~[(f x) (s y) (t z)]
          ~[(t x) (s y) (f z)])))))

(defcheck solution-78b918ca
  (fn [board]
    (let [transpose (apply mapv vector board)
          diagonals (mapv #(mapv nth board %) [[0 1 2] [2 1 0]])
          win-seq #(first
                     (get #{[:x :x :x]
                            [:o :o :o]}
                       % [nil]))]
      (some win-seq
        (concat
         board
         transpose
         diagonals)))))

(defcheck solution-78deaa9d
  (fn solve [board]
    (letfn [(transpose [matrix]
              (apply mapv vector matrix))
            (trace [matrix]
              (mapv (fn [coll idx] (nth coll idx))
                matrix (range)))
            (rotate [coll]
              (conj (subvec coll 1) (first coll)))
            (rotate-n [coll n]
              ((apply comp (repeat n rotate)) coll))
            (symmetric-trace [matrix]
              (->>
                (trace
                  [(rotate-n (first matrix) 2)
                   (second matrix)
                   (rotate-n (last matrix) 1)])
                (conj [] (trace matrix))))]
      (let [x-win [:x :x :x]
            o-win [:o :o :o]]
        (if (or (contains? (set board) x-win)
                (contains? (set (transpose board)) x-win)
                (contains? (set (symmetric-trace board)) x-win))
          :x
          (if (or (contains? (set board) o-win)
                  (contains? (set (transpose board)) o-win)
                  (contains? (set (symmetric-trace board)) o-win))
            :o
            nil))))))

(defcheck solution-78ffbdb2
  (fn [[[a1 a2 a3][b1 b2 b3][c1 c2 c3]]]
    (let [win? (fn [[a & coll]]
                 (if (every? #(and (= a %) (not= a :e)) coll) a))]
      (or (win? [a1 a2 a3]) (win? [b1 b2 b3]) (win? [c1 c2 c3])
          (win? [a1 b1 c1]) (win? [a2 b2 c2]) (win? [a3 b3 c3])
          (win? [a1 b2 c3]) (win? [a3 b2 c1])))))

(defcheck solution-79ad915b
  (fn [[a b c :as rows]] ;should work for any sensible n
    (letfn [(win [v] (if (and (= 1 (count (distinct v))) (not= :e (first v))) (first v)))]
      (let [cols (apply map vector rows)
            d1 (map #(nth % %2) rows (range))
            d2 (map #(nth % %2) rows (range (dec (count a)) -1 -1))
            diags [d1 d2]]
        (->>
          (concat rows cols diags)
          (keep win)
          first)))))

(defcheck solution-7a9e8f41
  (fn tic-tac [[[x0 y0 z0] [x1 y1 z1] [x2 y2 z2] :as all]]
    (let [check (conj all
                  [x0 x1 x2]
                  [y0 y1 y2]
                  [z0 z1 z2]
                  [x0 y1 z2]
                  [z0 y1 x2])]
      (cond (some #(every? (fn [v] (= :o v)) %) check) :o
            (some #(every? (fn [v] (= :x v)) %) check) :x
            :else nil))))

(defcheck solution-7bda804c
  (fn
    [b]
    (let [f first s second d last
          g #(f (filter identity %))
          c (fn [k v] (if (every? #(= k %) v) k nil))
          w (fn [r] (g (map #(c % r) [:x :o])))
          l (g (apply concat
                 (map (fn [a]
                        (map w a))
                   [b (apply map vector b)])))
          i (g (map
                 (fn [a]
                   (w (map (fn [f] (f (f a))) [f s d]))
                   )
                 [b (map reverse b)]))]
      (if l l i))))

(defcheck solution-7c17ab58
  (fn f [b]
    (let [transposed (apply map list b),
          d1 [(map-indexed #(nth %2 %) b)],
          d2 [(map-indexed #(nth %2 %) (reverse b))],
          lines (mapcat identity [b, transposed, d1,  d2])]
      (cond (some #{[:x :x :x]} lines) :x
            (some #{[:o :o :o]} lines) :o
            :else nil
            ))))

(defcheck solution-7c63d468
  (fn [board-2d]
    (let [board (map {:e 0, :x 1, :o -1} (flatten board-2d))
          three-squares-seqs-horiz (partition 3 (range 9))
          three-squares-seqs-vert (apply map list three-squares-seqs-horiz)
          three-squares-seqs-diag [(range 0 9 4) (range 2 7 2)]
          three-squares-seqs (concat three-squares-seqs-horiz
                                     three-squares-seqs-vert
                                     three-squares-seqs-diag)]
      (some (fn [three-squares]
              (let [result (apply + (map #(nth board %) three-squares))]
                (cond
                  (= result 3) :x
                  (= result -3) :o
                  :else nil)))
        three-squares-seqs))))

(defcheck solution-7d00347
  (fn tic-tac-toe- [board]
    (let [combs (fn [rs cs]
                  (map #(get-in board [%1 %2]) rs cs))
          rows [0 1 2]
          cols [[0 1 2]
                [2 1 0]
                [0 0 0]
                [1 1 1]
                [2 2 2]]]
      (some {[:x :x :x] :x [:o :o :o] :o}
        (concat board (for [col cols]
                        (combs rows col)))))))

(defcheck solution-7d1fb5cd
  (fn [board]
    (let [i [0 1 2]
          c (take 12 (cycle i))
          p (flatten (map #(repeat 3 %) i))
          zip #(map vector %1 %2)
          win? (fn [w]
                 (some
                   (fn [x] (every? #(= w (get-in board %)) x))
                   (partition
                     3 (into (zip (into i p) c) (zip c (into (reverse i) p))))))]
      (cond
        (win? :x) :x
        (win? :o) :o))))

(defcheck solution-7d88ffc8
  (fn [rows]
    (let [cols (partition 3 (apply interleave rows))
          d1   [(map-indexed #(%2 %1) rows)]
          d2   [(map-indexed #(%2 (- 2 %1)) rows)]
          sets (concat rows cols d1 d2)]
      (some
        #(cond
           (apply = :x %) :x
           (apply = :o %) :o
           :else nil)
        sets))))

(defcheck solution-7ddea228
  (fn me [arg]

    ;label each position
    (let [  pos-rows   (map vector arg (range))

          m-fn (fn [row arg]

                 (vector (second row) (second arg) (first arg))
                 )

          poses      (for [row pos-rows]
                       (map (partial m-fn row)   ( map vector (first row) (range) ))
                       )

          x-poses    (for [row poses]

                       (map #(drop-last %) (filter #(= :x (last %)) row))
                       )

          o-poses    (for [row poses]
                       (map #(drop-last %) (filter #(= :o (last %)) row))
                       )

          x-pposes  (partition 2 (flatten x-poses))

          o-pposes  (partition 2 (flatten o-poses))

          check-count (fn [arg]

                        (>= (count arg) 3)
                        )

          check-ver-hor   (fn [f arg]

                            (let [my-map (group-by f arg)]

                              (or   (if ((complement nil?) (my-map 0)) (= 3 (count (my-map 0))) false)

                                    (if ((complement nil?) (my-map 1)) (= 3 (count (my-map 1))) false)

                                    (if ((complement nil?) (my-map 2)) (= 3 (count (my-map 2))) false)

                                    )
                              )
                            )

          check-diag  (fn [arg]

                        (let [
                              f-fn (fn [arg]

                                     (or (and (= 1 (first arg)) (= 1 (second arg)) )
                                         (= -2 (- (first arg) (second arg)))
                                         (= 2  (- (first arg) (second arg)))
                                         )
                                     )

                              ]

                          (or (= 3 (count (filter #(= (first %) (second %)) arg)))
                              (= 3 (count (filter f-fn arg)) )
                              )
                          )
                        )

          winner (fn [arg]

                   (if (check-count arg)
                     (or (check-ver-hor first arg) (check-ver-hor second arg) (check-diag arg))
                     false
                     )
                   )

          ]


      (if (and (nil? x-pposes)  (nil? o-pposes))
        nil

        (cond
          (winner x-pposes) :x
          (winner o-pposes) :o
          :else nil
          )
        )

      )

    ))

(defcheck solution-7e27897d
  (fn [grid]
    (some
      #(let [f (first %)] (when (and (#{:x :o} f) (= % (repeat 3 f))) f))
      (concat (apply mapv vector grid) ;; reverse to test grid columns
              (conj grid               ;; grid lines
                [((grid 0) 0) ((grid 1) 1) ((grid 2) 2)]       ;;first diag
                [((grid 0) 2) ((grid 1) 1) ((grid 2) 0)])))))

(defcheck solution-7e5c906c
  (fn tic-tac-toe-winner [board]
    (letfn [(make-groups [board]
              (let [max-col (count board)
                    max-row (count (first board))]
                (concat
                 (for [i (range max-col)]
                   (for [j (range max-row)] ((board i) j)))
                 (for [j (range max-row)]
                   (for [i (range max-col)] ((board i) j)))
                 [(for [i (range max-row)] ((board i) i))]
                 [(for [i (reverse (range max-row))]
                    ((board i) (dec (- max-row i))))])))

            (game-winner [[cell-grp & cell-grps]]
              (if-not cell-grp
                nil
                (if (and (not (some #(= :e %1) cell-grp)) (apply = cell-grp))
                  (first cell-grp)
                  (recur cell-grps))))]

      (game-winner (make-groups board)))))

(defcheck solution-7e6439bf
  (fn winner [b]
    (some {[:o :o :o] :o [:x :x :x] :x}
      (concat b
              (apply map vector b)
              (for [i [[0 4 8] [2 4 6]]]
                (map (vec (flatten b)) i))))))

(defcheck solution-801c0cac
  (fn [B]
    (let [T (partial repeat 3)
          R (range 3)
          V (concat [R (reverse R)] (map #(T %) R))
          L (concat B (map #(map nth B %) V))]
      (some {(T :x) :x (T :o) :o} L))))

(defcheck solution-8035d90a
  (fn [m]
    (let [analyze (fn [s]
                    (if (or (and (= s ((m 0) 0)) (= s ((m 0) 1)) (= s ((m 0) 2)))
                            (and (= s ((m 1) 0)) (= s ((m 1) 1)) (= s ((m 1) 2)))
                            (and (= s ((m 2) 0)) (= s ((m 2) 1)) (= s ((m 2) 2)))
                            (and (= s ((m 0) 0)) (= s ((m 1) 0)) (= s ((m 2) 0)))
                            (and (= s ((m 0) 1)) (= s ((m 1) 1)) (= s ((m 2) 1)))
                            (and (= s ((m 0) 2)) (= s ((m 1) 2)) (= s ((m 2) 2)))
                            (and (= s ((m 0) 0)) (= s ((m 1) 1)) (= s ((m 2) 2)))
                            (and (= s ((m 0) 2)) (= s ((m 1) 1)) (= s ((m 2) 0))))
                      s
                      nil))]
      (or (analyze :o) (analyze :x)))))

(defcheck solution-80cad34a
  (fn [s]
    (let [r (range 3)
          h (for [i r j r] [i j])
          v (map #(vector (% 1) (% 0)) h)
          d (for [i r] [i i])
          f (for [i r] [i (- 2 i)])
          q (partition 3 (map (fn [[a b]] ((s a) b)) (concat v h d f)))]
      (some #{:x :o} (map #(if (apply = %) (first %) nil) q)))))

(defcheck solution-813c5a44
  (fn [sq] (letfn [ (rows [rectangle] rectangle) (columns [rectangle] (apply map vector rectangle)) (nth1 [coll n] (nth coll (dec n))) (range1 [from to] (range from (inc to))) (value-at [sq pos] (nth1 (nth1 sq (second pos)) (first pos))) (pair-range [from to] (map vector (range1 from to) (reverse (range1 from to)))) (diagonal [sq from to] (map #(value-at sq %) (pair-range from to))) (diagonal-sw-ne [sq] (map #(value-at sq %) (pair-range 1 (count sq)))) (diagonal-nw-se [sq] (map #(value-at sq %) (map #(vector % %) (range1 1 (count sq))))) (main-diagonals [sq] (vector (diagonal-nw-se sq) (diagonal-sw-ne sq))) (main-lines [sq] (concat (rows sq) (columns sq) (main-diagonals sq))) (tic-tac-toe [sq] (let [winning-lines (filter #(or (= [:x :x :x] %) (= [:o :o :o] %)) (main-lines sq))] (cond (empty? winning-lines) nil (= (first winning-lines) [:x :x :x]) :x (= (first winning-lines) [:o :o :o]) :o))) ] (tic-tac-toe sq))))

(defcheck solution-82d41693
  (fn tictac [board]
    (let [vertical [(vec (map first board)) (vec (map second board)) (vec (map #(nth % 2) board))]
          diag [[(first (first board)) (second (second board)) (nth (nth board 2) 2)]
                [(nth (first board) 2) (second (second board)) (first (nth board 2))]]
          all  (concat board vertical diag)
          fe  (filter #(not (some (fn [v] (= :e v)) %)) all)
          xs  (filter #(every? (fn [v] (= :x v)) %) fe)
          os  (filter #(every? (fn [v] (= :o v)) %) fe)]
      (cond (> (count xs) 0) :x
            (> (count os) 0) :o
            :otherwise nil)
      )
    ))

(defcheck solution-8395e2b4
  (fn [board]
    (let
     [flatboard (flatten board),
      indices [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]],
      sets (filter
             (fn [markset] (and (= (count markset) 1) (not= (first markset) :e)))
             (map
               (fn [indexblock]
                 (set
                   (map
                     (fn [index]
                       (nth flatboard index)
                       )
                     indexblock
                     )
                   )
                 )
               indices
               )
             )
      ]
      (if (= 1 (count sets)) (first (first sets)) nil)
      )
    ))

(defcheck solution-839d024f
  (fn [b] (let [
                cols (partition 3 (apply interleave b))
                rows b
                diags [[(get-in b [0 0]) (get-in b [1 1]) (get-in b [2 2])]
                       [(get-in b [0 2]) (get-in b [1 1]) (get-in b [2 0])]
                       ]
                C (concat cols rows diags)
                freqs (set (map #(frequencies %1) C))]
            (cond
              (contains? freqs {:x 3 }) :x
              (contains? freqs {:o 3 }) :o
              :else nil))))

(defcheck solution-83a2d20a
  (let [winner? #{[:x :x :x] [:o :o :o]}
        mkdiagf (fn [f g]
                  (fn [b] [(f b) ((comp second second) b) (g b)]))
        hs [first second last] ;; horizontals
        vs [#(map first %) #(map second %) #(map last %)]
        ds [(mkdiagf ffirst (comp last last))
            (mkdiagf (comp last first) (comp first last))]]
    (fn [board]
      (loop [fs (concat hs vs ds)]
        (when-first [f fs]
          (if-let [s (winner? (f board))]
            (first s)
            (recur (next fs))))))))

(defcheck solution-83b51120
  (fn [board]
    (first
      (some #{[:o :o :o] [:x :x :x]}
        (concat board (apply map list board)
                (map #(map (fn [f v] (f v)) board %) [[0 1 2] [2 1 0]]))))))

(defcheck solution-8414e038
  (fn analyzeTicTacToe
    ;Analyze a Tic-Tac-Toe board of arbitrary size for a winner.
    ;Input is a 2D array (dense matrix, no empty spots) where
    ;  :x represents X player moves
    ;  :o represents O player moves
    ;  :e represents empty spaces.
    ;Example input:
    ;    (def board [[:x :e :e]
    ;                [:o :x :e]
    ;                [:o :e :x]])
    ;    (analyzeTicTacToe board)
    ;    => :x
    [board]

    (let ; Replace :e with nulls, for simplicity
     [board (map (partial map #(condp = % :e nil %)) board)
      ; Check if all elements of the collection are the same
      same (fn [coll] (when (apply = coll) (first coll)))
      ; Check if all elements of any of the collections within this collection are the same
      same2d (fn [mtx] (some #(when (comp not nil? %) %) (map same mtx)))
      ; Transpose a matrix (borrowed from RosettaCode)
      transpose (fn [mtx] (vec (apply map vector mtx)))
      ; Extract the two main diagonals of the matrix
      diagonals (fn [mtx] (transpose (for [i (range (count mtx))] [(nth (nth mtx i) i) (nth (nth mtx i) (- (count mtx) 1 i))])) )]

      ; If any of the rows are the same, or any of the columns are the same, or any of the diagonals are all the same
      (or (same2d board) (same2d (transpose board)) (same2d (diagonals board)))
      )))

(defcheck solution-84233ff4
  (fn check-tic-tac-toe [board]
    (let [check-line (fn [line]
                       (if (= ((frequencies line) :o) 3) :o
                                                         (if (= ((frequencies line) :x) 3) :x
                                                                                           nil)))
          lines (concat board
                        (map (fn [fun] (map fun board))
                          (list first second last))
                        (list (list (first (first board))
                                (second (second board))
                                (last (last board)))
                          (list (last (first board))
                            (second (second board))
                            (first (last board)))))
          determine-winner (fn [line-results]
                             (if (some #{:o} line-results) :o
                                                           (if (some #{:x} line-results) :x
                                                                                         nil)))]
      (determine-winner (map check-line lines)))))

(defcheck solution-84517065
  (fn check-board [board]
    (letfn [
            (get-winner[result]
              (first (filter (complement nil?) result)))


            (check-lines []
              (for [line board player [:o :x]]
                (if (every? (partial = player) line)
                  player nil)))

            (check-collumns []
              (for [collumn [0 1 2] player [:o :x]]
                (if (every? (partial = player)
                      (map #(nth % collumn) board))
                  player nil)))

            (check-diagonals []
              (for [dgcoord  [[[0 0] [1 1] [2 2]]
                              [[2 0] [1 1] [0 2]]] player [:o :x]]
                (if (every? (partial = player)
                      (for [[line collumn] dgcoord]
                        (get-in board [line collumn])))
                  player nil)))]

      (get-winner
        (concat (check-lines)
                (check-collumns)
                (check-diagonals))))))

(defcheck solution-84c8d22f
  #(let [win (fn [a b c] (when (and (= a b c) (not= a :e)) a))
         [[a b c] [d e f] [g h i]] %]
     (or (win a b c) (win d e f) (win g h i) (win a e i)
         (win a d g) (win b e h) (win c f i) (win c e g))))

(defcheck solution-84c93539
  (fn [b]
    (letfn [(row [i] (nth b i))
            (col [j] (mapv #(nth % j) b))
            (dex [] (mapv #(get-in b [% %]) (range 3)))
            (sin [] (mapv #(get-in b [% (- 2 %)]) (range 3)))
            (sets []
              (concat
               (map row (range 3))
               (map col (range 3))
               [(dex) (sin)]))
            (winner [set]
              (when (and (apply = set)
                         (not= :e (first set)))
                (first set)))]
      (first (keep winner (sets))))))

(defcheck solution-84ff8be
  (fn [board]
    (letfn [(all-same [[x y z]]
              (when (and (= x y z) (not= x :e))
                x))
            (transpose [coll]
              (apply map list coll))
            (reverse-transpose [coll]
              (apply map list (map reverse coll)))
            (diagonal [coll]
              (map-indexed (fn [idx xs]
                             ((vec xs) idx))
                coll))]
      (some #{:x :o} (concat (keep all-same board)
                             (keep all-same (transpose board))
                             (list (all-same (diagonal board)))
                             (list (all-same (-> (reverse-transpose board) diagonal))))))))

(defcheck solution-8514a1d1
  (fn [board]
    (let [row   (fn [n b] (b n))
          col   (fn [n b] (into [] (map #(% n) b)))
          ldiag (fn [b]
                  (->> (reduce
                         (fn [[ld i] r]
                           (vector
                             (conj ld (r i))
                             (inc i)))
                         [[] 0] b)
                    (first)))
          rdiag (fn [b]
                  (->> (reduce
                         (fn [[rd i] r]
                           (vector
                             (conj rd (r i))
                             (dec i)))
                         [[] 2] b)
                    (first)))
          xs    (fn [s] (map #(if (= :x %) 1 0) s))
          os    (fn [s] (map #(if (= :o %) 1 0) s))
          won   (fn [p]
                  (->>
                    (map
                      (fn [f]
                        (->>
                          (f board)
                          (p)
                          (apply +)))
                      [(partial row 0)
                       (partial row 1)
                       (partial row 2)
                       (partial col 0)
                       (partial col 1)
                       (partial col 2)
                       ldiag
                       rdiag])
                    (filter #(= 3 %))
                    (seq)))]
      (cond
        (won xs) :x
        (won os) :o
        :else nil))))

(defcheck solution-85ae367
  (letfn[
         (tic-tac-toe-lines [board]
           (let [size (count board)]
             (concat board
                     (for [col (range size)] (map #(get % col) board))
                     [(map-indexed #(get %2 %1) board)]
                     [(map-indexed #(get %2 (dec (- size %1))) board)]
                     )))
         (check-line [line]
           (let [res (reduce #(if (= %1 %2) %1 nil) line)]
             (if (= res :e) nil res)
             ))]
    #(some check-line (tic-tac-toe-lines %))
    ))

(defcheck solution-863b8ed9
  #(first
     (some #{[:x :x :x][:o :o :o]}
       (-> %
         (into (apply map vector %))
         (conj (map nth % (range))
           (map nth (rseq %) (range)))))))

(defcheck solution-8653ede8
  (fn winner? [board]
    (letfn [(is-winning? [s]
              (if (and (apply = s) (not (= :e (first s))))
                (first s)
                nil))
            (is-winning-row? [b]
              (loop [rows b]
                (if (empty? rows)
                  false
                  (if (is-winning? (first rows))
                    (is-winning? (first rows))
                    (recur (rest rows))))))
            (is-winning-col? [b]
              (loop [cols (map (fn [i] (map #(nth % i) b)) (range 3))]
                (if (empty? cols)
                  false
                  (if (is-winning? (first cols))
                    (is-winning? (first cols))
                    (recur (rest cols))))))
            (is-winning-diag? [b]
              (let [diag (map #(get-in b [% %]) (range 3))]
                (is-winning? diag)))
            (is-winning-anti-diag? [b]
              (let [anti-diag (map #(get-in b [% (- 2 %)]) (range 3))]
                (is-winning? anti-diag)))]
      (if (is-winning-row? board)
        (is-winning-row? board)
        (if (is-winning-col? board)
          (is-winning-col? board)
          (if (is-winning-diag? board)
            (is-winning-diag? board)
            (is-winning-anti-diag? board)))))))

(defcheck solution-8688dbc5
  (fn [board]
    (let [s1 (set (get board 0))
          s2 (set (get board 1))
          s3 (set (get board 2))
          s4 (set [(get (get board 0) 0)
                   (get (get board 1) 0)
                   (get (get board 2) 0)])
          s5 (set [(get (get board 0) 1)
                   (get (get board 1) 1)
                   (get (get board 2) 1)])
          s6 (set [(get (get board 0) 2)
                   (get (get board 1) 2)
                   (get (get board 2) 2)])
          s7 (set [(get (get board 0) 0)
                   (get (get board 1) 1)
                   (get (get board 2) 2)])
          s8 (set [(get (get board 0) 2)
                   (get (get board 1) 1)
                   (get (get board 2) 0)])
          winner (filter #(or
                           (= #{:x} %)
                           (= #{:o} %)) [s1 s2 s3 s4 s5 s6 s7 s8])]
      (first (first winner)))))

(defcheck solution-8692cd3
  (fn [board]
    (let [[r1 r2 r3] board
          lines (vector r1 r2 r3 ; rows
                  (for [r board] (first r)) ; cols
                  (for [r board] (second r))
                  (for [r board] (last r))
                  [(get r1 0) (get r2 1) (get r3 2)] ; diagonals
                  [(get r1 2) (get r2 1) (get r3 0)])]
      (first (some #(when (and (apply = %)
                               (not (apply = :e %))) %) lines)))))

(defcheck solution-86ac464c
  (fn winner [rows]
    (let [cols (apply map vector rows)
          diags [(map get rows [0 1 2])
                 (map get rows [2 1 0])]]
      (loop [[x & xs] (concat rows cols diags)]
        (when-let [el (first x)]
          (if (and (apply = x) (not= :e el))
            el
            (recur xs)))))))

(defcheck solution-86b0f61c
  (fn [lns] (
              let [cols (apply map list lns)
                   ds (map #(map nth lns %) [[0 1 2] [2 1 0]])
                   ws {[:x :x :x] :x [:o :o :o] :o}
                   ] (
                       some ws (concat lns cols ds)
                       )
                     )))

(defcheck solution-86c59ea1
  (fn [g h k l s]
    (let [a (concat s
                    (l map list s)
                    [(h s (k))]
                    [(h s (k 2 -1 -1))])
          b (g (g (filter #(l = %) a)))]
      (if (not= b :e) b))) #(nth % 0 nil) #(map nth % %2) range apply)

(defcheck solution-86c86022
  (fn [board]
    (let [positions #{#{[0 0] [0 1] [0 2]} #{[1 0] [1 1] [1 2]} #{[2 0] [2 1] [2 2]}
                      #{[0 0] [1 0] [2 0]} #{[0 1] [1 1] [2 1]} #{[0 2] [1 2] [2 2]}
                      #{[0 0] [1 1] [2 2]} #{[2 0] [1 1] [0 2]}}]
      (first (for [piece [:x :o] pos positions
                   :when (= [piece piece piece] (map #(get-in board %) pos))]
               piece)))))

(defcheck solution-87312535
  (fn tic-tac-toe [[a b c]]
    (letfn [(same? [[x y z]]
              (cond
                (= x y z) (if (= :e x)
                            nil
                            x)
                :else :nil))]
      (let [line? (list
                    (same? a) (same? b) (same? c)
                    (same? (map #(first %) [a b c]))
                    (same? (map #(second %) [a b c]))
                    (same? (map #(last %) [a b c]))
                    (same? [(first a) (second b) (last c)])
                    (same? [(first c) (second b) (last a)]))]
        (cond
          (not (empty? (filter #(= :o %) line?))) :o
          (not (empty? (filter #(= :x %) line?))) :x
          :else nil)))))

(defcheck solution-8733c005
  (fn tic-tac-toe [board]
    (let [size (range 3)
          ;; Create horizontal and vertical lines
          lines (mapcat #(list
                           (map vector (repeat %) size)
                           (map vector size (repeat %)))
                  size)
          ;; Create both diagonal lines
          lines (conj lines
                  (map vector size size)
                  (map vector (range 2 -1 -1) size))]
      (->> lines
        (map
          (partial map (fn [[y x]]
                         (-> board
                           (nth y)
                           (nth x)))))
        (filter #(not-any? (partial = :e) %))
        (filter #(->> % distinct count (= 1)))
        ffirst))))

(defcheck solution-877daa2c
  (fn [coll]
    (some
      #(if (apply = %) (#{:x :o} (first %)) nil)
      (concat coll
              (partition (count coll) (apply interleave coll))
              [(map #(nth (nth coll %) %) (range (count coll)))
               (map #(nth (nth coll %) (- (count coll) 1 %)) (range (count coll)))]))))

(defcheck solution-87a91f28
  (fn ttt
    [board]
    (let [c1 (map first board)
          c2 (map second board)
          c3 (map last board)
          d1 (list (first c1) (second c2) (last c3))
          d2 (list (first c3) (second c2) (last c1))
          all (concat [c1 c2 c3 d1 d2] board)
          won? (fn [sym] (pos? (count (filter (fn [r] (every? #(= % sym) r)) all))))]
      (cond
        (won? :x) :x
        (won? :o) :o
        :else nil))))

(defcheck solution-88265f24
  (fn [[[a _ c]
        [_ e _]
        [g _ i] :as board]]
    (->> (concat
          board
          (apply map vector board)
          [[a e i] [c e g]])
      (some {[:x :x :x] :x [:o :o :o] :o}))))

(defcheck solution-89350943
  (fn [[row1 row2 row3 :as pg]]
    (let [[col1 col2 col3] (partition 3 (apply interleave pg))
          diag1 [(row1 0) (row2 1) (row3 2)]
          diag2 [(row1 2) (row2 1) (row3 0)]
          rd (fn [c] (reduce #(when (and (= %1 %2) (not= %1 :e)) %1) c))]
      (reduce #(or %1 %2) (map rd [row1 row2 row3 col1 col2 col3 diag1 diag2])))))

(defcheck solution-893a8243
  (letfn
   [  ; First test: horizontal line of key "k"?
    (test1 [board k]
      (some (partial = #{k}) (map set board)))

    ; Second test: diagonal of key "k"?
    (test2 [board k]
      ( = #{k} (set (map #(get-in board [% %]) [0 1 2]))))

    ; Third test: the other diagonal of key "k"?
    (test3 [board k]
      (= #{k} (set (map #(get-in board %) [[2 0] [1 1] [0 2]]))))

    ; Transpose board.
    (transpose [board]
      (apply map list board))

    ; Full test for key "k".
    (full-test [board k]
      (or (test1 board k)
          (test2 board k)
          (test1 (transpose board) k)
          (test3 board k)))]

    ; Full test
    (fn winner [board]
      (cond
        (true? (full-test board :o)) :o
        (true? (full-test board :x)) :x
        :else nil))))

(defcheck solution-897a03c5
  (fn [R A M b]
    (some #(and (A = %) (#{:x :o} (first %)))
      `(~@b
        ~@(A M list b)
        ~(M #(%2 %) R b)
        ~(M #(%2 (- 2 %)) R b)))) [0 1 2] apply map)

(defcheck solution-89dc2d04
  (letfn [(check [c]
            (cond
              (every? #{:x} c) :x
              (every? #{:o} c) :o
              :else nil))
          (row [c] (map check c))
          (col [c] (row (apply mapv vector c)))
          (diagx [c] (check (map-indexed #(%2 %1) c)))
          (diagy [c] (check (map-indexed #(%2 (- 2 %1)) c)))
          (main [c] (->> c ((juxt row col diagx diagy)) flatten (filter (complement nil?)) first))]
    main))

(defcheck solution-8ac177f9
  (fn [x]
    (let [a
          (map first
            (filter #(apply = %)
              (concat x
                      [(mapv first x)]
                      [(mapv second x)]
                      [(mapv last x)]
                      [[(get-in x [0 0]) (get-in x [1 1]) (get-in x [2 2])]]
                      [[(get-in x [0 2]) (get-in x [1 1]) (get-in x [2 0])]])))]
      (cond (some #{:x} a) :x
            (some #{:o} a) :o
            :else nil))))

(defcheck solution-8b01cc4f
  (fn [v]
    (let [l1 (list (if (= ((v 0) 0) ((v 0) 1) ((v 0) 2)) ((v 0) 0) nil)
               (if (= ((v 1) 0) ((v 1) 1) ((v 1) 2)) ((v 1) 0) nil)
               (if (= ((v 2) 0) ((v 2) 1) ((v 2) 2)) ((v 2) 0) nil)
               (if (= ((v 0) 0) ((v 1) 0) ((v 2) 0)) ((v 0) 0) nil)
               (if (= ((v 0) 1) ((v 1) 1) ((v 2) 1)) ((v 0) 1) nil)
               (if (= ((v 0) 2) ((v 1) 2) ((v 2) 2)) ((v 0) 2) nil)
               (if (= ((v 0) 0) ((v 1) 1) ((v 2) 2)) ((v 0) 0) nil)
               (if (= ((v 0) 2) ((v 1) 1) ((v 2) 0)) ((v 0) 2) nil))
          l2 (map #(if (or (nil? %) (= :e %)) nil %) l1)]
      (cond (some #(= :o %) l2) :o
            (some #(= :x %) l2) :x
            :else nil))))

(defcheck solution-8b8aa165
  (fn [b]
    (let [fb (into [] (flatten b))
          p [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]
          f #( if
              (and
               (not= :e (fb (first %)))
               (= (fb (first %)) (fb (second %)) (fb (last %))) )
               (fb (first %))
               nil
               )]
      (some f p))))

(defcheck solution-8b958c9a
  (fn [boards]
    (letfn [(found [s]
              (let [[f & r] (filter #(or (every? (partial = :x) %)
                                         (every? (partial = :o) %)) s)]
                (if-not (nil? f)
                  (first f)
                  nil)))]
      (or
       (found boards)
       (found (apply map #(vector %1 %2 %3) boards))
       (found [[(nth (first boards) 0) (nth (second boards) 1) (nth (nth boards 2) 2)]
               [(nth (first boards) 2) (nth (second boards) 1) (nth (nth boards 2) 0)]])))))

(defcheck solution-8bb3b65c
  (fn [grid]
    (letfn [(check-row [[a b c]] (and (= a b c) (#{ :x :o } a)))
            (check [rows]
              (or (some check-row rows)
                  (check-row (map #(get-in rows [% %]) (range 3)))))]
      (or (check grid)
          (check (vec (reverse (apply map vector grid))))
          nil))))

(defcheck solution-8be39135
  (fn [board]
    (letfn [(alleq [xs]
              (when (and (apply = xs)
                         (not= :e (first xs)))
                (first xs)))
            (diag [matrix]
              (map-indexed #(nth %2 %1) matrix))
            (diags [matrix]
              [(diag matrix) (diag (reverse matrix))])]
      (some
        alleq
        (concat board
                (apply map vector board)
                (diags board))))))

(defcheck solution-8d9182d3
  (fn [board]
    (let [vecs (conj board
                 (map first board)
                 (map second board)
                 (map last board)
                 [(first (first board)) (second (second board)) (last (last board))]
                 [(last (first board)) (second (second board)) (first (last board))])]
      (cond (some #(= [:x :x :x] %) vecs) :x
            (some #(= [:o :o :o] %) vecs) :o
            :else nil))))

(defcheck solution-8d9c7e59
  (fn [[x y z]]
    (let [eox (fn [o] (if (= o :e) nil o))]
      (cond
        (apply = x) (eox (first x))
        (apply = y) (eox (first y))
        (apply = z) (eox (first z))
        (= (first x) (first y) (first z)) (eox (first x))
        (= (second x) (second y) (second z)) (eox (second x))
        (= (last x) (last y) (last z)) (eox (last x))
        (= (first x) (second y) (last z)) (eox (first x))
        (= (last x) (second y) (first z)) (eox (last x))
        :else nil
        ))))

(defcheck solution-8ddb5c6d
  (fn [m]
    (letfn [(S [[r c]]
              (nth (nth m r) c))

            (X [v]
              (= [:x :x :x] (map S v)))

            (O [v]
              (= [:o :o :o] (map S v)))]

      (let [l [[[0 0] [0 1] [0 2]]
               [[1 0] [1 1] [1 2]]
               [[2 0] [2 1] [2 2]]
               [[0 0] [1 0] [2 0]]
               [[0 1] [1 1] [2 1]]
               [[0 2] [1 2] [2 2]]
               [[0 0] [1 1] [2 2]]
               [[0 2] [1 1] [2 0]]]]
        (cond (some X l) :x
              (some O l) :o
              :else nil)))))

(defcheck solution-8de3c0cf
  (fn [b]
    (let [rs (for [x [0 1 2]] (for [y [0 1 2]] [x y]))
          p (concat rs (map #(map reverse %) rs)
                    [[[0 0] [1 1] [2 2]]
                     [[0 2] [1 1] [2 0]]])
          w (fn [x s]
              (every? #(do %)
                (map (fn [[r c]] (= x (nth (nth b r) c)))
                  s)))
          t (fn [x]
              (some #(w x %) p))]
      (cond (t :x) :x
            (t :o) :o
            :t nil))))

(defcheck solution-8e2168d1
  (fn winner? [table]
    (let [get-table-value (fn [x y]
                            (-> table
                              (nth y)
                              (nth x)))
          collect (fn [xs]
                    (->> xs
                      (map (fn [f] (for [x (range 3)
                                         y (range 3)
                                         :when (f x y)] (get-table-value x y))))
                      (filter (partial apply =))
                      (map first)
                      (remove (partial = :e))
                      first))
          fix-x (fn [c]
                  (fn [x y] (= x c)))
          fix-y  (fn [c]
                   (fn [x y] (= y c)))
          fixes (fn [f] (map (fn [x] (f x)) (range 3)))
          diag1 (fn [x y] (= x y))
          diag2 (fn [x y] (= x (- 2 y)))
          filters (concat [diag1 diag2]
                          (fixes fix-x)
                          (fixes fix-y))]
      (collect filters))))

(defcheck solution-8e7d2e63
  (fn [xs]
    (letfn [(build [ys]
              (letfn [(g [a b] (get-in ys [a b]))]
                (-> (conj ys [(g 0 0) (g 1 0) (g 2 0)])
                  (conj [(g 0 1) (g 1 1) (g 2 1)])
                  (conj [(g 0 2) (g 1 2) (g 2 2)])
                  (conj [(g 0 0) (g 1 1) (g 2 2)])
                  (conj [(g 0 2) (g 1 1) (g 2 0)]))))]
      (cond (not-empty (filter #(every? (fn [v] (= :x v)) %) (build xs))) :x
            (not-empty (filter #(every? (fn [v] (= :o v)) %) (build xs))) :o
            :else nil))))

(defcheck solution-8ee37e95
  (fn [b]
    (let [s? (fn [r s] (every? #(= % s) r))
          hm (fn [bo s] (some (fn [r] (s? r s)) bo))]
      (first (filter (fn [s]
                       (or (hm b s)
                           (hm (map (fn [i] (map #(nth % i) b)) (range 3)) s)
                           (s? (map (fn [i] (nth (nth b i) i)) (range 3)) s)
                           (s? (map (fn [i] (nth (nth b i) (- 2 i))) (range 3)) s))) [:x :o])))))

(defcheck solution-8f3a1d68
  (fn [board]
    (let [same? #(= 1 (count (set %)))
          k {:x :x :o :o :e nil}
          vert (vec (partition 3 (apply interleave board)))
          diag [(for [i (range 0 3)] ((board i) i))
                (for [i (range 0 3)] ((board i) (- 2 i)))]]
      (if-let [result (some #(if (same? %) %) (concat board vert diag))]
        (k (first result))))))

(defcheck solution-8f434479
  (fn [[a b c]]
    (let [t (vec (concat a b c))]
      (some (reduce (fn [s [x y z]]
                      (if (= (t x) (t y) (t z))
                        (conj s (t x)) s)) #{} [[0 1 2] [3 4 5] [6 7 8] [0 4 8] [2 4 6] [0 3 6] [1 4 7] [2 5 8]]) [:x :o]))))

(defcheck solution-8faea055
  (fn [b]
    (let [lines (map #(set %) (concat b
                                      (map #(map (fn [r] (nth r %)) b) (range 3))
                                      [(map-indexed #(nth %2 %1) b)]
                                      [(map-indexed #(nth %2 (- (dec (count b)) %1)) b)]))]
      (cond
        (some #(and (= 1 (count %)) (= :x (first %))) lines) :x
        (some #(and (= 1 (count %)) (= :o (first %))) lines) :o
        :else nil))))

(defcheck solution-8fe66152
  (fn [ttt-board]
    (first
      (filter #(not (nil? %))
        (for [set-3 (concat
                     ttt-board     ; rows
                     (for [col (range (count (first ttt-board)))]
                       (map #(get % col) ttt-board)) ; columns
                     [(map #(get-in ttt-board %) [[0 0] [1 1] [2 2]])] ; diagonal 1
                     [(map #(get-in ttt-board %) [[0 2] [1 1] [2 0]])]) ; diagonal 2
              ]
          (reduce #(if (and (not (= :e %1))
                            (= %1 %2))
                     %1
                     nil)
            set-3))))
    ))

(defcheck solution-8fe6cedb
  (fn [tbl]
    (loop [ll `(~(map #(get-in tbl %) [[0 0] [1 1] [2 2]])
                ~(map #(get-in tbl %) [[2 0] [1 1] [0 2]])
                ~@tbl
                ~@(apply map vector tbl))]
      (cond
        (empty? ll) nil
        (and (not= (ffirst ll) :e) (apply = (first ll))) (do #_(print ll) (ffirst ll))
        :else (recur (rest ll))))))

(defcheck solution-904dc911
  (fn [board]
    (let [m {:hor1 [[0 0] [0 1] [0 2]]
             :hor2 [[1 0] [1 1] [1 2]]
             :hor3 [[2 0] [2 1] [2 2]]
             :ver1 [[0 0] [1 0] [2 0]]
             :ver2 [[0 1] [1 1] [2 1]]
             :ver3 [[0 2] [1 2] [2 2]]
             :diag1 [[0 0] [1 1] [2 2]]
             :diag2 [[0 2] [1 1] [2 0]]}]
      (loop [rows '(:hor1 :hor2 :hor3 :ver1 :ver2 :ver3 :diag1 :diag2)]
        (cond (empty? rows)
              nil
              (every? (fn [[y x]]
                        (= ((board y) x) :x))
                (m (first rows)))
              :x
              (every? (fn [[y x]]
                        (= ((board y) x) :o))
                (m (first rows)))
              :o
              :else
              (recur (rest rows)))))))

(defcheck solution-9063b325
  (fn [board]
    (letfn [(rows [board]
              board)
            (cols [[[a b c]
                    [d e f]
                    [g h i]]]
              [[a d g]
               [b e h]
               [c f i]])
            (diags [[[a b c]
                     [d e f]
                     [g h i]]]
              [[a e i] [g e c]])
            (lines [board]
              (concat (rows board)
                      (cols board)
                      (diags board)))
            (winner [p board]
              (if (some (partial every? (partial = p)) (lines board)) p nil))]
      (or (winner :x board)
          (winner :o board)))))

(defcheck solution-906ded0b
  (fn ttt [coll]
    (let [merged (fn [x] (reduce str x))
          who? (fn [x] (cond
                         (= ":e:e:e" x ) nil
                         (= ":x:x:x" x) :x
                         (= ":o:o:o" x) :o
                         :else nil)
                 )
          r1 (map (comp who? merged) coll)
          r2 (apply map (fn [x y z] ((comp who? merged) [x y z])) coll)
          r3 ((comp who? merged) [(last (first coll))
                                  (second (second coll))
                                  (first (last coll))])
          r4 ((comp who? merged) [(first (first coll))
                                  (second (second coll))
                                  (last (last coll))])
          r5 (filter #((complement nil?) %) (set (concat r1 r2 [r3] [r4])) )
          ] (empty? r5) nil (first r5)  )))

(defcheck solution-90b6fd83
  (fn winner [board]

    (let
     ;; transform
     [transformed (map (fn [r1 r2 r3] [r1 r2 r3]) (nth board 0) (nth board 1) (nth board 2)),
      find-winner (fn [bd]
                    (let [row-results (map
                                        (fn [[n1 n2 n3]] (if (and (not (= n1 :e)) (= n1 n2 n3)) n1 nil))
                                        bd)]
                      (first (filter #(not (nil? %)) row-results))
                      )),
      winner1 (find-winner board)
      winner2 (find-winner transformed)
      center (nth (nth board 1) 1),
      lt (nth (nth board 0) 0),
      rt (nth (nth board 0) 2),
      lb (nth (nth board 2) 0),
      rb (nth (nth board 2) 2)
      ]
      (cond
        (not (nil? winner1)) winner1
        (not (nil? winner2)) winner2
        (and (= :x center) (or (= :x lt rb) (= :x rt lb))) :x
        (and (= :o center) (or (= :o lt rb) (= :o rt lb))) :o
        :else nil
        )
      )))

(defcheck solution-91018eb1
  (fn [board]
    (let [rows board,
          cols (reduce #(map conj %1 %2) (repeat []) board),
          diags [(map nth board (range 3)),
                 (map nth board (reverse (range 3)))],
          lines (concat rows cols diags)]
      (first (first
               (filter
                 #(and (apply = %) (not= (first %) :e))
                 lines))))))

(defcheck solution-9157a34e
  (fn [board]
    (letfn [(at [x y] (nth (nth board y) x))
            (eq [r]
              (if (and (apply = r) (not= :e (first r)))
                (first r)
                nil))]
      (some
        identity
        (concat
         (map eq board)
         (apply map #(eq %&) board)
         [(eq [(at 0 0) (at 1 1) (at 2 2)])
          (eq [(at 2 0) (at 1 1) (at 0 2)])])))))

(defcheck solution-91d07925
  (fn [bs]
    (letfn [(check-for
              [s]
              (some #{true}
                (map (partial = [s s s])
                  (conj (concat bs
                                (partition 3 (apply interleave bs)))
                    (map (partial get-in bs) [[0 0] [1 1] [2 2]])
                    (map (partial get-in bs) [[0 2] [1 1] [2 0]])))))]
      (cond
        (check-for :x) :x
        (check-for :o) :o
        :default nil))))

(defcheck solution-921bcf0b
  (fn tic-tac-toe [board]
    (let [a (fn [x y] (nth (nth board x) y))
          lines [(nth board 0) (nth board 1) (nth board 2)
                 [(a 0 0) (a 1 0) (a 2 0)]
                 [(a 0 1) (a 1 1) (a 2 1)]
                 [(a 0 2) (a 1 2) (a 2 2)]
                 [(a 0 0) (a 1 1) (a 2 2)]
                 [(a 2 0) (a 1 1) (a 0 2)]]]
      (cond
        (some (fn [x] (every? #(= :x %) x)) lines) :x
        (some (fn [x] (every? #(= :o %) x)) lines) :o
        :else                                      nil))))

(defcheck solution-921c464c
  (fn [coll]
    (let [x (vec (flatten coll)) cl [[0 1 2] [3 4 5] [6 7 8]
                                     [0 3 6] [1 4 7] [2 5 8]
                                     [0 4 8] [2 4 6]]]
      (reduce (fn [res [a b c]](if (and (= a b c) (not= a :e)) a res))
        nil
        (map (fn [[a b c]](vector (x a) (x b) (x c))) cl)))))

(defcheck solution-9286da74
  (fn [board]
    (let [[& rows] board
          cols (apply (partial map vector) rows)
          [[a1 _ b1] [_ c _] [b2 _ a2]] board
          diags [[a1 c a2] [b1 c b2]]
          ifsame (fn [args] (when (apply = args) (first args)))
          sames (map ifsame (concat rows cols diags))
          goods (filter #{:x :o} sames)
          winner (first goods)]
      winner)))

(defcheck solution-9317f39
  (fn [b]
    (first (some
             #(if (and (apply = %) (not= (first %) :e)) %)
             (concat
              b
              (apply map vector b)
              (map #(map nth b %) [[0 1 2] [2 1 0]]))))))

(defcheck solution-93557f45
  (letfn [
          (rows [b] (map identity b))
          (cols [b] (apply map #(vector %1 %2 %3) b))
          (dias [b]
            (let [size (count b)]
              (list
                (map #((b %1) %2) (range size) (range size))
                (map #((b %1) %2) (range size) (range (dec size) (dec 0) -1)))))
          (all [b] (concat (rows b) (cols b) (dias b)))
          (winner? [b]
            (let [a (all b)]
              (cond
                (some (partial = [:x :x :x]) a) :x
                (some (partial = [:o :o :o]) a) :o
                :else nil)))]
    winner?))

(defcheck solution-93ce54f9
  (fn [[[a b c] [d e f] [g h i] :as x]]
    (some {[:x :x :x] :x [:o :o :o] :o}
      (list* [a d g] [b e h] [c f i] [a e i] [c e g] x))))

(defcheck solution-94101439
  (letfn [(is-winning [[a b c]]
            (cond (= a b c :x) :x
                  (= a b c :o) :o))]
    (fn [rows]
      (some is-winning [(rows 0) (rows 1) (rows 2)
                        (map #(% 0) rows) (map #(% 1) rows) (map #(% 2) rows)
                        [((rows 0) 0) ((rows 1) 1) ((rows 2) 2) ]
                        [((rows 0) 2) ((rows 1) 1) ((rows 2) 0) ]]))))

(defcheck solution-9429eb12
  (fn [[r1 r2 r3 :as board]]
    (let [c1 (map first board)
          c2 (map second board)
          c3 (map last board)
          d1 [(first r1) (second r2) (last r3)]
          d2 [(last r1) (second r2)  (first r3)]
          all? (fn [v coll] (= (filter #(= % v) coll) coll))
          lines [r1 r2 r3 c1 c2 c3 d1 d2]
          xs (filter identity (map #(all? :x %) lines))
          os (filter identity (map #(all? :o %) lines))]
      (if (not (empty? xs)) :x (if (not (empty? os)) :o nil)))))

(defcheck solution-9482a848
  (fn tic-tac-toe [board]
    (letfn [(sym-win? [sym [frow srow trow]]
              (let [board [frow srow trow]
                    fcol (map first board)
                    scol (map second board)
                    tcol (map last board)
                    diag-1 [(first frow) (second srow) (last trow)]
                    diag-2 [(last frow) (second srow) (first trow)]
                    all-seq [frow srow trow fcol scol tcol diag-1 diag-2]
                    win? (fn [sym sequence]
                           (let [seq-set (set sequence)]
                             (and
                              (= 1 (count seq-set))
                              (= sym (sym seq-set)))))]
                (contains? (set (map #(win? sym %) all-seq)) true)))]
      (cond
        (sym-win? :o board) :o
        (sym-win? :x board) :x
        :else nil))))

(defcheck solution-94bc347
  (fn ttt-winner [board]
    (let [filter-fn (fn [rows] (first (filter #(and (= 1 (count %))
                                                    (not= (first %) :e))
                                        (map distinct rows))))
          h (filter-fn board)
          v (filter-fn (partition 3 (for [x (range 3)
                                          row board]
                                      (nth row x))))
          d (filter-fn [[(nth (nth board 0) 0)
                         (nth (nth board 1) 1)
                         (nth (nth board 2) 2)]
                        [(nth (nth board 0) 2)
                         (nth (nth board 1) 1)
                         (nth (nth board 2) 0)]])]
      (first (or h v d)))))

(defcheck solution-94e2a73d
  (letfn [(is-player [tic]
            (some #(= tic %) [:x :o]))
          (get-winner [sec]
            (let [tic (first sec)
                  winner (and (apply = sec) (is-player tic))]
              (if winner tic nil)))
          (board-cols [board]
            (apply map vector board))
          (board-diag [board]
            [(map-indexed (fn [i v] (v i)) board)])]
    (fn [board]
      (let [board-secs
            (reduce concat
              ((juxt identity board-cols board-diag #(board-diag (reverse %))) board))]
        (first
          (filter is-player
            (map get-winner board-secs)))))))

(defcheck solution-9593bbfa
  (let [
        lines [
               [[0 0] [0 1] [0 2]]
               [[1 0] [1 1] [1 2]]
               [[2 0] [2 1] [2 2]]
               [[0 0] [1 0] [2 0]]
               [[0 1] [1 1] [2 1]]
               [[0 2] [1 2] [2 2]]
               [[0 0] [1 1] [2 2]]
               [[0 2] [1 1] [2 0]]]
        get-line (fn [board line]
                   (let [vals (map #(get-in board %) line)]
                     (if (apply = vals)
                       (if (= (first vals) :e)
                         nil
                         (first vals))
                       nil)))]
    (fn [board]
      (some identity (map #(get-line board %) lines)))))

(defcheck solution-961be3e9
  (fn [rows]

    (letfn [
            (ex [s] (if (seq (rest s)) nil (first s)))
            (e [x] (if (= x :e) nil x))
            (winner [v]
              (ex (into #{} (map e v))))

            (fns []
              [
               (partial map first)
               (partial map second)
               (partial map #(get % 2))
               (partial map-indexed #(get %2 %))
               (partial map-indexed #(get %2 (- 2 %)))
               ]
              )

            (lines [rows]
              (into rows
                ((apply juxt (fns)) rows)))
            ]

      (some identity (map winner (lines rows)))

      )))

(defcheck solution-9653e585
  (fn [[l0 l1 l2]]
    (let [c (fn [i] [(nth l0 i) (nth l1 i) (nth l2 i)])
          d1 [(nth l0 0) (nth l1 1) (nth l2 2)]
          d2 [(nth l0 2) (nth l1 1) (nth l2 0)]
          s (set [l0 l1 l2 (c 0) (c 1) (c 2) d1 d2])
          owin (contains? s [:o :o :o])
          xwin (contains? s [:x :x :x])]
      (cond (and owin (not xwin)) :o
            (and xwin (not owin)) :x))))

(defcheck solution-97eed851
  (fn who-won? [m]
    (let [a [(first m) (second m) (last m),
             (map first m) (map second m) (map last m)
             [(ffirst m) (second (second m)) (last (last m))]
             [(first (last m)) (second (second m)) (last (first m))]]
          b (map set a)
          c (some #(#{#{:x} #{:o}} %) b)]
      (first c))))

(defcheck solution-980c814b
  (fn [v]
    (let [ll [[[0 0] [0 1] [0 2]]
              [[1 0] [1 1] [1 2]]
              [[2 0] [2 1] [2 2]]
              [[0 0] [1 0] [2 0]]
              [[0 1] [1 1] [2 1]]
              [[0 2] [1 2] [2 2]]
              [[0 0] [1 1] [2 2]]
              [[0 2] [1 1] [2 0]]]
          lines (filter #(= (first %) (second %) (last %))
                  (map #(map (fn [pos] ((v (first pos)) (second pos))) %) ll))]
      (if (empty? lines)
        nil
        (let [e (first (first lines))]
          (if (= e :e) nil e))))))

(defcheck solution-980ddc5c
  (fn dd [data]
    (let [a1 [[0 0] [1 1] [2 2]]
          a2 [[0 2] [1 1] [2 0]]
          a3 [[0 0] [0 1] [0 2]]
          a4 [[1 0] [1 1] [1 2]]
          a5 [[2 0] [2 1] [2 2]]
          a6 [[0 0] [1 0] [2 0]]
          a7 [[0 1] [1 1] [2 1]]
          a8 [[0 2] [1 2] [2 2]]
          d (map (fn [c]
                   (map #(get-in data %) c))
              [a1 a2 a3 a4 a5 a6 a7 a8])]
      (reduce (fn [a b] (or a b))
        (map
          (fn [dd]
            (condp every? dd
              #(= :x %) :x
              #(= :o %) :o
              nil))
          d)))))

(defcheck solution-9810df
  (fn tic-won? [t]
    (let [[a b c :as t] (for [x t] (map #(get {:e nil} % %) x))]
      (cond
        (or (apply = (map first t))      (apply = a)) (first a)
        (or (apply = (map #(nth % 2) t)) (apply = c)) (nth c 2)
        (or (apply = (map second t))
            (apply = b)
            (apply = (map #(nth % %2) t [0 1 2]))
            (apply = (map #(nth % %2) t [2 1 0]))) (second b)))))

(defcheck solution-983c786b
  (fn [rows]
    (let [cols (apply map vector rows)
          diag (fn [b] (map-indexed #(nth %2 %) b))
          diags (map diag [rows (map reverse rows)])]
      (some {[:x :x :x] :x,
             [:o :o :o] :o}
        (concat rows cols diags)))))

(defcheck solution-9868aeec
  (fn [[[a b c] [d e f] [g h i]]]
    (ffirst (filter #(and (= (first %) (second %) (last %)) (= 3 (count %))) (map (fn [ls] (filter #(not (= :e %)) ls)) (list [a b c] [d e f] [g h i] [a d g] [c f i] [b e h] [a e i] [c e g]))))))

(defcheck solution-98acdc5e
  (fn f [board]
    (let [l [[0 0] [0 1] [0 2]
             [1 0] [1 1] [1 2]
             [2 0] [2 1] [2 2]
             [0 0] [1 0] [2 0]
             [0 1] [1 1] [2 1]
             [0 2] [1 2] [2 2]
             [0 0] [1 1] [2 2]
             [0 2] [1 1] [2 0]]]

      ((reduce (fn [x y]
                 (let [ans (merge-with + x {y 1} {:count 1})]
                   ;(prn ans)
                   (if (= (ans :count) 3)
                     (cond (= (ans :o) 3) (assoc ans :win :o :x 0 :o 0 :count 0)
                           (= (ans :x) 3) (assoc ans :win :x :x 0 :o 0 :count 0)
                           :else (assoc ans :x 0 :o 0 :count 0)
                           )
                     ans
                     )
                   )
                 ) {:win nil} (map (fn [x]
                                     (nth (nth board (first x)) (second x))
                                     ) l)) :win)
      )
    ))

(defcheck solution-98bd7cc4
  (fn [s]
    (let [c1 (vector (first (first s)) (first (second s)) (first (nth s 2)))
          c2 (vector (second (first s)) (second (second s)) (second (nth s 2)))
          c3 (vector (nth (first s) 2) (nth (second s) 2) (nth (nth s 2) 2))
          d1 (vector (first (first s)) (second (second s)) (nth (nth s 2) 2))
          d2 (vector (nth (first s) 2) (second (second s)) (first (nth s 2)))
          p (hash-map :l1 (first s) :l2 (second s) :l3 (nth s 2) :c1 c1 :c2 c2 :c3 c3 :d1 d1 :d2 d2)
          e [:l1 :l2 :l3 :c1 :c2 :c3 :d1 :d2]]
      (last
        (sort
          (for [x e]
            (if (= (get p x) [:o :o :o])
              :o
              (if (= (get p x) [:x :x :x])
                :x
                nil))))))))

(defcheck solution-991f3a14
  (fn [board]
    (let [winning-combinations [
                                [[0 0] [0 1] [0 2]]  ; horizontal
                                [[1 0] [1 1] [1 2]]
                                [[2 0] [2 1] [2 2]]
                                [[0 0] [1 0] [2 0]]  ; vertical
                                [[0 1] [1 1] [2 1]]
                                [[0 2] [1 2] [2 2]]
                                [[0 0] [1 1] [2 2]]  ; diagonal
                                [[2 0] [1 1] [0 2]]
                                ]
          played-combinations (map #(for [[i j] %] (-> board (get i) (get j))) winning-combinations)
          wins (filter #(and (apply = %) (not= :e (first %))) played-combinations)]
      (if (empty? wins) nil (ffirst wins)))))

(defcheck solution-992503c9
  (fn [x]
    (first
      (disj
        (set
          (map
            first
            (filter
              (partial apply =)
              (concat
               x
               (apply mapv vector x)
               (list (map #(get-in x [%1 %1]) (range 3)))
               (list (map #(get-in x [(- 2 %1) %1]) (range 3))))
              ))
          )
        :e))))

(defcheck solution-99443a85
  (fn ttt [rows]
    (let [cols (apply map vector rows)
          diags (map #(map % (range 3)) [#((rows %) %) #((rows %) (- 2 %))])
          lines (concat rows cols diags)]
      (first (some (comp #{#{:x} #{:o}} set) lines)))))

(defcheck solution-994749b4
  (fn [x]
    (let [r (vec (flatten x)),
          row1 [(r 0) (r 1) (r 2)],
          row2 [(r 3) (r 4) (r 5)],
          row3 [(r 6) (r 7) (r 8)],
          col1 [(r 0) (r 3) (r 6)],
          col2 [(r 1) (r 4) (r 7)],
          col3 [(r 2) (r 5) (r 8)],
          dia1 [(r 0) (r 4) (r 8)],
          dia2 [(r 2) (r 4) (r 6)],
          a [row1 row2 row3 col1 col2 col3 dia1 dia2],
          f (fn [l] (some #(every? (fn [c] (= c l)) %) a))]
      (cond (f :x) :x
            (f :o) :o
            :else nil))))

(defcheck solution-996913fd
  (fn p73 [board]
    (let [indexes [[0 0 0] [1 1 1] [2 2 2] [0 1 2] [2 1 0]]
          all-rows (concat board
                           (map (fn [col]
                                  (map (fn [row idx] (nth row idx)) board col)) indexes))]
      (some (fn [row] (and (apply = row)
                           (#{:o :x} (first row))))
        all-rows))))

(defcheck solution-9984626f
  ; This works with a N x N board for arbitrary N.
  (fn [b]
    (let [rows #(apply (partial map vector) %)
          nn   #(nthnext % %2)
          won? (fn [p]
                 (some #(apply = (conj % p))
                   (concat
                    b (rows b)
                    (rows (map nn b (range)))
                    (rows (map nn (reverse b) (range))))))]
      (cond (won? :x) :x
            (won? :o) :o
            :else     nil))))

(defcheck solution-99c2b841
  (fn [[[a b c]
        [d e f]
        [g h k]]]
    (reduce #(or % %2) (map
                         (fn [[x y z]]
                           (if (and (= x y z) (not= x :e))
                             x
                             nil))
                         [[a b c] [d e f] [g h k]
                          [a d g] [b e h] [c f k]
                          [a e k] [c e g]]))))

(defcheck solution-99dc85df
  (fn  [board]
    (let [ rows 	board
          cols   (apply map vector board)
          diags  [(map get board [0 1 2]) (map get board [2 1 0])]
          all    (concat cols rows diags)

          wins?  (fn [x ls]  (= 3 (count (filter #(= % x) ls))))
          check  (fn [bools] (reduce #(or %1 %2)  bools))

          win-x? (check (map #(wins? :x %) all))
          win-o? (check (map #(wins? :o %) all))]

      (cond
        win-x? :x
        win-o? :o
        ))))

(defcheck solution-99ee8cb
  (fn [board]
    (letfn
     [(win? [a b c] (or (and (not= a :e) (= a b) (= b c) a) nil))
      (b [x y] (nth (nth board x) y))]
      (or
       (win? (b 0 0) (b 0 1) (b 0 2))
       (win? (b 1 0) (b 1 1) (b 1 2))
       (win? (b 2 0) (b 2 1) (b 2 2))
       (win? (b 0 0) (b 1 0) (b 2 0))
       (win? (b 0 1) (b 1 1) (b 2 1))
       (win? (b 0 2) (b 1 2) (b 2 2))
       (win? (b 0 0) (b 1 1) (b 2 2))
       (win? (b 0 2) (b 1 1) (b 2 0))))))

(defcheck solution-9a324711
  (fn winner [board]
    (let [rows (concat board
                       (apply mapv vector board)
                       (vector (mapv get board [0 1 2]))
                       (vector (mapv get board [2 1 0])))
          winning-rows? (fn [e] (> (count (filter (fn [row] (every? #(= e %) row)) rows)) 0))]
      (cond (winning-rows? :x) :x
            (winning-rows? :o) :o
            :else nil))))

(defcheck solution-9a73ad57
  (fn [board]
    (letfn [(sym-at [row col] (nth (nth board row) col))
            (won? [sym]
              (let [sss [sym sym sym]]
                (or (= (nth board 0) sss) ; horizontal
                    (= (nth board 1) sss)
                    (= (nth board 2) sss)
                    (= (map #(nth % 0) board) sss) ; diagonal
                    (= (map #(nth % 1) board) sss)
                    (= (map #(nth % 2) board) sss)
                    (= (list (sym-at 0 0) (sym-at 1 1) (sym-at 2 2)) sss)
                    (= (list (sym-at 0 2) (sym-at 1 1) (sym-at 0 2)) sss))))]
      (cond (won? :x) :x
            (won? :o) :o
            :else nil))))

(defcheck solution-9b22f662
  (fn tic-win [board]
    (let [trans (fn []
                  (let [diags [
                               [[0 0] [1 1] [2 2]]
                               [[0 2] [1 1] [2 0]]
                               ]
                        cols (map #(map (fn [y] [y %]) (range 0 3)) (range 0 3))
                        take-coords (fn [coords-seq]
                                      (map #(get-in board %) coords-seq))]

                    (concat board (map take-coords (concat cols diags)))))

          st (map set (trans))
          won? (fn [x] (some #{#{x}} st))]
      (cond (won? :x) :x
            (won? :o) :o
            :else nil))))

(defcheck solution-9b55ac9c
  (fn [[x y z]]
    (let [sq (into [x y z
                    [(first x) (second y) (last z)]
                    [(first z) (second y) (last x)]] (map vector x y z))
          won? (fn [[x & r]] (when (and (apply = x r) (not= :e x)) x))]
      (first (remove nil? (map won? sq))))))

(defcheck solution-9ba68159
  (fn [board]
    (letfn [(column [i]
              (map #(get % i) board))
            (aligned3-hori? [k]
              (some #(= (board %) (repeat 3 k)) (range 3)))
            (aligned3-vert? [k]
              (some #(= (column %) (repeat 3 k)) (range 3)))
            (aligned3-diag? [k]
              (or (=
                    (vector
                      (-> board (get 0) (get 0))
                      (-> board (get 1) (get 1))
                      (-> board (get 2) (get 2)))
                    (repeat 3 k))
                  (=
                    (vector
                      (-> board (get 2) (get 0))
                      (-> board (get 1) (get 1))
                      (-> board (get 0) (get 2)))
                    (repeat 3 k))))
            (won? [k]
              (or (aligned3-hori? k) (aligned3-vert? k) (aligned3-diag? k)))
            ]
      (if (won? :x)
        :x
        (if (won? :o)
          :o
          nil)))))

(defcheck solution-9bd75849
  (fn
    [[[a00 a01 a02]
      [a10 a11 a12]
      [a20 a21 a22]]]
    (let [s [[a00 a01 a02] [a10 a11 a12] [a20 a21 a22] [a00 a10 a20] [a01 a11 a21] [a02 a12 a22] [a00 a11 a22] [a02 a11 a20]]
          xs (count (filter #(= % 3) (map count (map (fn [x] (filter #(= % :x) x)) s))))
          os (count (filter #(= % 3) (map count (map (fn [x] (filter #(= % :o) x)) s))))]
      (if (= xs 1)
        :x
        (if (= os 1)
          :o
          nil))
      )
    ))

(defcheck solution-9c8057e8
  (fn tic-tac-toe- [board]
    ^{:doc "73. Write a function which analyzes a tic-tac-toe board and
  returns :x if X has won, :o if O has won, and nil if neither player
  has won."}
    (->> board ; rows
      (cons (map first board)) ; col1
      (cons (map second board)) ; col2
      (cons (map last board)) ; col3
      (cons (list (ffirst board) (second (second board)) (last (last board)))) ; diag1
      (cons (list (first (last board)) (second (second board)) (last (first board)))) ; diag1
      (filter (partial apply not= :e)) ; ignore empty rows
      (filter (partial apply =)) ; keep winning rows
      (ffirst) ; return the winner
      )))

(defcheck solution-9d6b9e16
  (fn __ [b]
    (let [rows b
          cols (partition 3 (apply interleave b))
          diags [[(ffirst b) (second (second b)) (nth (nth b 2) 2)]
                 [(first (nth b 2)) (second (second b)) (nth (first b) 2)]]
          candidates (reduce into [rows cols diags])]
      (ffirst (filter #(and
                        (= 1 (count (distinct %)))
                        (not= :e (first (distinct %)))) candidates)))))

(defcheck solution-9e357321
  (fn __ [c]
    (let [coll (flatten c)
          f #(apply (fn [x y z] (if (or (= x y z :x) (= x y z :o)) x nil))
               (map (vec coll) %))
          win [[0 1 2] [3 4 5] [6 7 8] ; row
               [0 3 6] [1 4 7] [2 5 8] ; collum
               [0 4 8] [2 4 6]]           ; diagonal
          ]
      (some f win))
    ))

(defcheck solution-9e3bd205
  (fn [b]
    (let [f  (fn [s] (cond (apply = :x s) :x
                           (apply = :o s) :o
                           :else nil))
          h  (set (map f b))
          c  (fn [x] (map #(get-in b [% x]) [0 1 2]))
          v  (set (for [x [0 1 2]] (f (c x)) ))
          g  (fn [z] (hash-set (f (map #(get-in b %) z))))
          d1 (g [[0 0] [1 1] [2 2]])
          d2 (g [[0 2] [1 1] [2 0]]) ]
      (or (h :x) (v :x) (d1 :x) (d2 :x)
          (h :o) (v :o) (d1 :o) (d2 :o)))))

(defcheck solution-9e531fa2
  (fn [b]
    (letfn [(w [[[a b c]
                 [d e f]
                 [g h i]] p] (or (= p a b c)
                                 (= p d e f)
                                 (= p g h i)
                                 (= p a d g)
                                 (= p b e h)
                                 (= p c f i)
                                 (= p a e i)
                                 (= p c e g)))]
      (cond (w b :x) :x
            (w b :o) :o
            :else nil))))

(defcheck solution-9ed70f61
  (fn [rows]
    (let [cols (for [i (range 3)]
                 (map #(nth % i) rows))
          diags (let [[[a _ b]
                       [_ c _]
                       [d _ e]] rows] [[a c e] [b c d]])]
      (ffirst (filter
                #(and (apply = %) (not= :e (first %)))
                (concat rows cols diags))))))

(defcheck solution-9f4ffa11
  (fn tic-tac-toe [[x y z]]
    (first
      (filter #(not= :e %)
        (concat
         (for [line [x y z]
               :when (apply = line)]
           (line 0))
         (for [col [0 1 2]
               :when (= (x col) (y col) (z col))]
           (x col))
         (if (or (= (x 0) (y 1) (z 2))
                 (= (x 2) (y 1) (z 0)))
           [(y 1)]
           nil))))))

(defcheck solution-9f8730a
  (fn [b]
    (let [j repeat
          d (range 2 -1 -1)
          r (range 3)
          f (fn [a c] (map #(get-in b %) (map list a c)))
          t #(list % % %)]
      (first (some #{(t :x) (t :o)}
               (into [(f r r) (f r d)]
                 (mapcat identity (for [i r]
                                    [(f (j i) r) (f r (j i))]))))))))

(defcheck solution-9fce215c
  (fn [board]
    (letfn [(agg-diag-tl-br
              [board]
              (map #(get-in board %) (for [index (range (count board))]
                                       [index index])))
            (agg-diag-bl-tr
              [board]
              (map #(get-in board %) (for [index (range (count board))]
                                       [(- (count board) (+ index 1)) index])))
            (agg-vertical
              [board]
              (apply interleave board))]

      (let [combinations (partition (count board) (flatten (conj board (agg-vertical board) (agg-diag-tl-br board) (agg-diag-bl-tr board))))]
        (cond (some #(= [:o :o :o] %) combinations) :o
              (some #(= [:x :x :x] %) combinations) :x
              :else nil
              )))
    ))

(defcheck solution-a013901e
  (fn [c]
    (let [f first s second l last g apply
          t #(if (g = %) (f %))
          x (fn [c] (g #(or % %2 %3) (map t c)))
          [a b d] c
          m (s b)
          w (or
             (x c)
             (x (g map vector c))
             (if (= (f a) m (l d)) m)
             (if (= (l a) m (f d)) m))]
      (if (not= :e w) w))))

(defcheck solution-a0a28988
  (fn [b]
    (some (fn [p] (first (keep #(if (apply = p %) p)
                           `(~@b
                             ~@(apply map list b)
                             ~(map get b [0 1 2])
                             ~(map get b [2 1 0])))))
      [:x :o])))

(defcheck solution-a0b3a8d9
  (fn [board]
    (let [diags (fn [board]
                  (vector (map-indexed #(nth %2 %) board)
                    (map-indexed #(nth %2 (- 2 %)) board)))
          vecs (fn [board]
                 (concat (map #(into '() %) board)
                         (vector (map first board))
                         (vector (map second board))
                         (vector (map #(nth % 2) board))
                         (diags board)))
          triplets (vecs board)
          x? (> (.indexOf triplets '(:x :x :x)) -1)
          o? (> (.indexOf triplets '(:o :o :o)) -1)]
      (cond
        x? :x
        o? :o
        :else nil))))

(defcheck solution-a18ae7d2
  (fn [tic]
    (let [rows tic
          cols (partition 3 (apply interleave tic))
          diag1 (take-nth 4 (apply concat tic))
          diag2 (take-nth 4 (apply concat (reverse tic)))
          all (concat rows cols (list diag1 diag2))]
      #_(println diag1 diag2)
      (some #(when (and (apply = %) (not= (first %) :e)) (first %))  all)
      )
    ))

(defcheck solution-a1e353a6
  (fn prob73 [rows]
    (letfn
     [(winner [triple]
        (cond
          (apply = :o triple) :o
          (apply = :x triple) :x))]
      (or
       (when-first [w (filter winner rows)] (first w))
       (when-first
        [w (filter winner
             (map (fn [col] (map #(nth % col) rows)) (range 3)))]
         (first w))
       (winner (map #((rows %) %) (range 3)))
       (winner (map #((rows %) (- 2 %)) (range 3)))))))

(defcheck solution-a2cf4301
  (fn [[r0 r1 r2]]
    (let [paths [[[r0 0] [r0 1] [r0 2]]
                 [[r0 0] [r1 1] [r2 2]]
                 [[r1 0] [r1 1] [r1 2]]
                 [[r0 0] [r1 0] [r2 0]]
                 [[r0 1] [r1 1] [r2 1]]
                 [[r0 2] [r1 2] [r2 2]]
                 [[r2 0] [r2 1] [r2 2]]
                 [[r2 0] [r1 1] [r0 2]]]]
      (loop [[path & tail] paths]
        (let [res (map (fn [[f i]] (f i)) path)]
          (cond
            (every? #{:x} res) :x
            (every? #{:o} res) :o
            (seq tail) (recur tail)))))))

(defcheck solution-a32425ca
  (fn [[v1 v2 v3 :as s]]
    (let [ all `[ ~@s ~@(partition 3 (apply interleave s))
                 [~(first v1) ~(second v2) ~(last v3)]
                 [~(last v1) ~(second v2) ~(first v3)]]]

      (first (first (filter (fn [[i1 i2 i3]] (and (not= i1 :e) (= i1 i2 i3))) all))))))

(defcheck solution-a34e79fd
  (fn Win? [board]
    (letfn [(f [x]
              (if (apply = x)
                (let [y (get x 0)]
                  (if (= y :e)
                    nil
                    y))))]
      (or (reduce #(or %1 %2) (map f board))
          (reduce #(or %1 %2) (map #(f (vec (map (fn [x] (get x %)) board))) (range 3)))
          (f (vec (map #(get (get board %) %) (range 3))))
          (f (vec (map #(get (get board %) (- 2 %)) (range 3))))
          ))))

(defcheck solution-a40916b
  (fn [board]
    (let [rows (clojure.walk/prewalk-replace {:e nil} board)]
      (letfn [(tran [mat]
                (map (fn [i] (map #(nth % i) mat)) (range (count mat))))
              (chk-row [row]
                (let [x (into #{} row)]
                  (if (= 1 (count x)) (first x) nil)))
              (chk-rows [mat]
                (some identity (map chk-row mat)))
              (diag [mat]
                (map-indexed
                  (fn [i row] (first (drop i row)))
                  mat))]
        (or
         (chk-rows rows)
         (chk-rows (tran rows))
         (chk-row (diag rows))
         (chk-row (diag (map reverse rows))))))))

(defcheck solution-a4e18f5d
  (fn [b]
    (let
     [dims [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]
      board (vec (flatten b))
      values (map #(map board %) dims)]
      (ffirst (filter #(and (apply = %) (apply not= :e %)) values)))))

(defcheck solution-a4f02b7d
  (fn ttt [b]
    (let [all= (fn [coll vals]
                 "If all the elements of coll are equal to each other, and that value is one of the
                elements of vals, return that value.  Otherwise return nil.  Obviously it's not a
                good idea to use a vals containing nil."
                 (if (and (= (count (set coll)) 1) (contains? (set vals) (first coll)))
                   (first coll)
                   nil))]
      (some identity
        [ (some #(all= % [:x :o]) b)                                        ; 3 in a row
         (some #(all= % [:x :o]) (apply map vector b))                     ; 3 in a col
         (all= (map #(get %2 %1) (range (count b)) b) [:x :o])             ; 3 on \ diag
         (all= (map #(get %2 %1) (reverse (range (count b))) b) [:x :o])   ; 3 on / diag
         ]))))

(defcheck solution-a5051212
  (fn [board]
    (let [rows   board
          cols   (apply map vector board)
          ul-lr  [(map-indexed #(get %2 %1) board)]
          ur-ll  [(map-indexed #(get %2 (- 2 %1)) board)]
          lines  (concat rows cols ul-lr ur-ll)]
      (first (some #{#{:o} #{:x}} (map set lines))))))

(defcheck solution-a52c65e3
  (fn [board]
    (let [r (range 3)
          flat (concat
                (for [x r y r]
                  ((board y) x))
                (for [y r x r]
                  ((board y) x))
                (map #((board %) %) r)
                (map #((board %) (- (last r) %)) r))]
      (->> flat
        (partition 3)
        (map (fn [[a b c]]
               (cond (= a b c :x) :x
                     (= a b c :o) :o
                     :else nil)))
        (remove nil?)
        (first)))))

(defcheck solution-a5d7b757
  (fn [t]
    (let [g (vec (flatten t))
          w #(let [v (map g %)]
               (if (and (apply = v) (not= (first v) :e)) (first v) nil))
          g [[0 1 2] [3 4 5] [6 7 8]
             [0 3 6] [1 4 7] [3 5 8]
             [0 4 8] [2 4 6]]]
      (first (drop-while nil? (map w g))))))

(defcheck solution-a6d9ea15
  (fn [board] (let [
                    horiz-win (fn [pos]
                                (if (= ((board (first pos)) (second pos))
                                      ((board (first pos)) (mod (+ (second pos) 1) 3))
                                      ((board (first pos)) (mod (+ (second pos) 2) 3))
                                      ) ((board (first pos)) (second pos)) nil
                                        )
                                )
                    verti-win (fn [pos]
                                (if (= ((board (first pos)) (second pos))
                                      ((board (mod (+ (first pos) 1) 3)) (second pos))
                                      ((board (mod (+ (first pos) 2) 3)) (second pos))
                                      ) ((board (first pos)) (second pos)) nil
                                        )
                                )
                    main-diag (fn [pos]
                                (if (not= (first pos) (second pos)) nil
                                                                    (if (= ((board (first pos)) (first pos))
                                                                          ((board (mod (+ (first pos) 1) 3)) (mod (+ (first pos) 1) 3))
                                                                          ((board (mod (+ (first pos) 2) 3)) (mod (+ (first pos) 2) 3))
                                                                          ) ((board (first pos)) (first pos)) nil
                                                                            )
                                                                    )
                                )
                    off-diag (fn [pos] (if (not (= 2 (+ (first pos) (second pos)))) nil
                                                                                    (if (= ((board (first pos)) (second pos))
                                                                                          ((board (mod (+ (first pos) 1) 3)) (mod (- (second pos) 1) 3))
                                                                                          ((board (mod (+ (first pos) 2) 3)) (mod (- (second pos) 2) 3))
                                                                                          ) ((board (first pos)) (second pos)) nil
                                                                                            )
                                                                                    )
                               )]
                (first
                  (filter (fn [z] (and (not (nil? z)) (not= z :e)))
                    (flatten
                      (for [x (range 3) y (range 3)] ((juxt horiz-win verti-win main-diag off-diag) [x y]))
                      )
                    )
                  )
                )))

(defcheck solution-a6ddb62b
  (fn eka
    [board]
    (->> board
      (apply interleave)
      (partition 3)
      (concat board)
      (concat [(reduce #(conj %1 (nth %2 (count %1))) [] board)])
      (concat [(reduce #(conj %1 (nth %2 (count %1))) [] (reverse board))])
      (map #(into #{} %))
      (filter #(not= :e (first %)))
      (filter #(= 1 (count %)))
      (#(if-not (empty? %) (first (first %))))
      )
    ))

(defcheck solution-a7dc0d72
  (fn analyze [board]
    (letfn [(analyzeVec [v] (if (apply = v) (first v) nil))
            (getRow [n] (nth board n))
            (getCol [n] (map #(nth % n) board))
            (getDiag1 [] (vector (first (first board))
                           (second (second board))
                           (last (last board))))
            (getDiag2 [] (vector (last (first board))
                           (second (second board))
                           (first (last board))))]
      (let [rows (map #(analyzeVec (getRow %)) (range 3))
            cols (map #(analyzeVec (getCol %)) (range 3))
            diags (list (analyzeVec (getDiag1)) (analyzeVec (getDiag2)))
            results (concat rows cols diags)]
        (->> results
          (filter (complement nil?))
          (filter (complement #(= :e %)))
          (#(cond
              (empty? %) nil
              (apply = %) (first %)
              :else nil)))))))

(defcheck solution-a8331725
  (fn tic-tac-toe[[v1 v2 v3]]
    (letfn[ (wins [s]
              (or (every? #(= % s ) v1)
                  (every? #(= % s ) v2)
                  (every? #(= % s ) v3)
                  (every? #(= % s ) (vector (first v1) (first v2) (first v3)))
                  (every? #(= % s ) (vector (second v1) (second v2) (second v3)))
                  (every? #(= % s ) (vector (last v1) (last v2) (last v3)))
                  (every? #(= % s ) (vector (first v1) (second v2) (last v3)))
                  (every? #(= % s ) (vector (last v1) (second v2) (first v3)))
                  ))]
      (cond
        (wins :o) :o
        (wins :x) :x
        :else nil))))

(defcheck solution-a845304b
  (fn [tttb]
    (let [rows-cols-diags (concat tttb (apply map vector tttb) [[(nth (nth tttb 0) 0) (nth (nth tttb 1) 1) (nth (nth tttb 2) 2)]] [[(nth (nth tttb 2) 0) (nth (nth tttb 1) 1) (nth (nth tttb 0) 2)]])
          won (reduce (fn [result row] (or result (if (every? #(= :x %) row) :x false) (if (every? #(= :o %) row) :o false))) false rows-cols-diags)]
      (if won won nil))))

(defcheck solution-a855fb52
  (fn tic-tac [r]
    (letfn [(tic-tac-rows [player r]
              (some #(apply = player %) r))
            (transform-board [r]
              (apply map vector r))
            (diags [r]
              (vector
                (map-indexed #(nth %2 %) r)
                (map-indexed #(nth %2 (- (dec (count r)) %)) r)))
            (all-combinations [r]
              (concat r (transform-board r) (diags r)))
            (tic-tac-player [player r]
              (tic-tac-rows player (all-combinations r)))]
      (cond
        (tic-tac-player :x r) :x
        (tic-tac-player :o r) :o))))

(defcheck solution-a85d6710
  (fn [b] (let [m [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]]
            (some {[:x :x :x] :x [:o :o :o] :o} (map #(map (fn [l] (nth (apply concat b) l)) %) m)))))

(defcheck solution-a91ee26e
  (fn [boards]
    (let [rows boards
          cols (apply map vector rows)
          diag1 (map vector (first rows) (rest (second rows)) (rest (rest (last rows))))
          diag2 (map vector (rest (rest (first rows))) (rest (second rows)) (last rows))
          all (concat rows cols diag1 diag2)
          ]
      (cond
        (some #(= % [:x :x :x]) all) :x
        (some #(= % [:o :o :o]) all) :o
        :else nil))))

(defcheck solution-a990bf89
  (fn tictac

    ([x]

     (tictac (replace {:e nil} (first x)) (replace {:e nil} (second x)) (replace {:e nil} (last x))))

    ([x y z]

     (if (= (first x) (first y) (first z))

       (first x)

       (if (= (second x) (second y) (second z))

         (second x)

         (if (= (last x) (last y) (last z))

           (last x)

           (if (= (count(set x)) 1)

             (first x)

             (if (= (count(set y)) 1)

               (first y)

               (if (= (count(set z)) 1)

                 (first z)

                 (if (= (first x) (second y) (last z))

                   (first x)

                   (if (= (first z) (second y) (last x))

                     (first z)

                     nil)))))))))))

(defcheck solution-a9e3806b
  (fn [[[a11 a12 a13] [a21 a22 a23] [a31 a32 a33] :as r]]
    (cond (some (partial = [:x :x :x]) r) :x
          (some (partial = [:o :o :o]) r) :o
          (some (partial = [:x :x :x]) [[a11 a21 a31] [a12 a22 a32] [a13 a23 a33]]) :x
          (some (partial = [:o :o :o]) [[a11 a21 a31] [a12 a22 a32] [a13 a23 a33]]) :o
          (some (partial = [:x :x :x]) [[a11 a22 a33] [a13 a22 a31]]) :x
          (some (partial = [:o :o :o]) [[a11 a22 a33] [a13 a22 a31]]) :o
          :o nil)))

(defcheck solution-aa708e0d
  (fn [board]
    (let [i [0 1 2]
          c (take 12 (cycle i))
          p (flatten (map #(repeat 3 %) i))
          f (fn [a b]
              (map (fn [x y] [x y]) a b))
          t (fn [w]
              (some
                (fn [x] (every? #(= w (get-in board %)) x))
                (partition 3 (into (f (into i p) c) (f c (into (reverse i) p))))))]
      (cond
        (t :x) :x
        (t :o) :o))))

(defcheck solution-aa7813ec
  (fn [vecs-vec]
    (letfn [(col-won? [player col-start game-vec]
              (apply
                (partial = player)
                (for [x (range col-start 9 3)] (nth game-vec x))))
            (row-won? [player row-start game-vec]
              (apply
                (partial = player)
                (for [x (nth (partition 3 game-vec) row-start)] x)))
            (diag-won? [player dir game-vec]
              (let [spots (cond
                            (= dir "left->right") [0 4 8]
                            :else [2 4 6])]
                (apply
                  (partial = player)
                  (for [x spots] (nth game-vec x)))))]
      (let [g-vec (flatten vecs-vec)
            x-cols-won (or (col-won? :x 0 g-vec)
                           (col-won? :x 1 g-vec)
                           (col-won? :x 2 g-vec))
            o-cols-won (or (col-won? :o 0 g-vec)
                           (col-won? :o 1 g-vec)
                           (col-won? :o 2 g-vec))
            x-rows-won (or (row-won? :x 0 g-vec)
                           (row-won? :x 1 g-vec)
                           (row-won? :x 2 g-vec))
            o-rows-won (or (row-won? :o 0 g-vec)
                           (row-won? :o 1 g-vec)
                           (row-won? :o 2 g-vec))
            x-diag-won (or (diag-won? :x "left->right" g-vec)
                           (diag-won? :x "right->left" g-vec))
            o-diag-won (or (diag-won? :o "left->right" g-vec)
                           (diag-won? :o "right->left" g-vec))]
        (if (or x-cols-won x-rows-won x-diag-won)
          :x
          (if (or o-cols-won o-rows-won o-diag-won)
            :o
            nil))))))

(defcheck solution-aae11036
  (letfn [(diags [b]
            [(map #(get-in b [% %]) [0 1 2])
             (map #(get-in b [% (- 2 %)]) [0 1 2])])
          (cols [b]
            (apply map vector b))
          (rows [b] b)
          (winner [r] (cond
                        (apply = :o r) :o
                        (apply = :x r) :x))]
    (fn win? [board]
      (some identity (map winner (concat (diags board) (cols board) (rows board)))))))

(defcheck solution-ab08c4cc
  (fn  [ [[ a1 a2 a3] [b1 b2 b3] [c1 c2 c3]] ]
    (let [ rows [ [ a1 a2 a3] [b1 b2 b3] [c1 c2 c3]
                 [ a1 b1 c1] [a2 b2 c2] [a3 b3 c3]
                 [ a1 b2 c3] [a3 b2 c1] ]
          three-in-a-row  (set (map first (filter #(apply = %) rows))) ]
      (cond
        (contains? three-in-a-row :x) :x
        (contains? three-in-a-row :o) :o
        :else nil
        ))))

(defcheck solution-ab2db4a9
  (fn [horizontals]
    (let
     [lines
      (concat
       horizontals
       (apply map vector horizontals)
       (vector
         (map
           #(nth (nth horizontals %) %)
           (range 0 (count horizontals))))
       (vector
         (map
           #(nth (nth (reverse horizontals) %) %)
           (range 0 (count horizontals)))))]
      (first
        (first
          (filter
            #(= (count %) 1)
            (filter
              #(not(contains? % :e))
              (map set lines))))))))

(defcheck solution-ab2f7722
  (fn [board]
    (if (and (not= :e (first (first board))) (apply = (first board)))
      (first (first board))
      (if (and (not= :e (first (second board))) (apply = (second board)))
        (first (second board))
        (if (and (not= :e (first (last board))) (apply = (last board)))
          (first (last board))
          (if (and (not= :e (first (first board))) (= (first (first board)) (first (second board)) (first (last board))))
            (first (first board))
            (if (and (not= :e (second (first board))) (= (second (first board)) (second (second board)) (second (last board))))
              (second (first board))
              (if (and (not= :e (last (first board))) (= (last (first board)) (last (second board)) (last (last board))))
                (last (first board))
                (if (and (not= :e (first (first board))) (= (first (first board)) (second (second board)) (last (last board))))
                  (first (first board))
                  (if (and (not= :e (last (first board))) (= (last (first board)) (second (second board)) (first (last board))))
                    (last (first board))
                    nil))))))))))

(defcheck solution-ab4dc4dc
  (fn [board]
    (let [b (map
              (fn [p] (map #(cond (= % :e) 0
                                  (= % :x) 1
                                  (= % :o) -1)
                        p))
              board)
          naname (fn [x]
                   (loop [x x s 0 i 0]
                     (if (empty? x)
                       s
                       (recur (rest x) (+ s (nth (first x ) i)) (+ 1 i)))))
          naname2 (fn [x]
                    (loop [x x s 0 i 0]
                      (if (empty? x)
                        s
                        (recur (rest x) (+ s (nth (first x ) (- 2 i))) (+ 1 i)))))
          c (fn [x] (cond (some #(= % 3) x) :x
                          (some #(= % -3 )x) :o
                          :else nil))]
      (c (concat
          (list (naname b))
          (list (naname2 b))
          (map #(reduce + %) b)
          (map #(+ %1 %2 %3)
            (nth b 0)
            (nth b 1)
            (nth b 2)))))))

(defcheck solution-ab52714a
  (fn [board]
    (let
     [horizontal (for [a [0 1 2] b [0 1 2]] (vector a b))
      vertical (for [a [0 1 2] b [0 1 2]] (vector b a))
      diagonal [[0 0] [1 1] [2 2] [0 2] [1 1] [2 0]]
      winning-pos (partition 3 (into diagonal (into horizontal vertical)))
      has-won?
      (fn [player]
        (loop [winning-pos winning-pos]
          (if (empty? winning-pos)
            false
            (if (apply
                  =
                  (cons player (reduce #(conj %1 (get-in board %2))
                                 []
                                 (first winning-pos))))
              true
              (recur (rest winning-pos))))))]
      (if (has-won? :o)
        :o
        (if (has-won? :x)
          :x
          nil)))))

(defcheck solution-ab5663f2
  (fn [b]
    (let [
          goals (concat
                 b
                 (apply map vector b)
                 [[(ffirst b) (second (second b)) (last (last b))]
                  [(last (first b)) (second (second b)) (first (last b))]])
          win (fn [p] (some (partial every? (partial = p)) goals))]
      (cond (win :x) :x (win :o) :o))))

(defcheck solution-ab63170d
  (fn [m]
    (let [r (set (conj (concat m (apply map list m))
                   (map #((m %) %2) (reverse (range 3)) (range))
                   (map #((m %) %2) (range 3) (range))))]
      (cond
        (some #{[:x :x :x]} r) :x
        (some #{[:o :o :o]} r) :o))))

(defcheck solution-ab9149ac
  (fn [g]
    (let [diagonals      [[(get-in g [0 0]) (get-in g [1 1]) (get-in g [2 2])]
                          [(get-in g [0 2]) (get-in g [1 1]) (get-in g [2 0])]]
          winning-combos (concat g (apply map vector g) diagonals)]
      (some {[:x :x :x] :x [:o :o :o] :o} winning-combos))))

(defcheck solution-ab9beb68
  (fn [coll]
    (let [combos [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]
          board (vec (flatten coll))]
      (first (filter #(not= :e %)
               (map first
                 (filter #(apply = %)
                   (map (fn [x] (map #(board %) x)) combos))))))))

(defcheck solution-abd768f4
  #(some
     {[:x :x :x] :x [:o :o :o] :o}
     (concat % (apply map list %) [(map nth % [0 1 2]) (map nth % [2 1 0])])))

(defcheck solution-ac02ec37
  (fn tic-tac-toe
    [b]
    (let [across [(first b) (second b) (last b)]
          down [(map first b) (map second b) (map last b)]
          zig [(first (first b)) (second (second b)) (last (last b))]
          zag [(first (last b)) (second (second b)) (last (first b))]
          candidates (concat across down [zig] [zag])
          x-wins (some #(every? #{:x} %) candidates)
          o-wins (some #(every? #{:o} %) candidates)]
      (cond x-wins :x
            o-wins :o
            :else nil))))

(defcheck solution-ac40d7e3
  (fn tic-tac-toe [board]
    ;; Validating the board by pre-condition:
    {:pre [(= 3 (count board)),                 ; The board has 3 rows.
           (every? #(= 3 (count %)) board),     ; Each row has 3 entries.
           (every? (partial every? #{:x :o :e})
             board),                      ; Each entry is :x, :o, or :e.
           ;; And the number of :x's is either equal to the number of :o's,
           ;; or exactly one greater:
           (let [count-piece
                 (fn [sym] (sym (frequencies (flatten board))))]
             (or (= (count-piece :x) (count-piece :o))
                 (= (count-piece :x) (inc (count-piece :o)))))]}

    ;; Now that we know the board is structurally correct, we create some tools to
    ;; analyze it:

    (let [;; It will be convenient to work with the board in the form of a single
          ;; vector, so that we can address positions by index as in the diagram:
          ;;   [[0 1 2]
          ;;    [3 4 5]
          ;;    [6 7 8]]
          flat-board (->> board flatten vec),

          ;; There are only eight winning positions in tic-tac-toe; if a player
          ;; occupies all three entries in a given position, that player has
          ;; won. This set enumerates them all.
          wins #{[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]},

          ;; The check-pos function wraps one of these winning positions in a
          ;; predicate that returns :x or :o in the case of a win by that
          ;; position, and nil otherwise.
          check-pos (fn [[a b c :as pos]]
                      (cond
                        (apply not= (map flat-board pos))  nil
                        (= (flat-board a) :e)              nil
                        :else                              (flat-board a)))]

      ;; And now it's just a matter of checking all positions.
      (some check-pos wins))))

(defcheck solution-ac44eafe
  (fn [g]
    (let [h (fn [v] (into #{} (flatten (filter (fn [n] (= 1 (count n))) (map distinct v)))))
          rows (h g)
          cols (h (partition 3 (apply interleave g)))
          diag (h (vector (vector (nth (flatten g) 2) (nth (flatten g) 4) (nth (flatten g) 6)) (vector (nth (flatten g) 0) (nth (flatten g) 4) (nth (flatten g) 8))))
          ]
      (if (or (contains? rows :x) (contains? cols :x) (contains? diag :x))
        :x (if (or (contains? rows :o) (contains? cols :o) (contains? diag :o))
             :o nil)))))

(defcheck solution-ac58d2fb
  (fn check-board [board]
    (letfn [(check-horizontal [horizontal]
              (->> horizontal
                (frequencies)
                (some (fn [[key val]]
                        (if (= val 3)
                          key)))
                (#{:x :o})))
            (check-all-horizontals [rows]
              (some check-horizontal rows))
            (check-all-verticals [[r1 r2 r3]]
              (some #{:x :o} (map (fn hi [& column]
                                    (check-horizontal column))
                               r1 r2 r3)))
            (check-top-right-diagonal [[[_ _ a] [_ b] [c]]]
              (check-horizontal [a b c]))
            (check-top-left-diagonal [[[a] [_ b] [_ _ c]]]
              (check-horizontal [a b c]))]
      (or
       (check-all-horizontals board)
       (check-all-verticals board)
       (check-top-right-diagonal board)
       (check-top-left-diagonal board)))))

(defcheck solution-acbe66be
  #(some {[:o :o :o] :o [:x :x :x] :x}
     (partition 3
       (map
         (vec (flatten %))
         [0 1 2 3 4 5 6 7 8
          0 3 6 1 4 7 2 5 8
          0 4 8 2 4 6]))))

(defcheck solution-ad0c03cd
  (fn [board]
    (let [check
          (fn [array]
            (cond
              (some #(= [:x :x :x] %) array) :x
              (some #(= [:o :o :o] %) array) :o
              :else nil))]
      (if-let [winner (check board)]
        winner
        (if-let [winner (check (apply map vector board))]
          winner
          (if-let [winner (check
                            (vector (map get board [0 1 2])
                              (map get board [2 1 0])))]
            winner
            nil))))))

(defcheck solution-ad278e9c
  (fn __ [b]
    (let [r (range (count b))]
      (cond (some true? (map #(apply = :x (nth b %)) r)) :x
            (some true? (map #(apply = :o (nth b %)) r)) :o
            (some true? (map (fn [x] (apply = :x (map #(nth % x) b))) r)) :x
            (some true? (map (fn [x] (apply = :o (map #(nth % x) b))) r)) :o
            (true? (apply = :x (map #(nth (nth b %) %) r))) :x
            (true? (apply = :o (map #(nth (nth b %) %) r))) :o
            (true? (apply = :x (map #(nth (nth b (- 2 %)) %) r))) :x
            (true? (apply = :o (map #(nth (nth b (- 2 %)) %) r))) :o
            ))))

(defcheck solution-ad55dfd3
  (fn [board]
    (let [check-three-boxes (fn [boxes]
                              (if (and (apply = boxes) (not= (first boxes) :e))
                                (first boxes) nil))
          check-seq-of-threes (fn [rows-or-cols]
                                (if (empty? rows-or-cols)
                                  false
                                  (or (check-three-boxes (first rows-or-cols))
                                      (recur (rest rows-or-cols)))))]
      (or
       (check-seq-of-threes board)
       (check-seq-of-threes (apply map vector board))
       (check-three-boxes
         (map #(get-in board [%1 %1]) (range 3)))
       (check-three-boxes
         (map #(get-in board [%1 (- 2 %1)]) (range 3)))))))

(defcheck solution-ad9ac52b
  (fn tic-tac [board]
    (let [A board
          At (apply map vector A)
          As (map reverse A)
          diag (fn [b] (let [[[x _ _] [_ y _] [_ _ z]] b] [x y z]))
          test (fn [y] (ffirst (drop-while #(apply not= %) y)))
          sol? (fn [sol] (if (= sol :e) nil sol))]
      (cond
        (when (test A) true) (sol? (test A))
        (when (test At) true) (sol? (test At))
        (apply = (diag A)) (sol? (first (diag A)))
        (apply = (diag As)) (sol? (first (diag As)))
        :else nil
        ))))

(defcheck solution-adc617e4
  (fn [m]
    (let [r m
          c (apply map vector m)
          d [(map-indexed #(%2 %) m), (map-indexed #(%2 (- 2 %)) m)]
          t (fn [s] (if (and (= (count (distinct s)) 1) (not= (first s) :e)) (first s)))
          f (fn [c] (some t c))
          r (or (f r) (f c) (f d))
          ]
      r)))

(defcheck solution-adde228d
  (let [wins #{7 56 73 84 146 273 292 448}
        to-code (fn [b v] (->> b
                            flatten
                            (map-indexed #(if (= %2 v) (bit-shift-left 1 %1) 0))
                            (reduce + 0)))]
    (fn [b] (let [x (to-code b :x)
                  o (to-code b :o)]
              (first (mapcat #(cond (= % (bit-and % x)) '(:x)
                                    (= % (bit-and % o)) '(:o)
                                    :else '())
                       wins))))))

(defcheck solution-ae0843de
  (fn [b]
    (let [
          e (fn [x y m] (= m (nth (nth b y) x)))
          r (fn [n m] (every? (fn[x]x)(list (e 0 n m) (e 1 n m) (e 2 n m))))
          c (fn [n m] (every? (fn[x]x)(list (e n 0 m) (e n 1 m) (e n 2 m))))
          dr (fn [m] (every? (fn[x]x)(list (e 0 0 m) (e 1 1 m) (e 2 2 m))))
          dl (fn [m] (every? (fn[x]x)(list (e 0 2 m) (e 1 1 m) (e 2 0 m))))
          ]
      (cond
        (r 0 :x) :x
        (r 1 :x) :x
        (r 2 :x) :x
        (c 0 :x) :x
        (c 1 :x) :x
        (c 2 :x) :x
        (dr :x) :x
        (dl :x) :x
        (r 0 :o) :o
        (r 1 :o) :o
        (r 2 :o) :o
        (c 0 :o) :o
        (c 1 :o) :o
        (c 2 :o) :o
        (dr :o) :o
        (dl :o) :o
        :else nil
        ))))

(defcheck solution-ae185986
  (fn tic-tac-toe [board]
    (->>
      (lazy-cat
        (for [row (range 3)] (map #(get % row) board))
        (for [col (range 3)] (get board col))
        (for [k [ (vec (zipmap (range 3) (range 3)))
                 (vec (zipmap (range 3) (reverse (range 3)))) ]]
          (map #(get-in board %) k)))
      (some #{[:o :o :o] [:x :x :x]})
      first)))

(defcheck solution-ae23b541
  (fn [tab]
    (let [idx (for [x (range 3) y (range 3)] [x y])
          fl (map #(partial (fn [l [x y]] (= x l)) %) (range 3))
          fc (map #(partial (fn [c [x y]] (= y c)) %) (range 3))
          fs (concat fl fc [(fn [[x y]] (= x y))]
                     [(fn [[x y]] (= 2 (+ x y)))])
          rs (map set (map #(map (fn [e] (get-in tab e)) %)
                        (map #(filter % idx) fs)))]
      (ffirst (filter #(and (= 1 (count %)) (not (% :e))) rs)))))

(defcheck solution-ae520971
  (fn tic-tac-toe [board]
    (letfn [
            (rows [bd] (seq bd))
            (cols [bd] (for [i (range 3)] (map #(nth % i) (rows bd))))
            (diags [bd] [(map #(get-in bd %) [[0 0] [1 1] [2 2]])
                         (map #(get-in bd %) [[0 2] [1 1] [2 0]])])
            (winner [v] (let [s (set v)]
                          (and (= 1 (count s)) (not= (first s) :e) (first s))))
            (horiz [bd] (some winner (rows bd)))
            (vert [bd] (some winner (cols bd)))
            (diag [bd] (some winner (diags bd)))]
      (or
       (horiz board)
       (vert board)
       (diag board)))))

(defcheck solution-ae727468
  (fn [board]
    (let [cs [[0 0 0]
              [1 1 1]
              [2 2 2]
              [0 1 2]
              [2 1 0]]]
      (first (first
               (filter #(and (not (contains? % :e)) (= 1 (count %)))
                 (map set (concat board (map (fn [c] (map get board c)) cs)))))))))

(defcheck solution-aebb0cd2
  ; Who really wants to parametrize horizontal lines, vertical lines, and diagonals for this?
  ; Also, that line of duplicated code is so minimal that I'll just let it be.
  (let [winning-combs #{[1 2 3] [4 5 6] [7 8 9] [1 4 7] [2 5 8] [3 6 9] [1 5 9] [3 5 7]}]
    (fn [[[a b c] [d e f] [g h i]]] (let [h {1 a, 2 b, 3 c, 4 d, 5 e, 6 f, 7 g, 8 h, 9 i}]
                                      (cond
                                        (some (fn [comb] (every? #(= (h %) :x) comb)) winning-combs) :x
                                        (some (fn [comb] (every? #(= (h %) :o) comb)) winning-combs) :o
                                        true nil)))))

(defcheck solution-afd0a38b
  (fn [b]
    (let [b-flat (replace {:e nil} (flatten b))
          to-check [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]]

      (ffirst (remove nil?
                (for [i to-check]
                  (->> (map #(nth b-flat %) i)
                    frequencies
                    (filter #(= (second %) 3))
                    last)))))))

(defcheck solution-afe720a8
  (fn ttt-winner [board]
    (let [ttt-cross (fn ttt-cross [board]
                      (let [row0 (first board)
                            row1 (fnext board)
                            row2 (fnext (next board))]
                        (cond
                          (and (not= (row0 0) :e) (= (row0 0) (row1 1) (row2 2))) (row0 0)
                          (and (not= (row0 2) :e) (= (row0 2) (row1 1) (row2 0))) (row0 2)
                          :else nil)))
          ttt-vertical (fn ttt-vertical [board]
                         (let [row0 (first board)
                               row1 (fnext board)
                               row2 (fnext (next board))]
                           (cond
                             (and (not= (row0 0) :e) (= (row0 0) (row1 0) (row2 0))) (row0 0)
                             (and (not= (row0 1) :e) (= (row0 1) (row1 1) (row2 1))) (row0 1)
                             (and (not= (row0 2) :e) (= (row0 2) (row1 2) (row2 2))) (row0 2)
                             :else nil)))
          ttt-horizontal (fn ttt-horizontal [board]
                           (some (fn [row] (if (and (apply = row) (not= (first row) :e))
                                             (first row))) board))]
      (or (ttt-horizontal board)
          (ttt-vertical board)
          (ttt-cross board)))))

(defcheck solution-b017d59
  (fn [x] (#{:x :o} (first (first (first
                                    (filter #(#{1} (count %))
                                      (map #(partition-by identity %)
                                        (concat x
                                                (apply (fn [a b c d e f g h i]
                                                         [[a d g][b e h][c f i][a e i][c e g]]) (flatten x)))))))))))

(defcheck solution-b04cb02d
  (fn [b]
    ((fn [xs]
       (let [k (map (fn [[x y]] ((b y) x)) (first xs))]
         (if (empty? xs) nil
                         (cond (every? #(= :o %) k) :o
                               (every? #(= :x %) k) :x
                               :default (recur (rest xs))))))
     [[[0 0][0 1][0 2]] [[1 0][1 1][1 2]] [[2 0][2 1][2 2]]
      [[0 0][1 0][2 0]] [[0 1][1 1][2 1]] [[0 2][1 2][2 2]]
      [[0 0][1 1][2 2]] [[0 2][1 1][2 0]]])))

(defcheck solution-b0f1b120
  (fn [board]
    (first
      (some #{#{:x} #{:o}}
        (map set
          (concat
           board
           (apply map vector board)
           [ (map get board (range))
            (map get (reverse board) (range) )]))))))

(defcheck solution-b1888fe
  (fn [board]
    (letfn [(columns [b] (apply map vector b))
            (lrdiag [b] (map #(nth (nth b %) %) (-> b count range)))
            (rldiag [b] (map #(nth (nth b %) (- (count b) % 1)) (-> b count range)))
            (winner-slice [r]
              (cond
                (not (every? #(= (first r) %) r)) nil
                (= (first r) :e) nil
                :else (first r)))
            (all-slices [b] (concat b (columns b) [(lrdiag b)] [(rldiag b)]))
            (winner [b] (first (remove nil? (map winner-slice (all-slices b)))))]
      (winner board))))

(defcheck solution-b1c14052
  (fn [board]
    (let [t (apply map vector board)
          cross (map get board [0 1 2])
          r-crs (map get board [2 1 0])]
      (some {[:x :x :x] :x [:o :o :o] :o} `[~@board ~@t ~cross ~r-crs]))))

(defcheck solution-b1dd9233
  (fn [board]
    (first
      (for [
            row
            (map
              (fn [x] (map #(get-in board [(quot % 3) (mod % 3)]) x))
              [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])
            :when (and (apply = row) (not= (first row) :e))]
        (first row)))))

(defcheck solution-b1f77eec
  (fn [m]
    (-> (apply concat m)
      vec
      (map [0 1 2
            3 4 5
            6 7 8
            0 3 6
            1 4 7
            2 5 8
            0 4 8
            2 4 6])
      (->>
        (partition 3)
        (map set)
        (filter #(= 1 (count %)))
        (filter #(not= #{:e} %))
        first first))))

(defcheck solution-b2b61417
  (fn grade [input]
    (let [[[a b c] [d e f] [g h i]] input]
      (cond (and (= a d g) (not (= a :e))) a
            (and (= a b c) (not (= a :e))) a
            (and (= a e i) (not (= a :e))) a
            (and (= b e h) (not (= b :e))) b
            (and (= c f i) (not (= c :e))) c
            (and (= c e g) (not (= c :e))) c
            (and (= d e f) (not (= d :e))) d
            (and (= g h i) (not (= g :e))) g
            :else nil))))

(defcheck solution-b2c56f6c
  (fn [brd]
    (let [rows (for [i (range 3) j (range 3)] [i j])
          cols (for [i (range 3) j (range 3)] [j i])
          ldiag [[0 0][1 1][2 2]]
          rdiag [[2 0][1 1][0 2]]
          winner (->> (partition 3 (concat rows cols ldiag rdiag))
                   (map (fn [s] (map #(reduce get brd %) s)))
                   (map set)
                   (filter #(= 1 (count %)))
                   (ffirst))]
      (when (and winner (not= :e winner)) winner))))

(defcheck solution-b2e1526c
  (fn f [b]
    (letfn [(chk [a b c]
              (cond (= a b c :e) false
                    (= a b c :o) :o
                    (= a b c :x) :x))]
      (let [x
            (filter identity (concat (map #(apply chk %) b)
                                     [(apply chk (map first b))
                                      (apply chk (map second b))
                                      (apply chk (map #(first (rest %)) b))]
                                     [(chk (first (first b))
                                        (second (second b))
                                        (nth (nth b 2) 2))
                                      (chk (nth (first b) 2)
                                        (second (second b))
                                        (first (nth b 2)))]))]
        (if (empty? x) nil (first x))))))

(defcheck solution-b2fb3221
  #(letfn [(win? [xs] (and (not (= :e (first xs))) (apply = xs) (first xs)))
           (diagonals [[[a _ b] [_ c _] [d _ e]]]
             (list [a c e] [b c d]))]
     (or (some win? %) ; rows
         (some win? (apply map list %)) ; columns
         (some win? (diagonals %)))))

(defcheck solution-b30dd09f
  (fn [coll]
    ; use destruct for each member + :as
    (first (filter #(not (nil? %1))
             (map (fn [what]
                    (let [all (concat coll
                                      ; rows, cols, diag, rdiag
                                      (apply map vector coll)
                                      [(map-indexed #(%2   %1 ) coll)]
                                      [(map-indexed #(%2 (- (count %2) %1 1)) coll)])]
                      ; use (#{[:x :x :x] [:o :o :o]} result) to match
                      (when (some true? (map (fn [cand] (every? #(= % what) cand)) all)) what))) [ :x :o])))))

(defcheck solution-b3394502
  (fn [[r1 r2 r3]]
    (let [idx (fn [[a b c]] [(r1 a) (r2 b) (r3 c)])
          lines (concat [r1 r2 r3] (map idx '((0 0 0) (1 1 1) (2 2 2) (0 1 2) (2 1 0))))]
      (cond
        (some #(every? #{:o} %) lines) :o
        (some #(every? #{:x} %) lines) :x
        :else nil))))

(defcheck solution-b36156d8
  (fn [[[a b c]
        [d e f]
        [g h i]]]
    (let [lines [[a b c] [d e f] [g h i] [a d g] [b e h] [c f i] [a e i] [c e g]]
          winning-line (fn [line]
                         (cond (every? #{:x} line) :x
                               (every? #{:o} line) :o))]
      (some winning-line lines))))

(defcheck solution-b38fc6ef
  (fn [board]
    (let [
          unfn (fn [b] (filter #(= 1 (count %)) (map distinct b)))
          rows (unfn board)
          cols (unfn (apply map list board))
          crss (unfn [(map get board [0 1 2]) (map get board [2 1 0])])]
      (some #{:x :o} (flatten [rows cols crss])))))

(defcheck solution-b426dc7c
  (fn tic-tac-toe [board]
    (let [array (to-array-2d board)
          size (count board)
          rng (range size)
          rows (map (fn [i] (map (fn [j] (aget array i j)) rng)) rng)
          colls (map (fn [j] (map (fn [i] (aget array i j)) rng)) rng)
          diags [(map (fn[i] (aget array i i)) rng) (map (fn[i] (aget array i (- size (inc i)))) rng)]
          checks (concat rows colls diags)
          winner? (fn [check] (when (and (not-any? #(= :e %) check) (apply = check)) (first check)))]
      (some winner? checks))))

(defcheck solution-b48766dd
  #(reduce-kv % nil (apply merge-with +
                      (map hash-map
                        (flatten %2)
                        [2 3 4 3 4 5 4 5 6]))) #(if (= 0 (rem %3 3)) (#{:x :o} %2) %))

(defcheck solution-b5a45766
  (fn ttt [b]
    (letfn [(w [[[a b c]
                 [d e f]
                 [g h i]] p] (or (= p a b c)
                                 (= p d e f)
                                 (= p g h i)
                                 (= p a d g)
                                 (= p b e h)
                                 (= p c f i)
                                 (= p a e i)
                                 (= p c e g)))]
      (cond (w b :x) :x
            (w b :o) :o
            :else nil))))

(defcheck solution-b5eb08e2
  (fn [board]
    (some #(cond
             (every? #{:x} %) :x
             (every? #{:o} %) :o
             :else false)
      (-> board
        (into (apply mapv vector board))
        (into (mapv (partial map #(nth % %2) board)
                [(range (count board))
                 (range (dec (count board)) -1 -1)]))))))

(defcheck solution-b68db7bb
  (fn [board]
    (let [lines-coordinates [[[0 0] [1 0] [2 0]]
                             [[0 1] [1 1] [2 1]]
                             [[0 2] [1 2] [2 2]]
                             [[0 0] [0 1] [0 2]]
                             [[1 0] [1 1] [1 2]]
                             [[2 0] [2 1] [2 2]]
                             [[0 0] [1 1] [2 2]]
                             [[2 0] [1 1] [0 2]]]
          line-val          (fn [board line-coordinates]
                              (let [vals (map #(get-in board %) line-coordinates)]
                                (cond (every? #(= % :x) vals) :x
                                      (every? #(= % :o) vals) :o
                                      :else nil)))]
      (some #{:x :o}
        (map #(line-val board %)
          lines-coordinates)))))

(defcheck solution-b6cc2f15
  (letfn [(rows [board] board)
          (cols [board] (apply map vector board))
          (diags [board]
            (let [r (dec (count board))]
              [(vec (map-indexed (fn [i col] (get col i)) board))
               (vec (map-indexed (fn [i col] (get col (- r i))) board))]))
          (lines [board]
            (->> board ((juxt rows cols diags)) (apply concat)))]
    (fn winner [board]
      (let [ls (lines board)]
        (cond
          (some #(= % [:x :x :x]) ls) :x
          (some #(= % [:o :o :o]) ls) :o
          :else                       nil)))))

(defcheck solution-b72cc2ba
  (fn test [[r1 r2 r3 :as rows]]
    (let [diagonals [[(r1 0) (r2 1) (r3 2)] [(r1 2) (r2 1) (r3 0)]]
          lines (concat rows diagonals (apply map vector rows))]
      (cond
        (seq (filter #(= % [:x :x :x]) lines)) :x
        (seq (filter #(= % [:o :o :o]) lines)) :o
        :else nil))))

(defcheck solution-b737cbce
  (fn [m]
    (first
      (first
        (filter #(and (not= (first %) :e) (apply = %))
          (concat m (apply map vector m)
                  [(map #(get-in m [% %])        (range 3))
                   (map #(get-in m [% (- 2  %)]) (range 3))]))))))

(defcheck solution-b7b44bb1
  (fn win? [[[x0 y0 z0] [x1 y1 z1] [x2 y2 z2]]]
    (let [condidates [[x0 y0 z0]
                      [x1 y1 z1]
                      [x2 y2 z2]
                      [x0 x1 x2]
                      [y0 y1 y2]
                      [z0 z1 z2]
                      [x0 y1 z2]
                      [x2 y1 z0]]]
      (ffirst (for [condidate condidates
                    :when (and (apply = condidate) (every? #{:x :o} condidate))]
                condidate)))))

(defcheck solution-b90d88ab
  (fn [board]
    (let [cols (apply map vector board)
          diag1 (map-indexed #(get %2 %1) board)
          diag2 (reverse (map-indexed #(get %2 %1) (reverse board)))
          triplets (concat board cols (list (vec diag1)) (list (vec diag2)))]
      (some {[:x :x :x] :x [:o :o :o] :o} triplets))))

(defcheck solution-b9cdbb1f
  (fn [rows]
    (let [dim (count rows)
          indexes (range 0 dim)
          cols (apply map vector rows)
          diags (vector (map #(%1 %2) rows (reverse indexes)) (map #(%1 %2) rows indexes))
          matches (distinct (filter #{:x :o} (map #(when (apply = %) (first %)) (concat rows cols diags))))]
      (when (= 1 (count matches)) (first matches)))))

(defcheck solution-ba34e373
  (fn tt [b]
    (let [di #(= 1 (count (distinct %)))
          ne #(not (= (first %) :e))
          uni #(and (di %) (ne %))
          br (apply map vector b)
          di [[(ffirst b) (second (second b)) (last (last b))]
              [(first (last b)) (second (second b)) (last (first b))]]
          bind (fn [x] (map vector (map uni x) (map first x)))
          [horz vert diag] (map bind [b br di])]
      (cond
        (some first horz) (second (first (filter first horz)))
        (some first vert) (second (first (filter first vert)))
        (some first diag) (second (first (filter first diag)))
        :else nil))))

(defcheck solution-ba8fee78
  (fn [rows]
    (let [chk (concat
               rows
               (for [i (range 3)] (map #(nth % i) rows))
               (for [d [[0 1 2] [2 1 0]]] (map #(nth % %2) rows d)))]
      (first (filter #{:x :o} (map first (filter #(= 1 (count %)) (map distinct chk))))))))

(defcheck solution-ba94488b
  (fn [b]
    (let [n (count b)
          n2 (* n n)
          fb (vec (flatten b))
          rpos (partition n (range 0 n2))
          cpos (for [x (range 0 n)]
                 (take n (iterate (partial + n) x)))
          dspos (list (range 0 n2 (inc n))
                  (range (dec n) 7 (dec n)))
          pos (reduce #(apply conj %1 %2) '() (list rpos cpos dspos))
          verify (fn [r p]
                   (let [fs (map #(get fb %) p)]
                     (cond
                       (every? (partial = :x) fs) :x
                       (every? (partial = :o) fs) :o
                       :default r))
                   )]
      (reduce verify nil pos))))

(defcheck solution-ba9910d2
  (fn [b]
    (let [c (count b)
          r (range c)
          w (map #(if (apply = %) (first %)) b)
          v (apply map (fn [& v] (if (apply = v) (first v))) b)
          h (fn [g] (if (apply = (for [i r] (g i))) (g 0)))
          d (h #(get-in b [% %]))
          e (h #(get-in b [% (dec (- c %))]))]
      (some #(when (or (= :x %) (= :o %)) %) (concat w v [d e])))))

(defcheck solution-bb1e762b
  (fn [board]
    (let [cell-values (fn [& cells]
                        (vec (map #(get-in board %) cells)))
          horizontal-rows board
          vertical-rows (apply map vector board)
          diagonal-rows [(cell-values [0 0] [1 1] [2 2])
                         (cell-values [0 2] [1 1] [2 0])]
          all-rows (concat
                    horizontal-rows
                    vertical-rows
                    diagonal-rows)]
      (condp #(some #{%1} %2) all-rows
        [:x :x :x] :x
        [:o :o :o] :o
        nil))))

(defcheck solution-bb393658
  (fn tic-tac [rows]
    (let [all-equal? (fn [r x] (every? #(= % x) r))
          verts (apply map vector rows)
          diags [[(get-in rows [0 0]), (get-in rows [1 1]), (get-in rows [2 2])]
                 [(get-in rows [0 2]), (get-in rows [1 1]), (get-in rows [2 0])]]]
      (if
       (some #(all-equal? % :x) (concat rows verts diags))
        :x
        (if (some #(all-equal? % :o) (concat rows verts diags))
          :o
          nil)))))

(defcheck solution-bb6fdbf0
  (fn [[[a b c :as f] [d e f :as s] [g h i :as t]]]
    (some {[:o :o :o] :o [:x :x :x] :x}
      [f s t [a d g] [b e h] [c f i] [a e i] [c e g]])))

(defcheck solution-bbd4ff4b
  (fn tic-tac [board]
    (let [ column (fn [c] (map (fn [r] (get-in board [r c])) [0 1 2]))
          diag1 (map (fn [i] (get-in board [i i]) ) [0 1 2])
          diag2 (map (fn [i] (get-in board [i (- 2 i)]) ) [0 1 2])
          cols (map column [0 1 2])
          [w & _] (filter (fn [x] (and (= 1 (count x)) (not (x :e))))
                    (map (fn [x] (into #{} x))
                      (concat board [diag1 diag2] cols)))]
      (first (into [] w)))))

(defcheck solution-bbe4f6ae
  (fn win?
    [board]
    (let [b1 (nth board 0)
          b2 (nth board 1)
          b3 (nth board 2)
          c1 (map #(get-in board [% 0]) [0 1 2])
          c2 (map #(get-in board [% 1]) [0 1 2])
          c3 (map #(get-in board [% 2]) [0 1 2])
          d1 (map #(get-in board [% %]) [0 1 2])
          d2 (map #(get-in board %) [[2 0] [1 1] [0 2]])]
      (let [xs [b1 b2 b3 c1 c2 c3 d1 d2]
            condi (map (fn [a] (let [x (set a)]
                                 (if (= 1 (count x))
                                   (first x)
                                   nil)))
                    xs)
            win (filter #(not (or (nil? %) (= :e %))) condi)]
        (if (empty? win)
          nil
          (first win))))))

(defcheck solution-bcbaa5b3
  (fn ttt [col]
    (let [cell-vaule (fn [m n] (nth (nth col n) m))
          h-line (for [x (range 3) ]  [[x 0] [x 1] [x 2]])
          v-line (for [y (range 3) ]  [[0 y] [1 y] [2 y]])
          d-line [  [[0 0] [1 1] [ 2 2] ]    [[2 0] [1 1] [0 2]]  ]
          all-lines (concat h-line  v-line d-line)
          check-line (fn [theline]
                       (let [line-values (map (fn [[x y]] (cell-vaule x y ))  theline ) ]
                         (cond (every? #(= :o %) line-values) :o
                               (every? #(= :x %) line-values) :x
                               :else nil
                               )))
          ]
      (first (filter identity (map check-line all-lines)))

      )


    ))

(defcheck solution-bcf905a8
  (fn [board]
    (letfn [
            (vertical [board n]
              (take-nth 3 (drop n board)))
            (verticals [board]
              (map (partial vertical board) (range 3)))
            (horizontal [board n]
              (nth (partition 3 board) n))
            (horizontals [board]
              (map (partial horizontal board) (range 3)))
            (diag-1 [board]
              (take-nth 4 board))
            (diag-2 [board]
              (take-nth 2 (drop-last 2 (drop 2 board))))
            (diagonals [board]
              ((juxt diag-1 diag-2) board))
            (scoring-rows [board]
              (concat (horizontals board)
                      (verticals board)
                      (diagonals board)))
            (winning-row [[s & p]]
              (if (every? (partial = s) p)
                s))]
      (->> board
        flatten
        scoring-rows
        (map winning-row)
        (filter identity)
        (remove (partial = :e))
        first))))

(defcheck solution-bd04a56e
  (fn [state]
    (let [column ((fn [state]
                    (for [x (range 0 3)]
                      (map #(nth % x) state))) state)
          diag [[(get-in state [0 0]) (get-in state [1 1]) (get-in state [2 2])]
                [(get-in state [0 2]) (get-in state [1 1]) (get-in state [2 0])]]
          row state
          lines (concat row column diag)
          winner? (fn [p] (some #{p} (map #(if (every? #{p} %) p) lines)))]

      (cond
        (winner? :x) :x
        (winner? :o) :o
        :else nil))))

(defcheck solution-bdab987f
  (fn ttt
    [board]
    (let [board (map (fn [lista] (replace {:e nil} lista)) board)]
      (letfn [(line-winner [line]
                (reduce (fn [c1 c2] (if (= c1 c2) c1 nil)) line))
              (third [x]
                (second (next x)))]
        (or
         (line-winner (first board))
         (line-winner (fnext board))
         (line-winner (third board))

         (line-winner [(ffirst board) (first (second board)) (first (third board))])
         (line-winner [(second (first board)) (second (second board)) (second (third board))])
         (line-winner [(third (first board)) (third (second board)) (third (third board))])

         (line-winner [(ffirst board) (second (second board)) (third (third board))])
         (line-winner [(first (third board)) (second (second board)) (third (first board))]))))))

(defcheck solution-bdd76585
  (fn [x] (ffirst (filter #(and (apply = %) (not= (first %) :e)) (partition 3 (map (vec (flatten x)) '(0 1 2 3 4 5 6 7 8 0 3 6 1 4 7 2 5 8 0 4 8 2 4 6)))))))

(defcheck solution-bdfa80cb
  (fn [a]
    (letfn [(iz [sym]
              (#{"111000000", "000111000", "000000111","100100100",
                 "010010010", "001001001","100010001","001010100"}
               (apply str (map #(if (= sym %) "1" "0") (flatten a))))
              )]
      (cond
        (iz :o) :o
        (iz :x) :x
        )
      )
    ))

(defcheck solution-be067796
  (fn [board]
    (let [	sboard (apply concat board)
          wins [#{0 1 2} #{3 4 5} #{6 7 8} #{0 3 6} #{1 4 7} #{2 5 8} #{0 4 8} #{2 4 6}]
          xs (keep-indexed #(if (= :x %2) %1) sboard)
          os (keep-indexed #(if (= :o %2) %1) sboard)
          wcomp (fn [coll]
                  (->> 	(map #(filter identity (map % coll)) wins)
                    (map count)
                    (filter #(= 3 %))
                    seq))]

      (cond
        (wcomp xs) :x
        (wcomp os) :o
        :else nil))))

(defcheck solution-be24ebf5
  (fn [[[c11 c12 c13] [c21 c22 c23] [c31 c32 c33]]]
    (let
     [winlines [[c11 c12 c13] [c21 c22 c23] [c31 c32 c33] [c11 c21 c31] [c12 c22 c32] [c13 c23 c33] [c11 c22 c33] [c13 c22 c13]]
      wins (map #(first %) (filter #(apply = %) winlines))]
      (cond
        (some #(= :x %) wins) :x
        (some #(= :o %) wins) :o
        ))))

(defcheck solution-be291142
  (fn [brd]
    (let [diag #(mapcat (fn [r c] [(nth r c)]) % (range))
          data (concat brd (apply map vector brd) [(diag brd) (diag (reverse brd))])
          won? (fn [p] (some #(apply = (conj % p)) data))]
      (cond
        (won? :x) :x
        (won? :o) :o
        :else nil))))

(defcheck solution-be4dbdf2
  (fn [board]
    (->> (concat board
                 (apply map vector board)
                 (let [[[a _ _] [_ b _] [_ _ c]] board] [[a b c]])
                 (let [[[_ _ a] [_ b _] [c _ _]] board] [[a b c]]))
      (some #{[:x :x :x] [:o :o :o]})
      first)))

(defcheck solution-be919f35
  (fn [b]
    (let [f first
          s second
          l last
          p partial
          m map
          a (concat b [(m f b)
                       (m s b)
                       (m l b)
                       [(-> b f f)
                        (-> b s s)
                        (-> b l l)]
                       [(-> b f l)
                        (-> b s s)
                        (-> b l f)]])
          r (->> a
              (m set)
              (filter #(= 1 (count %)))
              (f)
              (set))]
      (or (:x r) (:o r)))))

(defcheck solution-be95b867
  (fn [matrix]
    (letfn [(judge [v]
              (cond (every? #(= :o %) v) :o
                    (every? #(= :x %) v) :x
                    :else nil))
            (transpose [m]
              (apply mapv vector m))]
      (let [crosses [(map #(-> matrix (nth %1) (nth %1)) (range 0 3))
                     (map #(-> matrix (nth %1) (nth (- 2 %1))) (range 0 3))]
            res (apply clojure.set/union (map set
                                           [(map #(judge %) matrix)
                                            (map #(judge %) (transpose matrix))
                                            (map #(judge %) crosses)]))]
        (cond (some #(= :o %) res) :o
              (some #(= :x %) res) :x
              :else nil)))))

(defcheck solution-be9a721f
  (fn check [b]
    (letfn [(win-vert? [b p] (reduce #(or %1 %2) (apply (partial map #(= p %1 %2 %3)) b)))
            (win-hori? [b p] (win-vert? (partition 3 (apply interleave b)) p))
            (win-diag? [b p] (letfn [(test [x] (every? identity (map #(= p (get %1 %2)) x (range 3))))] (or (test b) (test (reverse b)))))
            (win? [b p] (or (win-vert? b p) (win-hori? b p) (win-diag? b p)))]
      (cond (win? b :x) :x (win? b :o) :o :else nil))))

(defcheck solution-bef441e9
  (fn [b]
    (->> [(first b) (second b) (last b)
          (map first b) (map second b) (map last b)
          (map #((b %) %) (range 3)) (map #((b (- 2 %)) %) (range 3))]
      (map #(if (apply = %) (first %) :e))
      (filter (partial not= :e))
      (first))))

(defcheck solution-befb16b6
  (fn tic-tac-toe [m]
    (let [t1
          (loop [i 0] ;;;Checa as linhas
            (if (= ((m i) 0) ((m i) 1) ((m i) 2))
              ((m i) 0)
              (if (< i 2) (recur (inc i)))))
          t2
          (loop [j 0] ;;;Checa as colunas
            (if (= ((m 0) j) ((m 1) j) ((m 2) j))
              ((m 0) j)
              (if (< j 2) (recur (inc j)))))
          t3
          (if (= ((m 0) 0) ((m 1) 1) ((m 2) 2)) ;;Checa diag prin
            ((m 0) 0))
          t4
          (if (= ((m 0) 2) ((m 1) 1) ((m 2) 0)) ;;Checa diag sec
            ((m 2) 0))]
      (if (and (not= t1 :e) (not= t1 nil)) t1
                                           (if (and (not= t2 :e) (not= t2 nil)) t2
                                                                                (if (and (not= t3 :e) (not= t3 nil)) t3
                                                                                                                     (if (and (not= t4 :e) (not= t4 nil)) t4)))))))

(defcheck solution-bf0aa452
  (fn ttt [xs]
    (let [pieces-in-coords (fn [board t coords]
                             (every? true? (map #(= t (board %)) coords)))
          pieces-in-positions (fn [board t]
                                (some true? (map (partial pieces-in-coords board t)
                                              [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])))
          board (vec (apply concat xs))]
      (cond (pieces-in-positions board :x) :x
            (pieces-in-positions board :o) :o
            :else nil))))

(defcheck solution-bf196c23
  (fn [A]
    (->> [0 1 2 [0 0 0] [1 1 1] [2 2 2] [0 1 2] [2 1 0]]
      (map
        (fn [a] (if (coll? a) (map #(%1 %2) A a) (A a))))
      (filter (partial apply =))
      (filter #(not= (last %) :e))
      (last)
      (#(and % (last %)))

      )))

(defcheck solution-bf3f2b64
  (fn [colls]
    (let [
          mp (fn [k] (map (fn [coll] (map #(when (= k %) %) coll)) colls))
          check
             (fn [colls] (or
                          (reduce #(or % %2) (map #(reduce (fn [x y] (and x y)) %) colls))
                          (reduce #(or % %2) (apply (partial map #(and % %2 %3)) colls))
                          (and (first (first colls)) (second (second colls)) (last (last colls)))
                          (and (last (first colls)) (second (second colls)) (first (last colls)))
                          ))]
      (or (check (mp :x)) (check (mp :o))))))

(defcheck solution-bfbf3aa1
  (fn winner [board]
    (letfn [(transpose [m] (vec (apply map vector m)))
            (major-diagonal [m] (vec (map-indexed #(%2 %1) m)))
            (minor-diagonal [m] (major-diagonal (vec (reverse (apply map vector m)))))
            (won-by? [player board]
              (let [row [player player player]]
                (or (some #{row} board)
                    (some #{row} (transpose board))
                    (= row (major-diagonal board))
                    (= row (minor-diagonal board)))))]
      (cond (won-by? :x board) :x
            (won-by? :o board) :o))))

(defcheck solution-c0317f3d
  (fn [board]
    (let [
          diags (fn [b] (split-at 3 (map #(nth (flatten b) %) '(0 4 8 2 4 6))))
          verts (fn [b] (apply map list b))
          combos (concat board (diags board) (verts board))
          filter-s (filter #(= (count %) 1) (map distinct combos))
          filter-e (filter #(not= (first %) :e) filter-s)
          ]
      (ffirst filter-e))))

(defcheck solution-c0cd49a3
  (fn [board] (let [transpose #(apply map vector %)
                    diagonal  #(first (reduce (fn [[a n] xs] [(conj a (nth xs n)) (+ n 1)]) [[] 0] %))
                    mirror 	(fn [xs] (map #(vec (reverse %)) xs))
                    lines     #(concat % (transpose %) [(diagonal %) (diagonal (mirror %))])
                    winner? 	(fn [player row] (reduce #(and %1 (= player %2)) true row))
                    winner 	#(if (winner? :x %) :x (if (winner? :o %) :o nil))]
                (first (drop-while #(= nil %) (map winner (lines board)))))))

(defcheck solution-c0d723f8
  (fn ttt-win [b]
    (letfn [(all-three [side] (fn [cells] (every? (partial = side) cells)))
            (row? [side rows] (some (all-three side) rows))
            (diag? [side diag] ((all-three side) (map nth b diag)))
            (win? [side] (or (row? side b)
                             (row? side (apply map vector b))
                             (diag? side [0 1 2])
                             (diag? side [2 1 0])))]
      (cond (win? :x) :x
            (win? :o) :o
            :else nil))))

(defcheck solution-c12cd883
  (fn [board]
    (letfn [(winner [p row] (apply = p row))
            (row [n b] (nth b n))
            (col [n b] (nth (apply map vector b) n))
            (d1 [b] [(get-in b [0 0]) (get-in b [1 1]) (get-in b [2 2])])
            (d2 [b] [(get-in b [2 0]) (get-in b [1 1]) (get-in b [0 2])])
            (game [p b] (some identity
                          (concat (list (winner p (d1 b)) (winner p (d2 b)))
                                  (for [i (range 3)]
                                    (or (winner p (row i b)) (winner p (col i b)))))))]
      (cond (game :x board) :x (game :o board) :o :else nil))))

(defcheck solution-c165f59a
  (fn [v]
    (letfn [(swap [v]
              [(vec (map first v))
               (vec (map second v))
               (vec (map #(% 2) v))])
            (win? [m v]
              (or
               (some #(every? #{m} %) v)
               (some #(every? #{m} %) (swap v))
               (= m ((v 0) 0) ((v 1) 1) ((v 2) 2))
               (= m ((v 0) 2) ((v 1) 1) ((v 2) 0))))]
      (cond
        (win? :x v) :x
        (win? :o v) :o
        :else nil))))

(defcheck solution-c197fb53
  (fn [board]
    (ffirst
      (filter
        #(or (= % [:x :x :x]) (= % [:o :o :o]))
        (concat
         board
         (partition 3 (apply interleave board))
         (map
           #(take-nth 4 (flatten %))
           [board (map reverse board)]))))))

(defcheck solution-c22b6282
  (fn [s]
    (#{:x :o}
     (ffirst
       (filter
         #(= (count (distinct %)) 1)
         (concat s
                 (map #(map nth s %)
                   (conj (map #(repeat 3 %) (range 3))
                     (range 3) (reverse (range 3))))))))))

(defcheck solution-c26216a5
  (fn quigagne [pl]
    (let [lignes [[1 1 1 0 0 0 0 0 0]
                  [0 0 0 1 1 1 0 0 0]
                  [0 0 0 0 0 0 1 1 1]
                  [1 0 0 1 0 0 1 0 0]
                  [0 1 0 0 1 0 0 1 0]
                  [0 0 1 0 0 1 0 0 1]
                  [1 0 0 0 1 0 0 0 1]
                  [0 0 1 0 1 0 1 0 0]]
          fmatch
                 (fn m [p l j]
                   (= 3
                     (count (for [i (range 9) :when (and (= (nth l i) 1) (= (nth p i) j))] 1))
                     )
                   )
          plateau (flatten pl)
          ]
      (if (not-any? #(fmatch plateau % :x) lignes)
        (if (not-any? #(fmatch plateau % :o) lignes) nil
                                                     :o)
        :x)
      )))

(defcheck solution-c26f593d
  (fn[rows]
    (let [cols (partition 3 (for [idx [0 1 2] c rows] (nth c idx)))
          d1 (keep-indexed (fn[idx item] (when (#{0 4 8} idx) item)) (mapcat identity rows))
          d2 (keep-indexed (fn[idx item] (when (#{2 4 6} idx) item)) (mapcat identity rows))]
      (let [res (ffirst (filter #(= 1 (count (set %))) (concat rows cols [d1] [d2])))]
        (when (not= :e res)
          res)))))

(defcheck solution-c273c17
  (let
   [check (fn [& sets]
            (first (filter #(not (nil? %))
                     (map
                       (fn [ss]
                         (let [r (first (filter #(or (= % #{:x}) (= % #{:o})) ss))]
                           (if r (first r) nil)))
                       sets))))]
    (fn ttt [board]
      (check
        (map set board)
        (map set (apply map list board))
        (list (set (map #(nth (nth board %) %) (range 3))))
        (list (set (map #(nth (nth board %) (- 2 %)) (range 3))))
        ))))

(defcheck solution-c2daa303
  (fn [board]
    (let [f (fn [l] (#(or (= % [0 1 2]) (= % [2 1 0]) (= 1 (count (distinct %))))
                     (flatten l)))
          fx (fn [y] (map (fn [v] (map first (filter #(= (second %) y) (map-indexed vector v)))) board))]
      (cond (f (fx :x)) :x
            (f (fx :o)) :o))))

(defcheck solution-c33753a
  (fn f73 [col]
    (letfn [(hor [c n] (list (first (nth c n)) (apply = (nth c n))))
            (ver [c n] (list (nth (first c) n) (apply = (map #(nth % n) c))))
            (dig [c] (list (second (second c))
                       (or (apply = (list (first (first c))
                                      (second (second c))
                                      (nth (nth c 2) 2)))
                           (apply = (list (nth (first c) 2)
                                      (second (second c))
                                      (first (nth c 2)))))))]
      (let [winner (filter #(true? (second %))
                     (concat (map #(hor col %) (range 0 2))
                             (map #(ver col %) (range 0 2))
                             (list (dig col))))]
        (cond (= :x (first (first winner)))
              :x
              (= :o (first (first winner)))
              :o)))))

(defcheck solution-c38870d9
  (fn [board]
    (letfn
     [(row [r] (let [s (set r)] (if (and (= 1 (count s)) (not (= :e (first s)))) (first s) false)))
      (get-col [board] (apply map vector board))
      (get-cross [board]
        (let [ct (count (first board))]
          (loop [c 0 b board x1 [] x2 []]
            (if (empty? b)
              [x1 x2]
              (recur (+ c 1) (rest b) (conj x1 (nth (first b) c)) (conj x2 (nth (first b) (- ct c 1))))))))
      ]
      (first (filter (complement false?) (apply concat (map #(map row %) [board (get-col board) (get-cross board)])))))))

(defcheck solution-c38e78ff
  (fn [[[r11 r12 r13 :as r1]
        [r21 r22 r23 :as r2]
        [r31 r32 r33 :as r3] :as board]]
    (let [triples (list r1 r2 r3
                    (map first board) (map second board) (map #(% 2) board)
                    [r11 r22 r33]
                    [r31 r22 r13])]
      ((fn aux [tpls]
         (cond
           (empty? tpls) nil
           (= (first tpls) [:x :x :x]) :x
           (= (first tpls) [:o :o :o]) :o
           :else (recur (rest tpls))))
       triples))))

(defcheck solution-c3c3d42c
  (fn my-game [xss]
    (let [xs (flatten xss)
          same #(apply = %)
          is [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]
          r (map #(map (fn [i] (nth xs i)) %) is)
          t (filter same r)
          w (remove #(= :e (first %)) t)]
      (when w (ffirst w)))))

(defcheck solution-c3c5ce28
  (fn tic-tac-toe
    [[[a b c] [d e f] [g h i]]]
    (letfn [(all?
              [p xs]
              (every? #(= p %) xs))
            (winner?
              [p]
              (some #(all? p %) [[a b c] [d e f] [g h i] [a d g] [b e h] [c f i] [a e i] [c e g]]))]
      (cond (winner? :x) :x (winner? :o) :o :else nil))))

(defcheck solution-c46e0233
  (fn [[ r1 r2 r3 :as brd ]]
    (let [vld #{:x :o}
          inverted (reduce (fn [b r] (map #(conj %1 %2) b r)) [[][][]] brd)
          chk-row (fn chk-r
                    ([l] (chk-r l brd))
                    ([l b] (some #(apply = (conj % l)) b)))
          chk-col #(chk-row % inverted)
          chk-dia (fn [l]
                    (or
                     (= l
                       (first r1)
                       (first (rest r2))
                       (last  r3))
                     (= l
                       (first r3)
                       (first (rest r2))
                       (last  r1))))
          winner (filter #(or (chk-row %) (chk-col %) (chk-dia %)) #{:x :o})
          ]
      (if (empty? winner)
        nil
        (first winner))
      )
    ))

(defcheck solution-c4ad3539
  (fn tictactoe
    [m]
    (letfn [(tr [m] (apply map vector m))
            (diag [[[a _ b][_ c _][d _ e]]] [[a c e] [b c d]])]
      (first (first
               (filter
                 #(and
                   (= 1 (count (distinct %)))
                   (not= :e (first %)))
                 (concat m (diag m) (tr m))))))))

(defcheck solution-c4bf63cb
  (fn [[row1 row2 row3 :as rows]]
    (let [vrow1 (map first rows)
          vrow2 (map second rows)
          vrow3 (map last rows)
          drow1 [(first row1) (second row2) (last row3)]
          drow2 [(first row3) (second row2) (last row1)]
          all-rows (conj rows vrow1 vrow2 vrow3 drow1 drow2)
          win-row (fn [p row]
                    (every? #(= p %) row))]
      (cond
        (some #(win-row :o %) all-rows) :o
        (some #(win-row :x %) all-rows) :x
        :else nil))))

(defcheck solution-c50c5a3b
  (fn [R A M b]
    (some #(and (A = %) (#{:x :o} (first %)))
      ` (~@ b
         ~@ (A M list b)
         ~ (M #(%2 %) R b)
         ~(M #(%2 (- 2 %)) R b)))) [0 1 2] apply map)

(defcheck solution-c59fe2f4
  (fn [coll]
    (->> (concat
          (map identity coll)
          (apply map (fn [& args] args) coll)
          (vector (map-indexed #(nth %2 %1) coll)
            (map-indexed #(nth %2 %1) (reverse coll))))
      (map distinct)
      (filter #(= 1 (count %)))
      flatten
      first
      {:x :x :o :o})))

(defcheck solution-c6052794
  (fn [board]
    (let [lines (concat board
                        (for [i (range 3)] (map #(nth % i) board))
                        (vector (for [i (range 3)] (get-in board [i i])))
                        (vector (for [i (range 3)] (get-in board [i (- 3 1 i)]))))
          is-winner (fn [player] (some #(every? #{player} %) lines))]
      (cond
        (is-winner :x) :x
        (is-winner :o) :o
        :else nil))))

(defcheck solution-c63b2318
  (fn [b]
    (let [check (fn [[[a b c] [d e f] [g h i]] s]
                  (some identity [(= a b c s) (= d e f s) (= g h i s)
                                  (= a d g s) (= b e h s) (= c f i s)
                                  (= a e i s) (= c e g s)]))]
      (cond (check b :x) :x (check b :o) :o))))

(defcheck solution-c668d884
  (fn [[[a b c :as x] [d e f :as y] [g h i :as z]]]
    (ffirst
      (filter
        #(and (not-any? #{:e} %) (apply = %))
        [x y z [a d g] [b e h] [c f i] [a e i] [c e g]]))))

(defcheck solution-c6ff38de
  (fn [rs]
    (letfn [(transpose [r] (apply map (cons list r)))
            (diag [r] (map-indexed #(get (vec %2) %) r))
            (mirrorv [r] (map reverse r))]
      (->>     (concat rs (transpose rs) [(diag rs)] [(diag (mirrorv rs))])
        (filter #(not= [:e :e :e] %))
        (map #(partition-by list %))
        (filter #(= 1 (count %)))
        ffirst
        first
        ))))

(defcheck solution-c721bb7
  (fn [[[a b c :as f] [d e f :as s] [g h i :as t]]]
    (some {[:o :o :o] :o [:x :x :x] :x}
      [f s t [a d g] [b e h] [c f i] [a e i] [c e g]])))

(defcheck solution-c7401ce4
  (fn  [board]
    (let [verticals (apply map vector board)
          diagonals (apply (partial map vector) (for [i [0 1 2]] [(get-in board [i i]) (get-in board [i (- 2 i)])]))
          check-lines (map distinct (concat board verticals diagonals))
          winner-cond (filter #{[:x] [:o]} check-lines)]
      (cond
        (= winner-cond [[:x]]) :x
        (= winner-cond [[:o]]) :o
        :else nil))))

(defcheck solution-c77af3e9
  (fn ticTacToe [board]
    (letfn [(row [i] (board i))
            (col [i] (mapv #(nth % i) board))
            (lr [] [(get-in board [0 0]) (get-in board [1 1]) (get-in board [2 2])])
            (rl [] [(get-in board [0 2]) (get-in board [1 1]) (get-in board [2 0])])
            (elemWin [elem]
              (or
               (every? #(= elem %) (row 0))
               (every? #(= elem %) (row 1))
               (every? #(= elem %) (row 2))
               (every? #(= elem %) (col 0))
               (every? #(= elem %) (col 1))
               (every? #(= elem %) (col 2))
               (every? #(= elem %) (lr))
               (every? #(= elem %) (rl))))]
      (cond
        (elemWin :x) :x
        (elemWin :o) :o      :else nil))))

(defcheck solution-c7931b27
  (fn
    [x]
    (let [rows [[1 1 1 2 1 3]
                [2 1 2 2 2 3]
                [3 1 3 2 3 3]
                [1 1 2 1 3 1]
                [2 1 2 2 2 3]
                [3 1 3 2 3 3]
                [1 1 2 2 3 3]
                [1 3 2 2 3 1]]]
      (let [isolate-row
            (fn isolate [rowdef board]
              (if (empty? rowdef)
                []
                (cons (nth (nth board (dec (first rowdef)))
                        (dec (second rowdef)))
                  (isolate (drop 2 rowdef) board))))]
        (let [check-row
              (fn [row] (reduce #(if (= %1 %2) %1 nil) row))]
          (let [checked-list (map check-row (map #(isolate-row % x) rows))]
            (cond
              (some #(= :x %) checked-list) :x
              (some #(= :o %) checked-list) :o
              :else nil)))))))

(defcheck solution-c817d308
  (fn tic-tac-toe [[r1 r2 r3 :as rows]]
    (let [cols  (apply map vector rows)
          diags [[(r1 0) (r2 1) (r3 2)]
                 [(r1 2) (r2 1) (r3 0)]]
          three-in-row (fn [flag colls] (some #(apply = flag %) colls))
          win (fn [flag] (some #(three-in-row flag %) [rows cols diags]))]
      (cond (win :x) :x
            (win :o) :o
            :else    nil))))

(defcheck solution-c8ca769b
  (fn who-won [board]
    (let [rows board
          cols (apply map vector board)
          sz (count board)
          diags (list (map #(get (get board %) %) (range sz)) (map #(get (get board %) (- (dec sz) %)) (range sz)))
          all-same (fn [colls]
                     (first
                       (filter
                         (fn [coll]
                           (and
                            (not= (first coll) :e)
                            (every? #(= (first coll) %) coll)
                            ))
                         colls)
                       )
                     )
          ]
      (first (concat (all-same board) (all-same cols) (all-same diags))
        )
      )))

(defcheck solution-c91810b7
  (fn check [board]
    (let [rows (for [row (range 3)]
                 (into [] (for [x (range 3)] [row x])))
          cols (for [col (range 3)]
                 (into [] (for [x (range 3)] [x col])))
          diags (vector (for [d (range 3)] [d d ] )
                  (for [d (range 3)] [(- 2 d) d ] ))
          complete-line? (fn [board coll coin]
                           (every? #(= coin (get (get board (first %)) (second %))) coll))
          wins? (fn [board coin]
                  (or 	(some #(complete-line? board % coin) cols)
                       (some #(complete-line? board % coin) rows)
                       (some #(complete-line? board % coin) diags))) ]
      (cond (wins? board :x) :x
            (wins? board :o) :o
            :else nil))))

(defcheck solution-c936e833
  (fn [x] (let [rs x,
                cs (apply (partial map list) rs),
                l (count rs),
                d1 (map #((rs %) %) (range l)),
                d2 (map #((rs %) (dec (- l %))) (range l)),
                all (conj (concat rs cs) d1 d2)]
            (first (some #{(repeat l :o) (repeat l :x)} all)))))

(defcheck solution-ca0d6d5f
  (fn tic-tac
    [board]
    (let[
         not-e #(not= (first %) :e)
         eq #(apply = %)
         horz (filter  not-e (filter eq board))
         vert (filter not-e (filter eq (apply mapv vector board)))
         cross1 (filter not-e (filter eq [(map #(nth (flatten board) %) [0 4 8])]))
         cross2 (filter not-e (filter eq [(map #(nth (flatten board) %) [2 4 6])]))
         ]
      (some identity (map (comp first first) [horz vert cross1 cross2]))
      )
    ))

(defcheck solution-ca14734d
  (fn [board]
    (let [xy #(nth (nth board %1) %2)
          trios (partition 3
                  (filter #(not (nil? %))
                    (for [d [:a :d :z1 :z2]
                          x (range 0 3)
                          y (range 0 3)]
                      (condp = d
                        :a (xy x y)
                        :d (xy y x)
                        :z1 (if (= x y) (xy x y))
                        :z2 (if (= x y) (xy (- 2 x) y))))))]
      (cond
        (some #(= [:x :x :x] %) trios) :x
        (some #(= [:o :o :o] %) trios) :o
        :default nil))))

(defcheck solution-ca2df035
  (fn [b]
    (let [v (ffirst (filter (partial apply =)
                      (map (fn [line] (mapv (partial get-in b) line))
                        (concat
                         (mapv (fn [r] (mapv (fn [c] [c r]) (range 3))) (range 3))
                         (mapv (fn [r] (mapv (fn [c] [r c]) (range 3))) (range 3))
                         [(mapv (fn [cr] [cr cr]) (range 3))]
                         [(mapv (fn [cr] [(- 2 cr) cr]) (range 3))]))))]
      (if (= v :e) nil v))))

(defcheck solution-cac000bb
  (fn [board]
    (let [indices (range (count (first board)))
          possible-lines (concat
                          (map identity board)
                          (apply (partial map vector) board)
                          (vector (map get board indices))
                          (vector (map get (reverse board) indices)))]
      (cond
        (some (fn [line] (every? #(= :x %) line)) possible-lines) :x
        (some (fn [line] (every? #(= :o %) line)) possible-lines) :o
        :else nil))))

(defcheck solution-caffabdd
  (fn [tct]
    (let [t (concat
             tct
             (map (fn [c] (map #(nth % c) tct)) (range 0 3))
             [(reduce #(conj %1 (nth %2 (count %1))) [] tct)]
             [(reduce #(conj %1 (nth %2 (dec (- (count %2) (count %1))))) [] tct)])
          g (fn [s] (some (fn [e] (every? #(= s %) e)) t))]
      (cond
        (g :x) :x
        (g :o) :o
        :default nil))))

(defcheck solution-cb0fe046
  #(let [x ((fn[[u v w]]
              (if (= (count (distinct u)) 1)
                (first u)
                (if (= (count (distinct v)) 1)
                  (first v)
                  (if (= (count (distinct w)) 1)
                    (first w)
                    (if (= (first u) (first v) (first w))
                      (first u)
                      (if (= (second u) (second v) (second w))
                        (second u)
                        (if (= (last u) (last v) (last w))
                          (last u)
                          (if (= (first u) (second v) (last w))
                            (first u)
                            (if (= (last u) (second v) (first w))
                              (last u)
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ) %)
         ]
     (if (= x :e) nil x)
     ))

(defcheck solution-cb2b37cc
  (fn [board]
    (let [board (mapv #(mapv {:x 1 :o -1 :e 0} %) board)
          win-x (some #{3 -3} (lazy-cat
                                (apply map + board)
                                (apply map + (apply map vector board))
                                [(apply + (map get board [0 1 2]))
                                 (apply + (map get board [2 1 0]))]))]
      (condp = win-x
        3 :x
        -3 :o
        nil))))

(defcheck solution-cb33f512
  (fn [mt]
    (letfn [(all [sym] (fn [s] (if (every? #(= % sym) s) sym)))
            (vert [m] (apply map vector m))
            (diag [m] (map
                        (fn [s] (map-indexed #(nth %2 %) s))
                        [m
                         (map reverse m)]))
            (f [st] #(some (all %) st))]
      (let [st (concat mt (vert mt) (diag mt))]
        (some (f st) [:x :o])))))

(defcheck solution-cbb3fd39
  (fn [board]
    (letfn [(transpose [vov]
              (apply map vector vov))
            (row-win [row]
              (when (and (apply = row)
                         (not (= (first row) :e)))
                (first row)))]
      (or (some row-win board)
          (some row-win (transpose board))
          (row-win (map nth board [0 1 2]))
          (row-win (map nth board [2 1 0]))))))

(defcheck solution-cbc93d8c
  (fn tic-tac-toe [rows]
    (let [[row1 row2 row3] rows
          col1 (map first rows)
          col2 (map second rows)
          col3 (map #(nth % 2) rows)
          dia1 [(first row1) (second row2) (nth row3 2)]
          dia2 [(nth row1 2) (second row2) (first row3)]
          lines [row1 row2 row3 col1 col2 col3 dia1 dia2]
          isx? #(= :x %) iso? #(= :o %)]
      (cond
        (some true? (map #(every? isx? %) lines)) :x
        (some true? (map #(every? iso? %) lines)) :o
        true nil))))

(defcheck solution-cc0c86ce
  (fn [t]
    (letfn
     [(win [tab pl]
        (let [wins
              [[[0 0] [0 1] [0 2]]
               [[1 0] [1 1] [1 2]]
               [[2 0] [2 1] [2 2]]
               [[0 0] [1 0] [2 0]]
               [[0 1] [1 1] [2 1]]
               [[0 2] [1 2] [2 2]]
               [[0 0] [1 1] [2 2]]
               [[0 2] [1 1] [2 0]]]]
          (some
            (fn [winpos]
              (every?
                #(= ((tab (first %)) (second %)) pl)
                winpos))
            wins)))]
      (cond (win t :x) :x (win t :o) :o :else nil))))

(defcheck solution-cc4735b4
  (fn ttt-winner [[[b1 b2 b3]
                   [b4 b5 b6]
                   [b7 b8 b9]]]
    (let [win? #(or (= % b1 b2 b3) (= % b4 b5 b6) (= % b7 b8 b9)
                    (= % b1 b4 b7) (= % b2 b5 b8) (= % b3 b6 b9)
                    (= % b1 b5 b9) (= % b3 b5 b7))]
      (cond
        (win? :x) :x
        (win? :o) :o
        :else nil))))

(defcheck solution-cc64a632
  (fn [[& rows :as board]]
    (let [cols (apply map vector board)
          diags (map
                  #(for [c %1] (get-in board c))
                  [[[0 0][1 1][2 2]]
                   [[0 2][1 1][2 0]]])
          entries (concat rows cols diags)
          valid-entry? #(= [% % %] %2)]
      (cond
        (some #(valid-entry? :x %1) entries) :x
        (some #(valid-entry? :o %1) entries) :o))))

(defcheck solution-cc6f3a61
  (fn [b]
    (let [pos [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [6 4 2]] board (flatten b)]
      ((fn [res] (cond (some #{:x} res) :x (some #{:o} res) :o :else nil))
       (map (fn [v] (reduce (fn [x y] (if (= x y) x false))
                      (map (fn [x] (nth (flatten board) x)) v)))
         pos))
      )))

(defcheck solution-cc7586c1
  #(let [lst (last (sort (flatten (map keys (filter (fn[a](not (empty? (flatten (filter (partial = 3) (vals a)))))) (map frequencies (concat % (apply map list %) [(for [i [2 4 6]] (nth (flatten %) i))] [(for [i [0 4 8]] (nth (flatten %) i))])))))))] (if (= lst :e) nil lst)))

(defcheck solution-cd46bec2
  (fn [board]
    (let [rows board
          cols (map (fn [i] (map #(get % i) board)) (range 3))
          d0 (for [i (range 3)] (get (get board i) i))
          d1 (for [i (range 3)] (get (get board i) (- 2 i)))
          win (fn [v] (and (= 1 (count (set v))) (first v)))]
      (first (filter #(and (not= :e %) %) (map win (concat rows cols [d0 d1])))))))

(defcheck solution-ce63a630
  (fn [ttt]
    (let [t (vec (reduce concat ttt))
          r [[0 1 2]
             [3 4 5]
             [6 7 8]
             [0 3 6]
             [1 4 7]
             [2 5 8]
             [0 4 8]
             [2 4 6]]
          players [:x :o]]
      (let [tr (map #(map t %) r)]
        (first (for [player players :when (some #(= (repeat 3 player) %) tr)]
                 player))))))

(defcheck solution-ce9ffc87
  (fn [acs]
    (let [dns (apply map vector acs)
          [[a _ d]
           [_ b _]
           [e _ c]] acs

          diags (list [a b c] [d b e])]
      (->> (concat acs dns diags)
        (map distinct)
        (filter #(and (= (count %) 1)(not= :e (first %))))
        ffirst))))

(defcheck solution-cefc9238
  (fn [board]
    (let [row-to-winner (fn [row] (reduce #(if (or (not= % %2) (= % :e)) nil %) row))]
      (let [check (fn [colls] (reduce  #(if (nil? %) %2 %) (map row-to-winner colls)))]
        (let [rotate (fn [colls] (apply map #(vector %1 %2 %3) (reverse colls)))]
          (let [diag (fn [coll] (vector (ffirst coll) (second (second coll)) (last (last coll))))]
            (or (first (remove nil?(vector (check board) (check (rotate board))  (row-to-winner(diag board))  (row-to-winner(diag (rotate board)))  ))) nil )))))))

(defcheck solution-cf279537
  ; &#20309;&#12363;&#21069;&#12459;&#12540;&#12489;&#12466;&#12540;&#12512;&#12391;&#12418;&#12381;&#12358;&#12384;&#12387;&#12383;&#12364;&#36229;&#12468;&#12522;&#25276;&#12375;
  (fn [v]
    (let [lines [ (v 0) (v 1) (v 2)
                 (mapv #(get-in v %) [[0 0][1 0][2 0]])
                 (mapv #(get-in v %) [[0 1][1 1][2 1]])
                 (mapv #(get-in v %) [[0 2][1 2][2 2]])
                 (mapv #(get-in v %) [[0 0][1 1][2 2]])
                 (mapv #(get-in v %) [[0 2][1 1][2 0]])]]
      (cond (some (fn [l] (every? #(= :x %) l)) lines) :x
            (some (fn [l] (every? #(= :o %) l)) lines) :o
            :else nil))))

(defcheck solution-cf3533aa
  (fn [xs]
    (letfn [(row [i] ( cond (= (xs i) [:x :x :x] ) :x
                            (= (xs i) [:o :o :o] ) :o
                            :else nil
                            ))
            (col [j] (let [colj [((xs 0) j) ((xs 1) j) ((xs 2) j)]]
                       (cond (= colj [:x :x :x]) :x
                             (= colj [:o :o :o]) :o
                             :else nil
                             )
                       ))
            (dia1 [xs] (let [d [((xs 0) 0) ((xs 1 )1) ((xs 2) 2)]]
                         (cond (= d [:x :x :x] ) :x
                               (= d [:o :o :o] ) :o
                               :else nil
                               )
                         ))
            (dia2 [xs] (let [d [((xs 0) 2) ((xs 1 ) 1) ((xs 2) 0)]]
                         (cond (= d [:x :x :x] ) :x
                               (= d [:o :o :o] ) :o
                               :else nil
                               )
                         ))
            ]
      (let [ergset (set (concat
                         (map row [0 1 2]) (map col [0 1 2]) (list (dia1 xs) (dia2 xs))
                         ))]
        (cond (ergset :x) :x
              (ergset :o) :o
              :else nil
              )
        )
      )
    ))

(defcheck solution-cf7a9e88
  (fn [[[a b c]
        [d e f]
        [g h i]]]
    (->> [[a b c]
          [d e f]
          [g h i]

          [a d g]
          [b e h]
          [c f i]

          [a e i]
          [c e g]]
      (filter #(apply = %))
      (filter #(not= :e (first %)))
      ffirst)))

(defcheck solution-cf9a8b5b
  (fn [[[a1 a2 a3][b1 b2 b3][c1 c2 c3]]]
    (let [winlines [
                    [a1 a2 a3] [b1 b2 b3] [c1 c2 c3]
                    [a1 b1 c1] [a2 b2 c2] [a3 b3 c3]
                    [a1 b2 c3] [a3 b2 c1]] ]
      (first(first(filter (fn [line] (and (#{:x :o} (first line)) (every? #(= (first line) %) line))) winlines))))
    ))

(defcheck solution-cfb2c662
  (fn tic-tac-toe [x]
    (letfn [(transpose [m]
              (apply mapv vector m))
            (get-winner-3 [row]
              (let [t (distinct row)]
                (if (= 1 (count t))
                  (first t)))
              )
            (get-winner-3-ignore [row]
              (let [w (get-winner-3 row)]
                (if (= :e w)
                  nil
                  w)))
            (get-diag [m]
              (for [i (range (count m))]
                ((m i) i)
                ))
            (get-diag2 [m]
              (for [i (range (count m))]
                ((m i) (- (count m) i 1))
                ))
            (get-all-subsets [g]
              (conj (into g
                      (transpose g))
                (get-diag g)
                (get-diag2 g)))
            ]
      (first (filter #(not (nil? %)) (map get-winner-3-ignore (get-all-subsets x)))))))

(defcheck solution-cfcc67be
  (fn tic-tac-toe [board]
    (let [same? (fn [sec] (if (apply = sec) (first sec) nil))
          rows (map same? board)
          cols (map same? (apply map vector board))
          diag1 (same? (map get board (range 3)))
          diag2 (same? (map get board (range 2 -1 -1)))]
      (some #{:x :o} (concat rows cols [diag1] [diag2])))))

(defcheck solution-d00ee15d
  (fn [board]
    (let [lanes (apply conj board
                  (map get board [0 1 2])
                  (map get board [2 1 0])
                  (apply map vector board))
          winners {[:x :x :x] :x
                   [:o :o :o] :o}]
      (some winners lanes))))

(defcheck solution-d08d0075
  (fn [b]
    (let [w '(t m b l c r d p)
          c {:tl [0 0] :tc [0 1] :tr [0 2]
             :ml [1 0] :mc [1 1] :mr [1 2]
             :bl [2 0] :bc [2 1] :br [2 2]
             :d1 [0 0] :d2 [1 1] :d3 [2 2]
             :p1 [0 2] :p2 [1 1] :p3 [2 0]}]
      (letfn [(get-coords [s]
                (map #(c (keyword %)) (filter #(re-seq (re-pattern (str s)) %) (map name (keys c)))))
              (get-row [board coords]
                (map #((board (first %)) (second %)) coords))
              (winner? [row]
                (reduce #(if (and (= % %2) (not (= % :e))) % nil) row))]
        (loop [rm w, r nil]
          (cond (not (nil? r)) r
                (empty? rm) nil
                :else (recur (rest rm) (winner? (get-row b (get-coords (first rm)))))))))))

(defcheck solution-d093b51f
  (fn a [b]
    (let [p (concat
             b
             (partition 3 (apply mapcat list b))
             [
              [ ((b 0) 0) ((b 1) 1) ((b 2) 2) ]
              [ ((b 0) 2) ((b 1) 1) ((b 2) 0) ]
              ])]
      (cond
        (some #{[:x :x :x]} p) :x
        (some #{[:o :o :o]} p) :o
        :else nil
        ))))

(defcheck solution-d09a95cb
  (fn ttt [b]
    (letfn [(cols [b] (apply map vector b))
            (diag1 [b] (for [x (range 3)] (nth (nth b x) x)))
            (diag2 [b] (for [x (range 3)] (nth (nth b x) (- 2 x))))
            (ws [b] (apply conj b (conj (cols b) (diag1 b) (diag2 b))))
            ]
      (let [s (set (for [x [:x :o] r (ws b)] (if (every? #(= x %) r) x 0)))]
        (or (s :x) (s :o))))))

(defcheck solution-d0b6f6f7
  (letfn
   [(diaganol [[[h & t] & t :as rows]]
      (when h (cons h (diaganol (map rest t)))))

    (vertical [xs] (apply map vector xs))

    (find-paths [board] (concat board
                                [(diaganol board)]
                                [(diaganol (reverse board))]
                                (vertical board)))]

    (fn anal-tic [board]
      (->> (find-paths board)
        (remove #(apply = :e %))
        (filter #(apply = %))
        (ffirst)))))

(defcheck solution-d0d29862
  (fn winttt [m]
    (let [winner (ffirst (filter (fn [v] (= (v 0) (v 1) (v 2)))
                           (let [all (conj (concat m (apply mapv vector m))
                                       [((m 0) 0) ((m 1) 1) ((m 2) 2)] [((m 2) 0) ((m 1) 1) ((m 0) 2)])]
                             all)))]
      (if (= :e winner) nil winner))))

(defcheck solution-d0d3828c
  (fn [s] (let [e (fn [[[a b c][e f g][h i j]] k]
                    (and (or (= a b c k) (= e f g k) (= h i j k)
                             (= a e h k) (= b f i k) (= c g j k)
                             (= a f j k) (= c f h k))
                         k))]
            (or (e s :x) (e s :o) nil))))

(defcheck solution-d116d0f7
  (fn find-winner [b]
    (let [horizontal
                        (fn [b]
                          (cond
                            (some #(= % [:x :x :x]) b) :x
                            (some #(= % [:o :o :o]) b) :o
                            :else nil))
          rotated-board (apply map vector b)
          diag-a (map #(nth %1 %2) b (range 0 3))
          diag-b (map #(nth %1 %2) b [2 1 0])
          ]
      (or
       (horizontal (concat b [diag-a diag-b]))
       (horizontal rotated-board)))))

(defcheck solution-d155dd25
  (fn attt [b]
    (let [win-pos [[[0 0] [0 1] [0 2]]  ; 1st row
                   [[1 0] [1 1] [1 2]]  ; 2nd row
                   [[2 0] [2 1] [2 2]]  ; 3rd row
                   [[0 0] [1 0] [2 0]]  ; 1st column
                   [[0 1] [1 1] [2 1]]  ; 2nd column
                   [[0 2] [1 2] [2 2]]  ; 3rd column
                   [[0 0] [1 1] [2 2]]  ; left to right diagonale
                   [[0 2] [1 1] [2 0]]] ; right to left diagonale
          get-value-at (fn [b p]
                         ((b (p 0)) (p 1)))
          get-values-at (fn [b ps]
                          (loop [ps1 ps, result []]
                            (if (empty? ps1)
                              result
                              (recur (rest ps1) (conj result (get-value-at b (first ps1))))
                              )))
          same? (fn [values v]
                  (= (count values) (count (filter #(= v %) values))))
          get-winner (fn [values]
                       (loop [values1 values]
                         (if (empty? values1)
                           nil
                           (let [vals (first values1)]
                             (cond
                               (same? vals :x) :x
                               (same? vals :o) :o
                               :else (recur (rest values1)))
                             ))))
          get-values-at-poss (fn [b poss]
                               (loop [result [], poss1 poss]
                                 (if (empty? poss1)
                                   result
                                   (let [f (first poss1)
                                         v (get-values-at b f)]
                                     (recur (conj result v) (rest poss1))
                                     ))))
          vals-at-win-poss (get-values-at-poss b win-pos)]
      (get-winner vals-at-win-poss)
      )))

(defcheck solution-d17bf6da
  (fn [board]
    (letfn [(check-horizonal [board]
              (first (flatten (filter #(every? #{:x :o } %) (filter #(= 1 (count (set %))) board)))))]
      (let [h (check-horizonal board)]
        (if
         h
          h
          (let [v (check-horizonal (apply (partial map #(vec [%1 %2 %3])) board))]
            (if
             v
              v
              (let [fd (check-horizonal [[(first (first board)) (second (second board)) (last (last board))]])]
                (if
                 fd
                  fd
                  (check-horizonal [[(last (first board)) (second (second board)) (first (last board))]]))))))))))

(defcheck solution-d1dc7a84
  (fn [[a b c]]
    (letfn
     [
      (w [t] (ffirst (filter #(apply not= (cons :e %)) (filter #(apply = %) t))))
      ]
      (w
        (concat
         [a b c] [[(nth a 0) (nth b 1) (nth c 2)]] [[(nth a 2) (nth b 1) (nth c 0)]]
         (reduce
           (fn [r x]
             (map #(apply cons %) (partition 2 (interleave x r)))
             )
           [[] [] []]
           [a b c]
           )
         )
        )
      )
    ))

(defcheck solution-d2a49ac6
  (fn tic [rows]
    (let [cols (apply map vector rows)
          c (second (second rows))
          d1 [(ffirst rows) c (last (last rows))]
          d2 [(last (first rows)) c (first (last rows))]
          lines (concat rows cols [d1 d2])]
      (letfn [(line [player s]
                (apply = (cons player s)))
              (won [player]
                (some identity (map (partial line player) lines)))]
        (cond (won :x) :x
              (won :o) :o
              :else nil)))))

(defcheck solution-d2cef7fe
  #(->> %
     (concat (map (fn [d] (map get % d)) [[0 1 2] [2 1 0] [0 0 0] [1 1 1] [2 2 2]]))
     (some {[:x :x :x] :x [:o :o :o] :o})))

(defcheck solution-d3520ffe
  (fn tic-tac-analyze [board]
    (let [row-indexes (map (partial repeat 3) (range 0 3))
          diag-indexes [(range 0 3)
                        (reverse (range 0 3))]
          transed-board (apply map vector board)
          check (fn [shape rows indexes]
                  (= #{shape}
                    (into #{} (map nth rows indexes))))
          winning? (fn [shape]
                     (or (some (partial check shape board) row-indexes)
                         (some (partial check shape transed-board) row-indexes)
                         (some (partial check shape transed-board) diag-indexes)))]
      (cond (winning? :x) :x
            (winning? :o) :o))))

(defcheck solution-d372f4d2
  (fn [board]
    (letfn [
            (rows [b] b)
            (cols [b] (partition 3 (apply interleave b)))
            (diags [b] (letfn [(m [row col] (nth (nth b row) col))]
                         (vector (vector  (m 0 0) (m 1 1) (m 2 2))
                           (vector (m 2 0) (m 1 1) (m 0 2)))))
            (allrows [b] (concat (rows b) (cols b) (diags b)))
            (winner [r] (reduce-kv (fn [i k v] (when (= 3 v) k)) nil (frequencies r)))]
      (->> board
        (allrows)
        (map winner)
        (filter #(contains? #{:x :o} %))
        first))))

(defcheck solution-d3f3237c
  (fn solve [rows]
    (let [cols  (apply map vector rows)
          diag1 (map #(get-in rows [% %]) (range 3))
          diag2 (map #(get-in rows [% (- 2 %)]) (range 3))
          moves (concat rows cols [diag1 diag2])
          win   (some #(when (= 1 (count (set %))) %) moves)]
      (#{:x :o} (first win)))))

(defcheck solution-d3fbb133
  (letfn [(r [i] #(nth % i))
          (c [i] (fn [m] (map #(nth % i) m)))
          (d [& k] (fn [m] (map #(get-in m %) k)))
          (q [b e]
            (some #(every? #{e} %)
              (map #(% b)
                [(r 0) (r 1) (r 2) (c 0) (c 1) (c 2) (d [0 0] [1 1] [2 2]) (d [0 2] [1 1] [2 0])])))]
    (fn [b]
      (cond
        (q b :x) :x
        (q b :o) :o
        1 nil
        ))))

(defcheck solution-d46e79d
  (fn [grid]
    (letfn [(cols [grid]
              [(map #(nth % 0) grid)
               (map #(nth % 1) grid)
               (map #(nth % 2) grid)])
            (diag [grid]
              [[
                (nth (nth grid 0) 0)
                (nth (nth grid 1) 1)
                (nth (nth grid 2) 2)]
               [
                (nth (nth grid 2) 0)
                (nth (nth grid 1) 1)
                (nth (nth grid 0) 2)]])
            (won-row [who, row]
              (if (every? (partial = who) row)
                who
                nil))
            (won [rows]
              (some #(or (won-row :x %) (won-row :o %)) rows))]
      (or
       (won grid)
       (won (cols grid))
       (won (diag grid))))))

(defcheck solution-d4d75d78
  (fn [board]
    (let [flat-board (flatten board)
          wins       [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]
          check-win  (fn [win player] (if (apply = player (map (partial nth flat-board) win)) player nil))]
      (loop [wins-left wins]
        (if-let [[w & ws] (seq wins-left)]
          (cond
            (check-win w :x) :x
            (check-win w :o) :o
            :else            (recur ws)))))))

(defcheck solution-d4fa5adf
  (let [board_size 3
        board-transpose (partial apply map list)]
    (letfn [(win-horizontal? [player board]
              (some (partial every? (partial = player))
                board))

            (win-vertical? [player board]
              (win-horizontal? player (board-transpose board)))

            (win-diagonal?
              [player board]
              (win-horizontal? player
                (board-transpose
                  (for [n (range board_size)]
                    [(nth (nth board n)
                       n)
                     (nth (nth board n)
                       (- board_size n 1))]))))

            (win-board?
              ([board]
               (cond
                 (win-board? :x board) :x
                 (win-board? :o board) :o
                 :else nil))
              ([player board]
               (some #(% player board)
                 [win-horizontal? win-vertical? win-diagonal?])))]
      #(win-board? %))))

(defcheck solution-d5d50f8d
  (fn who-won? [board]
    (let [ps [(range 0 3) (range 3 6) (range 6 9)
              (range 0 9 3) (range 1 9 3) (range 2 9 3)
              (range 0 9 4) (range 2 8 2)]
          b (flatten board)]
      (letfn [(get-at [b ps] (map #(nth b %) ps))]
        (let [c (map #(get-at b %) ps)
              xs (repeat 3 :x)
              os (repeat 3 :o)]
          (cond
            (some (partial = xs) c) :x
            (some (partial = os) c) :o
            :default nil))))))

(defcheck solution-d6277c2a
  (fn [rows]
    (let [cols (apply map vector rows)
          diags [[(nth (nth rows 0) 0) (nth (nth rows 1) 1) (nth (nth rows 2) 2)]
                 [(nth (nth rows 0) 2) (nth (nth rows 1) 1) (nth (nth rows 2) 0)]]
          lines (concat rows cols diags)
          winner (fn [[a b c]]
                   (cond
                     (= :x a b c) :x
                     (= :o a b c) :o))]
      (reduce #(if (nil? %) (winner %2) %) nil lines))))

(defcheck solution-d62b8b79
  (fn [board]
    (let [all-lines
          (concat board
                  (apply mapv vector board)
                  [(take-nth 4 (flatten board))]
                  [(take 3 (take-nth 2 (drop 2 (flatten board))))])]
      #_(println all-lines)
      (or (ffirst (filter (partial apply = :x) all-lines))
          (ffirst (filter (partial apply = :o) all-lines))
          nil))))

(defcheck solution-d6336940
  (fn [b]
    (let [w (fn [r] (some (fn[x]
                            (when
                             (every?
                               #(and
                                 (= (get-in b %) (get-in b (nth x 0)))
                                 (not (= (get-in b %) :e)))
                               x) (get-in b (nth x 0)))) r))
          a (vec (for [i [0 1 2] j [0 1 2]] [i j]))
          h (partition 3 a)
          v [(take-nth 3 a) (take-nth 3 (rest a)) (take-nth 3 (drop 2 a))]
          d [[(a 0) (a 4) (a 8)] [(a 2) (a 4) (a 6)]]]
      (or (w h) (w v) (w d) nil))))

(defcheck solution-d66e81d2
  (fn [ttt]
    (ffirst
      (filter
        #(and
          (not (:e %))
          (= 1 (count %)))
        (map set
          (concat
           ttt
           (apply map list ttt)
           [(map-indexed #(nth %2 %) ttt)
            (map-indexed #(nth %2 (- 2 %)) ttt)]))))))

(defcheck solution-d68811b3
  (fn analyze-tictak [gamefld]
    (when (and (= 3 (count gamefld))
               (every? #(and (vector? %) (= 3 (count %))) gamefld)
               (every? (fn [row] (every? #{:e :x :o} row)) gamefld))
      (let [height (count gamefld)
            width (count gamefld)
            fldidx (for [i (range 0 height) j (range 0 width)] [i j])]
        (letfn [
                (flatten-seqs [colls] (reduce (fn [acc b] (into acc b)) [] colls))
                (in-fld? [[i j]]
                  (and (>= i 0) (>= j 0) (< i height) (< j width)))
                (move-left [[i j]] [i (dec j)])
                (move-right [[i j]] [i (inc j)])
                (move-up [[i j]] [(dec i) j])
                (move-down [[i j]] [(inc i) j])
                (move-upleft [[i j]] [(dec i) (dec j)])
                (move-upright [[i j]] [(dec i) (inc j)])
                (move-downleft [[i j]] [(inc i) (dec j)])
                (move-downright [[i j]] [(inc i) (inc j)])
                (get-pt-arrow [point movefn]
                  (letfn [(loc-grow-arrow [pt]
                            (when (in-fld? pt)
                              (lazy-seq (cons pt (loc-grow-arrow (movefn pt))))))]
                    (vec (loc-grow-arrow point))))
                (stroke-right [pt] (get-pt-arrow pt move-right))
                (stroke-down [pt] (get-pt-arrow pt move-down))
                (get-item [[i j]] (get-in gamefld [i j]))]
          (let [fst-row [[0 0] [0 1] [0 2]]
                fst-col [[0 0] [1 0] [2 0]]
                d-strokes (-> [] (conj ,,, (get-pt-arrow [0 0] move-downright))
                            (conj ,,, (get-pt-arrow [0 2] move-downleft)))
                h-strokes (vec (map stroke-right fst-col))
                v-strokes (vec (map stroke-down fst-row))
                all-strokes (vec (concat d-strokes h-strokes v-strokes))
                all-results (vec (map (fn [stk] (vec (map get-item stk)))
                                   all-strokes))
                wins-set (reduce (fn [acc bv] (if (every? #(= % (first bv)) bv)
                                                (conj acc (first bv))
                                                acc))
                           #{} all-results)
                ]
            (cond
              (:x wins-set) :x
              (:o wins-set) :o
              :otherwise    nil) ))))))

(defcheck solution-d6d2274c
  (fn [b]
    (letfn
     [
      (pos [k] (reduce #(+ %1 %1 (if (= %2 k) 1 0)) 0 (flatten b)))
      (win [k] (some #(= (bit-and (pos k) %1) %1) '(0700 0070 0007 0444 0222 0111 0421 0124)))
      ]
      (if (win :x) :x (if (win :o) :o nil))
      )
    ))

(defcheck solution-d707ff63
  (fn q73 [board]
    (letfn [
            (check-line [code line] (every? #(= code %) line))
            (check-hori [code board] (some identity (map #(check-line code %) board)))
            (transpose [board] (apply map vector board))
            (diagonal [board] [[(ffirst board) (second (second board)) (last (last board))]])
            (check [code board]
              (or
               (check-hori code board)
               (check-hori code (transpose board))
               (check-hori code (diagonal board))
               (check-hori code (diagonal (map #(apply vector (reverse %)) board)))))]
      (cond (check :o board) :o
            (check :x board) :x
            :else nil))))

(defcheck solution-d734771
  #(let [c (vec (flatten %))
         l [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [6 4 2]]
         j (map (fn [x] (map (fn [y] (get c y)) x)) l)]
     (cond (some #{[:x :x :x]} j) :x
           (some #{[:o :o :o]} j) :o
           :else nil)))

(defcheck solution-d74c1f81
  (fn mycheck [b]
    (let [p (map (partial partition 2) [[0 0 0 1 0 2] [1 0 1 1 1 2] [2 0 2 1 2 2]
                                        [0 0 1 0 2 0] [0 1 1 1 2 1] [0 2 1 2 2 2]
                                        [0 0 1 1 2 2] [0 2 1 1 2 0] ])
          checkpos (map (fn [p2] (map #(get (get b (first %)) (second %)) p2)) p)
          allx (some (fn [coll] (apply (partial = :x) coll)) checkpos)
          allo (some (fn [coll] (apply (partial = :o) coll)) checkpos)
          ]
      (if allx :x (if allo :o nil)))))

(defcheck solution-d7910431
  (fn [[a b c]]
    (first
      (keep #(if (not= :e %) %)
        (map (partial reduce #(if (= % %2) %))
          (into
            [a b c
             [(a 0) (b 1) (c 2)]
             [(a 2) (b 1) (c 0)]]
            (partition 3 (interleave a b c))))))))

(defcheck solution-d7f76fe1
  (fn [x]
    (let [result (set
                   (concat
                    (map set x)
                    (->> x (apply map vector) (map set))
                    (list
                      (->> x ((juxt ffirst #(second (second %)) #(last (last %)))) set)
                      (->> x ((juxt #(last (first %)) #(second (second %)) #(first (last %)))) set))))]
      (first
        (or (result #{:o})
            (result #{:x})))
      )))

(defcheck solution-d8a2f4d9
  (fn [m] (let [
                cl (fn [n] (map #(% n) m))
                d  (fn [x]
                     (loop [[f & r :as x] x, n 0, a []]
                       (if (seq x) (recur r (inc n) (conj a (f n))) a )))
                dd (fn [x]
                     (loop [[f & r :as x] x, n 2, a []]
                       (if (seq x) (recur r (dec n) (conj a (f n))) a )))
                x (concat m [(cl 0)] [(cl 1)] [(cl 2)] [(d m)] [(dd m)])
                win? (fn [p] (some identity (map (fn [c] (every? #(= p %) c)) x)))
                ] (if (win? :x) :x (if (win? :o) :o nil)))))

(defcheck solution-d8a62788
  (fn [bd]
    (let [ldiag [[0 0] [1 1] [2 2]]
          rdiag [[0 2] [1 1] [2 0]]
          rows (map #(vector [% 0] [% 1] [% 2]) (range 3))
          cols (map #(vector [0 %] [1 %] [2 %]) (range 3))]
      (first (filter (comp not nil?)
               (for [line (cons ldiag (cons rdiag (concat rows cols)))]
                 (let [res (distinct (map #(nth (nth bd (second %)) (first %)) line))]
                   (if (and (= (count res) 1) (not (= :e (first res))))
                     (first res)
                     nil))))))))

(defcheck solution-d8f87d5f
  (fn [v]
    (let [verts (partition 3 (apply interleave v))
          diags [ [((v 0) 0) ((v 1) 1) ((v 2) 2)]
                 [((v 0) 2) ((v 1) 1) ((v 2) 0)]]
          complete (concat v verts diags)]
      (if (some #(= % [:x :x :x]) complete)
        :x
        (if (some #(= % [:o :o :o]) complete)
          :o
          nil)))))

(defcheck solution-d963a4df
  (fn [b]
    (letfn [(check [cs]
              (let [ps (map (partial get-in b) cs)]
                (when (and (apply = ps)
                           (not= (first ps) :e))
                  (first ps))))]
      (some check [[[0 0] [0 1] [0 2]]
                   [[1 0] [1 1] [1 2]]
                   [[2 0] [2 1] [2 2]]
                   [[0 0] [1 0] [2 0]]
                   [[0 1] [1 1] [2 1]]
                   [[0 2] [1 2] [2 2]]
                   [[0 0] [1 1] [2 2]]
                   [[0 2] [1 1] [2 0]]]))))

(defcheck solution-d9a63699
  (fn [coll]
    (letfn [(all-the-same [c]
              (reduce
                #(if (= %1 %2) %1 nil)
                c))]
      (let [same-row (first (filter
                              #(not (nil? %))
                              (map all-the-same coll)))
            same-column (first (filter
                                 #(not (nil? %))
                                 (map all-the-same
                                   (apply map vector coll))))
            diagonal-1 (all-the-same (reduce
                                       #(conj %1 (nth %2 (count %1)))
                                       []
                                       coll))
            diagonal-2 (all-the-same (reduce
                                       #(conj %1 (nth %2 (- (dec (count coll))
                                                            (count %1))))
                                       []
                                       coll))]
        (if (= same-row same-column diagonal-1 diagonal-2)
          nil
          (or same-row same-column diagonal-1 diagonal-2))))))

(defcheck solution-d9ec8d10
  (fn [board]
    (let [b (map-indexed (fn [i x] {x (bit-shift-left 1 i)}) (flatten board))]
      (some identity
        (for [p [:x :o]]
          (some identity
            (let [s (apply + (map #(or (p %) 0) b))]
              (for [mask [7 56 448 73 146 292 273 84]]
                (when (= mask (bit-and mask s)) p)))))))))

(defcheck solution-da0c2896
  (fn [board]
    (let [rows `[~@board
                 ~@(apply mapv vector board)
                 ~[(get-in board [0 0])
                   (get-in board [1 1])
                   (get-in board [2 2])]
                 ~[(get-in board [0 2])
                   (get-in board [1 1])
                   (get-in board [2 0])]]]
      (->> rows
        (filter (fn [row] (every? #(not= :e %) row)))
        (filter #(apply = %))
        first
        first))))

(defcheck solution-da52c082
  (fn [board]
    (first
      (some #{[:x :x :x] [:o :o :o]}
        (concat board
                (apply map vector board)
                (map
                  #(for [i (range 3)] (nth (nth % i) i))
                  [board (reverse board)]))))))

(defcheck solution-da6febc4
  (fn [rows]
    (let [columns (apply map vector rows)
          diagonals [[(get-in rows [0 0])
                      (get-in rows [1 1])
                      (get-in rows [2 2])]
                     [(get-in rows [0 2])
                      (get-in rows [1 1])
                      (get-in rows [2 0])]]]
      (some #(if (and
                  (not= :e (first %))
                  (apply = %))
               (first %))
        (concat rows columns diagonals)))))

(defcheck solution-da7bcd
  #(reduce % nil (concat %4 (%2 %4) (%2 (map %3 %4 [0 1 2] [2 1 0])))) #(or % (if (and (not= (last %2) :e) (apply = %2)) (last %2))) #(apply map list %) #(list (% %2) (% %3)))

(defcheck solution-da9c9135
  (fn [b]
    (letfn [(win [k]
              (some #(every? #{k} %)
                (concat b                                                    ; original board
                        (apply map vector b)                                       ; transposed
                        (vector (map #(nth %1 %2) b (range 3)))                    ; 1st diagonal
                        (vector (map #(nth %1 %2) b (reverse (range 3)))))))]      ; 2nd diagonal
      (cond
        (win :x) :x
        (win :o) :o))))

(defcheck solution-daa3cc4e
  (fn [x] (first (filter identity (map (fn [a] (if (or (apply = (conj a :x)) (apply = (conj a :o))) (first a) nil))
                                    (concat x
                                            (for [i [0 1 2]] (map #(nth % i) x))
                                            [(list (ffirst x) (second (second x)) (last (last x)))
                                             (list (first (last x)) (second (second x)) (last (first x)))])) ))))

(defcheck solution-dad564d
  (fn [[[a b c][d e f][g h i]]]
    (let [candidates [[a b c] [d e f] [g h i] [a d g] [b e h] [c f i] [a e i] [c e g]]
          lines (filter #(= 1 (count %)) (map set candidates))
          count_x (count (filter #(= #{:x} %) lines))
          count_o (count (filter #(= #{:o} %) lines))]
      (if (= count_x count_o)
        nil
        (if (< count_x count_o)
          :o
          :x)))))

(defcheck solution-db0067ed
  (fn ttt [board]
    (let [rows board
          columns (apply map list board)
          diagonals [[(first (first board)) (second (second board)) (nth (nth board 2) 2)]
                     [(first (nth board 2)) (second (second board)) (nth (first board) 2)]]
          lines (concat rows columns diagonals)
          matches (filter #(apply = %) lines)
          winner (first (first (filter #(not-any? #{:e} %) matches)))]
      winner)))

(defcheck solution-db3ff56b
  (fn [b]
    (first
      (filter #(or (= % :x) (= % :o))
        (map first
          (filter (partial apply =)
            (concat b
                    (for [x (range 3)] (for [y (range 3)] (get-in b [y x])))
                    (vector (for [x (range 3)] (get-in b [x x]))
                      (for [x (range 3)] (get-in b [x (Math/abs (- x 2))]))))))))))

(defcheck solution-dbae3b4d
  (fn [-seq]
    (let [r-fn #(partial map %)
          d-fn #(let [[[a _ _] [_ b _] [_ _ c]] %]
                  [a b c])
          data (into (into -seq [(d-fn -seq) (d-fn (reverse -seq))]) (map (partial into []) ((juxt (r-fn first) (r-fn second) (r-fn last)) -seq)))]
      (some
        #(if (= 3 (val (last (sort-by val
                               (frequencies (filter (partial not= :e) %)))))) (first %))  (remove (partial = [:e :e :e]) data))

      )
    ))

(defcheck solution-dc0d783d
  (fn [board]
    (let [vs board
          hs (map (fn [n]
                    (map #(% n) board)) [0 1 2])
          d1 (map #(get-in board [% %]) [0 1 2])
          d2 (map #(get-in board [%1 %2]) [0 1 2] [2 1 0])
          rows (concat vs hs [d1 d2])]
      (first (first (filter #(or (every? (partial = :x) %)
                                 (every? (partial = :o) %)) rows))))))

(defcheck solution-dc3a6a25
  (fn [[[a b c] [d e f] [g h i]]]
    (#(or (% a b c) (% d e f) (% g h i)
          (% a d g) (% b e h) (% c f i)
          (% a e i) (% g e c))
     #(when (= %1 %2 %3) (if (= %1 :e) nil %1)))))

(defcheck solution-dc695d68
  (fn ticktac [col] (letfn
                     [
                      (checkrow
                        [row]
                        (cond
                          (= row (repeat 3 :x)) :x
                          (= row (repeat 3 :o)) :o
                          :else nil))]
                      (or
                       (reduce #(or %1 (checkrow %2)) nil col)
                       (reduce #(or %1 (checkrow %2)) nil (apply map list col))
                       (checkrow (map #(nth %1 %2) col (range 3)))
                       (checkrow (map #(nth %1 %2) col (range 2 -1 -1)))))))

(defcheck solution-dca04d04
  (fn [board]
    (some
      #(and (apply = %) (#{:o :x} (first %)))
      (conj
        (concat board (apply map list board))
        (map-indexed #(nth %2 %1) board)
        (map-indexed #(nth %2 (- 2 %1)) board)))))

(defcheck solution-dcb41008
  (fn p73 [tab]
    (let [fs [(fn [x y] (= x 1)) (fn [x y] (= x 2)) (fn [x y] (= x 3))
              (fn [x y] (= y 1)) (fn [x y] (= y 2)) (fn [x y] (= y 3))
              (fn [x y] (or (and (= x 1) (= y 1)) (and (= x 2) (= y 2)) (and (= x 3) (= y 3))))
              (fn [x y] (or (and (= x 1) (= y 3)) (and (= x 2) (= y 2)) (and (= x 3) (= y 1))))]]
      (letfn [(idxc [tab]
                (reduce merge {} (for [row (zipmap (range 1 (inc 3)) tab)]
                                   (let [[ri r] row]
                                     (reduce conj {} (for [col (zipmap (range 1 (inc 3)) r)] (let [[ci c] col] [[ri ci] c])))))))
              (soroi [ps k]
                [(and (= 3 (count ps)) (every? #(= % k) (vals ps))) ps])
              (three [ided k f]
                (soroi (filter (fn [e] (let [[x y] (key e)] (f x y))) ided) k))
              (placed [ided k]
                (map (fn [f] (three ided k f)) fs))
              (chk [ided k]
                (some (comp not false?) (map first (placed ided k))))
              ]
        (let [ided (idxc tab)]
          (if (chk ided :x) :x
                            (if (chk ided :o) :o)))))
    ))

(defcheck solution-dcbbd965
  (fn analyze-tic-tac-toe-board
    [xs]
    {:pre [(every? true? (map (partial every? keyword?) xs))]}
    (letfn [(col [n] (map #(nth % n) xs))
            (win? [y ys] (some true?
                           (map (partial every? #{y}) ys)))]
      (let [rows xs
            cols (map col (range 0 3))
            diagonal [0 1 2]
            diagonals (vector
                        (map nth xs diagonal)
                        (map nth (reverse xs) diagonal))]
        (cond
          (win? :x (concat rows cols diagonals)) :x
          (win? :o (concat rows cols diagonals)) :o
          :else nil)))))

(defcheck solution-dcddbe1f
  (fn [rows]
    (let [columns (apply map vector rows)
          diags [(map #(% %2) [first second last] rows)
                 (map #(% %2) [last second first] rows)]
          lines (concat rows columns diags)]
      (ffirst (filter
                #(and (= 1 (count %)) (not= (first %) :e))
                (map distinct lines))))))

(defcheck solution-dd0e9e5f
  (let [wins (fn [s t]
               (cond
                 (some identity (for [x (range 3)] (every? #(= s (nth % x)) t))) s

                 (some identity (for [x (range 3)] (every? #(= s %) (nth t x)))) s

                 (every? #(= s %) (for [x (range 3)] (nth (nth t x) x))) s
                 (every? #(= s %) (for [x (range 3)] (nth (nth t x) (- 2 x)))) s

                 ))]

    #(cond
       (wins :x %) :x
       (wins :o %) :o
       )))

(defcheck solution-dd201c9e
  (fn [m]
    (let [cols  (fn [m] (into [] (for [y (range 3)] (into [] (for [x (range 3)] (get (get m x) y)))))),
          rows  (fn [m] (into [] (for [y (range 3)] (into [] (get m y))))),
          diags (fn [m] (vector [((m 0) 0) ((m 1) 1) ((m 2) 2)], [((m 0) 2) ((m 1) 1) ((m 2) 0)])),
          seq-contains? (fn [sequence item]
                          (if (empty? sequence)
                            false
                            (reduce #(or %1 %2) (map #(= %1 item) sequence))))
          v (concat (cols m) (rows m) (diags m))]
      (cond
        (seq-contains? v [:x :x :x]) :x
        (seq-contains? v [:o :o :o]) :o
        :else nil))))

(defcheck solution-dd701360
  (fn tic-tac-toe-board [board]
    (letfn [(get-indexes [ele v]
              (keep identity
                (for [i (range 0 (count v))
                      j (range 0 (count (first v)))]
                  (when (= ele (get-in v [i j]))
                    [i j]))))
            (win? [indexes]
              (and (= 3 (count indexes))
                   (= (map (partial * 2) (second indexes))
                     (map + (first indexes)
                       (last  indexes)))))]
      (cond
        (win? (get-indexes :x board)) :x
        (win? (get-indexes :o board)) :o
        :else nil))))

(defcheck solution-ddc43db6
  (fn [board]
    (letfn [(diag1 [b]
              [((b 0) 2) ((b 1) 1) ((b 2) 0)])
            (diag2 [b]
              [((b 0) 0) ((b 1) 1) ((b 2) 2)])]
      (first (first (filter
                      #(and (not (= :e (first %))) (apply = %))
                      (concat [ (diag1 board) (diag2 board) (map last board) (map second board) (map first board)] board )))))))

(defcheck solution-dde57cce
  #(some {[:o :o :o] :o [:x :x :x] :x}
     (partition 3
       (map
         (vec (flatten %))
         [0 1 2 3 4 5 6 7 8
          0 3 6 1 4 7 2 5 8
          0 4 8 2 4 6]))))

(defcheck solution-de21c96b
  (fn boardWinner [board]
    (letfn [(transpose [m] (apply mapv vector m) )
            (diagonal [[r1 r2 r3]] [(first r1) (second r2) (last r3)])
            (diagonals [m] [(diagonal m) (diagonal (map reverse m))])
            (rowWinner [row] (case row
                               [:x :x :x] [:x]
                               [:o :o :o] [:o]
                               nil))]
      (let [rows board
            columns (transpose board)
            diags (diagonals board)
            potentialWins (concat rows columns diags)]
        (first (apply concat (map rowWinner potentialWins)))
        )
      )
    ))

(defcheck solution-de3c5650
  (fn [board]
    (->> (for [r (range 3)]
           [(set (for [c (range 3)] (get-in board [r c]) )) ;; rows
            (set (for [c (range 3)] (get-in board [c r]) )) ;; colums
            ]
           )
      (cons (set (for [r (range 3)] (get-in board [r r]))))
      (cons (set (for [r (range 3)] (get-in board [r (- 2 r)]))))
      flatten
      (filter #(= (count %) 1))
      (filter #(not (= #{:e} %)))
      first
      first)
    ))

(defcheck solution-de5bf400
  (fn [b]
    (letfn [(h [n] (nth b n))
            (v [n] (map #(nth % n) b))
            (d [n] (cond (= n 0) [(nth (nth b 0) 0) (nth (nth b 1) 1) (nth (nth b 2) 2)]
                         (= n 1) [(nth (nth b 0) 2) (nth (nth b 1) 1) (nth (nth b 2) 0)]))
            (fill? [ox line] (every? #(= ox %) line))
            (fill-o? [line] (fill? :o line))
            (fill-x? [line] (fill? :x line))]
      (let [lines [(h 0) (h 1) (h 2) (v 0) (v 1) (v 2) (d 0) (d 1)]]
        (cond (some fill-o? lines) :o
              (some fill-x? lines) :x
              :else nil)))))

(defcheck solution-df8e0732
  (fn[a](
         (fn[m]
           (cond
             (nil? m) nil
             (= m '(:e)) nil
             :else (first m))
           )

         (first
           (filter (fn[i](= (count i) 1))
             (map distinct
               (map
                 (fn[t]( map (fn[c](nth (nth a (last c)) (first c))) t ))
                 [
                  [[0 0][1 0][2 0]]
                  [[0 0][1 1][2 2]]
                  [[0 0][0 1][0 2]]
                  [[1 0][1 1][1 2]]
                  [[2 0][1 1][0 2]]
                  [[2 0][2 1][2 2]]
                  [[0 1][1 1][2 1]]
                  [[0 2][1 2][2 2]]
                  ]
                 )
               )
             )
           )
         )))

(defcheck solution-df9e4354
  ;not elegant, but working.
  (fn [board]
    (let [group-indexes [[0 1 2] [3 4 5] [6 7 8]
                         [0 3 6] [1 4 7] [2 5 8]
                         [0 4 8] [2 4 6]]
          groups (map (fn [indexes]
                        (map (fn [index] ((vec (flatten board)) index)) indexes))
                   group-indexes)]
      (cond
        (some #(= [:x :x :x] %) groups) :x
        (some #(= [:o :o :o] %) groups) :o
        :else nil))))

(defcheck solution-e021595f
  (fn [game]
    (let [chx [[0 3 1 0 0 1]
               [0 3 0 1 1 0]
               [0 1 9 9 1 1]
               [2 1 9 9 -1 1]]
          game-rc (fn [g row col]
                    (get (get g row) col))
          bingo (atom nil)]
      ;; each chx is a pattern to check one or three times (one for diagonals)
      ;; each check consists of testing three cells
      (doseq [[start-row check-ct chk-row-inc chk-col-inc cell-row-inc cell-col-inc] chx]
        (when (nil? @bingo)
          (dotimes [chk check-ct]
            (let [played (atom nil)]
              (dotimes [cell 3]
                (when-not (= @played :nope) ;; already not a match
                  (let [p (game-rc game
                            (+ start-row (* chk chk-row-inc) (* cell cell-row-inc))
                            (+ (* chk chk-col-inc)(* cell cell-col-inc)))]
                    (cond
                      (= p :e) (reset! played :nope)
                      (nil? @played) (reset! played p)
                      (not= p @played) (reset! played :nope)))))
              (when-let [win (some #{@played} [:x :o])]
                #_(println :bingo win :chk chk)
                (reset! bingo win))))))
      @bingo)))

(defcheck solution-e03c891e
  (fn [M b]
    (let [c (partition 3 (apply interleave b))
          d-f #(M nth % (range))
          s (concat b c [(d-f b) (d-f (M reverse b))])]
      (ffirst (filter #{#{:x} #{:o}} (M set s))))) map)

(defcheck solution-e0748752
  (fn [a] (let [a2 [(first a) (second a) (last a)
                    [(first (first a)) (first (second a)) (first (last a))]
                    [(second (first a)) (second (second a)) (second (last a))]
                    [(last (first a)) (last (second a)) (last (last a))]
                    [(first (first a)) (second (second a)) (last (last a))]
                    [(last (first a)) (second (second a)) (first (last a))]]
                r  (concat
                    (filter (fn [row] (every? #(= :x %) row)) a2)
                    (filter (fn [row] (every? #(= :o %) row)) a2))]
            (first (first r)))))

(defcheck solution-e0ce752f
  (fn tic-tac-toe [xs]
    (let[wins? (fn [[x y z]] (cond (= :o x y z) :o (= :x x y z) :x :else nil))
         tests (concat xs (apply map list xs) (let [[[a b c] [d e f] [g h i]] xs] [[a e i] [c e g]]))]
      (some identity (map wins? tests)))))

(defcheck solution-e0d25eca
  #(letfn [(getRow [b r] (get b r))
           (getCol [b c] (for[r b] (get r c)))
           (getDia [b f s] (loop [rb b i s r []]
                             (if (seq rb) (recur (rest rb) (f i) (conj r (get (first rb) i)))
                                          r)))
           (win [v] (let [fv (first v)]
                      (if (and (apply = v) (not= fv :e)) fv false)))
           (testRowCol [b f]
             (reduce (fn [x y] (if (not= y false) y x)) false (map win (map (partial f b) (range 0 3)))))]
     (let [rowResult (testRowCol % getRow)]
       (if (not (false? rowResult)) rowResult
                                    (let [colResult (testRowCol % getCol)]
                                      (if (not (false? colResult)) colResult
                                                                   (let [dia1Result (win (getDia % inc 0))]
                                                                     (if (not (false? dia1Result)) dia1Result
                                                                                                   (let [dia2Result (win (getDia % dec 2))]
                                                                                                     (if (not (false? dia2Result)) dia2Result
                                                                                                                                   nil))))))))))

(defcheck solution-e12e4ae1
  (fn tic-tac [board]
    (letfn [(line? [key line]
              (= 3 (count (filter #(= key %) (map #((board (first %)) (second %)) line)))))
            (who-win? [line]
              (cond
                (line? :x line) :x
                (line? :o line) :o
                :else nil))]
      (let [lines [[[0 0] [0 1] [0 2]]
                   [[1 0] [1 1] [1 2]]
                   [[2 0] [2 1] [2 2]]
                   [[0 0] [1 0] [2 0]]
                   [[0 1] [1 1] [1 2]]
                   [[0 2] [1 2] [2 2]]
                   [[0 0] [1 1] [2 2]]
                   [[2 0] [1 1] [0 2]]]]
        (first (filter #(not (nil? %)) (map who-win? lines)))))))

(defcheck solution-e1303e89
  (fn tictac [g]
    (letfn [(won? [player]
              (reduce #(or (every? #{player} %2) %) false
                (concat g
                        [[((get g 0) 0) ((get g 1) 1) ((get g 2) 2)]]
                        [[((get g 0) 2) ((get g 1) 1) ((get g 2) 0)]]
                        (for [i (range 3)]
                          (vec (map #(get % i) g))))))]
      (cond
        (won? :x) :x
        (won? :o) :o))))

(defcheck solution-e1338308
  (fn [board]
    (let [rows board ;get rows
          columns (apply map vector board) ;get columns
          diagonals [(map get board [0 1 2]) (map get board [2 1 0])]] ;get diagonals
      (loop [remaining (concat rows columns diagonals)] ;all potential triplets
        (if-let [item (set (first remaining))]
          (if (and (not-any? #(= :e %) item) (or (every? #(= :x %) item) (every? #(= :o %) item)));find the winner
            (first item)
            (recur (rest remaining)))
          nil)))))

(defcheck solution-e14fa80f
  (fn[b]
    (let [checks [[0 0 inc nil]
                  [0 0 inc inc]
                  [0 0 nil inc]
                  [1 0 nil inc]
                  [2 0 dec inc]
                  [2 0 nil inc]
                  [0 1 inc nil]
                  [0 2 inc nil]]
          didwin (fn [cx cy fx fy r]
                   (if (and (<= cx 2) (>= cx 0) (<= cy 2) (>= cy 0))
                     (recur (if (not= nil fx) (fx cx) cx)
                       (if (not= nil fy) (fy cy) cy)
                       fx fy
                       (cons ((b cy) cx) r))
                     r))
          res (map #(didwin (% 0) (% 1) (% 2) (% 3) '()) checks)
          res2 (reduce (fn [x v]
                         (conj x (if (and (apply = v) (not= (first v) :e))
                                   (first v)
                                   nil))) '() res)]
      (if (some #(= :x %) res2) :x
                                (if (some #(= :o %) res2) :o nil)))))

(defcheck solution-e169de86
  (fn [rows]
    (letfn [(check-win [player]
              (or
               (some (fn [row] (every? #(= player %) row)) rows)
               (some (fn [col-idx] (every? #(= player (% col-idx)) rows)) (range 3))
               (= player ((rows 0) 0) ((rows 1) 1) ((rows 2) 2))
               (= player ((rows 0) 2) ((rows 1) 1) ((rows 2) 0))))]
      (cond (check-win :x) :x
            (check-win :o) :o
            :else nil))))

(defcheck solution-e16c2e4
  (fn [b]
    (letfn [(transp [] (apply map vector b))
            (row? [r p] (every? #(= % p) r))
            (diag [f] (map #(nth (f (nth b %)) %) (range 0 3)))
            (hor? [b p] (some #(row? % p) b))
            (ver? [p] (hor? (transp) p))
            (diag? [p] (hor? [(diag identity) (diag reverse)] p))
            (win? [p] (or (hor? b p) (ver? p) (diag? p)))]
      (cond (win? :x) :x
            (win? :o) :o
            :else nil))))

(defcheck solution-e1ce3a19
  (fn oxo [board]
    (let [r0 (first board)
          r1 (second board)
          r2 (last board)
          c0 (map first board)
          c1 (map second board)
          c2 (map last board)
          d1 [(first r0) (second r1) (last r2)]
          d2 [(last r0) (second r1) (first r2)]
          win-lines [r0 r1 r2 c0 c1 c2 d1 d2]
          won-lines (->> win-lines
                      (map set)
                      (filter #(= 1 (count %)))
                      (filter #(not= #{:e} %))
                      (map first))
          winner (first won-lines)]
      winner)))

(defcheck solution-e1f2bf58
  (fn x [[[a b c] [d e f] [g h i]]]
    (let [s [[a b c] [d e f] [g h i]
             [a d g] [b e h] [c f i]
             [a e i] [c e g]]
          xx (fn[ss](count(filter #(= :x %) ss)))
          oo (fn[ss](count(filter #(= :o %) ss)))
          xxx (map xx s)
          ooo (map oo s)
          xxxx (count (filter #(< 2 %) xxx))
          oooo (count (filter #(< 2 %) ooo))
          ]
      (if (< 0 xxxx)
        :x
        (if (< 0 oooo)
          :o
          nil)
        )
      )
    ))

(defcheck solution-e2031260
  (fn [[a b c :as m]]
    (let [q (set `(~a ~b ~c ~@(apply map list m)
                   ~[(a 0) (b 1) (c 2)]
                   ~[(a 2) (b 1) (c 0)]))]
      (cond (q [:x :x :x]) :x
            (q [:o :o :o]) :o))))

(defcheck solution-e20bdd94
  (fn [board]
    (let [goals [[0 1 2] [3 4 5] [6 7 8]
                 [0 3 6] [1 4 7] [2 5 8]
                 [0 4 8] [2 4 6]]]
      (letfn [(wins? [user]
                (let [occupied (->> (apply concat board)
                                 (map (fn [i u] (if (= u user) i)) (range))
                                 set)]
                  (some (fn [cells] (every? #(occupied %) cells)) goals)))]
        (cond (wins? :x) :x (wins? :o) :o :else nil)))))

(defcheck solution-e275fb08
  (fn check [board]
    (first (first (filter #(and (= 1 (count %)) (not-empty (clojure.set/intersection #{:o :x} %)))
                    (clojure.set/union
                      ; rows
                      (set (map #(into #{} %) board))
                      ; cols
                      (set (for [i (range 3)]
                             (set (map #(nth % i) board))))
                      ;diag l-r
                      (set (list (set (for [i (range 3)]
                                        (get-in board [i i])))
                             ;diag r-l
                             (set (for [i (range 3)]
                                    (get-in board [i (- 2 i)])))))))))))

(defcheck solution-e2978c5
  (fn tic-tac-toe [board]
    (let [cols (apply map list board)
          dias (map #(map nth board %) (list (range) (range 2 -1 -1)))
          all-lines (concat cols dias board)]
      (letfn [(count-item [side coll]
                (and (= 3 (apply + (for [i coll] (if (= i side) 1 0))))
                     side))]
        (or (some identity (map (partial count-item :x) all-lines))
            (some identity (map (partial count-item :o) all-lines)))))))

(defcheck solution-e3234557
  (fn [board]
    (letfn [(hs [b] b)
            (vs [b]
              [(vec (map first b))
               (vec (map second b))
               (vec (map last b))])
            (ds [b]
              [[(first (first b))
                (second (second b))
                (last (last b))]
               [(first (last b))
                (second (second b))
                (last (first b))]])
            (rs [b]
              (concat (hs b) (vs b) (ds b)))]
      (let [ws
            (set (flatten (filter #(= (count %) 1)
                            (map keys
                              (map frequencies (rs board))))))]
        (cond
          (some #{:x} ws) :x
          (some #{:o} ws) :o
          :else nil)))))

(defcheck solution-e33f142e
  (fn [g]
    (let [g (into [] (flatten g))
          x? #(= :x %)
          o? #(= :o %)
          all? (partial every? identity)
          any? (partial some identity)
          ws ['(0 1 2) '(3 4 5) '(6 7 8)
              '(0 3 6) '(1 4 7) '(2 5 8)
              '(0 4 8) '(2 4 6)]
          winner
          (fn [player?]
            (any? (map #(all? (map player? (map g %))) ws)))]

      (cond (winner x?) :x
            (winner o?) :o))))

(defcheck solution-e4b26ac0
  (fn [b] (some #{:x :o}
            (apply clojure.set/union
              (filter #(= 1 (count %))
                (map set (apply concat
                           ((juxt
                              identity
                              (partial apply map vector)
                              (comp vector vec (partial map-indexed #(nth %2 %1)))
                              (comp vector vec (partial map-indexed #(nth %2 %1)) (partial map reverse))
                              ) b))))))))

(defcheck solution-e4bacafb
  (fn [d]
    ( let [brd (concat d (apply map vector d)
                       [(map get d [0 1 2])] [(map get d [2 1 0])])]
      (first(first(filter #(and ((complement contains?) % :e) ( = 1 (count %))) (map set brd)))
        ))))

(defcheck solution-e4cc9618
  (fn [board]
    (some (fn [v] (some #(when (apply = % v) %) [:x :o]))
      (->>
        (for [[[x y] f g] `[ [[0 0] ~inc ~inc]
                            [[0 2] ~inc ~dec]
                            ~@(for [x [0 1 2]] [[x 0] identity inc])
                            ~@(for [x [0 1 2]] [[0 x] inc identity])]]
          (take 3 (map vector (iterate f x) (iterate g y))))
        (map (fn [v] (map #(get-in board %) v)))))))

(defcheck solution-e4e73965
  (fn ttt-winner
    [[[a b c]
      [d e f]
      [g h i]]]
    (loop [choices [:x :o :e]]
      (cond
        (= (first choices) :e) nil
        (= a b c (first choices)) (first choices)
        (= d e f (first choices)) (first choices)
        (= g h i (first choices)) (first choices)
        (= a d g (first choices)) (first choices)
        (= b e h (first choices)) (first choices)
        (= c f i (first choices)) (first choices)
        (= a e i (first choices)) (first choices)
        (= g e c (first choices)) (first choices)
        :else (recur (rest choices))))))

(defcheck solution-e52fb585
  (fn tic [X]
    (let [wi [[0 1 2] [3 4 5] [6 7 8]
              [0 3 6] [1 4 7] [2 5 8]
              [0 4 8] [2 4 6]]
          xf (flatten X)]
      (first (remove #(nil? %) (for [w wi]
                                 (let [arn (map #(nth xf %) w)]
                                   (when (and (not= (nth arn 0) :e)
                                              (and (= (nth arn 0) (nth arn 1))
                                                   (= (nth arn 1) (nth arn 2))))
                                     (nth arn 0)))))))))

(defcheck solution-e53f3d9
  #(let [f (fn [s]
             (if (or (and (= s ((% 0) 0)) (= s ((% 0) 1)) (= s ((% 0) 2)))
                     (and (= s ((% 0) 0)) (= s ((% 1) 0)) (= s ((% 2) 0)))
                     (and (= s ((% 0) 0)) (= s ((% 1) 1)) (= s ((% 2) 2)))
                     (and (= s ((% 0) 1)) (= s ((% 1) 1)) (= s ((% 2) 1)))
                     (and (= s ((% 0) 2)) (= s ((% 1) 2)) (= s ((% 2) 2)))
                     (and (= s ((% 0) 2)) (= s ((% 1) 1)) (= s ((% 2) 0)))
                     (and (= s ((% 1) 0)) (= s ((% 1) 1)) (= s ((% 1) 2)))
                     (and (= s ((% 2) 0)) (= s ((% 2) 1)) (= s ((% 2) 2))))
               s
               nil))]
     (or (f :o) (f :x))))

(defcheck solution-e549aab4
  (fn tictactoe [x]
    (let [y (vec (flatten x))]
      (if (or (and (= (get y 0) :x) (= (get y 1) :x) (= (get y 2) :x))
              (and (= (get y 3) :x) (= (get y 4) :x) (= (get y 5) :x))
              (and (= (get y 6) :x) (= (get y 7) :x) (= (get y 8) :x))
              (and (= (get y 0) :x) (= (get y 3) :x) (= (get y 6) :x))
              (and (= (get y 1) :x) (= (get y 4) :x) (= (get y 7) :x))
              (and (= (get y 2) :x) (= (get y 5) :x) (= (get y 8) :x))
              (and (= (get y 0) :x) (= (get y 4) :x) (= (get y 8) :x))
              (and (= (get y 2) :x) (= (get y 4) :x) (= (get y 6) :x)))
        :x
        (if (or (and (= (get y 0) :o) (= (get y 1) :o) (= (get y 2) :o))
                (and (= (get y 3) :o) (= (get y 4) :o) (= (get y 5) :o))
                (and (= (get y 6) :o) (= (get y 7) :o) (= (get y 8) :o))
                (and (= (get y 0) :o) (= (get y 3) :o) (= (get y 6) :o))
                (and (= (get y 1) :o) (= (get y 4) :o) (= (get y 7) :o))
                (and (= (get y 2) :o) (= (get y 5) :o) (= (get y 8) :o))
                (and (= (get y 0) :o) (= (get y 4) :o) (= (get y 8) :o))
                (and (= (get y 2) :o) (= (get y 4) :o) (= (get y 6) :o)))
          :o
          nil)))))

(defcheck solution-e5e28e92
  (fn [coll]
    (let [colls coll
          colls (conj colls (vec (map first coll)))
          colls (conj colls (vec (map second coll)))
          colls (conj colls (vec (map last coll)))
          colls (conj colls [((coll 0) 0) ((coll 1) 1) ((coll 2) 2)])
          colls (conj colls [((coll 0) 2) ((coll 1) 1) ((coll 2) 0)])
          colls (filter #(apply = %) colls)
          colls (filter #(not= :e (first %)) colls)]
      (if (seq colls)
        ((first colls) 0)
        nil))))

(defcheck solution-e611e19f
  (fn [g]
    (letfn
     [(lines [grid]
        (concat
         grid
         (apply map list grid)
         (list (map nth grid (range 0 3)))
         (list (map nth (reverse grid) (range 0 3)))))]
      (let [eqlins (->> (lines g)
                     (filter (partial apply =))
                     (map first))]
        (reduce (fn [v1 v2] (cond (= v2 :o) :o
                                  (= v2 :x) :x
                                  :else v1))
          nil eqlins)))))

(defcheck solution-e69e614d
  (fn [board]
    (let [win? (fn [coll player] (->> coll (filter (partial = player))
                                   count
                                   (= 3)))
          row-win? (fn [row player] (win? (board row) player))
          col-win? (fn [col player] (win? (map #(nth % col) board) player))
          main-diag-win? (fn [player] (win? (map (fn [idx row] (nth row idx)) [0 1 2] board) player))
          cross-diag-win? (fn [player] (win? (map (fn [idx row] (nth row idx)) [2 1 0] board) player))]
      (cond
        (or (some #(row-win? % :x) [0 1 2])
            (some #(col-win? % :x) [0 1 2])
            (main-diag-win? :x)
            (cross-diag-win? :x)) :x
        (or (some #(row-win? % :o) [0 1 2])
            (some #(col-win? % :o) [0 1 2])
            (main-diag-win? :o)
            (cross-diag-win? :o)) :o
        :else nil))))

(defcheck solution-e6b246fa
  (fn tic-tac-toe
    [x]
    (let [one (first (first x))
          two (second (first x))
          three (get (first x) 2)
          four (first (second x))
          five (second (second x))
          six (get (second x) 2)
          seven (first (get x 2))
          eight (second (get x 2))
          nine (get (get x 2) 2)]
      (let [comb-one (vector one two three)
            comb-two (vector four five six)
            comb-three (vector seven eight nine)
            comb-four (vector one four seven)
            comb-five (vector two five eight)
            comb-six (vector three six nine)
            comb-seven (vector one five nine)
            comb-eight (vector three five seven)]
        (let [all-of-them (vector comb-one comb-two comb-three comb-four comb-five comb-six comb-seven comb-eight)
              wins? (fn [x y]
                      (if (empty? y)
                        false
                        (if (every? (sorted-set x) (first y))
                          true
                          (recur x (rest y)))))]
          (if (wins? :o all-of-them)
            :o
            (if (wins? :x all-of-them)
              :x nil)))))))

(defcheck solution-e6b5af1d
  (fn [[[a _ b] [_ c _] [d _ e] :as l]]
    (first
      (some #{[:x :x :x] [:o :o :o]}
        (concat l
                (apply map (cons list l))
                [[a c e] [d c b]])))))

(defcheck solution-e7815226
  (fn [board]
    (->> (concat board
                 (apply map list board)
                 (map #(map (partial get-in board) %) [[[0 0] [1 1] [2 2]] [[2 0] [1 1] [0 2]]]))
      (filter (partial apply =))
      (remove (partial = [:e :e :e]))
      (ffirst))))

(defcheck solution-e7f393e5
  (fn [board]
    (letfn [(hrow [c] (some (fn [row] (every? #(= c %) row)) board))
            (vrow [c] (some (fn [col] (every? #(= c (nth % col)) board)) (range 3)))
            (drow [c] (some #(every? (fn [[row col]] (= c (get-in board [row col]))) %) [[[0 0] [1 1] [2 2]] [[2 0] [1 1] [0 2]]]))]
      (some #(when (or (hrow %) (vrow %) (drow %)) %) [:x :o]))))

(defcheck solution-e85857c9
  (let [lookup (fn [b [x y]] (nth (nth b y) x))
        winners (concat
                 (map (fn [x] (map #(vector x %) (range 3))) (range 3))
                 (map (fn [y] (map #(vector % y) (range 3))) (range 3))
                 [(map #(vector % %) (range 3)) (map #(vector % (- 2 %)) (range 3))])]
    #_(println winners)
    (fn [board]
      (loop [w winners]
        (if (empty? w)
          nil
          (let [s (set (map (partial lookup board) (first w)))]
            (if (= 1 (count s))
              (if (= :e (first s)) nil (first s))
              (recur (rest w)))))))))

(defcheck solution-e86a3cea
  (fn [t] (let [line (fn [l] (let [d (distinct l)] (if (= (count d) 1) (first d) :e)))
                c (fn [x y] (fn [s] (comp (nth (nth s x) y))))
                diags (fn [x] [((juxt (c 0 0) (c 1 1) (c 2 2)) x)
                               ((juxt (c 2 0) (c 1 1) (c 0 2)) x)])]
            (first (filter #(not (= % :e)) (map line (concat t (apply mapv vector t) (diags t))))))))

(defcheck solution-e89f72a4
  (fn [b]
    (let [c1 [(vec (map first b))]
          c2 [(vec (map second b))]
          c3 [(vec (map last b))]
          d1 [[(first (first b)) (second (second b)) (last (last b))]]
          d2 [[(last (first b)) (second (second b)) (first (last b))]]]
      (let [a (remove #(= :e %) (map first (filter #(apply = %) (reduce into [b c1 c2 c3 d1 d2]))))]
        (if (empty? a)
          nil
          (first a))))))

(defcheck solution-e8e0f351
  (fn [tttb]
    (let [cnt (count tttb)
          diags [(for [i (range cnt)] ((tttb i) i)) (for [i (range cnt)] ((tttb (- cnt i 1)) i))]
          cols (for [x (range cnt)] (for [y (range cnt)] ((tttb y) x)))
          full-axes (remove (fn [a] (some #(= :e %) a)) (concat tttb diags cols))]
      (first (first (filter #(apply = %) full-axes))))))

(defcheck solution-e90c5c79
  (fn ttt [coll]
    (let [rotate (apply map list coll)
          sub (fn [x y] (map #(nth y %) x))
          fl_coll (flatten coll)]
      (cond
        (some #(= [:x :x :x] %) coll) :x
        (some #(= [:x :x :x] %) rotate) :x
        (some #(= [:o :o :o] %) coll) :o
        (some #(= [:o :o :o] %) rotate) :o
        (= [:x :x :x] (sub [0 4 8] fl_coll)) :x
        (= [:x :x :x] (sub [2 4 6] fl_coll)) :x
        (= [:o :o :o] (sub [0 4 8] fl_coll)) :o
        (= [:o :o :o] (sub [2 4 6] fl_coll)) :o
        :else nil))))

(defcheck solution-e90d78a3
  (fn [b]
    (let [ss ( ->>
               (vector (vec (for [x [0 1 2] y [0 1 2] :when (= x y)] [x y])))
               (concat (vector (vec (for [x [0 1 2] y [0 1 2] :when (= (+ x y) 2)] [x y]))))
               (concat (for [x [0 1 2]] [[0 x] [1 x] [2 x]]))
               (concat (for [x [0 1 2]] [[x 0] [x 1] [x 2]])))]
      (letfn [
              (eval-s [k]
                (reduce
                  (fn [acc s]
                    (or acc
                        (let [p0 (get s 0) p1 (get s 1) p2 (get s 2)
                              v0 ((get b (get p0 0)) (get p0 1))
                              v1 ((get b (get p1 0)) (get p1 1))
                              v2 ((get b (get p2 0)) (get p2 1))]
                          (= k v0 v1 v2)))) false ss))]
        (cond
          (eval-s :x) :x
          (eval-s :o) :o
          :default nil)))))

(defcheck solution-e919b311
  (fn  [& c]
    (let [array1 (first c)
          cmax (count array1)
          ha
                 (for [array2 array1]
                   (if (apply = array2) (first array2) nil))
          va1
                 (apply (partial map list) (first c))
          va
                 (for [array2 va1]
                   (if (apply = array2) (first array2) nil))
          aa1
                 (for [i (range cmax)]
                   ((array1 i) i))
          aa
                 (list (if (apply = aa1) (first aa1) nil))
          ra1
                 (for [i (range cmax)]
                   ((array1 i) (- (dec cmax) i)))
          ra
                 (list (if (apply = ra1) (first ra1) nil))
          fa (concat ha va aa ra)
          ret (first (filter #(not= nil %) fa))]
      (if (not= :e ret) ret nil))))

(defcheck solution-e952e2f8
  (fn winner [board]
    (letfn [(ew [n]
              (if (apply = (board n)) (first (board n))))
            (nw [n]
              (if (apply = (map #(% n) board)) ((first board) n)))
            (nw-se []
              (let [[[x _ _] [_ y _] [_ _ z]] board]
                (if (= x y z) x)))
            (ne-sw []
              (let [[[_ _ x] [_ y _] [z _ _]] board]
                (if (= x y z) x)))]
      (some #{:x :o} (conj (mapcat (juxt ew nw) (range 3)) (nw-se) (ne-sw))))))

(defcheck solution-e97bfe61
  (fn won[st](

              #(if (nil? %) nil (first %))   (some #(and (not (= (nth % 0) :e)) (= (nth % 0) (nth % 1) (nth % 2)) %) [(nth st 0)(nth st 1)(nth st 2)
                                                                                                                      [(nth (nth st 0) 0) (nth (nth st 1) 0) (nth (nth st 2) 0)]
                                                                                                                      [(nth (nth st 0) 1) (nth (nth st 1) 1) (nth (nth st 2) 1)]
                                                                                                                      [(nth (nth st 0) 2) (nth (nth st 1) 2) (nth (nth st 2) 2)]
                                                                                                                      [(nth (nth st 0) 0) (nth (nth st 1) 1) (nth (nth st 2) 2)]
                                                                                                                      [(nth (nth st 0) 2) (nth (nth st 1) 1) (nth (nth st 2) 0)]]

                                               ))))

(defcheck solution-e9a00a12
  (fn [board] (let [rows (map set board)
                    cols (apply map #(set %&) board)
                    dias (map set [(map #(get-in board [% %]) (range 3))
                                   (map #(get-in board [% (- 2 %)]) (range 3))])
                    all (concat rows cols dias)]
                (if (some #(= #{:x} %) all)
                  :x
                  (if (some #(= #{:o} %) all)
                    :o
                    nil)))))

(defcheck solution-ea3e941f
  (fn [y] (let [z (cons :e (apply concat y)) jo
                  #(and
                    (= %1 (nth z %2))
                    (= %1 (nth z %3))
                    (= %1 (nth z %4)))]
            (if (or
                 (jo :x 1 2 3)
                 (jo :x 4 5 6)
                 (jo :x 7 8 9)
                 (jo :x 1 4 7)
                 (jo :x 2 5 8)
                 (jo :x 3 6 9)
                 (jo :x 1 5 9)
                 (jo :x 3 5 7))
              :x (if (or
                      (jo :o 1 2 3)
                      (jo :o 4 5 6)
                      (jo :o 7 8 9)
                      (jo :o 1 4 7)
                      (jo :o 2 5 8)
                      (jo :o 3 6 9)
                      (jo :o 1 5 9)
                      (jo :o 3 5 7))
                   :o nil)))))

(defcheck solution-ea8738fa
  (fn ttt [b]
    (let [i (range 0 (count b))
          size (apply max i)
          eq (fn [a b] (when (= a b) a))
          win? (fn [c] (some #{:x :o} c))
          reduce-eq (fn [c] (reduce eq (first c) (rest c)))
          row (win? (map reduce-eq b))
          col (win? (map reduce-eq (apply map vector b)))
          diag1 (win? (vector (reduce-eq (map #((b %) %) i))))
          diag2 (win? (vector (reduce-eq (map #((b (- size %)) %) i))))
          ]
      (some #(identity %) (vector row col diag1 diag2)))))

(defcheck solution-ea951f79
  (fn [board]
    (let [lines [[[0 0] [0 1] [0 2]]
                 [[1 0] [1 1] [1 2]]
                 [[2 0] [2 1] [2 2]]
                 [[0 0] [1 0] [2 0]]
                 [[0 1] [1 1] [2 1]]
                 [[0 2] [1 2] [2 2]]
                 [[0 0] [1 1] [2 2]]
                 [[0 2] [1 1] [2 0]]]
          check (fn [player] (some #(every? (partial = player)
                                      (map (partial get-in board) %))
                               lines))]
      (cond
        (check :x) :x
        (check :o) :o))))

(defcheck solution-eacc8009
  (fn [board]
    (let [wins [[[0 0] [0 1] [0 2]] [[1 0] [1 1] [1 2]] [[2 0] [2 1] [2 2]]
                [[0 0] [1 0] [2 0]] [[0 1] [1 1] [2 1]] [[0 2] [1 2] [2 2]]
                [[0 0] [1 1] [2 2]] [[0 2] [1 1] [2 0]]]
          win? (fn [[[r c] & tail]]
                 (let [player ((board r) c)]
                   (when (every? #(= player %)
                           (map #((board (% 0)) (% 1)) tail))
                     player)))]
      (some #(when (or (= :x %) (= :o %))
               %)
        (map win? wins)))))

(defcheck solution-eae7a175
  (fn analyze [board]
    (letfn [(analyze-items [& items]
              (cond
                (apply = :o items) :o
                (apply = :x items) :x
                :else nil))]
      (some
        identity
        (concat
         (for [row board]
           (apply analyze-items row))
         (for [col (apply map vector board)]
           (apply analyze-items col))
         (for [diagonal [[(get-in board [0 0])
                          (get-in board [1 1])
                          (get-in board [2 2])]
                         [(get-in board [0 2])
                          (get-in board [1 1])
                          (get-in board [2 0])]]]
           (apply analyze-items diagonal)))))))

(defcheck solution-eb036ae1
  (fn [grid]
    (let [lines (concat grid
                        (partition 3 (apply interleave grid))
                        (list (map #(nth % %2) grid [0 1 2]))
                        (list (map #(nth % %2) grid [2 1 0])))]
      (some (fn [sym]
              (when (some identity (map #(every? (partial = sym) %) lines)) sym)) [:x :o]))))

(defcheck solution-ebbd2ca5
  (fn tic-tac [bd]
    (let [pos (fn [x y] (-> bd (nth x) (nth y)))
          won? (fn [x]
                 (or
                  (or (= x (pos 0 0) (pos 0 1) (pos 0 2)) (= x (pos 1 0) (pos 1 1) (pos 1 2)) (= x (pos 2 0) (pos 2 1) (pos 2 2)))
                  (or (= x (pos 0 0) (pos 1 0) (pos 2 0)) (= x (pos 0 1) (pos 1 1) (pos 2 1)) (= x (pos 0 2) (pos 1 2) (pos 2 2)))
                  (or (= x (pos 0 0) (pos 1 1) (pos 2 2)) (= x (pos 0 2) (pos 1 1) (pos 2 0)))))]
      (cond
        (won? :x) :x
        (won? :o) :o
        :else nil))))

(defcheck solution-ebcf99b9
  (fn [a x b]
    (ffirst
      (remove #(or (some #{:e} %) (a not= %))
        `(~@b ~@(a map list b) ~(x b [0 1 2]) ~(x b [2 1 0]))))) apply #(map get % %2))

(defcheck solution-ebfaf6d
  (fn [board]
    (let
     [cols (map (fn [n] (map #(% n) board)) [0 1 2])
      diag (fn [f] (map #((board %1) %2) [0 1 2] (f [0 1 2])))
      diags (vector (diag identity) (diag reverse))
      lines (concat board cols diags)
      win-for?  (fn [e]
                  (reduce
                    (fn [v i]
                      (or v (every? #(= % e) i)))
                    false lines))]
      (cond
        (win-for? :x) :x
        (win-for? :o) :o
        :else nil))))

(defcheck solution-ec9fab8d
  (fn tictac [board]
    (letfn [(hor? [[a & b]]
              (cond
                (nil? a) nil
                (and (not= :e (first a)) (apply = a)) (first a)
                :else (hor? b)))


            (ver? [b] (hor? (for [i (range (count b))] (map #(nth % i) b))))
            (diag? [b]
              (let [nor
                    (map (fn [[i v]] (nth v i)) (map vector (range) b))]
                (if (and (not= (first nor) :e) (apply = nor)) (first nor) nil)))
            ]
      (or (hor? board)
          (ver? board)
          (diag? board)
          (diag? (map reverse board))
          ))))

(defcheck solution-eccf3d30
  (fn [ls]
    (let [winner? (fn [ls] (or (every? #(= % :x) ls) (every? #(= % :o) ls)))
          t-w (fn [l] (when (winner? l) (first l)))]
      (some t-w
        (lazy-cat
          ls
          (map #(map (fn [l] (get l %)) ls) [0 1 2])
          ((juxt #(map first %) #(map second %))
           (map
             #(vector (get %1 %2)
                (get %1 (- 2 %2)))
             ls [0 1 2])))))))

(defcheck solution-ed08a835
  (fn [board]
    (let [ cols (for [i (range 3)] (map #(nth % i) board))
          diags [(for [i (range 3)]
                   (nth (nth  board i) i))
                 (for [i (range 3)]
                   (nth (nth board i) (- 2 i)))]
          parts (concat board cols diags)
          wins (->> parts (map distinct)
                 (filter #(= 1 (count %))) (map #(nth % 0))
                 (filter #(not= :e %)))
          nwins (count (filter #(not= :e %) wins))]
      (if (= 1 nwins) (first wins)))))

(defcheck solution-ed5858cb
  (fn [b] (first (some #{#{:x} #{:o}} (map #(set (map (partial nth (flatten b))%)) [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])))))

(defcheck solution-ed671c1b
  #(let [pairs [[0 0][0 1][0 2][1 0][1 1][1 2][2 0][2 1][2 2][0 0][1 0][2 0][0 1][1 1][2 1][0 2][1 2][2 2][0 0][1 1][2 2][0 2][1 1][2 0]]
         l (partition 3 (map (partial get-in %) pairs))
         f (fn [i]
             (cond (every? (partial = :o) i) :o
                   (every? (partial = :x) i) :x
                   :else nil))]
     (some f l)))

(defcheck solution-ed79e2bb
  (fn [[[a d g] [b e h] [c f i]]]
    (some #(first
             ((set (partition 3
                     [a d g
                      b e h
                      c f i
                      a b c
                      d e f
                      g h i
                      a e i
                      g e c]))
              [% % %]))
      [:x :o])))

(defcheck solution-ed845ec2
  (fn[[[xl _ xr] [_ y _] [zl _ zr] :as xs]]
    (ffirst
      (filter #(and (= 1 (count %)) (not= [:e] %))
        (map distinct
          (concat [[xl y zr] [xr y zl]]
                  xs
                  (apply map vector xs)))))))

(defcheck solution-eda57cea
  (fn [x]
    (let[y (#(partition 3 3 (apply interleave %)) x)]
      (first
        (remove #(= % :e)
          (map first
            (filter #(apply = %)
              (conj
                (concat x y)
                (flatten (partition 1 4 (flatten x)))
                (flatten (rest(reverse (rest( partition 1 2 (flatten y)))))))
              )))))))

(defcheck solution-edd03073
  (fn [field]
    (let [horizontal field
          vertical (apply map vector field)
          diagonal (map (partial map (partial get-in field))
                     [[[0 0] [1 1] [2 2]]
                      [[0 2] [1 1] [2 0]]])
          lines (concat horizontal vertical diagonal)]
      (some (fn [line] (if (or (every? (partial = :x) line)
                               (every? (partial = :o) line))
                         (first line)))
        lines))))

(defcheck solution-ee01d1f4
  (fn [xs]
    (let [ptn (fn [l]
                (reduce
                  #(conj % (range %2 (+ %2 (inc (* l (dec l)))) l))
                  (reduce #(conj % (range (* %2 l) (* (inc %2) l)))
                    [(take l (iterate #(+ l (inc %)) 0))
                     (take l (iterate #(+ l (dec %)) (dec l)))] (range l))
                  (range l)))]
      (#{:x :o} (ffirst (filter (partial apply =) (for [p (ptn (count xs)) :let [g (apply concat xs)]]
                                                    (map #(nth g %) p))))))))

(defcheck solution-ee6f89d6
  (fn ttt [b]
    (let [get-square (fn [r c] (nth (nth b r) c))
          get-row (fn [r] (nth b r))
          get-col (fn [c] (for [r (range 3)] (get-square r c)))
          rows-cols (mapcat (fn [x] (list (get-row x) (get-col x))) (range 3))
          diag-1 (list (get-square 0 0) (get-square 1 1) (get-square 2 2))
          diag-2 (list (get-square 0 2) (get-square 1 1) (get-square 2 0))
          all (conj rows-cols diag-1 diag-2)]
      (->> all (remove (fn [x] (some #{:e} x))) (filter (fn [x] (apply = x))) ffirst))))

(defcheck solution-ee7df864
  (fn [b]
    (some (fn [v] (if (apply = v) (last v)))
      (map #(map (partial nth (replace {:e nil} (flatten b))) %)
        [[0 3 6] [1 4 7] [2 5 8] [0 1 2] [3 4 5] [6 7 8] [0 4 8] [2 4 6]]))))

(defcheck solution-eeba1066
  (fn [b]
    (let [
          x (map (fn [ix] (map #(nth (flatten b) %) ix)) [[0 4 8] [2 4 6]])
          ls (concat b (apply map list b) x)
          won? (fn [w] (if (some #(apply = w %) ls) w nil))
          ]
      (or (won? :x) (won? :o)))))

(defcheck solution-ef9ed05
  (fn [g]
    (some #(cond (every? (partial = :x) %) :x (every? (partial = :o) %) :o)
      (concat
       g
       (apply map list g)
       (map #(list ((g 0) %) ((g 1) 1) ((g 2) (- 2 %))) '(0 2))))))

(defcheck solution-efb39f4c
  (fn tic-tac-analyse [board]
    (let [rows board
          cols (map (fn [i] (map #(nth % i) board)) (range 3))
          d1 (map #(get-in board %) (for [i (range 3)] [i i]))
          d2 (map #(get-in board %) (for [i (range 3)] [i (- 2 i)]))
          result (filter #(or (= [:x :x :x] %) (= [:o :o :o] %)) (concat rows cols (list d1 d2)))]
      (first (first result)))))

(defcheck solution-efe8e3ef
  (fn check-tictactoe [b]
    (let [ d1 ((juxt ffirst (comp second second) (comp last last)) b)
          d2 ((juxt (comp last first) (comp second second) (comp first last)) b)
          tr (apply (partial map (fn [& xs] (concat xs))) b)
          l (conj (reduce conj b tr) d1 d2)
          f (fn [v]  (let [[k c] (last (sort-by second (frequencies v)))]
                       (if (and (= c 3) (not= k :e)) k)))]
      (reduce #(or %1 %2) (map f l)))))

(defcheck solution-f000ff4e
  (fn analyze-tic-tac-toe [board]
    (let [things-to-check
                 [{:step (fn [z] [(first z),(inc (second z))])
                   :start [[0,0] [1,0] [2,0]]}
                  {:step (fn [z] [(inc (first z)),(second z)])
                   :start [[0,0] [0,1] [0,2]]}
                  {:step (fn [z] [(inc (first z)),(inc (second z))])
                   :start [[0,0]]}
                  {:step (fn [z] [(inc (first z)),(dec (second z))])
                   :start [[0,2]]}]
          pieces #{:x :o}]
      (some pieces (map
                     #(if (reduce
                            (fn [b3 thing-to-check]
                              (or b3 (reduce
                                       (fn [b2 start]
                                         (or b2 (reduce
                                                  (fn [b coord] (let [[x y] coord]
                                                                  (and b (= % (nth (nth board x) y)))))
                                                  true
                                                  (take 3 (iterate (thing-to-check :step) start)))))
                                       false
                                       (thing-to-check :start))))
                            false
                            things-to-check)
                        %)
                     pieces)))))

(defcheck solution-f0b539ed
  (fn searchLine [matrix]
    (some
      #(cond
         (contains? % :e)  false
         (empty? (rest %)) (first %)
         :else false
         )
      (map
        #(set
           (map
             (fn[[x y]]
               (nth (nth matrix y) x)
               )
             %
             )
           )
        (conj (conj
                (let [flist [ (fn[n x y](= x n) ),
                             (fn[n x y](= y n) ) ]
                      ]
                  (for [f flist,n (range 0 3)]
                    (for [x (range 0 3),y(range 0 3):when (f n x y)]
                      [x y] )
                    )
                  )
                (for [n (range 0 3)] [n n] )
                )
          (for [n (range 0 3)] [n (- 2 n)] )
          )
        )
      )
    ))

(defcheck solution-f0d6793
  (letfn [(c [p b]
            (let [v (apply + (map-indexed #(if (= p %2)
                                             (bit-shift-left 1 %) 0) (flatten b)))]
              (some #(= % (bit-and % v)) [292 146 73 448 56 7 273 84])))]
    #(cond
       (c :x %) :x
       (c :o %) :o)))

(defcheck solution-f151fd97
  (fn [brd]
    (let [won? (fn [p]
                 (reduce #(or %1 %2)
                   (flatten
                     [ (for [y (range 0 3)] (= p ((brd 0) y) ((brd 1) y) ((brd 2) y)))
                      (for [x (range 0 3)] (= p ((brd x) 0) ((brd x) 1) ((brd x) 2)))
                      (= p ((brd 0) 0) ((brd 1) 1) ((brd 2) 2))
                      (= p ((brd 0) 2) ((brd 1) 1) ((brd 2) 0))])))]
      (cond (won? :x) :x (won? :o) :o :else nil))))

(defcheck solution-f1608a9d
  (fn [board]
    (let [win? (fn [k v] (and (not= :e k) (= 3 v)))
          win-f (fn [freqs]
                  (reduce (fn [win f]
                            (or win
                                (reduce-kv (fn [res k v]
                                             (or res (when (win? k v) k)))
                                  win
                                  f)))
                    nil
                    freqs))
          diags (let [rng (range (count board))]
                  (list
                    (map #(get-in board [% %]) rng)
                    (map (fn [m n] (get-in board [n m])) rng (reverse rng))))
          row-win (win-f (map frequencies board))
          coll-win (win-f (apply map (fn [& c] (frequencies c)) board))
          diag-win (win-f (map frequencies diags))]
      (or row-win coll-win diag-win))))

(defcheck solution-f1805cfc
  (fn tictactoe [board]
    (letfn [(winner [[a b c]] (if (and (not (= a :e)) (= a b c)) a))
            (cell-at [[x y]] (nth (nth board x) y))]
      (or (some winner board)
          (some winner (apply map list board))
          (winner (map cell-at [[0 0] [1 1] [2 2]]))
          (winner (map cell-at [[0 2] [1 1] [2 0]]))))))

(defcheck solution-f1a479c5
  (fn [b]
    (let [winners #{#{0 1 2}
                    #{3 4 5}
                    #{6 7 8}
                    #{0 3 6}
                    #{1 4 7}
                    #{2 5 8}
                    #{0 4 8}
                    #{2 4 6}}
          finder (fn [x]
                   (set
                     (map first
                       (filter #(= x (second %))
                         (map-indexed vector
                           (flatten b))))))
          xs (finder :x)
          os (finder :o)
          is (fn [s r] (if (some #(= 3 (count %))
                             (map #(clojure.set/intersection s %)
                               winners))
                         r))]

      (or (is xs :x) (is os :o)))))

(defcheck solution-f2704755
  (fn tic-tac-toe [board]
    (letfn [(winner [row]
              (cond
                (every? #(= % :x) row) :x
                (every? #(= % :o) row) :o
                :else nil
                ))
            (lines [board]
              (conj board
                (map #(nth % 0) board)
                (map #(nth % 1) board)
                (map #(nth % 2) board)
                (list (nth (nth board 0) 0)
                  (nth (nth board 1) 1)
                  (nth (nth board 2) 2))
                (list (nth (nth board 0) 2)
                  (nth (nth board 1) 1)
                  (nth (nth board 2) 0))))]
      (let [winners (map winner (lines board))]
        (if (some #{:o} winners)
          (if (some #{:x} winners)
            nil
            :o
            )
          (if (some #{:x} winners)
            :x
            nil
            ))))))

(defcheck solution-f2798b86
  (fn tic-tac-toe [matrix]
    (let [align (fn [items] (if (apply = items) ({:x :x :o :o :e nil} (first items)) nil))]
      (some align (concat matrix
                          (vector (map first matrix))
                          (vector (map second matrix))
                          (vector (map last matrix))
                          (vector [(first (first matrix)) (second (second matrix)) (last (last matrix))])
                          (vector [(first (last matrix)) (second (second matrix)) (last (first matrix))])
                          )))))

(defcheck solution-f3d27343
  (fn __
    [board]
    (letfn [(check-seq [to-check]
              (if (and
                   (=
                     (first to-check)
                     (second to-check)
                     (last to-check))
                   (not (= (first to-check) :e)))
                (first to-check)))
            (check-rows [board]
              (or
               (check-seq (first board))
               (check-seq (second board))
               (check-seq (last board))))
            (check-columns [board]
              (check-rows (partition 3 (apply interleave board))))
            (check-diagonals [board]
              (or
               (check-seq [(first (first board))
                           (second (second board))
                           (last (last board))])
               (check-seq [(last (first board))
                           (second (second board))
                           (first (last board))])))
            (check-winner [board]
              (or
               (check-rows board)
               (check-columns board)
               (check-diagonals board)))]
      (check-winner board))))

(defcheck solution-f3dff59b
  (fn ticTacToeX [m]
    (cond
      ((fn ticTacToeSymbol [m s]
         (let
          [
           x (get m 0)
           y (get m 1)
           z (get m 2)
           ]

           (or
            ((fn all [x s] (every? #(= % s) x)) x s)
            ((fn all [x s] (every? #(= % s) x)) y s)
            ((fn all [x s] (every? #(= % s) x)) z s)
            ((fn all [x s] (every? #(= % s) x)) ((fn column [x y z n] [(get x n)(get y n)(get z n)]) x y z 0) s)
            ((fn all [x s] (every? #(= % s) x)) ((fn column [x y z n] [(get x n)(get y n)(get z n)]) x y z 1) s)
            ((fn all [x s] (every? #(= % s) x)) ((fn column [x y z n] [(get x n)(get y n)(get z n)]) x y z 2) s)
            ((fn all [x s] (every? #(= % s) x)) ((fn diagonaLeft [x y z] [(get x 0)(get y 1)(get z 2)]) x y z) s)
            ((fn all [x s] (every? #(= % s) x)) ((fn diagonalRight [x y z] [(get x 2)(get y 1)(get z 0)]) x y z) s)
            ))
         ) m :x) :x
      ((fn ticTacToeSymbol [m s]
         (let
          [
           x (get m 0)
           y (get m 1)
           z (get m 2)
           ]

           (or
            ((fn all [x s] (every? #(= % s) x)) x s)
            ((fn all [x s] (every? #(= % s) x)) y s)
            ((fn all [x s] (every? #(= % s) x)) z s)
            ((fn all [x s] (every? #(= % s) x)) ((fn column [x y z n] [(get x n)(get y n)(get z n)]) x y z 0) s)
            ((fn all [x s] (every? #(= % s) x)) ((fn column [x y z n] [(get x n)(get y n)(get z n)]) x y z 1) s)
            ((fn all [x s] (every? #(= % s) x)) ((fn column [x y z n] [(get x n)(get y n)(get z n)]) x y z 2) s)
            ((fn all [x s] (every? #(= % s) x)) ((fn diagonalLeft [x y z] [(get x 0)(get y 1)(get z 2)]) x y z) s)
            ((fn all [x s] (every? #(= % s) x)) ((fn diagonalRight [x y z] [(get x 2)(get y 1)(get z 0)]) x y z) s)
            ))
         ) m :o) :o
      :else nil
      )
    ))

(defcheck solution-f45fa185
  (fn
    [board]
    (letfn [(pRow [value row]
              (every? (partial = value) (board row)))
            (pCol [value col]
              (every? (partial = value) (map #(% col) board)))
            (pDiag [value]
              (or (= value
                    (get-in board [0 0])
                    (get-in board [1 1])
                    (get-in board [2 2]))
                  (= value
                    (get-in board [0 2])
                    (get-in board [1 1])
                    (get-in board [2 0]))))
            (win [value]
              (or (some (partial pRow value) (range 3))
                  (some (partial pCol value) (range 3))
                  (pDiag value)))]
      (cond
        (win :x) :x
        (win :o) :o
        :else    nil))))

(defcheck solution-f534e24f
  (fn ttt [m]
    (letfn [(crow [r]
              (cond
                (every? #(= :o %) r) :o
                (every? #(= :x %) r) :x
                :else nil))
            (h-row [m] m)
            (v-row [m] (apply map vector m))
            (d-row [m]
              (list (for [x (range 3)] ((m x) x))
                (for [x (range 3)] ((m x) (- 2 x)))))]
      (some identity (map crow (mapcat identity ((juxt h-row v-row d-row) m)))))))

(defcheck solution-f5a09005
  (fn [board]
    (let [rng #(for [i (range 3)] (+ % (* i %2)))
          lines [[0 0 1 0] [0 1 1 0] [0 2 1 0] [0 0 1 1]
                 [0 0 0 1] [1 0 0 1] [2 0 0 1] [0 2 1 -1]]]
      (->> lines
        (map (fn [[x y dx dy]]
               (map (fn [x y] (nth (nth board y) x)) (rng x dx) (rng y dy))))
        (map set)
        (remove second)
        first first
        #{:x :o}
        ))))

(defcheck solution-f5bb7b4
  (fn [board]
    (first
      (some #{[:x :x :x] [:o :o :o]}
        (conj (concat board (apply mapv vector board))
          (mapv get board (range))
          (mapv get board [2 1 0]))))))

(defcheck solution-f5dc8b4e
  (letfn [(chk [c p1 p2 p3 v]
            (= c (nth v p1) (nth v p2) (nth v p3)))

          (win [c v]
            (or (chk c 0 1 2 v)
                (chk c 3 4 5 v)
                (chk c 6 7 8 v)
                (chk c 0 3 6 v)
                (chk c 1 4 7 v)
                (chk c 2 5 8 v)
                (chk c 0 4 8 v)
                (chk c 2 4 6 v)))]

    (fn [m]
      (let [m (flatten m)]
        (cond
          (win :x m) :x
          (win :o m) :o
          )))))

(defcheck solution-f5e9d593
  (fn analyze-tic-tac-toe
    [board]
    (let [colls (apply map list board)
          diagonal-l (list (map nth board [0 1 2]))
          diagonal-r (list (map nth board [2 1 0]))
          all (set (map set (concat board colls diagonal-l diagonal-r)))]
      (cond
        (all #{:x}) :x
        (all #{:o}) :o))))

(defcheck solution-f5fafcf4
  (fn [s]
    (letfn [(f [s] (some (fn [x] (when (every? #(= x %) s) x)) [:x :o]))
            (dia [[[a _ b] [_ x _] [c _ d]]] [[a x d] [b x c]])]
      (some f (concat s (apply map vector s) (dia s))))))

(defcheck solution-f62bea43
  (fn [board]
    (let [fields
          (concat
           board
           (apply map list board)
           [(for [i (range 3)] (get-in board [i i]))]
           [(for [i (range 3)] (get-in board [i (- 2 i)]))])]
      (some
        #(or (if (every? #{:x} %) :x)
             (if (every? #{:o} %) :o))
        fields))))

(defcheck solution-f640a278
  (fn [[[a b c]
        [d e f]
        [g h i]]]
    (cond (or (= a b c :x) (= d e f :x) (= g h i :x)
              (= a d g :x) (= b e h :x) (= c f i :x)
              (= a e i :x) (= c e g :x))
          :x
          (or (= a b c :o) (= d e f :o) (= g h i :o)
              (= a d g :o) (= b e h :o) (= c f i :o)
              (= a e i :o) (= c e g :o))
          :o
          :else nil)))

(defcheck solution-f659492c
  (fn [b]
    (letfn [(win [[a b c]] (if (= a b c) (#{:x :o} a)))]
      (or (some win b)
          (some win (apply map list b))
          (win (map #((b %) %) [0 1 2]))
          (win (map #((b %) (- 2 %)) [0 1 2]))))))

(defcheck solution-f6718028
  (fn [b]
    (let [skoro (ffirst (filter #(= (count %) 1) [
                                                  (distinct (get b 0)) (distinct (get b 1)) (distinct (get b 2))
                                                  (distinct (map #(nth % 0) b))
                                                  (distinct (map #(nth % 1) b))
                                                  (distinct (map #(nth % 2) b))
                                                  (distinct [(get (get b 0) 0) (get (get b 1) 1)(get (get b 2) 2)])
                                                  (distinct [(get (get b 0) 2) (get (get b 1) 1)(get (get b 2) 0)])

                                                  ]))]
      (if (= skoro :e) nil skoro)
      )
    ))

(defcheck solution-f688049f
  (fn [d]
    (let [n [0 1 2]
          s (set
              (concat
               d
               (apply map list d)
               [(for [a n] (get-in d [a a]))
                (for [a n] (get-in d [(- 2 a) a]))]))]
      (cond
        (s [:x :x :x]) :x
        (s [:o :o :o]) :o
        :e nil))))

(defcheck solution-f7785709
  (fn [[[a b c]
        [d e f]
        [g h i]]]
    (letfn [(win? [p]
              (or (and (= p a) (= p b) (= p c))
                  (and (= p d) (= p e) (= p f))
                  (and (= p g) (= p h) (= p i))
                  (and (= p a) (= p d) (= p g))
                  (and (= p b) (= p e) (= p h))
                  (and (= p c) (= p f) (= p i))
                  (and (= p a) (= p e) (= p i))
                  (and (= p g) (= p e) (= p c))))]
      (cond
        (win? :x) :x
        (win? :o) :o
        :else nil))))

(defcheck solution-f78dbcf1
  (fn [xs]
    (let [x2 (reduce (fn [x j]
                       (conj x (reduce (fn [z i] (conj z ((xs i) j))) [] [0 1 2]))
                       ) [] [0 1 2]
               )
          x3 (reduce (fn [x j]
                       (conj x (reduce (fn [z i]
                                         (conj z ((xs (if (= 0 j) i (- 2 i) ) ) i))
                                         ) [] [0 1 2] ))) [] [0 1])]
      (cond
        (some true? (map (fn [x] (every? #(= :x %) x)) (concat xs x2 x3)))
        :x
        (some true? (map (fn [x] (every? #(= :o %) x)) (concat xs x2 x3)))
        :o
        :else nil
        )
      )

    ))

(defcheck solution-f7c51a
  (fn winner [board] (let [col (apply mapv (fn [& xs] (vec xs)) board)
                           diag [(mapv nth board (range))
                                 (mapv nth board (range 2 -1 -1))]
                           all (concat board col diag)]
                       (cond
                         (some #(= [:x :x :x] %) all) :x
                         (some #(= [:o :o :o] %) all) :o
                         :else nil))))

(defcheck solution-f85629d7
  (fn [x]
    (let [f (flatten x)
          winning-combos [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]
          result (for [[x y z] winning-combos :when
                       (or
                        (= :x (nth f x) (nth f y) (nth f z))
                        (= :o (nth f x) (nth f y) (nth f z)))]
                   (nth f x))]
      (first result))))

(defcheck solution-f88e201
  (fn [r]
    (let [f #(map get % [0 1 2])]
      (first
        (some #{#{:o} #{:x}}
          (map set `(~@r
                     ~@(apply map list r)
                     ~(f r)
                     ~(f (rseq r)))))))))

(defcheck solution-f8ec225
  (letfn [(won? [[a b c]]
            (and (= a b c) (#{:x :o} a)))]
    (fn winner [[[r1c1 r1c2 r1c3 :as r1]
                 [r2c1 r2c2 r2c3 :as r2]
                 [r3c1 r3c2 r3c3 :as r3] :as rows]]
      (let [c1 (map #(% 0) rows)
            c2 (map #(% 1) rows)
            c3 (map #(% 2) rows)
            d1 [r1c1 r2c2 r3c3]
            d2 [r3c1 r2c2 r1c3]
            ]
        (some won? [r1 r2 r3 c1 c2 c3 d1 d2])))))

(defcheck solution-f8fdaf39
  (fn [board]
    (let [winning-positions [[[0 0] [0 1] [0 2]]
                             [[1 0] [1 1] [1 2]]
                             [[2 0] [2 1] [2 2]]
                             [[0 0] [1 0] [2 0]]
                             [[0 1] [1 1] [2 1]]
                             [[0 2] [1 2] [2 2]]
                             [[0 0] [1 1] [2 2]]
                             [[0 2] [1 1] [2 0]]]
          winners (map (fn [position]
                         (let [states (set
                                        (map (fn [[x y]] (get (get board x) y))
                                          position))]
                           (if (= #{:x} states) :x
                                                (if (= #{:o} states) :o
                                                                     nil))))
                    winning-positions)]
      (if (some #{:x} winners) :x
                               (if (some #{:o} winners) :o
                                                        nil)))))

(defcheck solution-f91cc4f1
  (fn [board]
    (letfn [(uniq?
              [s]
              (= (count (set s)) 1))
            (diagonal-cells
              [s]
              (map #((second %) (first %))
                (map-indexed list s)))
            (complete?
              [player]
              (some #(and (uniq? %) (= (first %) player))
                (concat
                 board
                 (apply map list board)
                 [(diagonal-cells board)]
                 [(diagonal-cells (reverse board))])))]
      (some #(if (complete? %) % false) [:x :o]))))

(defcheck solution-f935c97
  (fn [t]
    (->> [[0 0 0] [1 1 1] [2 2 2] [0 1 2] [2 1 0]]
      (map #(map nth t %))
      (concat t)
      (some (fn [r] (some #(when (= [% % %] r) %) [:x :o]))))))

(defcheck solution-f94ecb00
  (fn who-wins? [board]
    (let [wins-h (for [i [0 1 2]] (for [j [0 1 2]] [i j]))
          wins-v (for [i [0 1 2]] (for [j [0 1 2]] [j i]))
          wins-d '(([0 0] [1 1] [2 2]) ([0 2] [1 1] [2 0]))
          wins (concat wins-h wins-v wins-d)
          map-pos (map (fn [w]
                         (map (fn [elt] ((board (elt 0)) (elt 1))) w)) wins)
          winning-pos? (set (map #(if (apply = %) (first %)) map-pos))]
      (cond (winning-pos? :x) :x
            (winning-pos? :o) :o
            :else nil))))

(defcheck solution-f95406f7
  (fn [b]
    (letfn [(row? [b v] (->> (map (partial every? #(= v %)) b) (filter true?) first true?))
            (col? [b v] (row? (->> (first b) count range (map (fn [i] (map #(get % i) b)))) v))
            (diag? [b v] (let [l (count (first b))] (row? [(map #(get-in b [% %]) (range l))
                                                           (map #(get-in b [(- (dec l) %) %]) (range l))]
                                                      v)))]
      (cond (or (row? b :x) (col? b :x) (diag? b :x)) :x
            (or (row? b :o) (col? b :o) (diag? b :o)) :o))))

(defcheck solution-f97b34a8
  (fn __ [st]
    (let [
          allPoss [[1 1 1 0 0 0 0 0 0] [0 0 0 1 1 1 0 0 0] [0 0 0 0 0 0 1 1 1]
                   [1 0 0 1 0 0 1 0 0] [0 1 0 0 1 0 0 1 0] [0 0 1 0 0 1 0 0 1]
                   [1 0 0 0 1 0 0 0 1] [0 0 1 0 1 0 1 0 0]]

          f1x (fn [a b] (if (and (= a 1) (= b :x)) 1 0))
          f1o (fn [a b] (if (and (= a 1) (= b :o)) 11 0))

          ff (fn [v1 v2 fnn] (reduce + (map fnn v1 v2)))

          flat (flatten st)
          xx (> (count (filter #(= 3 %) (map (fn [xx] (ff xx flat f1x)) allPoss))) 0)
          yy (> (count (filter #(= 33 %) (map (fn [xx] (ff xx flat f1o)) allPoss))) 0)
          ]
      (if xx :x (if yy :o nil)))))

(defcheck solution-f9fedab6
  (fn [[[a b c] [d e f] [g h i]]]
    (reduce
      #(if (or (apply not= %2) (= :e (first %2)))
         %1 (first %2))
      nil
      [[a b c] [d e f] [g h i]
       [a d g] [b e f] [c f i]
       [a e i] [c e g]])))

(defcheck solution-f9ff44d9
  #(let [rows (concat (map % (range 3))
                      (map (fn [col]
                             (map (fn [row] ((% row) col))
                               (range 3)))
                        (range 3))
                      [(map (fn [i] ((% i) i))
                         (range 3))
                       (map (fn [i] ((% i) (- 2 i)))
                         (range 3))])]
     (or (and (some (partial = [:x :x :x]) rows) :x)
         (and (some (partial = [:o :o :o]) rows) :o)
         nil)))

(defcheck solution-fa3351b9
  (fn [b]
    (let [cols (apply map vector b)
          d1 [(map (fn [n] (get (get b n) n)) (range 3))]
          d2 [(map (fn [n] (get (get b (- 2 n)) n)) (range 3))]
          l (concat b cols d1 d2)
          x (filter (fn [n] (every? (fn [a] (= a :x)) n)) l)
          o (filter (fn [n] (every? (fn [a] (= a :o)) n)) l)]
      (cond
        (not (empty? x)) :x
        (not (empty? o)) :o
        :else nil))))

(defcheck solution-fa4301bd
  (fn [[a b c]]
    (let [q (into [a b c
                   [(first a) (second b) (last c)]
                   [(first c) (second b) (last a)]] (map vector a b c))
          won? (fn [[x & r]] (when (and (apply = x r) (not= :e x)) x))]
      (first (remove nil? (map won? q))))))

(defcheck solution-fa5927b8
  (fn [[[a b c][d e f][h i j]]]
    (let [l [[a b c][d e f][h i j][a d h][b e i][c f j][a e j][c e h]]
          x (filter #(apply = %) l)]
      (if (and (not-empty x) (not= (ffirst x) :e)) (ffirst x) nil))))

(defcheck solution-fb9cb4e2
  (fn myAnalizeTTT
    [ttt]
    (let [getCrossElementIds (fn [tictactoe] (map vector (take (count tictactoe) (range)) tictactoe))
          getCrossElements (fn [result tttCrosCount] (conj result (nth (second tttCrosCount) (first tttCrosCount))))
          tttv (map set ttt)
          ttth (map set (apply map list ttt))
          ttts1 (set (reduce getCrossElements [] (getCrossElementIds ttt)))
          ttts2 (set (reduce getCrossElements [] (getCrossElementIds (reverse ttt))))]
      (first (flatten (filter #(and (= 1 (count %)) (not (= % [:e]))) (map vec (flatten (list tttv ttth ttts1 ttts2)))))))))

(defcheck solution-fc32b1e9
  (fn [board]
    (let [board (apply concat board)
          tests [[0 3 6] [1 4 7] [2 5 8]
                 [0 1 2] [3 4 5] [6 7 8]
                 [0 4 8] [2 4 6]]
          lines (map #(set (map (partial nth board) %)) tests)
          equal (filter #(and (= (count %) 1) (not= (first %) :e)) lines)]
      (first (first equal)))))

(defcheck solution-fc7a1283
  (fn [[[a1 a2 a3] [b1 b2 b3] [c1 c2 c3]]]
    (let [v (vector (vector a1 a2 a3)
              (vector b1 b2 b3)
              (vector c1 c2 c3)
              (vector a1 b1 c1)
              (vector a2 b2 c2)
              (vector a3 b3 c3)
              (vector a1 b2 c3)
              (vector a3 b2 c1))
          s (set (map set v))]
      (condp #(contains? %2 %1) s
        #{:x} :x
        #{:o} :o
        nil))))

(defcheck solution-fcce15e2
  (fn [a]
    (first (some #{[:o :o :o] [:x :x :x]}
             (concat a (apply map list a)
                     (map
                       #(map (fn [v i] (v i)) a %)
                       [[0 1 2] [2 1 0]]))))))

(defcheck solution-fcf6bc35
  (fn [ [[a b c]
         [d e f]
         [h i j] :as board] ]

    (let
     [test-line (fn [line] (reduce #(when (= %1 %2) %1) line))
      lines  (concat board (apply map vector board) [[a e j] [c e h]])
      scored-lines (map test-line lines) ]

      (some #{:x :o} scored-lines)  )))

(defcheck solution-fcf9d391
  (fn [board]
    (let [winners
          (for [p [:x :o]]
            (some
              (fn [v] (every? #(= p %) v))
              (concat board
                      (apply map vector board)
                      [(reduce #(conj %1 (get-in board [%2 %2])) [] (range 3))]
                      [(reduce #(conj %1 (get-in board [%2 (- 2 %2)])) [] (range 3))]

                      )))]
      (cond (first winners) :x
            (second winners) :o))))

(defcheck solution-fcffa512
  (fn [board]
    (letfn [(transpose [mat]
              (if (= 1 (count mat))
                (map list (first mat))
                (map cons (first mat) (transpose (rest mat)))))]
      (let [won (fn [l] (or (= l [:x :x :x]) (= l [:o :o :o])))
            rows (filter won board)
            cols (filter won (transpose board))]
        (cond
          (not (empty? rows)) (ffirst rows)
          (not (empty? cols)) (ffirst cols)
          (won (map #(nth (nth board %) %) (range 3))) (ffirst board)
          (won (map #(nth (nth (reverse board) %) %) (range 3))) (first (last board))
          )))))

(defcheck solution-fd0d9d3e
  #(let [x (set
             (concat
              %
              (apply map vector %)
              [[(get-in % [0 0]) (get-in % [1 1]) (get-in % [2 2])]]
              [[(get-in % [0 2]) (get-in % [1 1]) (get-in % [2 0])]]
              ))]
     (if (x [:x :x :x]) :x (when (x [:o :o :o]) :o))))

(defcheck solution-fd585805
  (fn [board]
    (letfn [(wins? [player]
              (or (every? #(= (nth % 0) player) board)
                  (every? #(= (nth % 1) player) board)
                  (every? #(= (nth % 2) player) board)
                  (some #(= %
                           (repeat 3 player)
                           )
                    board)
                  (every? identity (map-indexed #(= (nth %2 %) player) board))
                  (every? identity (map-indexed #(= (nth %2 %) player) (reverse board)))))]
      (cond
        (wins? :x) :x
        (wins? :o) :o
        :else nil))))

(defcheck solution-fd6a2a32
  (fn [[a b c]]
    (let [n (first (first (filter (fn [[s t u]] (= s t u))
                            [a b c
                             [(nth a 0) (nth b 0) (nth c 0)]
                             [(nth a 1) (nth b 1) (nth c 1)]
                             [(nth a 2) (nth b 2) (nth c 2)]
                             [(nth a 0) (nth b 1) (nth c 2)]
                             [(nth a 2) (nth b 1) (nth c 0)]])))]
      (if (= n :e) nil n))))

(defcheck solution-fd7bb252
  (let [grps [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]]]
    (fn [game] (let [game (vec (apply concat game))]
                 (first (some #{#{:x} #{:o}} (map #(set (map game %)) grps)))))))

(defcheck solution-fdd754ce
  (fn [ds]
    (let [ r (range 3)
          l (concat
             (map (fn [x] (map #(vector x %) r)) r)
             (map (fn [x] (map #(vector % x) r)) r)
             [[ [0 0] [1 1] [2 2] ] [ [2 0] [1 1] [0 2] ]]) ]
      ( {:x :x :o :o }
       (first
         (map first
           (filter #(= (count %) 1)
             (map distinct (map #(map (fn [[a b]] ((ds a) b)) %) l)))))))))

(defcheck solution-fddb3374
  (fn [board]
    (letfn [(access   [x y]
              (nth (nth board x) y))
            (access-c [z]
              (access (first z) (second z)))
            (normal-or [& xs]
              (cond (empty? xs) nil
                    (first xs) (first xs)
                    :else (recur (rest xs))))
            (ex-equal [& xs]
              (if (and (apply = xs)
                       (not= (first xs) :e))
                (first xs) nil))]
      (apply normal-or
        (map (fn [rule] (apply ex-equal (map access-c rule)))
          '(((0 0) (0 1) (0 2)) ((1 0) (1 1) (1 2)) ((2 0) (2 1) (2 2))
            ((0 0) (1 0) (2 0)) ((0 1) (1 1) (2 1)) ((0 2) (1 2) (2 2))
            ((0 0) (1 1) (2 2)) ((2 0) (1 1) (0 2)))))
      )
    ))

(defcheck solution-fe1f7307
  (fn [board]
    (let [positions [[[0 0] [1 0] [2 0]]
                     [[0 1] [1 1] [2 1]]
                     [[0 2] [1 2] [2 2]]

                     [[0 0] [1 1] [2 2]]
                     [[0 2] [1 1] [2 0]]]
          lines (concat board (map (fn [line] (map #(get-in board %) line)) positions))]
      (some #(and (apply = %) (#{:x :o} (first %))) lines))))

(defcheck solution-ff8eeca7
  (fn tic-tac-toe
    [board]
    (ffirst
      (for [x (range 0 3)
            y (range 0 3)
            :let [routes (conj
                           []
                           (map #(vector (+ x %) y) (range 3))
                           (map #(vector x (+ x %)) (range 3))
                           (map #(vector (+ x %) (+ y %)) (range 3))
                           (map #(vector (- x %) (+ y %)) (range 3)))
                  marks (map #(map (fn [[x y]] (get-in board [y x])) %)
                          routes)
                  win (some #(when (and (not= :e (first %))
                                        (apply = %))
                               %)
                        marks)]
            :when win]
        win))))

(defcheck solution-ff921319
  (fn checkboard [board]
    (let [[[a11 a12 a13] [a21 a22 a23] [a31 a32 a33]] board
          won (into board [[a11 a21 a31] [a12 a22 a32] [a13 a23 a33] [a11 a22 a33] [a31 a22 a13]])]
      (if (seq (filter #(= % [:x :x :x]) won)) :x
                                               (if (seq (filter #(= % [:o :o :o]) won)) :o nil)))))
