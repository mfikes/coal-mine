(ns coal-mine.problem-94
  (:require [coal-mine.checks :refer [defcheck-94] :rename {defcheck-94 defcheck}]
            [clojure.test]
            [clojure.set]))

(defn char->ascii [c]
  #?(:clj (int c) :cljs (.charCodeAt c 0)))

(defcheck solution-101e9f20
  (fn [b]
    (letfn [(ln [x y]
              (->> (for [i [-1 0 1] j [-1 0 1] :when (not= 0 i j)]
                     (get-in b [(+ i x) (+ j y)]))
                (filter #(= \# %)) count))]
      (->> (for [x (range (count b))
                 y (range (count (first b)))]
             ({3 \# 2 (get-in b [x y])} (ln x y) " "))
        (partition (count b))
        (map #(apply str %))))))

(defcheck solution-102ed88c
  (fn [board]
    (let [max-x (count (first board))
          max-y (count board)]
      (letfn [(live?
                [cell]
                (= "#" (:content cell)))
              (neighbour-coordinates
                [cell-x cell-y]
                (->> (for [x [(dec cell-x) cell-x (inc cell-x)]
                           y [(dec cell-y) cell-y (inc cell-y)]]
                       [x y])
                  (filter (fn [pos] (not (= pos [cell-x cell-y]))))
                  (filter (fn [pos] (and (<= (first pos) max-x)
                                         (<= (second pos) max-y))))))
              (neighbours
                [cell]
                (filter (fn [c] (some #{[(:x c) (:y c)]}
                                  (neighbour-coordinates (:x cell) (:y cell)))) (cells)))
              (count-live-neighbours
                [cell]
                (reduce (fn [live-count cell]
                          (if (live? cell) (inc live-count)
                                           live-count)) 0 (neighbours cell)))
              (cells []
                (flatten (map-indexed (fn [row-index row]
                                        (map-indexed (fn [col-index col]
                                                       {:x       col-index
                                                        :y       row-index
                                                        :content (str col)}) row)) board)))]
        (map (partial apply str)
          (partition (count (first board))
            (map (fn [cell]
                   (cond (live? cell)
                         (cond (< (count-live-neighbours cell) 2) " "
                               (< (count-live-neighbours cell) 4) "#"
                               :else " ")
                         (and ((comp not live?) cell)
                              (= (count-live-neighbours cell) 3)) "#"
                         :else " "))
              (cells))))))))

(defcheck solution-10779e5a
  (fn life [b]
    (let [m  (dec (count b))
          n  (dec (count (first b)))
          b  (vec (map vec b))
          a? (fn a? [c]
               (case c
                 \# true
                 false))
          f  (fn f [c & cs]
               (let [ns (map #(get (get b (first %)) (last %)) cs)
                     n  (count (filter a? ns))]
                 (case c
                   \# (case n
                        0 \space
                        1 \space
                        2 \#
                        3 \#
                        \space)
                   (if (= 3 n)
                     \#
                     \space))))]
      (map
        #(apply str %)
        (map-indexed
          (fn x [i r]
            (map-indexed
              (fn y [j c]
                (let [di (dec i) dj (dec j) ii (inc i) ij (inc j)]
                  (cond
                    (= i 0) (cond
                              (= j 0) (f c [i ij] [ii j] [ii ij])
                              (= j n) (f c [i dj] [ii dj] [ii j])
                              :else (f c [i dj] [i ij] [ii dj] [ii j] [ii ij]))
                    (= i m) (cond
                              (= j 0) (f c [di j] [di ij] [i ij])
                              (= j n) (f c [di dj] [di j] [i dj])
                              :else (f c [di dj] [di j] [di ij] [i dj] [i ij]))
                    :else (cond
                            (= j 0) (f c [di j] [di ij] [i ij] [ii j] [ii ij])
                            (= j n) (f c [di dj] [di j] [i dj] [ii dj] [ii j])
                            :else (f c [di dj] [di j] [di ij] [i dj]
                                    [i ij] [ii dj] [ii j] [ii ij])))))
              r))
          b)))))

(defcheck solution-10f3f518
  (fn next-step [xss]
    (letfn [(gen-mat [r c]
              (vec (repeat r (vec (repeat c \space)))))
            (gen-next [cord]
              (let [[r c] cord]
                (for [r' [-1 0 1]
                      c' [-1 0 1]
                      :when (not= 0 r' c')]
                  [(+ r r') (+ c c')])))]
      (let [xss'    (vec (map vec xss))
            r-limit (count xss')
            c-limit (count (first xss'))
            new-bd  (gen-mat r-limit c-limit)]
        (vec (map #(apply str %) (reduce (fn [bd cord]
                                           (case (count
                                                   (filter #(= \# (get-in xss' %))
                                                     (gen-next cord)))
                                             2 (assoc-in bd cord (get-in xss' cord))
                                             3 (assoc-in bd cord \#)
                                             (assoc-in bd cord \space))) new-bd
                                   (for [x (range r-limit)
                                         y (range c-limit)]
                                     [x y]))))))))

(defcheck solution-119ad62
  (fn gol [m]
    (let [dim   (count (first m))
          mid   (loop [matrix m acc []]
                  (if (empty? matrix)
                    (flatten acc)
                    (recur (rest matrix)
                      (conj acc (map #(str %) (first matrix))))))
          ijtrn (fn [dat dim] (if (coll? dat)
                                (if (or (< (first dat) 0) (< (second dat) 0))
                                  -1
                                  (+ (last dat) (* (first dat) dim)))
                                (list (int (/ dat dim)) (mod dat dim))))
          nxst  (fn [i l d]
                  (let [ij    (ijtrn i l)
                        nbrs  [(ijtrn [(first ij) (dec (second ij))] l)
                               (ijtrn [(first ij) (inc (second ij))] l)
                               (ijtrn [(dec (first ij)) (dec (second ij))] l)
                               (ijtrn [(dec (first ij)) (second ij)] l)
                               (ijtrn [(dec (first ij)) (inc (second ij))] l)
                               (ijtrn [(inc (first ij)) (dec (second ij))] l)
                               (ijtrn [(inc (first ij)) (second ij)] l)
                               (ijtrn [(inc (first ij)) (inc (second ij))] l)]
                        nbrsl (apply + (map #(if (= "#" (nth d %)) 1 0)
                                         (filter #(and (> % -1) (< % (count d))) nbrs)))]
                    ;		   (print [i ij nbrs])
                    (if (= "#" (nth d i))
                      (if (or (= nbrsl 2) (= nbrsl 3)) "#" " ")
                      (if (= nbrsl 3) "#" " "))))]
      (->> (map #(nxst % dim mid) (range (count mid)))
        (partition dim)
        (map #(apply str %))))))

(defcheck solution-11d1cb35
  (fn next-board
    [board]
    (let [board-matrix (mapv #(vec %) board)]
      (map-indexed
        (fn [i row]
          (apply str
            (map-indexed
              (fn [j cell]
                (let [neighbours (for [i' (range (dec i) (+ 2 i))
                                       j' (range (dec j) (+ 2 j))
                                       :let [c (get-in board-matrix [i' j'])]
                                       :when (not= [i j] [i' j'])]
                                   c)
                      live-count (count (filter #(= \# %) neighbours))]
                  (cond
                    (and (= cell \space) (= 3 live-count)) \#
                    (and (= cell \#)
                         (or (< live-count 2) (> live-count 3))) \space
                    :else cell)))
              row)))
        board-matrix))))

(defcheck solution-1345f6cf
  (fn [gen]
    (letfn [(cell [s c]
              (let [live   "#"
                    dead   " "
                    n      (count (filter #(= % live) s))
                    alive? (= c live)]
                (cond (and alive? (< n 2)) dead
                      (and alive? (= n 2)) live
                      (and alive? (= n 3)) live
                      (and (not alive?) (= n 3)) live
                      :else dead)))]
      (let [t     [[0 -1] [0 1] [-1 0] [1 0] [-1 -1] [1 1] [-1 1] [1 -1]]
            board (vec (map #(vec (re-seq #"." %)) gen))]
        (for [x (range (count board))]
          (apply str
            (for [y (range (count board))]
              (let [index (map (fn [[a b]] [(+ x a) (+ y b)]) t)
                    n     (map #(get-in board %) index)]
                (cell n (get-in board [x y]))))))))))

(defcheck solution-13561c6
  (fn gol [input]
    (let [b          ((comp dec count) input)
          r          ((comp dec count first) input)
          board      (->> input
                       (mapv (comp (partial into [])
                               #(replace {\space :dead \# :live} %)
                               seq)))
          neighbors  (fn [[i j]]
                       (into [] (for [i' [(dec i) i (inc i)]
                                      j' [(dec j) j (inc j)]
                                      :when (and (<= 0 i' b) (<= 0 j' r) (not= [i j] [i' j']))]
                                  [i' j'])))
          next-state (fn [[status neighbors]]
                       (let [live-neighbors (->> (map (fn [[i j]]
                                                        (nth (nth board i) j)) neighbors)
                                              (filter (partial = :live))
                                              count)]
                         (if (= status :live)
                           (cond (< live-neighbors 2) " "
                                 (<= 2 live-neighbors 3) "#"
                                 :else " ")
                           (cond (= live-neighbors 3) "#"
                                 :else " "))))]
      (->> board
        (map-indexed (fn [i row] (map-indexed (fn [j elem] [elem [i j]]) row)))
        (map (partial map #(vector (first %) (neighbors (last %)))))
        (map (partial map next-state))
        (mapv clojure.string/join)))))

(defcheck solution-13e8d6d1
  (fn lifeStep [board]
    (letfn [(computeCharLoc [[t num]]
              (cond (and (= t \#) (= num 2)) \#
                    (= num 3) \#
                    :else \space
                    )
              )
            (getLocSum [x y]
              (computeCharLoc [(get-in board [x y])
                               (apply + (map #(if (= \# (get-in board %)) 1 0)
                                          [
                                           [(- x 1) (- y 1)]
                                           [(- x 1) y]
                                           [(- x 1) (+ y 1)]
                                           [x (- y 1)]
                                           [x (+ y 1)]
                                           [(+ x 1) (- y 1)]
                                           [(+ x 1) y]
                                           [(+ x 1) (+ y 1)]
                                           ])
                                 )])
              )
            ]

      (let [size (count board)]
        (for [x (range size)] (apply str (for [y (range size)] (getLocSum x y))))
        )
      )
    ))

(defcheck solution-13fb02bf
  (fn [b]
    (let [q (range (count (first b)))
          t (vec (map vec b))
          r (fn [k] (range (dec k) (+ k 2)))]
      (map #(apply str %)
        (reduce
          (fn [u [i j]]
            (update-in u [i j]
              #(let [c (reduce (fn [z k] (+ z (if (= (get-in t k) \#) 1 0))) 0 (for [x (r i) y (r j)] [x y]))]
                 (if (if (= % \#) (and (> c 2) (< c 5)) (= c 3)) \# \space))))
          t (for [i q j q] [i j]))))))

(defcheck solution-1436b9df
  (fn [chessboard]
    (let [nrow      (count chessboard)
          ncol      (count (nth chessboard 0))
          ; &#26681;&#25454;&#19979;&#26631;&#33719;&#21462;&#20803;&#32032;
          get-coord (fn [x y] (nth (nth chessboard x) y))
          ; &#30456;&#37051;&#29983;&#21629;&#30340;&#22352;&#26631;
          coords    (fn [x y] (filter
                                #(let [row-num (first %)
                                       col-num (last %)]
                                   ; &#36807;&#28388;&#36234;&#30028;&#30340;&#22352;&#26631;
                                   (and (>= row-num 0)
                                        (>= col-num 0)
                                        (< row-num nrow)
                                        (< col-num ncol)))
                                ; &#24403;&#21069;&#22352;&#26631;&#21608;&#22260;&#30340;&#20843;&#20010;&#22352;&#26631;&#65288;&#31515;&#21345;&#23572;&#31215;&#65289;
                                (for [xx [(dec x) x (inc x)]
                                      yy [(dec y) y (inc y)]
                                      :when (not (and (= xx x) (= yy y)))]
                                  [xx yy])))
          ; &#30456;&#37051;&#30340;&#29983;&#21629;
          neighbors (fn [x y] (map (partial apply get-coord) (coords x y)))
          ; &#30456;&#37051;&#29983;&#21629;&#29366;&#24577;&#26159;live&#65288;#&#65289;&#30340;&#20010;&#25968;
          nlive     (fn [x y] (count (filter #(= \# %) (neighbors x y))))
          ; &#19979;&#19968;&#36718;&#30340;&#29366;&#24577;&#65288;&#19968;&#32500;&#25968;&#32452;&#65289;
          result    (for [x (range 0 nrow)
                          y (range 0 ncol)]
                      (let [element  (get-coord x y)
                            live-num (nlive x y)]
                        (if (= \# element)
                          ; live&#29366;&#24577;&#30340;&#29983;&#21629;&#21608;&#22260;&#22914;&#26524;&#26377;2&#20010;&#25110;3&#20010;&#29983;&#21629;&#26159;live&#30340;&#65292;&#21017;&#19979;&#36718;&#32487;&#32493;&#23384;&#27963;&#65292;&#21542;&#21017;&#27515;&#20129;
                          (if (or (< live-num 2) (> live-num 3)) \space \#)
                          ; die&#29366;&#24577;&#30340;&#29983;&#21629;&#21608;&#22260;&#22914;&#26524;&#26377;&#21018;&#22909;3&#20010;&#29983;&#21629;&#26159;live&#30340;&#65292;&#21017;&#19979;&#36718;&#22797;&#27963;&#65292;&#21542;&#21017;&#27515;&#20129;
                          (if (= live-num 3) \# \space))))]
      ; &#25226;&#32467;&#26524;&#36716;&#25104;4Clojure&#38656;&#35201;&#30340;&#24418;&#24335;
      (map (partial apply str) (partition ncol result)))))

(defcheck solution-14413871
  (fn [area]
    (letfn [(cp [a b] (reduce #(into % (map (partial vector %2) b)) #{} a))
            (adj [[h w] area]
              (let [dh (dec h) dw (dec w) ih (inc h) iw (inc w)]
                (keep (fn [[h w]] (get (get area h) w)) [[dh dw] [dh w] [dh iw] [h dw] [h iw] [ih dw] [ih w] [ih iw]])))
            (rule [[h w :as p] area]
              (let [l (count (filter #(= % \#) (adj p area)))]
                (condp = (get (get area h) w)
                  \# (if (or (= l 2) (= l 3)) "#" " ")
                  \space (if (= l 3) "#" " "))))
            (ruleapply [area line]
              (reduce #(str % (rule %2 area)) "" line))]
      (let [r (range (count area)), line (group-by first (sort (fn [[_ a] [_ b]] (compare a b)) (cp r r)))]
        (reduce #(conj % (ruleapply area %2)) [] (map line r))))))

(defcheck solution-1452736f
  (fn [board] (letfn [

                      (neighbors [board row col]
                        (let [last-row (-> board count dec)
                              top-row  (if (= 0 row) last-row (dec row))
                              bot-row  (if (= last-row row) 0 (inc row))

                              last-col (-> board first count dec)
                              l-col    (if (= 0 col) last-col (dec col))
                              r-col    (if (= last-col col) 0 (inc col))]
                          [(get (board top-row) l-col) (get (board top-row) col) (get (board top-row) r-col)
                           (get (board row) l-col) (get (board row) r-col)
                           (get (board bot-row) l-col) (get (board bot-row) col) (get (board bot-row) r-col)]))

                      (cell-update [neighbors cell]
                        (let [nalive (count (filter #(= \# %) neighbors))]
                          (cond
                            (or (> 2 nalive) (< 3 nalive)) \space
                            (= 2 nalive) cell
                            (= 3 nalive) \#)))]

                (let [rows (range (count board))
                      cols (range (count (first board)))]
                  (for [i rows]
                    (apply str
                      (for [j cols]
                        (cell-update (neighbors board i j)
                          (get (board i) j)))))))))

(defcheck solution-14b21520
  (fn [m]
    (let [matrix (vec (map vec m))
          row    (count matrix)
          col    (count (first matrix))]
      (letfn [(get [x y] (nth (nth matrix x []) y nil))
              (getneighbor [x y] (frequencies (conj []
                                                (get (dec x) y) ;up
                                                (get (inc x) y) ;down
                                                (get x (dec y)) ;left
                                                (get x (inc y)) ;right
                                                (get (dec x) (dec y)) ;upleft
                                                (get (dec x) (inc y)) ;upright
                                                (get (inc x) (dec y)) ;downleft
                                                (get (inc x) (inc y)) ;downright
                                                )))
              (surviveOrDieOrReborn? [x y]
                (cond
                  (= \space (get x y)) (if (= 3 ((getneighbor x y) \#)) \# \space)
                  (= \# (get x y)) (cond
                                     (< ((getneighbor x y) \#) 2) \space
                                     (= ((getneighbor x y) \#) 2) \#
                                     (= ((getneighbor x y) \#) 3) \#
                                     (> ((getneighbor x y) \#) 3) \space)))]
        (map #(clojure.string/join %) (partition col (for [x (range row) y (range col)] (surviveOrDieOrReborn? x y))))))))

(defcheck solution-14ee7516
  (fn gol [input]
    (letfn [
            (parse [lines]
              (set
                (for [[i line] (map-indexed vector lines)
                      [j char] (map-indexed vector line)
                      :when (= char \#)]
                  [i j])))
            (adjacents [[i j]]
              (set
                (for [di [-1 0 1]
                      dj [-1 0 1]
                      :when (not (= 0 di dj))]
                  [(+ i di) (+ j dj)])))
            (neighbors [world cell]
              (count
                (clojure.set/intersection world (adjacents cell))))
            (format [rows cols cells]
              (for [i (range rows)]
                (apply str
                  (for [j (range cols)]
                    (if (contains? cells [i j]) \# \space)))))]
      (let [rows      (count input)
            cols      (count (first input))
            cells     (for [i (range rows)
                            j (range cols)]
                        [i j])
            current   (parse input)
            survivors (filter #(<= 2 (neighbors current %) 3)
                        current)
            newborns  (filter #(= 3 (neighbors current %))
                        cells)
            next-gen  (set (concat survivors newborns))]
        (format rows cols next-gen)))))

(defcheck solution-153fdae2
  (fn [b]
    (let [clean-board
                 (fn [b]
                   (vec (repeat (count b) (vec (repeat (count (b 0)) \space)))))

          neighbors
                 (fn [b [r c]]
                   (let [s [[(dec r) (dec c)] [(dec r) c] [(dec r) (inc c)]
                            [r (dec c)] [r (inc c)]
                            [(inc r) (dec c)] [(inc r) c] [(inc r) (inc c)]]]
                     (map (fn [x] (get-in b x)) s)))

          live-neighbors
                 (fn [b [r c]]
                   (count (filter (fn [x] (= x \#)) (neighbors b [r c]))))

          alive?
                 (fn [b [r c]]
                   (= \# (get-in b [r c])))

          lives? (fn [b [r c]]
                   (let [alive (alive? b [r c]) nc (live-neighbors b [r c])]
                     (cond
                       (and (true? alive) (<= 2 nc 3)) true
                       (and (false? alive) (= 3 nc)) true
                       :else false)))]
      (->>
        (for [r (range (count b)) c (range (count (b 0))) :when (lives? b [r c])]
          [r c])
        (reduce (fn [acc x] (assoc-in acc x \#)) (clean-board b))
        (map (fn [x] (apply str x)))))))

(defcheck solution-154dd4e0
  (fn game-of-life
    [board]
    (let [max_row (dec (count board))
          max_col (dec (count (first board)))]
      (letfn [(get-xy [x y]
                (if (or (or (< x 0) (< y 0))
                        (or (> x max_row) (> y max_col)))
                  " "
                  (nth (board x) y)))

              (count-neighbors [x y]
                (count (filter #(= \# %)
                         (for [f1 [inc dec identity]
                               f2 [inc dec identity]
                               :when (not (and
                                           (= f1 identity)
                                           (= f2 identity)))]
                           (get-xy (f1 x) (f2 y))))))

              (evolve-cell [x y]
                (let [n    (count-neighbors x y)
                      cell (get-xy x y)]
                  (cond
                    (= cell \#)                             ;if alive
                    (cond
                      (< n 2) \space
                      (< n 4) \#
                      :else \space)
                    (= cell \space)                         ;if dead
                    (cond
                      (= n 3) \#
                      :else \space))))]

        (for [row (range (inc max_row))]
          (apply str (for [col (range (inc max_col))] (evolve-cell row col))))))))

(defcheck solution-15b80c29
  (fn [b]
    (let [
          lx (count b) ly (count (first b))
          dv [-1 0 1]
          lc (set (mapcat identity (map-indexed (fn [i t] (keep-indexed (fn [j s] (when (= \# s) [i j])) t)) b)))
          gn (fn [[x y]] (dissoc (zipmap (filter #(and (<= 0 (% 0) lx) (<= 0 (% 1) ly)) (mapcat #(map (fn [d] [(+ d x) (+ % y)]) dv) dv)) (repeat 1)) [x y]))
          nc (reduce #(merge-with + %1 (gn %2)) {} lc)
          ]
      (for [x (range lx)]
        (apply str (for [y (range ly)]
                     (cond (and (= 2 (nc [x y])) (lc [x y])) \#
                           (= 3 (nc [x y])) \#
                           :else \space)))))))

(defcheck solution-1611720
  (fn g [a]
    (let [dim   (count a)
          cells (for [x (range dim)
                      y (range dim)
                      :let [curr [x y]]
                      :when (= \# (get-in a curr))]
                  curr)
          n     (mapcat (fn [[x y]]
                          (for [dx [-1 0 1]
                                dy [-1 0 1]
                                :let [curr [(+ dx x) (+ dy y)]]
                                :when (not= dx dy 0)]
                            curr)) cells)
          live  (->> n
                  (frequencies)
                  (filter
                    (fn [[k v]]
                      (or (= 3 v)
                          (and (= 2 v)
                               (not-empty (filter #{k} cells))))))
                  (keys))
          nxt   (reduce
                  #(assoc-in %1 %2 \#)
                  (vec (repeat dim (vec (repeat dim \space))))
                  live)]
      (mapv (partial apply str) nxt))))

(defcheck solution-164eec0d
  (fn [state]
    (let [ymax           (count state)
          xmax           (count (first state))
          valid-pos?     (fn [[x y]]
                           (and (>= x 0)
                                (>= y 0)
                                (< x xmax)
                                (< y ymax)))
          differences    (fn [x y]
                           (for [xdiff [1 0 -1]
                                 ydiff [1 0 -1]
                                 :when (not= 0 xdiff ydiff)
                                 ]
                             [(+ x xdiff) (+ y ydiff)]))
          alive?         (fn [[x y]]
                           (= \# (get (state y) x)))
          neighbors      (fn [x y]
                           (filter valid-pos? (differences x y)))
          live-neighbors (fn [x y]
                           (filter alive? (neighbors x y)))
          next           (fn [x y]
                           (let [neighbor-count (count (live-neighbors x y))
                                 current-value  (get (state y) x)
                                 ]
                             (cond (= neighbor-count 3)
                                   "#"

                                   (and (alive? [x y])
                                        (= neighbor-count 2))
                                   "#"

                                   :else
                                   " ")))]
      (for [y (range ymax)]
        (clojure.string/join (map #(next % y) (range xmax))))

      )))

(defcheck solution-16cf24d1
  (fn [board]
    (letfn [(next-state [is-alive? num-neighbors]
              (if (or (and is-alive?
                           (<= 2 num-neighbors 3))
                      (= 3 num-neighbors))
                \#
                \space))
            (alive? [board cell] (#{\#} (get-in board cell)))
            (live-neighbor-count [board cell]
              (let [neighbor-locations (for [x [1 0 -1]
                                             y [1 0 -1]
                                             :when (not= [x y] [0 0])]
                                         (map + cell [x y]))]
                (->> neighbor-locations
                  (map #(get-in board %))
                  (filter #{\#})
                  count)))]
      (->> (for [x (range (count board))
                 y (range (count (first board)))]
             [x y])
        (map (juxt (partial alive? board)
               (partial live-neighbor-count board)))
        (map (partial apply next-state))
        (partition (count (first board)))
        (map (partial apply str))))))

(defcheck solution-1786b222
  (fn [world]
    (let [reproduce (fn [x y]
                      (let [neighbours (for [i [-1 0 1]
                                             j [-1 0 1]
                                             :when (not= [i j] [0 0])]
                                         [(+ i x) (+ j y)])
                            current    (get-in world [x y])
                            live-cells (->> (map #(get-in world %) neighbours)
                                         (filter #(= % \#))
                                         count)]
                        (if (if (= current \#)
                              (<= 2 live-cells 3)
                              (= live-cells 3))
                          \#
                          \space)))
          h         (count world)
          w         (count (first world))]
      (->> (for [i (range 0 h) j (range 0 w)] [i j])
        (map #(reproduce (first %) (second %)))
        (partition w)
        (mapv #(apply str %))))))

(defcheck solution-17f0a568
  (fn game-of-life [data]
    (letfn [(valid-range [min max num]
              (->> (range (dec num) (+ 2 num))
                (drop-while #(> min %))
                (take-while #(> max %))
                (into [])))

            (neighbors [x y data]
              (let [xs (valid-range 0 (count (first data)) x)
                    ys (valid-range 0 (count data) y)]
                (for [x' xs y' ys
                      :when (not (and (= y' y) (= x' x)))]
                  [x' y'])))

            (alive-neighbors [x y data]
              (->> (neighbors x y data)
                (map (fn [[x y]] (nth (nth data y) x)))
                (filter #(= % \#))
                count))]
      (into
        []
        (map-indexed
          (fn [y row]
            (let [v (map-indexed
                      (fn [x char]
                        (let [a (alive-neighbors x y data)]
                          (cond
                            (or (< a 2) (> a 3)) \space
                            (and (= char \space) (= a 3)) \#
                            :else char)))
                      row)]
              (apply str v)))
          data)))))

(defcheck solution-1965eea3
  (fn [b] (let [cells  (into #{} (for [[x rs] (map-indexed vector b)
                                       [y v] (map-indexed vector rs)
                                       :when (= v \#)]
                                   [x y]))
                neigh  (fn [[x y]]
                         (count (for [p [[(dec x) (dec y)] [(dec x) y] [x (dec y)]
                                         [(dec x) (inc y)] [(inc x) (dec y)]
                                         [x (inc y)] [(inc x) y] [(inc x) (inc y)]]
                                      :when (contains? cells p)] nil)))
                mutate (fn [p v]
                         (let [n (neigh p)]
                           (cond (and (= v \#) (< n 2)) \space
                                 (and (= v \#) (<= n 3)) \#
                                 (and (= v \#) (> n 3)) \space
                                 (and (= v \space) (= n 3)) \#
                                 :else v
                                 )))]
            (vec (map-indexed (fn [x rs] (apply str (map-indexed (fn [y v] (mutate [x y] v)) rs))) b)))))

(defcheck solution-1aa2c360
  (let [adjacent8 (fn [[x y]]
                    (remove #{[x y]}
                      (for [dx (range -1 2) dy (range -1 2)] [(+ x dx) (+ y dy)])))
        cell-get  (fn [s [x y]] (get (get s y []) x nil))
        width     #(count (first %))
        height    count]
    (fn [s]
      (for [y (range (height s))]
        (apply str
          (for [x (range (width s))]
            (let [c (cell-get s [x y])
                  n (count (filter #{\#} (map (partial cell-get s) (adjacent8 [x y]))))]
              (if (= c \#)
                (if (or (= n 2) (= n 3)) \# \space)
                (if (= 3 n) \# \space)))))))))

(defcheck solution-1abc8705
  (fn [m]
    `(~(first m)
      ~@(map
          (fn [v]
            (apply
              str
              `(\space
                ~@(for [i (range 1 (dec (count m)))]
                    (let [x (count (filter (partial = \#) (mapcat #(subs % (dec i) (+ 2 i)) v)))]
                      (if (or (= x 3)
                              (and (= \# (nth (second v) i))
                                   (= x 4)))
                        \# \space)))
                \space)))
          (partition 3 1 m))
      ~(first m))))

(defcheck solution-1acdcb46
  (fn [s]
    (let [xc (count (first s)) yc (count s)]
      (letfn [(t [n m]
                (filter #(and (>= % 0) (< % m)) [n (dec n) (inc n)]))
              (n [[x y]]
                (let [xs (t x xc) ys (t y yc)]
                  (filter #(not= [x y] %)
                    (for [x xs y ys] [x y]))))
              (c [[x y]] ((vec (s y)) x))
              (l [ns] (count (filter #(= % \#) (map c ns))))
              (nc [c n]
                (if (= c \#)
                  (cond (or (< n 2) (> n 3)) " " :else "#")
                  (cond (= n 3) "#" :else " ")))]
        (for [y (range 0 yc)]
          (apply str (for [x (range 0 xc)]
                       (let [ns (n [x y])] (nc (c [x y]) (l ns))))))))))

(defcheck solution-1b046478
  (fn lifestep [grid]
    (let [neighbors [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
          m         (count grid)
          n         (count (first grid))]
      (letfn [(add-points [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
              (live? [[x y]] (= \# (nth (nth grid x) y)))
              (live-neighbors [[x y :as p]]
                (if (or (= x 0) (= x (dec m)) (= y 0) (= y (dec n))) 0
                                                                     (reduce + (map #(if (live? %) 1 0) (map #(add-points p %) neighbors)))))
              (step [] (map (fn [p] (let [n (live-neighbors p)]
                                      (if (live? p)
                                        (if (and (>= n 2) (<= n 3)) \# \space)
                                        (if (= n 3) \# \space))))
                         (for [x (range 0 m) y (range 0 n)] [x y])))]
        (map #(apply str %) (partition n (step)))))))

(defcheck solution-1bf7de1
  (fn life [b]
    (let [c (set (filter identity
                   (for [i (range (count b))
                         j (range (count (first b)))]
                     (if (= \# (nth (vec (nth b i)) j))
                       [i j]
                       false))))
          n (filter identity
              (for [i [inc identity dec]
                    j [inc identity dec]
                    k c]
                (if (= i j identity) false
                                     [(i (first k)) (j (second k))])))
          w (set (concat (map first (filter #(= 3 (second %)) (frequencies n)))
                   (filter #(= (get (frequencies n) %) 2) c)))]
      (for [i (range (count b))]
        (apply str (for [j (range (count (first b)))]
                     (if (contains? w [i j]) \# \space)))))))

(defcheck solution-1d6bbac9
  (fn [board]
    (letfn [(neighbor [board p]
              (let [a (first p) b (second p)]
                (filter #(let [c (first %) d (second %)]
                           (and (>= c 0) (>= d 0)
                                (< c (count board)) (< d (count (first board)))))
                  (for [i (range (dec a) (+ a 2)) j (range (dec b) (+ b 2)) :when (not= [i j] p)]
                    [i j]))))
            (live-cell-number [board p]
              (apply + (map #(if (= \# (get-in board %)) 1 0) (neighbor board p))))
            (next-gen [board p]
              (let [v (live-cell-number board p)]
                (if (= \# (get-in board p))
                  (cond (< v 2) \space
                        (> v 3) \space
                        :else \#)
                  (if (= v 3) \# \space))))]
      (map (fn [i]
             (apply str
               (map #(next-gen board [i %]) (range (count (first board))))))
        (range (count board))))))

(defcheck solution-1d6cdf09
  (fn [b]
    (let [m (mapv (fn [s] (mapv #(-> % first char->ascii (- 32) (/ 3)) (re-seq #"[ |#]" s))) b)
          n (fn [[a b]] (reduce + (- 0 (get-in m [a b]))
                          (map #(get-in m % 0)
                            (for [x [-1 0 1]
                                  y [-1 0 1]]
                              [(+ x a) (+ y b)]))))
          l #(let [v (n %2)]
               (cond (< v 2) 0
                     (> v 3) 0
                     (= v 3) 1
                     (= v 2) %))
          w (count (first m))]
      (map #(apply str %) (partition w
                            (for [x (range w)
                                  y (range (count m))]
                              (if (zero? (l (get-in m [x y]) [x y])) " " "#")))))))

(defcheck solution-1db394d8
  (fn __ [board]
    (let [m (count (first board))
          n (count board)]
      (letfn [(cell [board x y] (when (and (>= x 0) (>= y 0) (< x m) (< y n)) (nth (nth board y) x)))
              (alive? [cell] (= cell \#))
              (neighbors [board x y] (for [dx [-1 0 1] dy [-1 0 1] :when (not (and (zero? dx) (zero? dy)))]
                                       (cell board (+ x dx) (+ y dy))))
              (live-neighbors [board x y] (count (filter alive? (neighbors board x y))))]
        (vec
          (for [y (range n)]
            (apply str (for [x (range m)]
                         (let [l (live-neighbors board x y)]
                           (if (alive? (cell board x y))
                             (if (or (= 2 l) (= 3 l)) \# \space)
                             (if (= 3 l) \# \space)))))))))))

(defcheck solution-1e66be98
  (fn prob-0094
    [layout]

    (letfn [(layout-to-game
              [layout]
              {:dims [(count layout) (apply max (map #(count %) layout))]

               :men  (into (sorted-set) (for [ri (range (count layout))
                                              :let [rw (vec (layout ri))]
                                              ci (range (count rw))
                                              :when (= \# (rw ci))]
                                          [ri ci]))})

            (add-vv
              [v1 v2]
              (vec (map #(+ %1 %2) v1 v2)))

            (life-next-gen
              [{:keys [dims men] :as game}]
              (let [deltas  (filter #(not= [0, 0] %) (map vec (for [ro (range -1 2)
                                                                    co (range -1 2)]
                                                                [ro co])))

                    nxt-gen (into (sorted-set) (for [ri (range (first dims))
                                                     ci (range (second dims))
                                                     :let [n-idxs (map #(add-vv [ri ci] %) deltas)
                                                           n-cnt  (count (filter #(contains? men %) n-idxs))
                                                           pos    [ri ci]]
                                                     :when (or
                                                            (= 3 n-cnt)
                                                            (and (= 2 n-cnt) (contains? men pos)))]
                                                 pos))]
                (assoc game :men nxt-gen)))

            (game-to-layout
              [{:keys [dims men] :as game}]
              (into [] (for [ri (range (first dims))]
                         (apply str (for [ci (range (second dims))]
                                      (if (contains? men [ri ci]) \# \space))))))
            ]

      (game-to-layout (life-next-gen (layout-to-game layout))))))

(defcheck solution-1e781de2
  (fn [bd]
    (let [dead?      #(= \  %)
          live?      #(= \# %)
          getXY      (fn [x y]
                       (nth (nth bd y) x))
          xLen       (count (first bd))
          yLen       (count bd)
          neighbours [[0 1] [0 -1] [1 0] [-1 0]
                      [-1 -1] [-1 1] [1 -1] [1 1]]
          clamp      (fn [[x y]]
                       (if (and (> x 0)
                                (> y 0)
                                (< x xLen)
                                (< y yLen))
                         (vector x y)))]
      (for [y (range yLen)]
        (apply str
          (for [x (range xLen)]
            (let [numLiveNeighbours (count
                                      (filter (fn [[x y]] (live? (getXY x y)))
                                        (keep clamp
                                          (map (fn [[a b]] (vector (+ x a) (+ y b))) neighbours))))]
              (if (live? (getXY x y))
                (if (and (>= numLiveNeighbours 2)
                         (<= numLiveNeighbours 3))
                  \#
                  \ )
                (if (= numLiveNeighbours 3)
                  \#
                  \ )))))))))

(defcheck solution-1ecf9cda
  (fn gol-step [board]
    (letfn [(cell [x y]
              (get (board y) x))
            (out-of-bounds? [x y] (let [h (count board)
                                        w (count (first board))]
                                    (or (< x 0) (< y 0) (>= x w) (>= y h))))
            (neighbours [x y]
              (filter #(not (apply out-of-bounds? %))
                [[(- x 1) (- y 1)] [x (- y 1)] [(+ x 1) (- y 1)]
                 [(- x 1) y],,,,,,,,,,, [(+ x 1) y]
                 [(- x 1) (+ y 1)] [x (+ y 1)] [(+ x 1) (+ y 1)]]))
            (step-cell [x y]
              (let [neighbour-cells  (map #(apply cell %) (neighbours x y))
                    alive-neighbours (count (filter (partial = \#) neighbour-cells))]
                (if (= \# (cell x y))
                  (cond
                    (< alive-neighbours 2) \space
                    (< alive-neighbours 4) \#
                    :else \space)
                  (if (= alive-neighbours 3) \# \space))))]
      (map-indexed
        (fn [j row]
          (apply str
            (map-indexed (fn [i _]
                           (step-cell i j)) row))) board))))

(defcheck solution-1ef6282f
  (fn [board]
    (let [neighber-idxes [[-1 -1] [-1 0] [-1 1] [1 1] [1 0] [1 -1] [0 1] [0 -1]]
          get-neighbers  (fn [board idx-x idx-y]
                           (->> neighber-idxes
                             (map (fn [[x y]]
                                    (get-in board [(+ x idx-x) (+ y idx-y)])))
                             (remove nil?)
                             (frequencies)
                             ((fn [m] (merge {\space 0 \# 0} m)))))]
      (->> board
        (map-indexed (fn [idx-x row]
                       (->> row
                         (map-indexed (fn [idx-y cell]
                                        (let [cur       (get-in board [idx-x idx-y])
                                              neighbers (get-neighbers board idx-x idx-y)
                                              lives     (neighbers \#)
                                              deads     (neighbers \space)]
                                          ;;;   (prn "lives" lives "deads" deads)
                                          (cond
                                            (and (= cur \#) (< lives 2)) \space
                                            (and (= cur \#) (< 1 lives 4)) \#
                                            (and (= cur \#) (< 3 lives)) \space
                                            (and (= cur \space) (= 3 lives)) \#
                                            :else \space))))
                         (clojure.string/join))))
        ))))

(defcheck solution-1f163715
  (fn [ss]
    (let [alive \#, dead \space, x-size (count (first ss)), y-size (count ss)]
      (letfn [
              (cell [i j] (if (or (< i 0) (< j 0) (>= i x-size) (>= j y-size)) nil
                                                                               (nth (nth ss j) i)))
              (nbrs [i j]
                (filter identity (list (cell (inc i) j), (cell i (inc j)), (cell (inc i) (inc j)),
                                   (cell (dec i) j), (cell i (dec j)), (cell (dec i) (dec j)),
                                   (cell (inc i) (dec j)), (cell (dec i) (inc j)))))
              (live? [x] (= x alive))
              (n-live-nbrs [i j] (count (filter live? (nbrs i j))))
              (next [i j]
                (if (live? (cell i j))
                  (case (n-live-nbrs i j)
                    2 alive
                    3 alive
                    dead)
                  (if (= 3 (n-live-nbrs i j)) alive dead)))
              (convert [n c] (map #(apply str %) (partition n c)))]
        (convert x-size
          (mapcat (fn [j] (map (fn [i] (next i j)) (range x-size))) (range y-size)))
        ))))

(defcheck solution-1f3b773
  (fn [state]
    (letfn
     [(index-state [state] (map-indexed (fn [x row] (map-indexed (fn [y e] [e [x y]]) row)) state))
      (filter-live [indexed-state] (mapcat (fn [indexed-row] (filter #(= \# (first %)) indexed-row)) indexed-state))
      (legal-neighbours [[x y] n-row n-col] (let [dx (dec x) ix (inc x) dy (dec y) iy (inc y)] (filter (fn [[a b]] (and (>= a 0) (< a n-row) (>= b 0) (< b n-col))) [[dx dy] [dx y] [dx iy] [x dy] [x iy] [ix dy] [ix y] [ix iy]])))
      (life-impact [n-row n-col live-indexed] (into {} (map (fn [[a b]] [a (count b)]) (group-by identity (mapcat (fn [[_ idx]] (legal-neighbours idx n-row n-col)) live-indexed)))))
      (convert [cell-state nn] (let [alive? (= \# cell-state) n2? (= 2 nn) n3? (= 3 nn)] (if (or (and alive? (or n2? n3?)) (and (not alive?) n3?)) \# \space)))]
      (let
       [indexed-state (index-state state)
        impact        (life-impact (count state) (count (first state)) (filter-live indexed-state))]
        (map (fn [indexed-row] (apply str (map (fn [[cell idx]] (convert cell (get impact idx 0))) indexed-row))) indexed-state)))))

(defcheck solution-1f561578
  #(vec (map (partial apply str)
          (for [i (range (count %))]
            (for [j (range (count (first %)))]
              (let [c (count (filter (partial = \#) (map (partial get-in %) (vec (map vec
                                                                                   (for [a [(dec i) i (inc i)]
                                                                                         b [(dec j) j (inc j)]
                                                                                         :when (not (and (= a i)
                                                                                                         (= b j)))]
                                                                                     [a b]))))))]
                (case (get-in % [i j])
                  \  (case c 3 \# \ )
                  (case c
                    2 \#
                    3 \#
                    \ ))))))
     ))

(defcheck solution-1f5b7917
  (fn [board]
    (letfn [(alive? [[x y]] (and (>= x 0)
                                 (>= y 0)
                                 (< y (count board))
                                 (< x (count (first board)))
                                 (= (nth (nth board y) x) \#)))

            (living-neighbors [[x y]] (count (filter #(and (alive? %)
                                                           (not (= % [x y])))
                                               (for [x-offset (range -1 2)
                                                     y-offset (range -1 2)]
                                                 [(+ x x-offset) (+ y y-offset)]))))]
      (map #(apply str (map (fn [x] (if (or (and (= (living-neighbors [x %]) 2)
                                                 (alive? [x %]))
                                            (= (living-neighbors [x %]) 3))
                                      \#
                                      \ ))
                         (range (count (nth board %)))))
        (range (count board))))))

(defcheck solution-200303e9
  (fn [b]
    (let [[rows cols] [(count b) (count (first b))]
          cells   (for [y (range rows) x (range cols)] [y x])
          neigh   (fn [[y x]]
                    (let [xi (mod (inc x) cols)
                          xd (mod (dec x) cols)
                          yi (mod (inc y) rows)
                          yd (mod (dec y) rows)]
                      (set
                        (for [xx [x xi xd] yy [y yi yd] :when (not (= [x y] [xx yy]))]
                          [yy xx]))))
          counts  (fn [pos] (frequencies (map (fn [a] (get-in b a)) (neigh pos))))
          newcell (fn [pos]
                    (let [me (get-in b pos)
                          {d \space a \#} (counts pos)]
                      #_(println pos "." me "." d "." a ".")
                      (if (= me \space) (if (= a 3) \# \space)
                                        (cond
                                          (< a 2) \space (> a 3) \space :else \#))))]
      #_(println cols rows)
      #_(println (newcell (second cells)))
      #_(println (neigh [3 3]) (counts [3 3]))
      (map (partial apply str)
        (partition cols
          (map newcell cells))))))

(defcheck solution-20582987
  (fn [board]
    (let [
          prod            (fn [a b]
                            (apply concat (map #(map (fn [x] [% x]) b) a)))
          ofs             (disj (set
                                  (prod (range -1 2) (range -1 2))) [0 0])
          cell            (fn [[y x]]
                            (get (get board y) x))
          neighbours      (fn [y x]
                            (map (fn [[j i]] [(+ j y) (+ i x)]) ofs))
          live?           (fn [c] (= \# c))
          live-neighbours (fn [y x]
                            (count (filter #(live? (cell %)) (neighbours y x))))
          next-state      (fn [current nlives]
                            (if (= current \#)
                              (if (< nlives 2) " "
                                               (if (<= 2 nlives 3) "#"
                                                                   " "))
                              ;else
                              (if (= nlives 3) "#" " ")))]
      (map (fn [y] (clojure.string/join
                     (map (fn [x]
                            (next-state (cell [y x]) (live-neighbours y x)))
                       (range (count (board 0))))))
        (range (count board))))))

(defcheck solution-2068408a
  (fn nextboard [board]
    (letfn [
            (setlive [board [x y]]
              (for [r (range (count board))]
                (if (= r x)
                  (str (subs (nth board r) 0 y) \# (subs (nth board r) (inc y)))
                  (nth board r)
                  ))
              )
            (toboard [board cells]
              (loop [b board c cells]
                (if-let [s (seq c)]
                  (recur (setlive b (first s)) (rest s))
                  b))
              )
            (neighbors [[x y]]
              (for [dx [-1 0 1] dy [-1 0 1]
                    :when (not (and (zero? dx) (zero? dy)))]
                [(+ x dx) (+ y dy)])
              )
            (tocells [board]
              (set (for [i (range (count board)) j (range (count board)) :when (= (nth (nth board i) j) \#)]
                     [i j]))
              )]
      (let [cells      (tocells board)
            emptyBoard (repeat (count (first board)) (first board))]
        (toboard emptyBoard (set (for [[cell n] (frequencies (mapcat neighbors cells))
                                       :when (or (= n 3) (and (= n 2) (cells cell)))]
                                   cell)
                              ))))))

(defcheck solution-2087c1ed
  (fn [g]
    (let [c (count g)
          r (range c)]
      (map #(apply str %)
        (partition c
          (for [x r y r]
            (let
             [n (apply +
                  (for [i r j r :when (and (#{(- x 1) x (+ x 1)} i) (#{(- y 1) y (+ y 1)} j) (not= [i j] [x y]))]
                    (if (= (get (g i) j) \#) 1 0)))]
              (if (or (and (= (get (g x) y) \#) (#{2 3} n)) (= 3 n)) \# \space))))))))

(defcheck solution-21045d27
  (fn [b]
    (let [n         (count b)
          valid?    #(<= 0 % (dec n))
          nbr-count (fn [x y]
                      (->> (for [i (range (dec x) (+ x 2))
                                 j (range (dec y) (+ y 2))
                                 :when (and (not= [i j] [x y]) (valid? i) (valid? j))]
                             [i j])
                        (keep #(if (= \# (get-in b %)) %))
                        count))
          next-val  (fn [x y]
                      (condp (fn [[c f] [v nc]] (and (= c v) (f nc)))
                             [(get-in b [x y]) (nbr-count x y)]
                        [\# #(< % 2)] \space
                        [\# #(<= 2 % 3)] \#
                        [\# #(> % 3)] \space
                        [\space #(= % 3)] \#
                        \space))]
      (map #(apply str (map (partial next-val %) (range n))) (range n)))))

(defcheck solution-2181264e
  (fn [b]
    (letfn [(n [r c]
              (for [i (range -1 2)
                    j (range -1 2)
                    :when (not= 0 i j)]
                (get-in b [(+ r i) (+ c j)])))]
      (map #(apply str %)
        (partition (count (first b))
          (for [i (range (count b))
                j (range (count (first b)))]
            (let [l (count (filter #(= \# %) (n i j)))]
              (cond
                (and (= l 2)
                     (= \# (get-in b [i j]))) \#
                (= l 3) \#
                :else \space))))))))

(defcheck solution-2245601b
  (fn
    [board]
    (let [height (count board)
          width  (count (first board))]
      (letfn [(get-input-char [row col]
                (get (board row) col))
              (get-neighbor-coords [row col]
                (for [rdelt (range -1 2)
                      cdelt (range -1 2)
                      :when (or (not= rdelt 0) (not= cdelt 0))
                      :let [row' (+ row rdelt)
                            col' (+ col cdelt)]
                      :when (and (>= row' 0)
                                 (>= col' 0)
                                 (< row' height)
                                 (< col' width))]
                  [row' col']))
              (cell-alive [row col]
                (= \# (get-input-char row col)))
              (cell-alive-next-generation [row col]
                (let [nlc        (num-live-neighbors row col)
                      curr-alive (= \# (get-input-char row col))]
                  (or (= 3 nlc) (and (= 2 nlc) curr-alive))))
              (num-live-neighbors [row col]
                (let [ncs (get-neighbor-coords row col)]
                  (reduce (fn [a [row col]]
                            (if (cell-alive row col)
                              (inc a)
                              a)) 0 ncs)))
              (r [a row col]
                (lazy-seq
                  (if (>= row height)
                    nil
                    (let [a' (conj a (if (cell-alive-next-generation row col)
                                       \#
                                       \space))]
                      (if (= col (dec width))
                        (cons a' (r [] (inc row) 0))
                        (r a' row (inc col)))))))]
        (map (partial apply str) (r [] 0 0))))))

(defcheck solution-225a01e0
  (fn [curr-field]
    (let
     [
      is-alive                 (fn [life-state neighbours]
                                 (if
                                  (= neighbours 3)
                                   true
                                   (and
                                    life-state
                                    (= neighbours 2))))
      mask                     (fn [boolyph]
                                 (if
                                  boolyph
                                   \#
                                   \ ))
      list-possible-neighbours (fn [coord-x coord-y x-count y-count]
                                 (set (for [
                                            x (range
                                                (- coord-x 1)
                                                (+ coord-x 2))
                                            y (range
                                                (- coord-y 1)
                                                (+ coord-y 2))
                                            :when (and
                                                   (>= x 0)
                                                   (< x x-count)
                                                   (< y y-count)
                                                   (>= y 0)
                                                   (not
                                                     (and
                                                      (= x coord-x)
                                                      (= y coord-y))))
                                            ]
                                        [x y])))
      list-neighbours          (fn [field x y]
                                 (->> (list-possible-neighbours
                                        x
                                        y
                                        (count
                                          (first field))
                                        (count field))
                                   (filter (fn [[x y] & coord]
                                             (= \#
                                               (nth
                                                 (nth
                                                   field
                                                   y)
                                                 x))))
                                   set))
      exec                     (fn
                                 [curr-field]
                                 (map-indexed
                                   (fn [y cy]
                                     (clojure.string/join (map-indexed
                                                            (fn [x rx]
                                                              (mask (is-alive
                                                                      (= rx \#)
                                                                      (count (list-neighbours
                                                                               curr-field
                                                                               x
                                                                               y)))))
                                                            cy)))
                                   curr-field))
      ]
      (exec curr-field))))

(defcheck solution-22b7d99d
  (fn game-of-life [board]
    (let [neighbors      [[0 -1] [0 1] [-1 0] [1 0] [-1 -1] [1 1] [-1 1] [1 -1]]
          neighbor-count (fn [y x]
                           (count (filter #(= % \#)
                                    (map (fn [[step-y step-x]] (get-in board [(+ y step-y) (+ x step-x)])) neighbors))))]

      ;(println neighbors)
      (vec (map-indexed
             (fn [y s]
               (apply str (map-indexed
                            (fn [x c]
                              (let [n-count (neighbor-count y x)]
                                (cond
                                  (and (= c \#) (< n-count 2)) \space
                                  (and (= c \#) (or (= n-count 2) (= n-count 3))) \#
                                  (and (= c \#) (> n-count 3)) \space
                                  (and (= c \space) (= n-count 3)) \#
                                  :else \space)))
                            s)))
             board)))))

(defcheck solution-2356d913
  (fn [board]
    (let [gen1 (vec (map (fn [x] (vec (map #(if (= % \space) 0 1) x))) board)) m (count gen1) n (count (first gen1))]
      (letfn [(count-neib [i j] (apply + (filter #(not (nil? %)) (map #(get-in gen1 %)
                                                                   (for [x (range (dec i) (+ i 2)) y (range (dec j) (+ j 2))
                                                                         :when (or (not= x i) (not= y j))] [x y])))))]
        (map (fn [x] (apply str (map #(if (= % 0) " " "#") x)))
          (partition-all m (for [i (range m) j (range n)]
                             (let [cnt (count-neib i j) was (get-in gen1 [i j])]
                               (cond (and (= 0 was) (= 3 cnt)) 1
                                     (and (= 1 was) (or (= 2 cnt) (= 3 cnt))) 1
                                     :else 0)))))

        )
      )
    ))

(defcheck solution-23f8af7d
  (fn [b]
    (let [h (count b)]
      (map-indexed
        (fn [ri r]
          (let [w (count r)]
            (reduce
              str
              (map-indexed
                (fn [ci c]
                  (letfn [(surr [x l] [(dec x) x (inc x)])]
                    (let [nc (count
                               (filter #{\#}
                                 (for [ci* (surr ci w) ri* (surr ri h)
                                       :when (not= [ci ri] [ci* ri*])]
                                   (nth (nth b ri* ()) ci* nil))))]
                      (if ((if (= c \#) #{2 3} #{3}) nc) \# \space))))
                r))))
        b))))

(defcheck solution-2464db53
  (fn [b]
    (letfn [(cell
              [b r c]
              (when (and (>= r 0) (>= c 0)
                         (< r (count b)) (< c (count (first b))))
                (-> b
                  (nth,,, r)
                  (nth,,, c))))

            (nb
              [b r c]
              (reduce (fn [acc [rr cc]]
                        (if-let [val (cell b (+ r rr) (+ c cc))]
                          (conj acc val)
                          acc))
                []
                (for [x [-1 0 1] y [-1 0 1] :when (not (and (zero? x) (zero? y)))] [x y])))]
      (->> (for [r (range (count b))
                 c (range (count (first b)))]
             [r c])
        (map (fn [[r c]] [(cell b r c) (frequencies (nb b r c))]),,,)
        (map (fn [[val cnt]]
               (condp = val
                 \space (if (#{3} (get cnt \#)) \# \space)
                 \# (if (#{2 3} (get cnt \#)) \# \space))),,,)
        (partition (count (first b)),,,)
        (map (fn [cs] (apply str cs)),,,)))))

(defcheck solution-24a23779
  (fn [v]
    (let [t (vec (map (fn [s] (vec (map #(if (= % \#) 1 0) s))) v))
          h (count t)
          w (count (t 0))
          g (fn [[x y]] (get (get t y) x 0))
          n (fn [x y] (for [a (range (- x 1) (+ x 2)) b (range (- y 1) (+ y 2))]
                        [a b]))
          c (fn [x y] (apply + (map g (n x y))))
          s (fn [x y] (if (= 0 (g [x y]))
                        (if (= (c x y) 3) 1 0)
                        (let [d (c x y)] (if (or (< d 3) (> d 4)) 0 1))))]
      (for [y (range h)] (apply str (map [\  \#] (for [x (range w)] (s x y))))))))

(defcheck solution-24aaeea5
  (fn __
    [field]
    (letfn [(neighbours
              [m e]
              (let [ki (first e)
                    kj (second e)
                    s  #(get-in m [[%1 %2] :s])]
                [(s (dec ki) (dec kj)) (s (dec ki) kj) (s (dec ki) (inc kj))
                 (s ki (dec kj)) (s ki (inc kj))
                 (s (inc ki) (dec kj)) (s (inc ki) kj) (s (inc ki) (inc kj))]))
            (to-field
              [f]
              (reduce (fn [row i]
                        (conj row (clojure.string/join (map (comp second first val) (sort (filter (fn [e] (= i (first (key e)))) f))))))
                [] (range (count field))))
            (count-cells
              [n sym]
              (count (filter #(= % sym) n)))]
      (let [f (reduce (fn [res row]
                        (let [col (map-indexed vector (second row))]
                          (merge res (reduce (fn [r e]
                                               (assoc r [(first row) (first e)] {:s (second e)})
                                               ) {} col)))
                        ) {} (map-indexed vector field))]

        (to-field
          (reduce (fn [r c]
                    (let [n                     (neighbours f (first c))
                          fewer-than-two-live?  (and (= ((second c) :s) \#) (< (count-cells n \#) 2))
                          two-or-three-live?    (and (= ((second c) :s) \#) (or (= 2 (count-cells n \#)) (= 3 (count-cells n \#))))
                          more-than-three-live? (and (= ((second c) :s) \#) (> (count-cells n \#) 3))
                          one-dead-three-live?  (and (= ((second c) :s) \space) (= 3 (count-cells n \#)))]

                      (cond
                        fewer-than-two-live? (assoc r (first c) {:s \space})
                        two-or-three-live? (assoc r (first c) {:s \#})
                        more-than-three-live? (assoc r (first c) {:s \space})
                        one-dead-three-live? (assoc r (first c) {:s \#})
                        :else r)))
            f f))

        ))))

(defcheck solution-24c4b514
  (fn game-of-life
    [board]
    (letfn [(board-get [board r c] (get (get board r) c))
            (adjacent [r c]
              [[(dec r) (dec c)]
               [(dec r) c]
               [(dec r) (inc c)]
               [r (dec c)]
               [r (inc c)]
               [(inc r) (dec c)]
               [(inc r) c]
               [(inc r) (inc c)]])
            (live-neighbors [board r c]
              (count (filter #(= (apply board-get board %) \#) (adjacent r c))))]
      (for [r (range 0 (count board))]
        (apply str
          (for [c (range 0 (count (board r)))
                :let [live?      (= (board-get board r c) \#)
                      live-count (live-neighbors board r c)]]
            (if (or (and live? (<= 2 live-count 3))
                    (and (not live?) (= live-count 3)))
              \#
              \space)))))))

(defcheck solution-24ec886e
  (let [pm        {\space 0 \# 1}
        live-ns   {\space #{3} \# #{2 3}}
        neighbors (fn [b i j]
                    (apply +
                      (- (pm (get-in b [i j])))
                      (for [ii (range (max 0 (dec i)) (min (count b) (+ i 2)))
                            jj (range (max 0 (dec j)) (min (count (first b)) (+ j 2)))]
                        (pm (get-in b [ii jj])))))]
    (fn f [b]
      (map (partial apply str)
        (for [i (range (count b))]
          (for [j (range (count (first b)))]
            (if (contains? (live-ns (get-in b [i j]))
                  (neighbors b i j))
              \#
              \space)))))))

(defcheck solution-25c71710
  (fn Life [m]
    (letfn [(nnth [[i j]]
              (nth (nth m j '()) i \space))
            (live-neighbors [xy]
              (reduce
                #(if (= %2 \#) (inc %1) %1)
                0
                (map nnth
                  (map #(map + xy %)
                    '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))))))
            (result [alive n]
              (if alive
                (cond (< n 2) \space
                      (> n 3) \space
                      :else \#)
                (if (= n 3) \# \space)))]
      (map
        (fn [j]
          (apply str (map
                       (fn [i]
                         (result (= \# (nnth (list i j)))
                           (live-neighbors (list i j))))
                       (range (count (first m))))))
        (range (count m))))))

(defcheck solution-268ca796
  (fn [r]
    (let [c     #(range (count %))
          s     \space
          mx    (c (first r))
          my    (c r)
          board (fn [[x y]]
                  (let [i (fn [a b] (contains? (set b) a))]
                    (if (and (i x mx) (i y my))
                      (.charAt (r y) x) s)))
          mb    (memoize board)
          nd    (fn [n] (if (= 3 n) \# s))
          nl    (fn [n] (if (or (< n 2) (> n 3)) s \#))
          z     [[-1 -1] [-1 0] [-1 1] [0 -1] [0 0] [0 1] [1 -1] [1 0] [1 1]]
          n     (fn [x y] (remove #(= % [x y])
                            (map #(map + [x y] %) z)))
          ng    (fn [x y]
                  (let [n (count (filter #(= \# %) (map mb (n x y))))]
                    (if (= \# (mb [x y])) (nl n) (nd n))))]
      (map #(apply str (map (fn [x] (ng x %)) mx)) my))))

(defcheck solution-27657885
  (fn [g]
    (map #(apply str %)
      (let [
            M (fn [f & z] (apply map (partial map f) z))
            G (M {\# 1 \space 0} g)
            t #(apply map list %)
            r #(t (reverse %))
            q #(cons 0 (butlast %))
            p #(map q %)
            P (comp t p t)
            ]
        (M #(if (= 3 %2) \# (if (and (= 1 %) (= 2 %2)) \# \space))
          G
          (r (reduce #(M + (r %) %2)
               (let [l (map p (take 4 (iterate r G)))]
                 (into (vec l) (map P l)))))
          )))))

(defcheck solution-27ab69f6
  (fn [board]
    (let [n       (count board)
          matrix  (vec (map vec board))
          neighs
                  (fn [x y] (rest
                              (for [i [0 1 -1] j [0 1 -1]
                                    :when (<= 0 (+ x i))
                                    :when (< (+ x i) n)
                                    :when (<= 0 (+ y j))
                                    :when (< (+ y j) n)] [(+ x i) (+ y j)])))
          fate
                  (fn [[x y]]
                    (let [score (count (filter #(= \# %) (map #(get-in matrix %) (neighs x y))))
                          state (get-in matrix [x y])]
                      (cond
                        (and (= state \#) (< score 2)) \space
                        (and (= state \#) (< score 4)) \#
                        (= state \#) \space
                        (and (= state \space) (= score 3)) \#
                        :else \space)))
          start   (vec (for [i (range n)] (apply vector (for [j (range n)] \#))))
          ->board (fn [m] (vec (map (partial apply str) (map (partial map str) m))))]
      (->board (reduce #(assoc-in % %2 (fate %2)) start (for [i (range n) j (range n)] [i j]))))))

(defcheck solution-285aa4c5
  (fn life
    [v]
    (let [board                    (map-indexed #(vector %1 (map-indexed vector (seq %2))) v)
          rows-count               (count v)
          lines-count              (count (first v))
          neighbours-func          (fn [iv] (filter (fn [[fa sa]] (and (>= fa 0) (>= sa 0) (< fa rows-count) (< sa lines-count))) (map (fn [nv] (map + iv nv)) '([0 -1] [0 1] [-1 0] [1 0] [-1 -1] [1 1] [-1 1] [1 -1]))))
          cell-types               {:live \# :dead \space}
          live?                    (fn [cell] (= cell (:live cell-types)))
          dead?                    (fn [cell] (= cell (:dead cell-types)))
          get-indexes              (fn [initial-vector] (map (fn [[i v]] (map #(vector i (first %)) v)) initial-vector))
          map-to-neighbour-indexes (fn [s] (map #(map neighbours-func %) (get-indexes s)))
          map-to-neighbours        (fn [s] (map (fn [x] (map (fn [y] (map (fn [[a1 a2]] (second (nth (second (nth board a1)) a2))) y)) x)) (map-to-neighbour-indexes s))) ;test
          mapped-to-neighbours     (map-to-neighbours board)]
      (map (fn [[i v]] (apply str (map (fn [[ii value]] (let [arg (filter #{\#} (nth (nth mapped-to-neighbours i) ii))]
                                                          (if (live? value)
                                                            (if (< (count arg) 2)
                                                              (:dead cell-types)
                                                              (if (or (= (count arg) 2) (= (count arg) 3))
                                                                (:live cell-types)
                                                                (:dead cell-types)
                                                                )
                                                              )
                                                            (if (= (count arg) 3)
                                                              (:live cell-types)
                                                              (:dead cell-types)
                                                              )))) v))

             ) board)
      )))

(defcheck solution-28a07712
  (letfn [(nbs [[x y]] (for [dx [-1 0 1] dy [-1 0 1]
                             :when (not= dx dy 0)]
                         [(+ x dx) (+ y dy)]))
          (step [cs] (set (for [[p n] (frequencies (mapcat nbs cs))
                                :when (or (= n 3) (and (= n 2) (cs p)))]
                            p)))
          (cells [l w rs] (set (for [r (range l) c (range w)
                                     :when (= \# (get-in rs [r c]))]
                                 [r c])))
          (out [l w cells] (vec (for [x (range l)]
                                  (apply str
                                    (for [y (range w)]
                                      (if (cells [x y]) \# " "))))))
          (gol [in] (let [l (count in)
                          w (count (first in))]
                      (out l w
                        (step (cells l w in)))))]
    gol))

(defcheck solution-28f16f7b
  (fn [b]
    (let [r (range (count b)) d \  l \#
          o [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
          a (fn [p] (count (filter #{l} (map #(get-in b (map + p %)) o))))]
      (for [y r]
        (apply str (for [x r
                         :let [p [y x] s (get-in b p) n (a p)]]
                     (if (= s l)
                       (if (<= 2 n 3) l d)
                       (if (= n 3) l d))))))))

(defcheck solution-28fdf3a5
  (fn [b]
    (letfn [(neighbors [x y]
              (let [o [-1 0 1]]
                (for [j o k o
                      :when (not= [j k] [0 0])]
                  (get-in b [(+ y k) (+ x j)]))))]
      (for [y (range (count b))]
        (apply str
          (for [x (range (count (first b)))
                :let [n (count (filter #{\#} (neighbors x y)))]]
            (cond
              (= n 2) (get-in b [y x])
              (= n 3) \#
              :else \space)))))))

(defcheck solution-2a1d6a7c
  (fn [i-board]
    (let [
          c-count          (count (first i-board))
          r-count          (count i-board)
          in-board         (fn [i-coor]
                             (let [[i-row i-col] i-coor]
                               (and
                                (>= i-row 0)
                                (< i-row r-count)
                                (>= i-col 0)
                                (< i-col c-count))))
          has-life         (fn [i-coor]
                             (= \# (nth (nth i-board (first i-coor)) (last i-coor))))
          board-coor       (reduce
                             concat
                             '()
                             (map
                               (fn [my-row]
                                 (map #(vector my-row %) (range c-count)))
                               (range r-count)))
          live-list        (filter has-life board-coor)
          dead-list        (filter #(not (has-life %)) board-coor)
          neighbour        (fn [i-coor]
                             (let [[i-row i-col] i-coor]
                               (filter
                                 #(let [[my-row my-col] %] (not (and (= my-row i-row) (= my-col i-col))))
                                 (reduce
                                   concat
                                   '()
                                   (map
                                     (fn [i-coors]
                                       (filter in-board i-coors))
                                     (map
                                       (fn [my-row]
                                         (map
                                           (fn [my-col]
                                             (vector my-row my-col))
                                           (range (dec i-col) (+ i-col 2))))
                                       (range (dec i-row) (+ i-row 2))))))))
          living-neighbour (fn [i-coor] (filter has-life (neighbour i-coor)))
          lives-on         (fn [i-coor]
                             (let [n-count (count (living-neighbour i-coor))]
                               (or (= 2 n-count) (= 3 n-count))))
          reproducible     (fn [i-coor]
                             (= 3 (count (living-neighbour i-coor))))
          new-lives        (group-by
                             #(first %)
                             (concat
                               (filter lives-on live-list)
                               (filter reproducible dead-list)))
          draw-row         (fn plot-lives
                             ([i-row] (plot-lives [] 0 (sort (map last (get new-lives i-row)))))
                             ([result i-pos i-lives]
                              (if (= c-count i-pos)
                                (apply str result)
                                (let [c-life (first i-lives) n-pos (inc i-pos)]
                                  (if (= i-pos c-life)
                                    (plot-lives (conj result \#) n-pos (rest i-lives))
                                    (plot-lives (conj result \space) n-pos i-lives))))))
          ]
      (map draw-row (range r-count)))))

(defcheck solution-2b28531b
  (fn next-board [board]
    (letfn [
            (cell-set [board]
              (set (map #(vec (rest %))
                     (filter #(= \# (first %))
                       (for [y (partition 2 2
                                 (interleave (range (count board))
                                   (map #(partition 2 2
                                           (interleave (range (count %)) %)) board)))
                             x (second y)]
                         [(second x) (first y) (first x)])))))

            (count-neighbors [cset y x h w]
              (apply +
                (for [ny (range (dec y) (+ y 2))
                      nx (range (dec x) (+ x 2))
                      :when (< -1 ny h)
                      :when (< -1 nx w)
                      :when (not= [ny nx] [y x])]
                  (if (contains? cset [ny nx]) 1 0))))
            (next-gen [cset h w]
              (set (map #(vec (rest %))
                     (filter first
                       (for [y (range h) x (range w)]
                         (let [neighbors (count-neighbors cset y x h w)]
                           [(if (contains? cset [y x])
                              (<= 2 neighbors 3)
                              (== 3 neighbors)) y x]))))))

            (cell-board [cset h w]
              (for [y (range h)]
                (apply str (vec
                             (for [x (range w)]
                               (if (get cset [y x]) \# \space))))))]

      (let [h (count board)
            w (count (first board))]
        (cell-board
          (next-gen
            (cell-set board)
            h w)
          h w)))))

(defcheck solution-2b4c6462
  (fn next-generation [board]
    (letfn [(alive? [i j]
              (-> board (nth i) (nth j) (= \#)))
            (find-neighbours [i j]
              (for [di [-1 0 1]
                    dj [-1 0 1]
                    :let [ii (+ i di)
                          jj (+ j dj)]
                    :when (and (not (and (= i ii)
                                         (= j jj)))
                               (>= ii 0)
                               (< ii (count board))
                               (>= jj 0)
                               (< jj (count (first board))))]
                [ii jj]))
            (still-alive? [alive neighbours]
              (let [n-alive-neighbours (count (filter #(apply alive? %) neighbours))]
                (cond (< n-alive-neighbours 2) false
                      (and (= n-alive-neighbours 2) alive) true
                      (= n-alive-neighbours 3) true
                      :else false)))]
      (map (fn create-row [i]
             (apply str
               (map (fn [j] (if (still-alive? (alive? i j) (find-neighbours i j)) "#" " "))
                 (range (count (first board))))))
        (range (count board))))))

(defcheck solution-2bfc0411
  (fn next-board [board]
    (let [board-points    (let [s (count board)]
                            (map vec
                              (partition s
                                (for [x (range s)
                                      y (range (count (first board)))]
                                  [x y]))))
          neighbors       (fn neighbors [[x y]]
                            (for [dx [-1 0 1]
                                  dy [-1 0 1]
                                  :when (not= dx dy 0)]
                              [(+ x dx) (+ y dy)]))
          count-neighbors (fn count-neighbors [point]
                            (count (filter #(= % \#) (map #(get-in board %) (neighbors point)))))
          is-alive?       (fn is-alive? [point]
                            (let [p (get-in board point)]
                              (= p \#)))
          live?           (fn [[n a]]
                            (if a
                              (if (<= 2 n 3)
                                \#
                                \space)
                              (if (= n 3)
                                \#
                                \space)))
          mn              (fn [row]
                            (map (comp live? (juxt count-neighbors is-alive?)) row))]
      (map #(apply str %) (map concat (map mn board-points))))))

(defcheck solution-2d609761
  (fn next-game [[& rows :as board]]
    (map #(apply str %) (letfn [(seq-of-indices []
                                  (keep identity (mapcat identity
                                                   (for [i (range -1 2)]
                                                     (for [j (range -1 2)]
                                                       (if (not (and (zero? i) (zero? j)))
                                                         [i j]))))))]
                          (let [vec-board (vec (map vec rows))
                                dead      \space live \#
                                offsets   (seq-of-indices)]
                            (map-indexed (fn [row-index row]
                                           (map-indexed #(let [live-neighbor-amt (count (filter (fn [cell] (= live cell))
                                                                                          (for [coord-offset offsets]
                                                                                            (get-in vec-board (map + [row-index %] coord-offset) dead))))] ;assuming edge is dead
                                                           (cond (< live-neighbor-amt 2) dead
                                                                 (> live-neighbor-amt 3) dead
                                                                 (and (= dead %2) (= live-neighbor-amt 3)) live
                                                                 :live-cell-with-2-or-3-neighbors %2))
                                             row))
                              vec-board))))))

(defcheck solution-2db33cee
  (fn [board]
    (let [boarddata (map
                      (fn [line]
                        (map (fn [s] (if (= \# s) 1 0))
                          line))
                      board)
          bdw       (count (first boarddata))
          bdh       (count boarddata)
          cell      (fn [i j] (nth (nth boarddata j) i))
          neighbors (fn [i j]
                      (let [ip (if (< i (dec bdw)) (inc i) 0)
                            im (if (> i 0) (dec i) (dec bdw))
                            jp (if (< j (dec bdh)) (inc j) 0)
                            jm (if (> j 0) (dec j) (dec bdh))
                            ]
                        (reduce + (for [ii (range im (inc ip))
                                        jj (range jm (inc jp))
                                        :when (not (and (= ii i) (= jj j)))]
                                    (cell ii jj)))))
          liferule  (fn [status neighbors]
                      (let [and-on  (fn [v] (and (= status 1) v))
                            and-off (fn [v] (and (= status 0) v))]
                        (cond
                          (and-on (< neighbors 2)) 0
                          (and-on (or (= neighbors 2) (= neighbors 3))) 1
                          (and-on (> neighbors 3)) 0
                          (and-off (== neighbors 3)) 1
                          :else status)))
          newdata   (for [j (range bdh)]
                      (for [i (range bdw)]
                        (liferule (cell i j) (neighbors i j))
                        ))
          newboard  (map (fn [row]
                           (reduce str
                             (map
                               (fn [e] (if (= 1 e) "#" " "))
                               row))) newdata)
          ]
      newboard
      )))

(defcheck solution-2e69ea19
  (fn [board]
    (let [dimension         (count board)
          living-neighbours (fn [x y]
                              (count (filter
                                       #(= \# %)
                                       (for [ix (range (dec x) (+ 2 x))
                                             iy (range (dec y) (+ 2 y))
                                             :when (or (not= ix x) (not= iy y))
                                             :when (and (>= ix 0) (>= iy 0))
                                             :when (and (< ix dimension) (< iy dimension))]
                                         (get-in board [ix iy])))))]
      (->> (for [x (range dimension)
                 y (range dimension)
                 :let [was-alive  (= \# (get-in board [x y]))
                       neighbours (living-neighbours x y)]]
             (cond
               (and was-alive (< neighbours 2)) \space
               (and was-alive (or (= 2 neighbours) (= 3 neighbours))) \#
               (and was-alive (> neighbours 3)) \space
               (and (not was-alive) (= neighbours 3)) \#
               :else \space))
        (partition dimension)
        (map #(apply str %))))))

(defcheck solution-2eb99897
  (fn life [coll]
    (letfn [
            (strs->set [strs] (apply hash-set
                                (filter #(not (nil? %))
                                  (apply concat
                                    (map-indexed
                                      (fn [y row]
                                        (map-indexed
                                          (fn [x col]
                                            (if (= \# col) [x y])) row)) coll)))))
            (neighbors [[x y]]
              [[(dec x) y] [x (dec y)] [(inc x) y] [x (inc y)]
               [(dec x) (dec y)] [(inc x) (inc y)]
               [(dec x) (inc y)] [(inc x) (dec y)]])

            (plant-seeds [lifeset]
              (reduce (fn [bag tuple] (reduce #(assoc % %2
                                                        (if (contains? % %2)
                                                          (inc (% %2))
                                                          1)) bag (neighbors tuple))) {} lifeset))
            (sprout [seedmap lifeset]
              (set (filter #(not (nil? %))
                     (map (fn [[tuple neighborcount]] (cond
                                                        (< neighborcount 2) nil
                                                        (= neighborcount 2) (lifeset tuple)
                                                        (= neighborcount 3) tuple
                                                        :else nil
                                                        )) seedmap))))
            (set->strs [lifeset]
              (let [nuthin (apply vector (repeat (count coll) (apply vector (repeat (count coll) " "))))]
                (map (fn [row] (apply str row))
                  (reduce #(assoc-in % [(second %2) (first %2)] \#) nuthin lifeset))))
            ]
      (apply vector (-> coll
                      strs->set
                      plant-seeds
                      (sprout (strs->set coll))
                      set->strs)))))

(defcheck solution-2f2aab8a
  (fn life [gs]
    (let [neighbors   #{[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]}
          alive?      #{\#}
          born?       #{3}
          stay-alive? #{2 3}]
      (map-indexed
        (fn [i r]
          (apply str
            (map-indexed
              (fn [j c]
                (let [nalive (count (filter alive? (map (fn [[x y]] (get-in gs [(+ i x) (+ j y)])) neighbors)))]
                  (if ((if (alive? (get-in gs [i j]))
                         stay-alive?
                         born?) nalive)
                    \#
                    \space)))
              r)))
        gs))))

(defcheck solution-2f3db4fa
  (fn [bd]
    (letfn [
            (neighbour-count [bd y x]
              (count
                (for [
                      i (range -1 2)
                      j (range -1 2)
                      :when (not= 0 i j)
                      :when (= \# (get-in bd [(+ y i) (+ x j)]))]
                  1)))]
      (let [x (count (bd 0)) y (count bd)]
        (vec
          (map
            #(apply str %)
            (partition x
              (for [
                    i (range y)
                    j (range x)
                    :let [n (neighbour-count bd i j)]
                    :let [live (= \# (get-in bd [i j]))]]
                (if
                 (or
                  (and live (> n 1) (< n 4))
                  (and (not live) (= 3 n)))
                  \# \space)))))))))

(defcheck solution-300c1722
  (letfn [
          (read-board [bv] [(count (first bv)) (apply str bv)])
          (neighbors [[w bs] i]
            (let [left-edge (= 0 (rem i w)) right-edge (= (dec w) (rem i w))]
              (map #(and % (get bs (+ i %)))
                [(if left-edge nil (dec (- w))) (- w) (if right-edge nil (inc (- w)))
                 (if left-edge nil -1) (if right-edge nil 1)
                 (if left-edge nil (dec w)) w (if right-edge nil (inc w))])))
          (live-neighbors [b i] (->> (neighbors b i) (filter (partial = \#)) (count)))
          (update-board [[w bs :as b]]
            (let [bs' (apply str
                        (map-indexed
                          #(let [n (live-neighbors b %1)]
                             (cond
                               (and (= %2 \#) (or (< n 2) (> n 3))) \space
                               (and (= %2 \space) (= n 3)) \#
                               :else %2))
                          bs))]
              [w bs']))
          (write-board [[w bs]] (->> bs (partition w) (map #(apply str %))))]
    #(write-board (update-board (read-board %)))))

(defcheck solution-30fbd030
  (let [delt [-1 0 1]
        adj  (for [x delt y delt :when (some #(not= 0 %) [x y])] [x y])]
    (fn [board]
      (let [rows (count board)
            cols (count (first board))]
        (letfn [(at [pos] (get-in board pos))
                (move [pos delts] (vec (map + pos delts)))
                (valid? [[r c]] (and (< -1 r rows) (< -1 c cols)))
                (neighbors [pos] (map at (filter valid? (map (partial move pos) adj))))]
          (map (fn [r]
                 (apply str (for [pos (map #(vector r %) (range cols))]
                              (let [nfreq (frequencies (neighbors pos))
                                    nlive (get nfreq \# 0)
                                    live? ({\space false \# true} (at pos))]
                                (if live?
                                  (if (#{2 3} nlive) \# \space)
                                  (if (= 3 nlive) \# \space))))))
            (range rows)))))))

(defcheck solution-310de9d1
  (letfn
   [(find-alives [board rr rc]
      (set (for [r rr c rc
                 :when (= (nth (nth board r) c) \#)]
             [r c])))

    (alive-neighbors [alives [r c]]
      (for [[i j] [[(inc r) c] [(inc r) (inc c)] [(inc r) (dec c)]
                   [r (inc c)] [r (dec c)]
                   [(dec r) c] [(dec r) (inc c)] [(dec r) (dec c)]]
            :when (alives [i j])]
        [i j]))]

    (fn game-of-life [board]
      (let [rr     (range (count board))
            rc     (range (count (first board)))
            alives (find-alives board rr rc)]
        (for [r rr]
          (apply str
            (for [c rc]
              (let [num-alive (count (alive-neighbors alives [r c]))]
                (if (alives [r c])
                  (if (or (< num-alive 2)
                          (> num-alive 3))
                    \space \#)
                  (if (= num-alive 3)
                    \# \space))))))))))

(defcheck solution-329ee8e1
  (fn [s]
    (let [sm (mapcat #(map (fn [y v] [%1 y v]) (range) %2) (range) s)]
      (letfn [(nbrs [[x y _]]
                (let [x- (dec x), x+ (inc x)
                      y- (dec y), y+ (inc y)]
                  (reduce (fn [a [xi yi v]]
                            (if (and (= v \#)
                                     (or (not= x xi) (not= y yi))
                                     (or (= x- xi) (= xi x) (= xi x+))
                                     (or (= y- yi) (= yi y) (= yi y+)))
                              (inc a) a))
                    0 sm)))]
        (->> sm
          (map (fn [[_ _ v :as p]]
                 (let [n (nbrs p)]
                   (cond (and (= v \#) (or (= n 2) (= n 3))) \#
                         (and (= v \space) (= n 3)) \#
                         :else \space))))
          (partition (-> s first count))
          (map (partial reduce str)))))))

(defcheck solution-32ade827
  (fn gol [board]
    (let [adj        (fn [i j] (map (fn [[x y]] [(+ x i) (+ y j)]) [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))
          neighbours (fn [i j] (count (filter (fn [[x y]]
                                                (= \# (nth (nth board x nil) y nil)))
                                        (adj i j))))]
      (map-indexed
        (fn [i rows]
          (clojure.string/join ""
            (map-indexed
              (fn [j elem]
                (if (or (and (= elem \#) (< 1 (neighbours i j) 4))
                        (and (= elem \ ) (= 3 (neighbours i j))))
                  "#"
                  " "))
              rows)))
        board))))

(defcheck solution-339335f4
  (fn [board]
    (let [w          (count (first board))
          c          (juxt quot mod)
          g          (set (keep-indexed #({\# (c % w)} %2)
                            (mapcat seq board)))
          neighbours (fn [[x y]]
                       (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
                         [(+ dx x) (+ dy y)]))
          ng         (set (for [[loc n] (frequencies (mapcat neighbours g))
                                :when (or (= n 3) (and (= n 2) (g loc)))]
                            loc))]
      (->>
        (range (* w (count board)))
        (map #(if (ng (c % w)) \# " "))
        (partition w)
        (map #(apply str %))))))

(defcheck solution-33c589a0
  (fn [S]
    (map-indexed
      (fn [i s]
        (apply str
          (map-indexed
            (fn [j c]
              (let [d [-1 0 1]
                    n (count (filter #(= (get-in S %) \#)
                               (for [I d J d] [(+ i I) (+ j J)])))]
                (if (or (= n 3) (and (= n 4) (= c \#))) \# \ )))
            s)))
      S)))

(defcheck solution-34048307
  {["      "
    " ##   "
    " ##   "
    "   ## "
    "   ## "
    "      "] ["      "
               " ##   "
               " #    "
               "    # "
               "   ## "
               "      "] ["     "
                          "     "
                          " ### "
                          "     "
                          "     "] ["     "
                                    "  #  "
                                    "  #  "
                                    "  #  "
                                    "     "]
   ["      "
    "      "
    "  ### "
    " ###  "
    "      "
    "      "]
   ["      "
    "   #  "
    " #  # "
    " #  # "
    "  #   "
    "      "]})

(defcheck solution-35980ef7
  (fn [b]
    (let [alive \#

          dead  \space

          neighbor-coords
                (fn [[x y]]
                  (let [x- (dec x) x+ (inc x)
                        y- (dec y) y+ (inc y)]
                    (filter
                      (partial every? (complement neg?))
                      [[x- y+] [x y+] [x+ y+]
                       [x- y] [x+ y]
                       [x- y-] [x y-] [x+ y-]])))

          neighbors
                (fn [coord b]
                  (->> (neighbor-coords coord)
                    (map (partial get-in b))
                    (filter #(= alive %))
                    (count)))

          cell-tick
                (fn [cell neighbors]
                  (cond (= 2 neighbors) cell
                        (= 3 neighbors) alive
                        :else dead))

          w     (count (first b))

          world-tick
                (for [x (range w)
                      y (range (count b))
                      :let [cell (get-in b [x y])]]
                  (cell-tick cell (neighbors [x y] b)))]

      (->> world-tick
        (partition w)
        (map #(apply str %))))))

(defcheck solution-368763fe
  (fn [sar]
    (let [n        (count sar)
          ns       [(- -1 n) (- 0 n) (- 1 n) -1 1 (+ -1 n) n (+ 1 n)]
          se       (mapcat seq sar)
          nbrs     (fn [i] (map (partial nth se) (remove #(or (> 0 %) (<= (* n n) %)) (map (partial + i) ns))))
          next-gen (fn [t n]
                     (let [lc (reduce #(if (= \# %2) (inc %) %) 0 n)]
                       (if (or (< lc 2) (> lc 3)            ; 1) 3)
                               (if (= t \#)
                                 false                      ; 2)
                                 (= lc 2))) " " "#")))]     ; 4)
      (into [] (map #(apply str %)
                 (loop [c 0 s se r []]
                   (if (> c (* n n))
                     (map #(into [] %) (partition n (flatten r)))
                     (recur (inc c) (rest s) (conj r (next-gen (first s) (nbrs c)))))))))))

(defcheck solution-36df186d
  (fn [board]
    (letfn [(cell [b x y]
              (-> b
                (nth y " ")
                (nth x \space)))
            (neighbors [b x y]
              (let [offsets [[-1 -1] [0 -1] [1 -1]
                             [-1 0] [1 0]
                             [-1 1] [0 1] [1 1]]
                    coords  (map (fn [[x1 y1]] [(+ x x1) (+ y y1)]) offsets)]
                (map (fn [[x2 y2]] (cell board x2 y2)) coords)))
            (num-live-neighbors [b x y]
              (count (filter (partial = \#) (neighbors b x y))))
            (next-val [b x y]
              (let [n (num-live-neighbors b x y)]
                (cond
                  (< n 2) \space
                  (= n 2) (cell b x y)
                  (= n 3) \#
                  (> n 3) \space)))
            (dim [b] [(count (first b)) (count b)])
            (next-row [b n]
              (apply str (map #(next-val b % n) (range (first (dim b))))))
            (next-board [b]
              (map #(next-row b %) (range (second (dim b)))))
            ]

      (next-board board)
      )
    ))

(defcheck solution-3810fad4
  (fn [board]
    (let [n   (count board)
          b   (vec (map vec board))
          st  (for [x (range -1 2) y (range -1 2)
                    :when (not (and (zero? x) (zero? y)))] [x y])
          mv  (fn [[x y] [mx my]] [(+ x mx) (+ y my)])
          in? (fn [[x y]] (and (<= 0 x) (<= 0 y) (< x n) (< y n)))
          pk  (fn [[x y]] ((b x) y))
          nb  (fn [c] (map (partial mv c) st))
          hm  (fn [c] (count (filter (partial = \#)
                               (map pk (filter in? (nb c))))))
          nx  (fn [c]
                (let [p (pk c)
                      h (hm c)]
                  (cond (and (= \# p) (< h 2)) \space
                        (and (= \# p) (or (= 2 h) (= 3 h))) \#
                        (and (= \# p) (< 3 h)) \space
                        (and (= \space p) (= 3 h)) \#
                        :else p)))]
      (vec (map (partial apply str) (for [x (range n)]
                                      (for [y (range n)]
                                        (nx [x y]))))))))

(defcheck solution-38f9012
  (fn [b]
    (let [l   (count (first b))
          z   (list (repeat l 0))
          ns  (fn [s] (map #(if (= % \#) 1 0) s))
          rs  (fn [c] (apply str (map #(if (= % 1) \# " ") c)))
          nb  (map ns b)
          shl (fn [c] (concat (rest c) '(0)))
          shr (fn [c] (concat '(0) (butlast c)))
          lb  (map shl nb)
          rb  (map shr nb)
          ub  (concat (rest nb) z)
          db  (concat z (butlast nb))
          ulb (concat (rest lb) z)
          dlb (concat z (butlast lb))
          urb (concat (rest rb) z)
          drb (concat z (butlast rb))
          neb (map #(map + %1 %2 %3 %4 %5 %6 %7 %8) lb rb ub db ulb dlb drb urb)
          mv  (fn [s n] (cond
                          (and (= s 0) (= n 3)) 1
                          (and (= s 1) (or (< n 2) (> n 3))) 0
                          :else s))
          mb  (map #(map mv %1 %2) nb neb)]
      (map rs mb))))

(defcheck solution-3ae5561c
  (fn advance [state]
    (let [size               (count state)
          piece-at           (fn [[x y]] (-> state (get y) (get x)))
          alive?             (fn [[x y]] (= \# (piece-at [x y])))
          neighbor-count     (fn [[x y]]
                               (let [neighbors [[x (dec y)]
                                                [x (inc y)]
                                                [(dec x) y]
                                                [(inc x) y]
                                                [(dec x) (dec y)]
                                                [(dec x) (inc y)]
                                                [(inc x) (dec y)]
                                                [(inc x) (inc y)]]]
                                 (count (filter alive? neighbors))))
          construct-position (fn [[x y]]
                               {:neighbor-count (neighbor-count [x y])
                                :alive?         (alive? [x y])})
          next-marker        (fn [x y]
                               (let [{:keys [neighbor-count alive?]} (construct-position [x y])]
                                 (cond (and alive? (< neighbor-count 2)) " "
                                       (and alive? (> neighbor-count 3)) " "
                                       (and alive? (#{2 3} neighbor-count)) "#"
                                       (and (not alive?) (= 3 neighbor-count)) "#"
                                       :else " ")))]
      (for [row-index (range size)]
        (apply str (map next-marker (range size) (repeat row-index)))))))

(defcheck solution-3b1f8379
  (fn [board]
    (let [
          rows       (count board)
          cols       (count (first board))
          object     (apply hash-map (flatten (for [r (range rows)] (for [c (range cols)] [(str r c) {:r r :c c :alive (= "#" (subs (nth board r) c (inc c)))}]))))
          rneighbors (into {} (for [r (range rows)] [r (concat (if (pos? r) [(dec r)] [nil]) [r] (if (< r (dec rows)) [(inc r)] [nil]))]))
          cneighbors (into {} (for [c (range cols)] [c (concat (if (pos? c) [(dec c)] [nil]) [c] (if (< c (dec cols)) [(inc c)] [nil]))]))
          neighbors  (for [cell object] (let [o (second cell)] [(first cell) (flatten (for [r (rneighbors (o :r))] (remove #(or (< (count %) 2) (= (first cell) %)) (for [c (cneighbors (o :c))] (str r c)))))]))
          states     (apply hash-map (mapcat (fn [cell] [(first cell) {:alive (get-in object [(first cell) :alive]) :ncount (reduce #(if (get-in object [%2 :alive]) (inc %1) %1) 0 (second cell))}]) neighbors))]
      (for [r (range rows)] (apply str (for [c (range cols)] (let [state (states (str r c)) alive? (state :alive) ncount (state :ncount)] (if alive? (cond (< ncount 2) " " (< ncount 4) "#" :else " ") (if (= ncount 3) "#" " ")))))))))

(defcheck solution-3b2d909f
  (fn step [board]
    (let [max-y (count board)
          max-x (count (first board))]
      (letfn [(alive? [x y]
                (and (> y -1)
                     (> x -1)
                     (< y max-y)
                     (< x max-x)
                     (= \# (nth (nth board y) x))))]
        (map #(apply str %)
          (partition max-x
            (for [y (range max-y)
                  x (range max-x)]
              (let [neighbors (count (filter #(apply alive? %)
                                       [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
                                        [(dec x) y] [(inc x) y]
                                        [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]))
                    self      (alive? x y)]
                (cond
                  (< neighbors 2) " "
                  (= neighbors 2) (nth (nth board y) x)
                  (= neighbors 3) "#"
                  :else " ")))))))))

(defcheck solution-3b7deb62
  (fn next-state [world]
    (let [dead           \space
          alive          \#

          rows           (count world)
          cols           (count (first world))
          all-positions  (for [x (range rows) y (range cols)] [x y])

          neighbors
                         (fn [[x y]]
                           (filter (fn [[x y]] (and (>= x 0) (>= y 0) (< x rows) (< y rows)))
                             [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
                              [x (dec y)] [x (inc y)]
                              [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]]))

          get-cell
                         (fn [pos]
                           (get-in world pos))

          living-cell?
                         (fn [cell] (= cell alive))

          next-state-pos
                         (fn [pos]
                           (let [neighbor-cells (map #(get-in world %) (neighbors pos))
                                 live-neighbors (count (filter living-cell? neighbor-cells))]

                             (if (living-cell? (get-cell pos))
                               (if (or (= live-neighbors 2) (= live-neighbors 3))
                                 alive
                                 dead)
                               (if (= live-neighbors 3)
                                 alive
                                 dead))))

          hack-to-result (fn [cells] (into [] (map #(apply str %) (partition cols cells))))]

      (hack-to-result
        (map next-state-pos all-positions)))))

(defcheck solution-3b8695e2
  (fn live [board]
    (let
     [dim (count (first board))
      board-x
      ; expand by one extra zero-filled row/col from each side
          (map
            #(concat [0] (map (fn [x] ({\space 0 \# 1} x)) %) [0])
            (concat [(repeat dim \space)]
              board
              [(repeat dim \space)]
              ))
      xy  (fn [x y] (nth (nth board-x (inc x)) (inc y)))
      cnt (fn [x y]
            (apply +
              (for [dx [-1 0 1] dy [-1 0 1]
                    :when (not (= 0 dx dy))]
                (xy (+ x dx) (+ y dy)))))
      ]
      (for [x (range dim)]
        (apply str (for [y (range dim)
                         :let [alive? (= 1 (xy x y))
                               cntxy  (cnt x y)]]
                     (cond
                       (and alive? (< cntxy 2)) \space
                       (and alive? (<= cntxy 3)) \#
                       alive? \space
                       (= cntxy 3) \#
                       :else \space))))
      )))

(defcheck solution-3cc09e64
  (fn [board]
    (let [boardb  (vec (map #(vec (map {\space 0, \# 1} (seq %))) board))
          nrows   (count board)
          ncols   (count (first board))
          coords8 (filter #(not (and (= 0 (first %)) (= 0 (second %))))
                    (for [i (range -1 2) j (range -1 2)] [i j]))]
      (letfn [(getloc [r c]
                (if (or (< r 0) (< c 0) (>= r nrows) (>= c ncols)) 0
                                                                   (nth (nth boardb r) c)))
              (lucase [c nneis]
                (case c 1 (case nneis (2 3) \# \space)
                        0 (case nneis 3 \# \space)))
              (calcrow [r]
                (map #(lucase (getloc r %)
                        (reduce + (map (fn [[dr dc]] (getloc (+ r dr) (+ % dc)))
                                    coords8)))
                  (range ncols)))
              (rowstring [r] (apply str (calcrow r)))
              ]
        (map rowstring (range nrows))
        ))))

(defcheck solution-3cfe7de9
  (fn [board]
    (let [live?            (fn [x y] (= (get (get board y) x) \#))
          neighbours       [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
          count-neighbours (fn [x y] (count (filter (fn [[dx dy]] (live? (+ x dx) (+ y dy))) neighbours)))
          next-state       (fn [x y]
                             (let [cnt (count-neighbours x y)]
                               (if (live? x y)
                                 (if (and (>= cnt 2) (<= cnt 3)) \# \space)
                                 (if (= cnt 3) \# \space))))]
      (for [y (range (count board))]
        (apply str (for [x (range (count (get board y)))] (next-state x y)))))))

(defcheck solution-3d803627
  (let [indexed        (fn [coll] (map vector (range) coll))
        offsets        [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
        live?          (fn [c] (= c \#))
        live-neighbors (fn [addr board] (->> offsets
                                          (map #(map + addr %))
                                          (map #(get-in board %))
                                          (filter live?)))]
    (fn [board] (->> (for [[i row] (indexed board)]
                       (for [[j c] (indexed row)]
                         (let [n (count (live-neighbors [i j] board))]
                           (cond
                             (and (live? c) (< n 2)) \space
                             (and (live? c) (<= 2 n 3)) \#
                             (and (live? c) (> n 3)) \space
                             (= n 3) \#
                             :else c))))
                  (map #(apply str %))))))

(defcheck solution-3d9163a4
  (fn [board]
    (let [h           (count board)
          w           (count (nth board 0))
          directions  (remove #{[0 0]}
                        (mapcat (fn [x]
                                  (map (fn [y]
                                         [x y])
                                    [-1 0 1]))
                          [-1 0 1]))
          exist?      (fn [x y]
                        (= \#
                          (get
                            (get board y)
                            x)))
          neighbor    (fn [x y]
                        (count
                          (filter
                            (fn [[dx dy]]
                              (exist? (+ x dx)
                                (+ y dy)))
                            directions)))
          exist-next? (fn [x y]
                        (case (neighbor x y)
                          2 (exist? x y)
                          3 true
                          false))
          next-cell   (fn [x y]
                        (if (exist-next? x y)
                          \#
                          \space))]
      (map
        (fn [y]
          (apply str (map
                       (fn [x]
                         (next-cell x y))
                       (range 0 w))))
        (range 0 h)))))

(defcheck solution-3e45d566
  (fn [b]
    (let [m  (count b) n (count (first b))
          r  [-1 0 1]
          o  (for [i r j r :when (not= [i j] [0 0])] [i j])
          l? #(= \# (get-in b %))]
      (for [i (range m)]
        (apply str
          (for [j (range n) :let [p [i j]
                                  k (count (filter l? (map #(map + p %) o)))]]
            (if (or (and (l? p) (= k 2)) (= k 3)) \# \ )))))))

(defcheck solution-3e4a9466
  (fn [board]
    (let [dx         (count (first board))
          dy         (count board)
          neighbours (fn [pos board]
                       (let [xs [[-1 -1] [0 -1] [1 -1]
                                 [-1 0] [1 0]
                                 [-1 1] [0 1] [1 1]]]
                         (count (filter #(= % \#)
                                  (map #(get-in board
                                          (map + pos %)) xs)))))]
      (map
        #(apply str %)
        (partition
          dx
          (for [x (range dx) y (range dy)]
            (let [n     (neighbours [x y] board)
                  live? (= \# (get-in board [x y]))]
              (cond
                (and live? (< n 2)) \space
                (and live? (or (= n 2) (= n 3))) \#
                (and live? (> n 3)) \space
                (and (not live?) (= n 3)) \#
                :else \space))))))))

(defcheck solution-3e78e356
  (fn [board]
    (letfn [(neighbors [[row column]]                       ;get the neighbor positions
              (map (fn [[r c]] [(+ r row), (+ c column)]) '([-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1])))
            (cleanPositions [row column posSeq]             ;remove illegal positions
              (filter (fn [[r c]] (and (< -1 r row) (< -1 c column))) posSeq))]
      (let [row (count board) column (count (first board))]
        (reduce merge []                                    ;prepare the final board result
          (for [r (range row)]                              ;iterate over rows
            (apply str                                      ;prepare the row result
              (for [c (range column)]                       ;iterate over columns
                (let [localVal   (get (get board r) c)      ;get local value
                      numOfLives (count (filter #(= \# %) (map (fn [[r c]] (get (get board r) c)) (cleanPositions row column (neighbors [r c])))))] ;number of lives around
                  (if (= localVal \#)                       ;game rules
                    (cond
                      (< numOfLives 2) \space
                      (< numOfLives 4) \#
                      :else \space)
                    (if (= numOfLives 3) \# \space)))))))))))

(defcheck solution-3fb76afb
  (fn [b]
    (letfn [(get-in-board [board pos]
              (let [[r c] pos]
                (let [got (get (get board r) c)]
                  (if got got \space))
                ))
            (neighbours [[x y]]
              (for [i (range -1 2)
                    j (range -1 2)
                    :when (not (and (= 0 i) (= 0 j)))]
                [(+ x i) (+ y j)]))
            (count-neighbours [b [x y]]
              (apply + (for [n (neighbours [x y])
                             :when (= (get-in-board b n) \#)]
                         1)))
            (next-val [g pos]
              (case (count-neighbours g pos)
                2 (get-in-board g pos)
                3 \#
                \space))]
      (for [i (range (count b))]
        (apply str (for [j (range (count (b i)))]
                     (next-val b [i j])))))
    ))

(defcheck solution-3fe49e52
  (fn next-board [board]
    (let [r                (count board)
          c                (count (first board))
          alive?           (fn [x y]
                             (= \# (get-in board [x y])))
          neighbors        (fn [x y]
                             (for [xx [(dec x) x (inc x)]
                                   yy [(dec y) y (inc y)]
                                   :when (not (and (= x xx)
                                                   (= y yy)))]
                               (alive? xx yy)))
          count-neighbors  (fn [x y]
                             (count (filter identity
                                      (neighbors x y))))
          alive-next-tick? (fn [x y]
                             (let [cnt (count-neighbors x y)]
                               (or (= cnt 3)
                                   (and (alive? x y)
                                        (= cnt 2)))))]
      (for [i (range r)]
        (apply str
          (for [j (range c)]
            (if (alive-next-tick? i j)
              "#"
              " ")))))))

(defcheck solution-42400255
  (fn game-of-life [board]
    (letfn [(live-cells [x y]
              (count (filter #(= \# %)
                       (for [i (range -1 2) j (range -1 2)]
                         (get-in board [(+ y i) (+ x j)])))))]
      (map #(apply str %) (partition (count (first board))
                            (for [y (range (count (first board))) x (range (count board))]
                              (let [cell  (get-in board [y x])
                                    alive (= \# cell)]
                                (cond
                                  (and alive (< (live-cells x y) 3)) \space
                                  (and alive (> (live-cells x y) 4)) \space
                                  (and (not alive) (= (live-cells x y) 3)) \#
                                  :default cell)))))
      )))

(defcheck solution-42486182
  (fn board-next [board]
    (let [neibors-pos [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]
          t           {\# 1 \space 0}
          c           (count board)]
      (letfn [(pos-add [[xa ya] [xb yb]]
                [(+ xa xb) (+ ya yb)])
              (ceil [[x y]]
                (if (and (< -1 x c) (< -1 y c))
                  (nth (nth board x) y) \space))
              (neibors-num [[x y :as pos]]
                (map (fn [pos] (t (ceil pos)))
                  (map (fn [e] (pos-add pos e)) neibors-pos)))
              (ceil-next [ceil n]
                (if (= ceil \#)
                  (cond
                    (< -1 n 2) \space
                    (<= 2 n 3) \#
                    (< 3 n) \space)
                  (if (= n 3) \# \space)))]
        (let [poses (for [x (range c) y (range c)] [x y])
              next  (map (fn [pos] (ceil-next
                                     (ceil pos)
                                     (apply + (neibors-num pos)))) poses)]
          (map #(apply str %) (partition c c next)))))))

(defcheck solution-42569da2
  (let [zip             (partial map vector)
        enum            (partial zip (iterate inc 0))
        rfs             [dec identity inc]
        string          (partial apply str)
        neighbor-coords (fn [rmax cmax r c]
                          (for [f rfs :let [i (f r)] :when (<= 0 i rmax)
                                f rfs :let [j (f c)] :when (and (<= 0 j cmax)
                                                                (not= [r c] [i j]))]
                            [i j]))
        count-neighbors (fn [board coords]
                          (count (filter #{\#} (map #(get-in board %) coords))))]
    (fn [board]
      (let [f (comp (partial count-neighbors board)
                (partial neighbor-coords
                  (count board)
                  (count (first board))))]
        (for [[r xs] (enum board)]
          (string
            (for [[c x] (enum xs)]
              (condp = (f r c)
                2 x, 3 \#, \space))))))))

(defcheck solution-4257d4ef
  (fn game-of-life [board]
    (let [siz (count board)]
      (letfn [(add-to-borders [what a-coll] (concat [what] a-coll [what]))
              (to-bit-matrix [board] (map (partial map #(case %1 \# 1 0)) board))
              (extend [board] (add-to-borders (repeat (+ 2 siz) 0) (map (partial add-to-borders 0) (to-bit-matrix board))))
              (sum-surroundings [e i j]
                (apply +
                  (map
                    (fn [[x y]] (nth (nth e x) y))
                    [[i (dec j)] [i (inc j)] [(dec i) (dec j)] [(dec i) j] [(dec i) (inc j)] [(inc i) (dec j)] [(inc i) j] [(inc i) (inc j)]]
                    )
                  )
                )
              (freqs [board]
                (partition siz
                  (let [e (extend board)]
                    (for [i (range 1 (inc siz))
                          j (range 1 (inc siz))]
                      (sum-surroundings e i j)
                      ))))
              (to-hash-matrix [board] (map (comp (partial apply str) (partial map #(case %1 1 "#" " "))) board))
              ]
        (to-hash-matrix
          (map
            (fn [row freq-row]
              (map
                (fn [val f]
                  (cond
                    (and (= 1 val) (< f 2)) 0
                    (and (= 1 val) (> f 3)) 0
                    (and (= 0 val) (= f 3)) 1
                    :default val
                    )
                  )
                row freq-row))
            (to-bit-matrix board) (freqs board))
          )
        ))))

(defcheck solution-427dca5
  (fn [l]
    (let [g #(= (get-in l %) \#)
          n (fn [[x y]]
              (let [a (range -1 2)
                    k (for [i a j a :when (not (= 0 i j))]
                        [(+ x i) (+ y j)])]
                (reduce #(if (g %2) (inc %) %) 0 k)))
          v #(cond
               (= (n %) 3) \#
               (and (= (n %) 2) (g %)) \#
               :else " ")]
      (mapv #(apply str %) (for [i (range (count l))]
                             (for [j (range (count (first l)))]
                               (v [i j])))))
    ))

(defcheck solution-42cd3dc1
  (fn [b]
    (map-indexed
      #(apply str
         (for [j (range (count %2))]
           (case [(get %2 j \space)
                  (reduce + (for [x [-1 0 1] y [-1 0 1]]
                              ({\# 1} (get-in b [(+ %1 x) (+ j y)]) 0)))]
             ([\# 3] [\# 4] [\space 3]) \# \space)))
      b)))

(defcheck solution-42d358c6
  (fn c94
    [v]
    (letfn [(get-neighbors [i j xlimit ylimit]
              (for [x (filter #(and (>= % 0) (<= % xlimit)) [(dec i) i (inc i)])
                    y (filter #(and (>= % 0) (<= % ylimit)) [(dec j) j (inc j)])]
                [x y]))]
      (let [vs     (mapv vec v)
            xlimit (dec (count v))
            ylimit (dec (count (first v)))]
        (loop [i       0
               j       0
               vectors vs]
          (let [neighbors (filter #(not (= [i j] %)) (get-neighbors i j xlimit ylimit))
                val       (case (nth (nth vs i) j)
                            \space (if (= 3 (reduce (fn [r v]
                                                      (if (= (nth (nth vs (first v)) (second v)) \#)
                                                        (inc r)
                                                        r)) 0 neighbors))
                                     \X
                                     \space)
                            \# (let [lives (reduce (fn [r v]
                                                     (if (= (nth (nth vs (first v)) (second v)) \#)
                                                       (inc r)
                                                       r)) 0 neighbors)]
                                 (if (< lives 2)
                                   \Y
                                   (if (> lives 3)
                                     \Y
                                     \#))))]
            (if (= j ylimit)
              (if (= i xlimit)
                (mapv (fn [v]
                        (let [s (clojure.string/join v)]
                          (clojure.string/replace
                            (clojure.string/replace
                              s
                              #"X" "#")
                            #"Y" " "))) vectors)
                (recur (inc i) 0 (assoc vectors i (assoc (nth vectors i) j val))))
              (recur i (inc j) (assoc vectors i (assoc (nth vectors i) j val))))))))))

(defcheck solution-4346c187
  (let [

        make-board      (fn [lines]
                          (let [line-to-vec #(into [] (map {\space :e, \# :l} %))]
                            (into [] (map line-to-vec lines))))

        stringify-board (fn [board]
                          (into [] (map (fn [line] (reduce #(str % ({:e \space, :l \#} %2)) "" line)) board)))

        xcount          (fn [board] (count board))

        ycount          (fn [board] (count (board 0)))

        neighbors       (fn [[x y]] (map (fn [[dx dy]] [(+ dx x) (+ dy y)]) (for [x [-1 0 1] y [-1 0 1] :let [pos [x y]] :when (not= [0 0] pos)] pos)))

        live-neighbors  (fn [board pos]
                          (count (filter #{:l} (map #(get-in board %) (neighbors pos)))))

        new-live        (fn [board positions]
                          (filter #(= 3 (live-neighbors board %)) positions))

        new-dead        (fn [board positions]
                          (filter
                            #(or
                              (< (live-neighbors board %) 2)
                              (> (live-neighbors board %) 3))
                            positions))
        ]

    (fn [lines]
      (let [board     (make-board lines)
            positions (for [x (range (xcount board)) y (range (ycount board))] [x y])
            nl        (new-live board positions)
            nd        (new-dead board positions)
            b1        (reduce #(assoc-in % %2 :l) board nl)
            b2        (reduce #(assoc-in % %2 :e) b1 nd)]
        (stringify-board b2)))))

(defcheck solution-4386a322
  (fn [board]
    (let [alive   \#
          dead    \space
          offsets [[-1 -1] [0 -1] [1 -1]
                   [-1 0] [1 0]
                   [-1 1] [0 1] [1 1]]
          rules   {alive #{2 3}
                   dead  #{3}}]
      (letfn [(calc-dimensions [matrix]
                [[0 (dec (count matrix))] [0 (dec (count (first matrix)))]])

              (valid-coordinate? [[[min-row max-row] [min-col max-col]] [row col]]
                (and (<= min-col col max-col)
                     (<= min-row row max-row)))

              (alive? [matrix [x y]]
                (let [row (nth matrix x)
                      val (nth row y)]
                  (= alive val)))

              (alive-neighbor-count [[i j] matrix]
                (let [dimensions (calc-dimensions matrix)]
                  (->> (map #(map + % [i j]) offsets)
                    (filter #(valid-coordinate? dimensions %))
                    (map #(alive? matrix %))
                    (filter true?)
                    count)))

              (gen-cells [matrix]
                (for [[i row] (map-indexed vector matrix)
                      [j cell] (map-indexed vector row)]
                  (vector i j cell)))

              (calc-next-val [matrix [x y cell]]
                (let [neighbor-count (alive-neighbor-count [x y] matrix)
                      is-alive       (get rules cell)]
                  (if (is-alive neighbor-count) alive dead)))

              (calc-next-generation [board]
                (let [matrix (map seq board)]
                  (->> (gen-cells matrix)
                    (map #(calc-next-val matrix %))
                    (partition (count (first board)))
                    (map clojure.string/join))))
              ]
        (calc-next-generation board)

        )
      )
    ))

(defcheck solution-438b20ca
  (fn [board]
    (let [live-cell \#
          dead-cell \space
          live?     (fn [cell] (= live-cell cell))
          neighbors (fn [x y]
                      (let [spread (fn [x] [(dec x) x (inc x)])
                            coords (filter
                                     #(not= [x y] %)
                                     (for [a (spread x)
                                           b (spread y)]
                                       [a b]))]
                        (filter (complement nil?)
                          (map
                            (fn [[a b]]
                              (when-let [_ (contains? board a)]
                                (when-let [_ (contains? (vec (board a)) b)]
                                  ((vec (board a)) b))))
                            coords))))]
      (map-indexed
        (fn [r row]
          (apply
            str
            (map-indexed
              (fn [c cell]
                (let [live-neighbors (count (filter live? (neighbors r c)))]
                  (if (live? cell)
                    (cond
                      (< live-neighbors 2) dead-cell
                      (> live-neighbors 3) dead-cell
                      :else live-cell)
                    (if (= 3 live-neighbors)
                      live-cell
                      dead-cell))))
              row)))
        board))))

(defcheck solution-4546590
  (fn gamelife [board]
    (let [linecnt    (count board)
          rankcnt    (count (first board))
          bmap       (for [i (range linecnt)]
                       (for [j (range rankcnt)]
                         (hash-map [i j] (str (nth (nth board i) j)))))
          bmp        (apply concat bmap)
          bp         (apply merge bmp)
          uppn       (fn [loc bpm lm rm]
                       (let [[i j] loc]
                         (if (< (dec i) 0)
                           nil
                           (bpm [(dec i) j]))))
          downn      (fn [loc bpm lm rm]
                       (let [[i j] loc]
                         (if (> (inc i) (dec lm))
                           nil
                           (bpm [(inc i) j]))))
          leftn      (fn [loc bpm lm rm]
                       (let [[i j] loc]
                         (if (< (dec j) 0)
                           nil
                           (bpm [i (dec j)]))))
          rightn     (fn [loc bpm lm rm]
                       (let [[i j] loc]
                         (if (> (inc j) (dec rm))
                           nil
                           (bpm [i (inc j)]))))
          leftuppn   (fn [loc bpm lm rm]
                       (let [[i j] loc]
                         (if (or (< (dec j) 0) (< (dec i) 0))
                           nil
                           (bpm [(dec i) (dec j)]))))
          leftdownn  (fn [loc bpm lm rm]
                       (let [[i j] loc]
                         (if (or (< (dec j) 0) (> (inc i) (dec lm)))
                           nil
                           (bpm [(inc i) (dec j)]))))
          rightuppn  (fn [loc bpm lm rm]
                       (let [[i j] loc]
                         (if (or (> (inc j) (dec rm)) (< (dec i) 0))
                           nil
                           (bpm [(dec i) (inc j)]))))
          rightdownn (fn [loc bpm lm rm]
                       (let [[i j] loc]
                         (if (or (> (inc j) (dec rm)) (> (inc i) (dec lm)))
                           nil
                           (bpm [(inc i) (inc j)]))))
          neighbours (fn [loc bpm lm rm]
                       (list
                         (uppn loc bpm lm rm)
                         (downn loc bpm lm rm)
                         (leftn loc bpm lm rm)
                         (rightn loc bpm lm rm)
                         (leftuppn loc bpm lm rm)
                         (leftdownn loc bpm lm rm)
                         (rightuppn loc bpm lm rm)
                         (rightdownn loc bpm lm rm)))
          nstatus    (fn [loc bpm lm rm]
                       (let [status (if (= (bpm loc) "#") :live :dead)
                             liven  (count (filter #(= "#" %) (neighbours loc bpm lm rm)))]
                         (cond
                           (and (= status :live) (< liven 2)) " "
                           (and (= status :live) (or (= liven 2) (= liven 3))) "#"
                           (and (= status :live) (> liven 3)) " "
                           (and (= status :dead) (= liven 3)) "#"
                           :else " ")))]
      (for [i bmap]
        (apply str (for [j i]
                     (nstatus (first (keys j)) bp linecnt rankcnt)))))))

(defcheck solution-457ce766
  (letfn [(axis [a] (map #(+ a %) [-1 0 1]))
          (neighbours-indexes [x y]
            (remove (partial = [x y])
              (for [x_ (axis x) y_ (axis y)] (vector x_ y_))))
          (get-at [board x y] (get (get board y) x))
          (check-neighbours [board x y]
            (let [neigh-idxs (neighbours-indexes x y)]
              (frequencies
                (map (fn [[x_ y_]] (get-at board x_ y_)) neigh-idxs))))
          (live-cycle [cell neighbours]
            (if (= cell \#)
              (if (< 1 (get neighbours \#) 4) \# \space)
              (if (= (get neighbours \#) 3) \# \space)))]
    (fn [board]
      (let [x-dim (range (count (first board)))
            y-dim (range (count board))]
        (mapv (fn [y]
                (apply str
                  (map (fn [x]
                         (live-cycle (get-at board x y)
                           (check-neighbours board x y)))
                    x-dim)))
          y-dim)))))

(defcheck solution-46137bb
  (fn gol
    [input]
    (let [board      (mapv #(vec (seq %)) input)
          tetangga   (fn [i j]
                       (let [f (map #(get-in board [(dec i) %]) (map #(+ j %) [-1 0 1]))
                             s (map #(get-in board [i %]) (map #(+ j %) [-1 1]))
                             t (map #(get-in board [(inc i) %]) (map #(+ j %) [-1 0 1]))]
                         (count (filter #(= \# %) (flatten [f s t])))))
          condi-live (fn [n]
                       (cond
                         (< n 2) \space
                         (<= n 3) \#
                         (> n 3) \space))
          condi-dead (fn [n]
                       (if (= 3 n)
                         \#
                         \space))
          cr         (count board)
          cc         (count (first board))
          ij         (for [i (range 0 cr)
                           j (range 0 cc)]
                       [i j])
          raw        (mapv (fn [koor] (let [[i j] koor]
                                        [(get-in board [i j]) (tetangga i j)]))
                       ij)
          beres      (mapv (fn [x]
                             (let [[r rs] x]
                               (if (= r \#)
                                 (condi-live rs)
                                 (condi-dead rs))))
                       raw)]
      (map #(apply str %) (partition cc beres)))))

(defcheck solution-467b79a2
  (fn [strs]
    (let [
          m         (count strs)
          n         (count (first strs))
          nbs       (fn [[x y]]
                      (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
                        [(+ x dx) (+ y dy)]))
          conv-to   (fn [strs]
                      (reduce (fn [res pos]
                                (if (= \# (get-in strs pos))
                                  (conj res pos) res))
                        []
                        (for [i (range m)
                              j (range n)]
                          [i j])))
          conv-back (fn [poss]
                      (let [mapp (vec (repeat m (vec (repeat n \space))))]
                        (reduce #(assoc-in % %2 \#) mapp poss)))
          poss      (set (conv-to strs))
          mapp      (frequencies (apply clojure.set/union (map nbs poss)))
          new-poss  (filter
                      #(or (= 3 (mapp %)) (and (= 2 (mapp %)) (poss %)))
                      (map first mapp))]
      #_(println poss)
      (map (partial apply str) (conv-back new-poss)))))

(defcheck solution-46929fc1
  (fn gol [board]
    (let [b         (mapv (partial replace {\# 1 \space 0}) (map vec board))
          rows      (count b)
          cols      (count (first b))
          at        (fn [r c] (nth (nth b r) c))
          around    (fn [r c] (for [i [-1 0 1], j [-1 0 1]] (at (+ r i) (+ c j))))
          neighbors (fn [r c] (- (reduce + (around r c)) (at r c)))
          update    (fn [r c]
                      (cond
                        (= r 0) 0
                        (= c 0) 0
                        (= r (dec rows)) 0
                        (= c (dec cols)) 0
                        :else
                        (let [x (at r c)
                              n (neighbors r c)]
                          (cond
                            (< n 2) 0
                            (= n 3) 1
                            (> n 3) 0
                            :else x))))
          n         (for [i (range rows), j (range cols)] (update i j))
          next      (map (partial apply str) (partition cols (replace {1 \# 0 \space} n)))]
      next)))

(defcheck solution-46cfbd3b
  (fn [ar] (->>
             (map
               (fn [el]                                     ; <= gets all neighbors
                 (let [x (first el) y (last el)]
                   (filter

                     (fn [s]
                       (and
                        (not-any? neg? s)
                        (not-any? #(< (dec (count ar)) %) s))
                       )

                     (partition 2
                       (interleave
                         (map #(+ x %) [-1 0 1 -1 1 -1 0 1])
                         (map #(+ y %) [-1 -1 -1 0 0 1 1 1]))))))
               (partition 2 (flatten (map #(interleave (repeat %) (range (count ar))) (range (count ar))))))
             (map #(map (fn [el]
                          (let [x (first el) y (last el)]

                            (get
                              (get
                                ar x)
                              y)
                            )

                          ) %))

             (map #(filter (fn [i] (= \# i)) %))
             (map count)
             (interleave (flatten (map vec ar)))
             (partition 2)
             (map
               (fn [pair]
                 (let [now (first pair) c (last pair)]
                   (cond
                     (and (= \# now) (> 2 c)) \space
                     (and (= \# now) (or (= 2 c) (= 3 c))) \#
                     (and (= \# now) (< 3 c)) \space
                     (and (= \space now) (= 3 c)) \#
                     :else \space)
                   )))
             (partition (count ar))
             (map #(apply str %))
             )))

(defcheck solution-470f450c
  (fn [s]
    (let [pad    (first s)
          w      (count pad)
          s      (map #(map str %) s)
          blocks (->> s
                   (map #(partition 3 1 %))
                   (apply map list)
                   (map #(partition 3 1 %))
                   (apply map list))]
      (->>
        (for [pos blocks nhd pos]
          (let [alive? (= "#" (second (second nhd)))
                n      ((frequencies (flatten nhd)) "#")]
            (if alive?
              (case n (3 4) "#" " ")
              (case n 3 "#" " "))))
        (partition (- w 2))
        (map #(-> (flatten [" " % " "])
                clojure.string/join))
        (#(flatten [pad % pad]))
        ))))

(defcheck solution-4729367
  (fn [world]
    (let [w                (count (first world))
          h                (count world)
          in-world?        (fn [[x y]]
                             (and (>= x 0)
                                  (<= x w)
                                  (>= y 0)
                                  (<= y h)))
          alive?           (fn [[x y]]
                             (and (in-world? [x y])
                                  (= \# (get-in world [y x]))))
          neighbours       (fn [[x y]]
                             (for [i [-1 0 1]
                                   j [-1 0 1]
                                   :when (not (= i j 0))]
                               [(+ x i) (+ y j)]))
          count-neighbours (fn [pos]
                             (count (filter alive? (neighbours pos))))
          next-state       (fn [pos]
                             (let [n (count-neighbours pos)]
                               (if (or (= n 3)
                                       (and (= n 2)
                                            (alive? pos)))
                                 "#"
                                 " ")))
          evolve-line      (fn [y]
                             (apply str (map #(next-state [% y])
                                          (range w))))]
      (map evolve-line (range h)))))

(defcheck solution-472c1053
  (fn [b]
    (for [y (range (count b))]
      (apply str
        (for [x (range (count (nth b y)))]
          (let [v [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
                l (map #(nth (nth b (+ y (second %)) [\space]) (+ x (first %)) \space) v)
                n (count (filter #(= \# %) l))]
            (if (= (nth (nth b y) x) \#)
              (if (or (= n 2) (= n 3)) \# \space)
              (if (= n 3) \# \space))))))))

(defcheck solution-4801e900
  (fn life [board]
    (let [vb   (vec (map vec board))
          rows (count vb)
          cols (count (vb 0))
          neighbor-locs
               (fn [r c]
                 (for [dr [-1 0 1] dc [-1 0 1] :when (not= 0 dr dc)]
                   [(+ r dr) (+ c dc)]))
          num-live-neighbors
               (fn [vb r c]
                 (count (filter #(= \# %) (for [rc (neighbor-locs r c)] (get-in vb rc)))))
          nextgenlist
               (fn [vb]
                 (for [r (range rows) c (range cols)]
                   (let [live (num-live-neighbors vb r c)]
                     (cond
                       (< live 2) \space
                       (= live 2) (get-in vb [r c])
                       (= live 3) \#
                       (> live 3) \space))))]
      (vec (map #(apply str %) (partition rows (nextgenlist vb)))))))

(defcheck solution-4823c433
  (fn gol [board]
    (let
     [m         (count board)
      n         (count (first board))
      alives    (filter #(= \# (get-in board %))
                  (for [r (range m)
                        c (range n)]
                    [r c]))
      neighbors (for [[r c] alives
                      dr [-1 0 1]
                      dc [-1 0 1]
                      :when (not= dr dc 0)]
                  [(+ r dr) (+ c dc)])
      new       (map first
                  (filter (fn [[[r c] nbors]]
                            (cond
                              (< nbors 2) false
                              (= nbors 2) (= \# (get-in board [r c]))
                              (= nbors 3) true
                              :else false))
                    (frequencies neighbors)))
      new-board (vec (repeat m (vec (repeat n \space))))
      final     (reduce #(assoc-in % %2 \#) new-board new)]
      (mapv #(apply str %) final))))

(defcheck solution-48455a16
  (let [C count
        G get-in
        R range
        M map
        O -1]

    (fn [s]
      (for [r (R (C s))]
        (apply str
          (for [c (R (C (nth s 0)))]
            (if (#(or (and (= (G s [r c]) \#) (< 1 % 4)) (= % 3))
                 (C
                   (filter
                     #{\#}
                     (M
                       #(G s (M + % [r c]))
                       [[O O] [O 0] [O 1]
                        [1 1] [1 0] [1 O]
                        [0 1] [0 O]]))))
              \#
              \ )))))))

(defcheck solution-489b59ad
  (fn [board]
    (let [y-extent              (dec (count board))
          x-extent              (dec (count (first board)))
          live-cell-ranges      (fn [live-cells]
                                  (cond
                                    (< live-cells 2) "< 2"
                                    (> live-cells 3) "> 3"
                                    :else (str live-cells)))
          decision-table        {\#     {"< 2" \space, "2" \#, "3" \#, "> 3" \space},
                                 \space {"< 2" \space, "2" \space, "3" \#, "> 3" \space}}
          get-neighbours        (fn [x y]
                                  (let [offsets          (for [x-offset [0 1 -1] y-offset [0 1 -1]
                                                               :when (or (not= 0 x-offset)
                                                                         (not= 0 y-offset))]
                                                           [x-offset y-offset])
                                        all-neighbours   (map (fn [[x-offset y-offset]] [(- x x-offset) (- y y-offset)]) offsets)
                                        valid-neighbours (filter (fn [[x y]] (and (<= 0 x x-extent) (<= 0 y y-extent))) all-neighbours)]
                                    valid-neighbours))
          get-at                (fn [[x y]] (nth (nth board y) x))
          count-live-neighbours (fn [x y] (apply + (map {\# 1, \space 0} (map get-at (get-neighbours x y)))))]
      (map #(apply str %) (partition (inc x-extent)
                            (for [y (range (inc y-extent)) x (range (inc x-extent))]
                              (get-in decision-table [(get-at [x y]) (live-cell-ranges (count-live-neighbours x y))])))))
    ))

(defcheck solution-48e79923
  (fn game-of-life [m]
    (let [cell     (fn [x y] (if (or (>= y (count m)) (>= x (count (first m))) (< y 0) (< x 0)) nil (str (nth (nth m y) x))))
          live?    (fn [x y] (= (cell x y) "#"))
          lc       (fn [x y] (if (live? x y) 1 0))
          nghb     (fn [x y] (apply + (map (partial apply lc) [
                                                               [(- x 1) y]
                                                               [(+ x 1) y]
                                                               [x (- y 1)]
                                                               [x (+ y 1)]
                                                               [(- x 1) (- y 1)]
                                                               [(- x 1) (+ y 1)]
                                                               [(+ x 1) (- y 1)]
                                                               [(+ x 1) (+ y 1)]])))
          nxt      (fn [x y]
                     (let [live (live? x y)
                           nc   (nghb x y)]
                       (cond
                         (and live (< nc 2)) " "
                         (and live (< nc 4)) "#"
                         (and live (>= nc 4)) " "
                         (and (not live) (= nc 3)) "#"
                         :else " ")))
          next-row (fn [y row] (apply str (map-indexed (fn [i e] (nxt i y)) row)))]
      (map-indexed (fn [i e] (next-row i e)) m))))

(defcheck solution-49492fdd
  (fn igl [b]
    (let [rows   (range (count b)) cols (range (count (first b)))
          lcls   (into {} (filter #(= \# (second %))
                            (for [x rows y cols] [[x y] (nth (b x) y)])))
          close? (fn [[x y] [a b]] (and (<= (dec x) a (inc x))
                                        (<= (dec y) b (inc y))))
          cln    (fn [c] (count (filter #(and (not= c (key %))
                                              (close? c (key %))) lcls)))
          nec    (fn [c] (if (lcls c)
                           (if (<= 2 (cln c) 3) \# \space)
                           (if (= (cln c) 3) \# \space)))]
      (for [x rows] (apply str (for [y cols] (nec [x y])))))))

(defcheck solution-497f9007
  (fn [a]
    (let [w (count (a 0))
          h (count a)]
      (letfn [(life-at [x y]
                (if (and (>= x 0) (>= y 0)
                         (< x w) (< y h))
                  (if (= \# (nth (a y) x)) 1 0)
                  0))
              (rules [life n]
                (cond
                  (< n 2) " "
                  (< n 3) (if (= 1 life) "#" " ")
                  (< n 4) "#"
                  :else " "
                  ))
              (neighbours [x y]
                (rules (life-at x y) (reduce + (for [i (range -1 2) j (range -1 2)]
                                                 (if (= 0 i j)
                                                   0
                                                   (life-at (+ x i) (+ y j)))))))
              (row [i]
                (apply str (map #(neighbours % i) (range w))))]
        (map row (range h))))))

(defcheck solution-49d0cf07
  (fn [m]
    (let [m (vec (map vec (seq m)))
          h (count m)
          w (count (first m))
          c (fn [x y] (get-in m [(mod y h) (mod x w)]))
          n (fn [x y] (count
                        (filter
                          #(= % \#)
                          [(c (dec x) (dec y))
                           (c x (dec y))
                           (c (inc x) (dec y))
                           (c (dec x) y)
                           (c (inc x) y)
                           (c (dec x) (inc y))
                           (c x (inc y))
                           (c (inc x) (inc y))
                           ])))
          z (fn [x y]
              (let [nn    (n x y)
                    live? (= (c x y) \#)]
                (cond
                  (and live? (< nn 2)) \space
                  (and live? (> nn 3)) \space
                  live? \#
                  (and (not live?) (= nn 3)) \#
                  :default \space)))
          r (for [y (range h)]
              (for [x (range w)]
                (z x y)))]
      (map #(apply str %) r))))

(defcheck solution-4a33cc93
  (fn conway [field]
    (let [m       (count field)
          n       (count (first field))
          coords  (for [i (range m) j (range n)] [i j])
          nbs     (fn [[x y]]
                    (frequencies (keep identity (vector
                                                  (get-in field [(dec x) (dec y)])
                                                  (get-in field [(dec x) y])
                                                  (get-in field [(dec x) (inc y)])
                                                  (get-in field [x (dec y)])
                                                  (get-in field [(inc x) (dec y)])
                                                  (get-in field [(inc x) y])
                                                  (get-in field [(inc x) (inc y)])
                                                  (get-in field [x (inc y)])))))
          progeny (fn [[x y]]
                    (let [current (get-in field [x y])
                          near    (nbs [x y])]
                      (cond
                        (and (= \# current) (< (near \#) 2)) \space
                        (and (= \# current) (< (near \#) 4)) \#
                        (and (= \# current) (> (near \#) 3)) \space
                        (and (= \space current) (= (near \#) 3)) \#
                        :else current)))]
      (map #(apply str %) (partition m (map progeny coords))))))

(defcheck solution-4a6c6f5d
  (fn [bs]
    (let [b (mapv vec bs)
          h (count b)
          w (count (first b))]
      (->> (for [i (range h)]
             (for [j (range w)]
               (let [l? (= \# (get-in b [i j]))
                     k  (->> (for [x [-1 0 1] y [-1 0 1]]
                               (get-in b [(+ i y) (+ j x)]))
                          (filter #(= \# %))
                          count)]
                 (if l?
                   (case k 3 \# 4 \# \space)
                   (if (= k 3) \# \space)))))
        (map #(apply str %))))))

(defcheck solution-4ab4671f
  (fn [board]
    (let [live?   (fn [x y]
                    (= \# (-> board (nth y) (nth x))))
          neibors (fn [x y]
                    (->> (for [i (range (dec x) (+ 2 x))
                               j (range (dec y) (+ 2 y))
                               :when (or (not= i x) (not= j y))]
                           (live? i j))
                      (map #(if % 1 0))
                      (reduce + 0)))
          width   (count (first board))
          height  (count board)]
      (for [y (range height)]
        (apply str (for [x (range width)]
                     (cond
                       (#{0 (dec width)} x) \space
                       (#{0 (dec height)} y) \space
                       (> 2 (neibors x y)) \space
                       (< 3 (neibors x y)) \space
                       (= 3 (neibors x y)) \#
                       (live? x y) \#
                       :else \space)))))))

(defcheck solution-4ab6ccc8
  (fn conway [state]
    (letfn
     [(index-board
        [board]
        (map #(map-indexed vector %) board))
      (neighbor-in-row
        [row indexed-cell]
        (if (empty? row) 0
                         (let [idx    (first indexed-cell)
                               target (if (= idx 0) (take 2 row)
                                                    (take 3 (drop (dec (first indexed-cell)) row)))]
                           (count (filter #(= "#" (str (second %))) target)))))
      (calc-neighbors
        [row-above row row-below]
        (let [cts-above    (map #(neighbor-in-row row-above %) row)
              cts-below    (map #(neighbor-in-row row-below %) row)
              cts-adjacent (map #(if (= "#" (str (second %)))
                                   (dec (neighbor-in-row row %))
                                   (neighbor-in-row row %)) row)
              cts          (map + cts-above (map + cts-below cts-adjacent))]
          (map #(list %1 (str (second %2))) cts row)))
      (calc-board
        [acc1 acc2 board]
        (if (empty? board) (reverse acc1)
                           (recur
                             (conj acc1
                               (calc-neighbors (first acc2) (first board) (first (rest board))))
                             (conj acc2 (first board))
                             (rest board))))
      (process-cell
        [cell]
        (let [ct    (first cell)
              alive (= "#" (second cell))]
          (cond (and alive (< ct 2)) " "
                (and alive (or (= 3 ct) (= ct 2))) "#"
                (and alive (> ct 3)) " "
                (and (not alive) (= 3 ct)) "#"
                0 (second cell))))]
      (map #(apply str (map process-cell %)) (calc-board '() '() (index-board state))))))

(defcheck solution-4abd6b4d
  (fn [board]
    (letfn [(get-cell [r c] (nth (nth board r) c))
            (neighbors [r c]
              (for [x [-1 0 1] y [-1 0 1]
                    :let [nr (+ r x)
                          nc (+ c y)]
                    :when (and (>= nr 0)
                               (>= nc 0)
                               (< nr (count board))
                               (< nc (count (first board)))
                               (not (and (= nr r)
                                         (= nc c))))] [nr nc]))
            (get-alive-count [r c] ((frequencies (map #(apply get-cell %) (neighbors r c))) \#))
            (alive-rules [live-count]
              (cond (< live-count 2) \space
                    (> live-count 3) \space
                    :else \#))
            (dead-rules [live-count]
              (if (= live-count 3) \# \space))
            (new-state [r c]
              (let [cell        (get-cell r c)
                    alive-count (get-alive-count r c)]
                (if (= cell \#) (alive-rules alive-count)
                                (dead-rules alive-count))
                )
              )
            ]
      (map #(apply str %)
        (map-indexed (fn [r row]
                       (map-indexed (fn [c col] (new-state r c)) row)) board))
      )
    ))

(defcheck solution-4ae9f16a
  (fn [b]
    (let [n     (count b)
          diff  [[1 0] [-1 0] [0 1] [0 -1] [1 1] [1 -1] [-1 1] [-1 -1]]
          nb    (fn [[i j]]
                  (filter (fn [x] (every? #(and (>= % 0) (< % n)) x))
                    (map vector (map #(+ (% 0) i) diff) (map #(+ (% 1) j) diff))))
          live? (fn [[i j]] (= ((vec (b i)) j) \#))
          nlive (fn [x] (count (filter live? (nb x))))]
      (for [i (range n)]
        (apply str
          (for [j (range n)]
            (cond (= 3 (nlive [i j])) \#
                  (and (live? [i j]) (= 2 (nlive [i j]))) \#
                  :else \space)))))))

(defcheck solution-4b36ccb0
  (fn [board]
    (let [neighb (fn neighb [board i j]
                   (for [k (range (dec i) (inc (inc i))) l (range (dec j) (inc (inc j))) :when (not (and (= k i) (= l j)))]
                     (get (vec (get board k)) l)))
          rule   (fn rule [board i j]
                   (let [neighbours (neighb board i j)
                         live-cells (count (filter #(= \# %) neighbours))]
                     (if (= \# (get (vec (get board i)) j))
                       (cond (> 2 live-cells) \space
                             (> 4 live-cells) \#
                             :else \space)
                       (cond (= 3 live-cells) \#
                             :else \space))))]
      (map-indexed (fn [i v] (apply str (map-indexed (fn [j x] (rule board i j)) v))) board))))

(defcheck solution-4b6b6d79
  (letfn [(neighbor-coords [r c]
            (let [n   [-1 0 1]
                  rns (map (partial + r) n)
                  cns (map (partial + c) n)]
              (for [x rns y cns :when (or (not= r x) (not= c y))] [x y])))
          (is-alive [c]
            (= c \#))
          (score-cell [board x y]
            (let [get-cell (partial get-in board)]
              (reduce + 0 (map #(if (is-alive (get-cell %)) 1 0) (neighbor-coords x y)))))
          (cell-rules [c scr]
            (let [alive (is-alive c)
                  live  \#
                  die   \space]
              (cond
                (and alive (< scr 2)) die
                (and alive (contains? #{2 3} scr)) live
                (and alive (> scr 3)) die
                (and (not alive) (= scr 3)) live
                :else die)))
          (map-indexed-2d [f vss]
            (map-indexed (fn [i vs] (map-indexed (fn [j v] (f v i j)) vs)) vss))]
    (fn [board]
      (let [board     (vec (map vec board))
            next-cell (fn [cell x y] (cell-rules cell (score-cell board x y)))]
        (map (partial apply str) (map-indexed-2d next-cell board))))))

(defcheck solution-4bdc6fab
  (fn [board]
    (let [w            (count (first board))
          h            (count board)
          live?        (fn [[x y]] (and (>= x 0) (< x w) (>= y 0) (< y h) (= \# (get-in board [y x]))))
          neighbors    (fn [[x y]]
                         (count (filter live? [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)] [(inc x) (inc y)] [(dec x) (dec y)] [(inc x) (dec y)] [(dec x) (inc y)]])))
          should-live? (fn [xy]
                         (cond
                           (< (neighbors xy) 2) false
                           (= 2 (neighbors xy)) (live? xy)
                           (= 3 (neighbors xy)) true
                           true false))]
      (map (fn [y]
             (apply str (map (fn [x]
                               (if (should-live? [x y]) "#" " ")) (range w)))) (range h)))))

(defcheck solution-4be96b2b
  (fn [board]
    (let [adjacent-range (fn [n]
                           (map #(% n) [dec identity inc]))
          neighbors      (fn [[x y :as coord]]
                           (for [neighbor-x (adjacent-range x)
                                 neighbor-y (adjacent-range y)
                                 :when (not= [neighbor-x neighbor-y] coord)]
                             [neighbor-x neighbor-y]))

          width          (count (first board))
          height         (count board)
          coords         (for [x (range width)
                               y (range height)
                               :when (= (nth (nth board y) x) \#)]
                           [x, y])
          coords-set     (set coords)

          next-coords    (for [[coord num-neighbors] (frequencies (mapcat neighbors coords-set))
                               :when (or (= num-neighbors 3)
                                         (and (= num-neighbors 2)
                                              (coords-set coord)))]
                           coord)

          next-board     (let [blank-board (vec (repeat height (vec (repeat width \space))))
                               rows        (reduce (fn [board [x y]]
                                                     (assoc-in board [y x] \#))
                                             blank-board
                                             next-coords)]
                           (for [row rows]
                             (apply str row)))]
      next-board)))

(defcheck solution-4bf2d4a0
  (fn [board] (let [
                    [D L] [\space \#]
                    binb (map (partial map #({D 0 L 1} %)) board)
                    [w h] [(count (first board)) (count board)]
                    rn   #(map (partial + %) [-1 0 1])
                    in?  (fn [i j] (and (>= i 0) (< i w) (>= j 0) (< j h)))
                    ncnt (map-indexed (fn [j r]
                                        (map-indexed (fn [i c] (reduce +
                                                                 (for [x (rn i) y (rn j)
                                                                       :when (and (in? x y) (not= [x y] [i j]))]
                                                                   (nth (nth binb y) x))))
                                          r))
                           binb)]
                (map #(apply str
                        (map (fn [c nl]
                               (let [l (= c 1) d (not l)]
                                 (cond
                                   (and l (< nl 2)) D
                                   (and l (> nl 3)) D
                                   (and d (= nl 3)) L
                                   :else ({0 D 1 L} c)))) %1 %2))
                  binb ncnt))))

(defcheck solution-4d63111a
  (fn [game]
    (let [adjacent  (fn [[x y]]
                      (filter #(not (some #{-1} %))
                        [[(dec x) (dec y)]
                         [(dec x) y]
                         [(dec x) (inc y)]
                         [x (dec y)]
                         [x (inc y)]
                         [(inc x) (dec y)]
                         [(inc x) y]
                         [(inc x) (inc y)]]))
          step-cell (fn [cell game]
                      (let [live?           (fn [value] (= \# value))
                            value           (get-in game cell)
                            neighbours      (adjacent cell)
                            live-neighbours (count (filter live? (map #(get-in game %) neighbours)))
                            live-map        (zipmap (range 0 9) "  ##     ")
                            dead-map        (zipmap (range 0 9) "   #     ")]
                        (if (live? value)
                          (live-map live-neighbours)
                          (dead-map live-neighbours))))
          step      (fn [game]
                      (let [size (count game)]
                        (map #(apply str %)
                          (partition size (for [x (range 0 size)
                                                y (range 0 size)
                                                :let [cell [x y]]]
                                            (step-cell cell game))))))]
      (step game))))

(defcheck solution-4de6f277
  (fn [a] (

            map (fn [s] (apply str s)) (
                                         partition (count (first a)) (
                                                                       map char (

                                                                                  loop [

                                                                                        m

                                                                                          (concat
                                                                                            [(flatten (repeat (+ 2 (count (first a))) '(32)))]
                                                                                            (map (fn [w] (flatten (conj '(32) (map char->ascii (seq w)) '(32)))) a)
                                                                                            [(flatten (repeat (+ 2 (count (first a))) '(32)))])

                                                                                        n []

                                                                                        x 1
                                                                                        y 1
                                                                                        ] (

                                                                                            if (= y (dec (count m))) n
                                                                                                                     (if (= x (dec (count (first m))))
                                                                                                                       (recur m (concat n []) 1 (inc y))
                                                                                                                       (recur m
                                                                                                                         (conj n
                                                                                                                           (let [
                                                                                                                                 z (nth (nth m y) x)
                                                                                                                                 w (+ (nth (nth m (dec y)) (dec x)) (nth (nth m (dec y)) x) (nth (nth m (dec y)) (inc x)) (nth (nth m y) (dec x)) (nth (nth m y) (inc x)) (nth (nth m (inc y)) (dec x)) (nth (nth m (inc y)) x) (nth (nth m (inc y)) (inc x)))
                                                                                                                                 ] (
                                                                                                                                     if (= z 32) (if (= w 265) 35 32)
                                                                                                                                                 (cond
                                                                                                                                                   (< w 262) 32
                                                                                                                                                   (> w 265) 32
                                                                                                                                                   :else 35)
                                                                                                                                                 ))
                                                                                                                           )
                                                                                                                         (inc x) y)
                                                                                                                       )
                                                                                                                     )) ;

                                                                       )
                                         ))
    ))

(defcheck solution-4e019b6d
  (fn [b]
    (for [j (range (count (first b)))]
      (let [row-chars (for [i (range (count b))]
                        (let [nn     (count
                                       (for [ny (range (dec j) (+ 2 j))
                                             nx (range (dec i) (+ 2 i))
                                             :when (and (= \# (get-in b [ny nx]))
                                                        (or (not= ny j)
                                                            (not= nx i)))]
                                         [ny nx (= \# (get-in b [ny nx]))]))
                              reborn (if (= \# (get-in b [j i]))
                                       (#{2 3} nn)
                                       (#{3} nn))]
                          (if reborn \# \space)))]
        (apply str row-chars)))))

(defcheck solution-4f05c86f
  (fn [b]
    (map-indexed
      (fn [ri row]
        (apply
          str
          (map-indexed
            #(let [nc (count (for [x (range (dec ri) (+ ri 2)) y (range (dec %1) (+ %1 2))
                                   :when (and (not= [x y] [ri %1])
                                              (= \# (get-in b [x y])))]
                               1))]
               (cond
                 (= %2 \#)
                 (cond
                   (< nc 2) \space
                   (and (> nc 1) (< nc 4)) \#
                   :else \space)
                 (and (= %2 \space) (= nc 3)) \#
                 :else %2))
            row)))
      b)))

(defcheck solution-4f104ab4
  (fn [board]
    (let [neighbours (for [x [-1 0 1] y [-1 0 1] :when (not (= 0 x y))] [x y])
          xsize      (count (nth board 0))
          ysize      (count board)
          live?      (fn [x y] (and (<= 0 x (dec xsize))
                                    (<= 0 y (dec ysize))
                                    (= \# (nth (nth board y) x))))]
      (for [y (range ysize)]
        (clojure.string/join
          (for [x (range xsize)]
            (let [n (count (filter (fn [[i j]] (live? (+ x i) (+ y j))) neighbours))]
              (cond
                (and (live? x y) (< n 2)) \space
                (and (live? x y) (<= 2 n 3)) \#
                (and (live? x y) (< 3 n)) \space
                (and (not (live? x y)) (= 3 n)) \#
                :else (nth (nth board y) x)))))))))

(defcheck solution-4fa54716
  (fn life-game [board]
    (let [height          (count board)
          width           (count (first board))
          get-status      (fn [x y] (if (or (<= height x) (<= width y) (> 0 x) (> 0 y))
                                      \space
                                      (-> board (nth x) (nth y))))
          count-life-num  (fn [x y] (count (filter #(= % \#)
                                             (map #(apply get-status %)
                                               [[x (- y 1)]
                                                [x (+ y 1)]
                                                [(- x 1) y]
                                                [(+ x 1) y]
                                                [(- x 1) (- y 1)]
                                                [(- x 1) (+ y 1)]
                                                [(+ x 1) (- y 1)]
                                                [(+ x 1) (+ y 1)]]))))
          get-next-status (fn [x y]
                            (let [status            (get-status x y)
                                  num-of-neighbours (count-life-num x y)]
                              (if (= \# status)
                                (if (or (= num-of-neighbours 2) (= num-of-neighbours 3))
                                  \#
                                  \space)
                                (if (= num-of-neighbours 3)
                                  \#
                                  \space))))]
      (map (fn [i] (apply str (map (fn [j] (get-next-status i j)) (range width)))) (range height)))))

(defcheck solution-4fdae7d2
  (fn [s]
    (let
     [mi (count s) mj (count (s 0))
      ge (fn [i j] (get (s i) j))
      f  (fn [i j] (if (or (< i 0) (= i mi) (< j 0) (= j mj)) [] [(ge i j)]))
      nb (fn [i j] (concat (f (dec i) (dec j)) (f (dec i) j) (f (dec i) (inc j)) (f i (dec j)) (f i (inc j)) (f (inc i) (dec j)) (f (inc i) j) (f (inc i) (inc j))))
      lv (fn [i j] (count (filter #(= \# %) (nb i j))))
      g  (fn [i j c] (let [d (lv i j)] (if (or (= d 3) (and (= d 2) (= c \#))) \# \space)))]
      (map-indexed (fn [i s2] (apply str (map-indexed (fn [j c] (g i j c)) s2))) s))))

(defcheck solution-505bd1ef
  (fn next-board [board]
    (let [pad      (list (clojure.string/join (repeat (count (first board)) " ")))
          augboard (concat pad board pad)
          lives?   (fn [board x y]
                     (let [x1        (+ 3 x) y1 (+ 3 y) alive (= "#" (subs (board y) x (inc x)))
                           near      (count (remove #(or (= \space %))
                                              (clojure.string/join (subvec
                                                                     (vec (map #(subs (str " " % " ") x x1)
                                                                            augboard)) y y1))))
                           neighbors (if alive (dec near) near)]
                       (if (not alive) (if (= neighbors 3) "#" " ")
                                       (if (or (= neighbors 2) (= neighbors 3)) "#" " "))))]
      (map (fn [y] (clojure.string/join (map #(lives? board % y)
                                          (range (count (first board))))))
        (range (count board))))))

(defcheck solution-50b12ef5
  (fn lifegame [inputBox]
    (let [boxs     (reduce #(conj %1 (seq %2)) [] inputBox),
          maxx     (count boxs),
          maxy     (count (peek boxs)),
          getVal   (fn [x y]
                     (if (or (< x 0) (< y 0) (= x maxx) (= y maxy))
                       0
                       (if (= \# (nth (nth (seq boxs) x) y))
                         1
                         0)
                       )
                     ),
          boxVals  (fn [x y]
                     (+ (apply +
                          (for [a (range (- x 1) (+ x 2)),
                                b (range (- y 1) (+ y 2)) :when (not (and (= x a) (= y b)))]
                            (getVal a b)
                            )) (* (getVal x y) 10))
                     ),
          printBox (fn [box]
                     (do (doseq [s box]
                           #_(println (str s))
                           )
                         box)
                     )

          ]
      #_(printBox inputBox)

      (apply conj []
        (for [x (range 0 maxx)]
          (apply str (for [y (range 0 maxy)]
                       (let [val (boxVals x y)]
                         (cond (or (= val 12) (= val 13)) \#
                               (= val 3) \#
                               :else \space)
                         )
                       )
            )
          )
        )

      )

    ))

(defcheck solution-515d7a40
  (fn [x] (let [g (to-array-2d x)
                n (dec (alength g))
                m (dec (alength (aget g 0)))
                c (fn [a b] (count (filter #(= % \#) (list (aget g (dec a) (dec b))
                                                       (aget g (dec a) b)
                                                       (aget g (dec a) (inc b))
                                                       (aget g a (dec b))
                                                       (aget g a (inc b))
                                                       (aget g (inc a) (dec b))
                                                       (aget g (inc a) b)
                                                       (aget g (inc a) (inc b))))))]
            (conj (vec (cons (first x) (map #(str (apply str " " %) " ") (partition (dec m) (for [y (range 1 n)
                                                                                                  x (range 1 m)]
                                                                                              (let [v (aget g y x)
                                                                                                    w (c y x)]
                                                                                                (if (= v \#) (if (#{2 3} w) v \space) (if (= 3 w) \# v)))))))) (last x)))))

(defcheck solution-51b42f11
  (fn [b]
    (letfn [
            (valid-cell? [x y board]
              (and (>= x 0)
                   (>= y 0)
                   (< y (count board))
                   (< x (count (board y)))))

            (neighbor-coords [x y board]
              (filter #(valid-cell? (% 0) (% 1) board)
                [[x (inc y)]
                 [x (dec y)]
                 [(inc x) y]
                 [(dec x) y]
                 [(dec x) (dec y)]
                 [(dec x) (inc y)]
                 [(inc x) (inc y)]
                 [(inc x) (dec y)]]))

            (cell [x y board]
              ((board y) x))

            (neighbor-count [x y board]
              (count (filter #(= \# %)
                       (map #(cell (% 0) (% 1) board)
                         (neighbor-coords x y board)))))

            (next-gen-cell [x y board]
              (let [c (cell x y board)
                    n (neighbor-count x y board)]
                (if (= \# c)                                ; live cell
                  (cond (< n 2) \space
                        (> n 3) \space
                        :else \#)
                  (if (= n 3) \# \space))))                 ; dead cell

            (next-gen [board]
              (let [b      (into [] (map #(into [] (seq %)) board))
                    width  (count (first b))
                    height (count b)]
                (loop [x 0, y 0, acc b]
                  (cond (>= y height) (map #(reduce str %) acc)
                        (>= x width) (recur 0 (inc y) acc)
                        :else (recur (inc x) y
                                (assoc acc y (assoc (acc y) x
                                                            (next-gen-cell x y b))))))))]
      (next-gen b))))

(defcheck solution-51d98cdb
  (fn [z] (let [s (vec (map vec z))
                n (count s)
                m (count (first s))
                d [-1 0 1]
                R range
                P \space
                W \#]
            (letfn [(g [x y]
                      (let [v (get-in s [x y])
                            q (apply + (for [a d b d]
                                         (if (= W (get-in s [(+ a x) (+ b y)]))
                                           1
                                           0)))]
                        (cond
                          (and (= P v) (= q 3)) W
                          (and (= W v) (> q 2) (< q 5)) W
                          :else P)))]
              (map #(apply str %) (for [x (R 0 n)]
                                    (for [y (R 0 m)]
                                      (g x y)))

                )))))

(defcheck solution-52206fd7
  (fn conway [board]
    (let [cells     (set (for [y (range (count board))
                               x (range (count (get board y)))
                               :when (not= \space (get-in board [y x]))]
                           [x y]))
          width     (count (first board))
          height    (count board)
          neighbors (fn [[x y]]
                      (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
                        [(+ x dx) (+ y dy)]))
          step      (fn [cells]
                      (set (for [[loc n] (frequencies (mapcat neighbors cells))
                                 :when (or (= n 3) (and (= n 2) (cells loc)))]
                             loc)))
          serialize (fn [alive]
                      (mapv #(apply str %)
                        (partition width
                          (for [y (range height) x (range width)
                                :let [sym (if (alive [x y]) \# \space)]]
                            sym))))]
      (-> cells step serialize))))

(defcheck solution-52adcdc2
  (fn [b]
    (letfn [(nbors [b x y]
              (count
                (filter (fn [a] (= a \#))
                  (mapcat
                    (fn [i] (map
                              (fn [j] (if (and (= i 0) (= j 0))
                                        nil
                                        (get (get b (- x i)) (- y j))))
                              (range -1 2)))
                    (range -1 2)))))]
      (map (fn [a] (apply str a))
        (map (fn [i]
               (map
                 (fn [j] [i j]
                   (let [e (get (get b i) j)
                         n (nbors b i j)]
                     (if (= e \#)
                       (if (or (= 3 n) (= 2 n))
                         \# \space)
                       (if (= 3 n)
                         \# \space))))
                 (range (count b)))) (range (count (first b))))))))

(defcheck solution-52b2d92e
  (fn life [ls] (let [yy (count ls)
                      xx (count (ls 0))]
                  (for [y (range yy)]
                    (apply str (for [x (range xx)]
                                 (if (or (= x (dec xx))
                                         (= y (dec yy))
                                         (= 0 (* x y))) " "
                                                        (condp =
                                                               (-
                                                                (count (filter #(= % \#) (apply str (map #(subs % (dec x) (+ 2 x)) [(ls (dec y)) (ls y) (ls (inc y))]))))
                                                                (if (= (get (ls y) x) \#) 1 0))
                                                          3 \#
                                                          2 (get (ls y) x)
                                                          " "))))))))

(defcheck solution-52d490d9
  (fn [world]
    (let [parse-board
          (fn [row-strings]
            {:size {:width  (reduce #(max %1 (count %2)) 0 row-strings)
                    :height (count row-strings)}
             :cells
                   (->>
                     (map-indexed
                       (fn [row s]
                         (map-indexed
                           (fn [col c]
                             (when (= c \#)
                               [col row]))
                           s))
                       row-strings)
                     (apply concat)
                     (remove nil?)
                     (set))})

          present-board
          (fn [cells [width height]]
            (vec
              (for [row (range height)]
                (apply
                  str
                  (for [col (range width)]
                    (if (contains? cells [col row])
                      "#" " "))))))

          neighbors
          (fn [cell]
            (let [[col row] cell]
              (set (for [c [-1 0 1] r [-1 0 1]
                         :when (not= 0 c r)]
                     [(+ col c) (+ row r)]))))

          count-live-neighbors
          (fn [cells cell]
            (count (clojure.set/intersection cells (neighbors cell))))

          lives?
          (fn [current-generation-cells cell]
            (if (contains? current-generation-cells cell)
              (<= 2 (count-live-neighbors current-generation-cells cell) 3)
              (= 3 (count-live-neighbors current-generation-cells cell))))

          {{:keys [width height]} :size :keys [cells]} (parse-board world)]
      (present-board
        (->> (for [col (range width) row (range height)] [col row])
          (filter (partial lives? cells))
          (set))
        [width height]))))

(defcheck solution-5364b132
  (fn [bb]
    (letfn [
            ; calc neighbour cells
            (nbs [[x y]]
              (->> '([-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1])
                (map #(vector (+ x (% 0)) (+ y (% 1))))
                (map #(get-in bb %))
                (filter #(= \# %))
                (count)
                ))

            ; calc next status
            (new_cell [[x y]]
              (let [nb_count (nbs [x y])]
                (if (= \# (get-in bb [x y]))
                  (if (or (< nb_count 2) (> nb_count 3)) \space \#)
                  (if (= 3 nb_count) \# \space))))

            ; calc new board
            (new_bb [bb]
              (let [rows (count bb), cols (count (bb 0))]
                (->>
                  (for [x (range rows), y (range cols)] [x y]) ; all coords
                  (map new_cell)
                  (partition rows)
                  (map #(apply str %)))))
            ]
      (new_bb bb))))

(defcheck solution-53a3c31e
  (fn __ [gamefield]
    (let [field                  (map (partial map identity) gamefield)
          fieldsize              (count field)
          rotate-left            (fn [x] (conj (vec (rest x)) (first x)))
          rotate-right           (fn [x] (cons (last x) (vec (drop-last x))))
          left-neighbors         (map rotate-left field)
          right-neighbors        (map rotate-right field)
          bottom-neighbors       (rotate-left field)
          top-neighbors          (rotate-right field)
          top-right-neighbors    (rotate-right left-neighbors)
          top-left-neighbors     (rotate-right right-neighbors)
          bottom-right-neighbors (rotate-left left-neighbors)
          bottom-left-neighbors  (rotate-left right-neighbors)
          neighbors              (map #(hash-map :# (count (filter #{\#} %)) :space (count (filter #{\space} %)))
                                   (apply (partial map (partial conj []))
                                     (map flatten [left-neighbors right-neighbors
                                                   bottom-neighbors top-neighbors
                                                   top-right-neighbors top-left-neighbors
                                                   bottom-left-neighbors bottom-right-neighbors])))
          ]
      (map #(apply str %)
        (partition fieldsize
          (map
            #(if (= %1 \#)
               (if (#{2 3} (:# %2)) \# \space)
               (if (= 5 (:space %2)) \# \space)) (flatten field)
            neighbors)))
      )))

(defcheck solution-544aa402
  (fn [b]
    (let [cell           (fn [[x y]]
                           (nth (b y) x))
          alive?         (fn [[x y]]
                           (if (or (< y 0) (>= y (count b)))
                             0
                             (if (or (< x 0) (>= x (count (b y))))
                               0
                               (if (= \# (nth (b y) x))
                                 1
                                 0))))
          env            [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
          neighbourcount (fn [c]
                           (apply + (map #(alive? (map + c %)) env)))
          newval         (fn [c]
                           (if (or (< (neighbourcount c) 2) (> (neighbourcount c) 3))
                             " "
                             (if (= (neighbourcount c) 3)
                               "#"
                               (cell c))))
          newrow         (fn [y]
                           (apply str (map #(newval [% y]) (range (count (b y))))))
          ]
      (map #(newrow %) (range (count b))))))

(defcheck solution-558052e7
  (fn l [f]
    (letfn [(empty-row [n] (apply list (repeat n " ")))
            (snth [coll i] (if (and (>= i 0) (< i (count coll))) (nth coll i) " "))
            (around [i c p n]
              [(snth c (dec i)) (snth c (inc i))
               (snth p (dec i)) (snth p i) (snth p (inc i))
               (snth n (dec i)) (snth n i) (snth n (inc i))]
              )
            (new-map [c p n]
              (map-indexed
                #(let [a (count (filter (fn [e] (= "#" (str e))) (around %1 c p n)))]
                   (cond
                     (and (= (str %2) "#") (< a 2)) " "
                     (and (= (str %2) "#") (< a 4)) "#"
                     (and (= (str %2) "#")) " "
                     (and (= (str %2) " ") (= 3 a)) "#"
                     :else " "
                     )
                   )
                c)
              )]
      (loop [prev (empty-row (count (first f)))
             cur  (first f)
             next (second f)
             r    (rest (rest (conj (conj f prev) prev)))
             res  []]
        (if (empty? r) res
                       (recur cur next (first r) (rest r) (conj res (apply str (new-map cur prev next))))
                       )
        )
      )
    ))

(defcheck solution-56bb7b28
  (fn
    [board]
    (letfn [(board-dimensions [] [(count board) (count (board 0))])
            (alive? [point] (= \# (get-in board point)))
            (constrain [[r c]]
              (let [[mr mc] (board-dimensions)]
                (when (and (>= r 0)
                           (< r mr)
                           (>= c 0)
                           (< c mc))
                  [r c])))
            (neighbors [[r c]]
              (for [dr (range -1 2)
                    dc (range -1 2)
                    :when (not (and (= dr 0) (= dc 0)))]
                (remove nil? (constrain [(+ r dr) (+ c dc)]))))
            (tick-cell [point]
              (let [neighbor-count (count (filter alive? (neighbors point)))]
                (if (alive? point)
                  (if (some #{neighbor-count} #{2 3}) \# \space)
                  (if (= 3 neighbor-count) \# \space))))]
      (for [r (range (count board))]
        (apply str
          (for [c (range (count (board 0)))]
            (tick-cell [r c])))))))

(defcheck solution-57aa8b21
  (fn [coll]
    (let [previous
                 (set (mapcat #(mapcat (fn [i x] (if (= x \#) [[% i]] [])) (range) %2) (range) coll))
          result (->> previous
                   (mapcat (fn [[a b]] (for [m [[0 -1 1]]
                                             x m
                                             y m
                                             :when (not= 0 x y)]
                                         [(+ a x) (+ b y)])))
                   frequencies
                   (keep (fn [[cell f]] (if (or (= f 3) (and (previous cell) (= f 2)))
                                          cell)))
                   set)]
      (vec (for [[i row] (map vector (range) coll)]
             (apply str (for [j (range (count row))]
                          (if (result [i j]) "#" " "))))))))

(defcheck solution-5832f815
  (fn next-gen [generation]
    (letfn [(index-neighbours [index max-index]
              (cond
                (= index 0) [index (inc index)]
                (= max-index index) [(dec index) index]
                :else [(dec index) index (inc index)]))
            (cell-neighbours [row column max-row max-column]
              (let [neightbour-rows (index-neighbours row max-row)
                    neightbour-cols (index-neighbours column max-column)]
                (for [r neightbour-rows c neightbour-cols :when (not (and (= r row) (= c column)))]
                  [r c])))

            (get-cell [generation row column]
              (= \# (.charAt (generation row) column)))

            (count-live-neighbours [generation row column max-row max-column]
              (let [neighbours        (cell-neighbours row column max-row max-column)
                    neighbours-values (map (fn [[row column]] (get-cell generation row column)) neighbours)]
                (count (filter identity neighbours-values))))
            (cell-next-gen [cell live-neghbours-count]
              (if cell
                (cond
                  (> live-neghbours-count 3) false
                  (< live-neghbours-count 2) false
                  :else true
                  )
                (if (= live-neghbours-count 3)
                  true
                  false)))
            ]

      (let [row-count    (count generation)
            max-row      (dec row-count)
            column-count (count (first generation))
            max-column   (dec column-count)
            ]
        (into
          []
          (map
            #(apply str %)
            (partition column-count
              (map
                #(if % \# \space)
                (for [row (range 0 row-count) column (range 0 column-count)]
                  (cell-next-gen
                    (get-cell generation row column)
                    (count-live-neighbours generation row column (dec row-count) (dec column-count))))))))))))

(defcheck solution-5859f1ee
  (fn life [sb]
    (let [
          b      (vec (map vec sb))
          to-str (fn [b] (map #(reduce str %) b))
          cells  (fn [b]
                   (let [rng (range 1 (dec (count b)))]
                     (for [x rng y rng] [x y])))
          rng    (fn [n] (map #(+ n %) (range -1 2)))
          l?     (fn [p] (= \# (get-in b p)))
          lc     (fn [[x y]]
                   (count
                     (filter l? (for [x1 (rng x) y1 (rng y)] [x1 y1]))))
          nx     (fn [p]
                   (let [cnt (lc p) lv? (l? p)]
                     (cond
                       (and lv? (= 3 cnt)) \#
                       (and lv? (= 4 cnt)) \#
                       (and (not lv?) (= 3 cnt)) \#
                       :else \space
                       )))
          ]
      (to-str
        (reduce
          #(assoc-in %1 %2 (nx %2))
          b
          (cells b)))
      )))

(defcheck solution-58fb7166
  (fn [starting-board]
    (let [living?      (fn [[i j]]
                         (= \# (get-in starting-board [i j])))
          neighbors    (fn [[i j]]
                         (count
                           (filter identity
                             (for [[id jd] [[0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1]]]
                               (living? [(+ i id) (+ j jd)])))))
          should-live? (fn [p]
                         (case (neighbors p)
                           0 false
                           1 false
                           2 (living? p)
                           3 true
                           4 false
                           5 false
                           6 false
                           7 false
                           8 false))]
      (for [i (range (count starting-board))]
        (apply str
          (for [j (range (count (get starting-board i)))]
            (if (should-live? [i j])
              \#
              \space)))))))

(defcheck solution-591ceb99
  (fn next-board [strs]
    (let [h (count strs), w (count (first strs))]
      (letfn [(life? [i j]
                (= \# (nth (nth strs i) j)))
              (in-board? [i j]
                (and (>= i 0) (< i h) (>= j 0) (< j w)))
              (count-around [i j]
                (reduce +
                  (for [x [-1 0 1], y [-1 0 1] :when (not (= x y 0))]
                    (cond (not (in-board? (+ i x) (+ j y))) 0
                          (life? (+ i x) (+ j y)) 1
                          :else 0))))]
        (for [y (range h)]
          (apply str
            (for [x (range w)]
              (let [around (count-around y x)]
                (if (life? y x)
                  (if (or (= around 2) (= around 3)) \# \space)
                  (if (= around 3) \# \space))))))))))

(defcheck solution-59336732
  (fn gol [board]
    (let [bget     (fn bget [row col]
                     (if (or
                          (>= row (count board)) (< row 0)
                          (>= col (count (nth board row))) (< col 0))
                       nil
                       (nth (nth board row) col)))
          occupied (fn occupied [row col] (if (= (bget row col) \#) 1 0))
          neighbors
                   (fn neighbors [row col]
                     (+
                       (occupied (inc row) col) (occupied (dec row) col)
                       (occupied row (inc col)) (occupied row (dec col))
                       (occupied (inc row) (inc col)) (occupied (dec row) (dec col))
                       (occupied (dec row) (inc col)) (occupied (inc row) (dec col))))]
      (for [row (range (count board))]
        (apply str
          (for [col (range (count (nth board row)))]
            (case (neighbors row col)
              0 \ , 1 \ , 2 (bget row col), 3 \#, \ )))))))

(defcheck solution-5941dfd7
  (fn gol [s]
    (letfn [(neighbors [x y]
              (let [x1 (if (< (- x 1) 0) 0 (- x 1))
                    y1 (if (< (- y 1) 0) 0 (- y 1))
                    x2 (if (> (+ x 1) (dec (count s))) (dec (count s)) (+ x 1))
                    y2 (if (> (+ y 1) (dec (count s))) (dec (count s)) (+ y 1))]
                (for [a (range x1 (inc x2))]
                  (for [o (range y1 (inc y2))]
                    (if (and (= a x) (= o y))
                      (if (= (nth (nth s a) o) \#) \l \d)
                      (nth (nth s a) o))))))
            (fs [x y]
              (let [n (frequencies (flatten (neighbors x y)))]
                (if (= (n \l) 1)
                  (cond
                    (nil? (n \#)) \space
                    (< (n \#) 2) \space
                    (or (= (n \#) 2) (= (n \#) 3)) \#
                    (> (n \#) 3) \space)

                  (if (= (n \#) 3)
                    \#
                    \space))))]

      (map #(apply str %) (for [x (range (count s))]
                            (for [y (range (count s))]
                              (fs x y)))))))

(defcheck solution-59b94e6d
  (fn nextGen [b]
    (let [b'      (vec (map vec b))
          getCell (fn [x y] (get (get b' y []) x \space))
          countNeighbors
                  (fn [x y]
                    (let [offsets [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
                      (count (for [[dx dy] offsets
                                   :when (= \# (getCell (+ x dx) (+ y dy)))]
                               \#))))]
      (for [y (range (count b'))]
        (->> (for [x (range (count (get b' y)))]
               (let [n (countNeighbors x y)]
                 (if (= (getCell x y) \space)
                   (if (= 3 n) \# \space)
                   (if (or (= 2 n) (= 3 n)) \# \space))))
          (apply str))))))

(defcheck solution-5a9b0afa
  (fn [in]
    (let [pos (fn [[r c]] (get (get in r) c))]
      (for [row (range (count in))]
        (apply str (for [col (range (count (nth in row)))]
                     (let [u     (dec row)
                           d     (inc row)
                           l     (dec col)
                           r     (inc col)
                           ns    [[u l] [u col] [u r] [row r] [d r] [d col] [d l] [row l]]
                           score (count (filter #(when-let [p (pos %)] (= \# p)) ns))
                           state (pos [row col])]
                       (if (or (= score 3) (and (= state \#) (= score 2)))
                         \# \space))))))))

(defcheck solution-5af5723d
  (fn [s]
    (letfn [(to-board [s] (vec
                            (map-indexed
                              (fn [y row]
                                (->> row
                                  seq
                                  (map-indexed
                                    (fn [x v] [[y x] ({\space 0
                                                       \#     1} v)]))
                                  vec))
                              s)))

            (display-board [grid] (map (fn [row]
                                         (apply str
                                           (map (fn [[c v]]
                                                  ([\space \#] v))
                                             row)))
                                    grid))

            (val-at [grid x y] (let [row  (get grid y [])
                                     cell (get row x [])]
                                 (get cell 1 0)))

            (num-neighbors [grid x y] (+ (val-at grid (dec x) (dec y))
                                        (val-at grid x (dec y))
                                        (val-at grid (inc x) (dec y))
                                        (val-at grid (dec x) y)
                                        (val-at grid (inc x) y)
                                        (val-at grid (dec x) (inc y))
                                        (val-at grid x (inc y))
                                        (val-at grid (inc x) (inc y))))

            (mapcells [f grid] (map (fn [row]
                                      (map
                                        (fn [cell] (f cell))
                                        row))
                                 grid))

            (next-gen? [curr-gen num-neighbors] (cond (> 2 num-neighbors) false
                                                      (< 3 num-neighbors) false
                                                      (= 3 num-neighbors) true
                                                      :else curr-gen))]
      (let [old (to-board s)
            new (mapcells
                  (fn [[[y x] v]] [[y x]
                                   (if
                                    (next-gen?
                                      (not (zero? (val-at old x y)))
                                      (num-neighbors old x y))
                                     1 0)])
                  old)]

        (display-board new)))))

(defcheck solution-5b0a794a
  (fn [b] (letfn [
                  (count-neighbors [b x y]
                    (count (for [xoff [-1 0 1]
                                 yoff [-1 0 1]
                                 :when (not= 0 xoff yoff)
                                 :let [xpos  (+ x xoff)
                                       ypos  (+ y yoff)
                                       piece (get-in b [xpos ypos] \space)]
                                 :when (= piece \#)]
                             1)))]
            (let [width (count (first b)) height (count b)]
              (map #(apply str %1) (partition width (for [
                                                          x (range width)
                                                          y (range height)
                                                          :let [current    (get-in b [x y])
                                                                neighbors  (count-neighbors b x y)
                                                                emptylives (and (= current \space) (= neighbors 3))
                                                                livelives  (and (= current \#) (>= neighbors 2) (<= neighbors 3))
                                                                newval     (if (or emptylives livelives) \# \space)
                                                                ]
                                                          ]
                                                      newval
                                                      ))
                )))))

(defcheck solution-5b0bda6c
  (fn myf [board]
    (letfn [(coor [board x y]
              (let [limit (dec (count board))]
                (if (or (< x 0) (> x limit) (< y 0) (> y limit)) \space
                                                                 (nth (nth board y) x))))
            (next-state [board x y]
              (let [center         (nth (nth board y) x)
                    neighbors      (for [dx [x (inc x) (dec x)]
                                         dy [y (inc y) (dec y)]
                                         :when (or (not= dx x) (not= dy y))]
                                     (coor board dx dy))
                    live-neigh-cnt (count (filter #(= % \#) neighbors))]
                (cond (= live-neigh-cnt 3) \#
                      (and (= live-neigh-cnt 2) (= center \#)) \#
                      :else \space)))]
      (->> (for [x (range (count board)), y (range (count board))]
             (next-state board y x))
        (partition (count board))
        (map #(apply str %))))))

(defcheck solution-5b7eda48
  (fn [cells]
    (letfn [(add-cells [line]
              (concat [(last line)] line [(first line)]))
            (change-state [[c1 [c21 c22 c23] c3]]
              (let [neighs (concat c1 c3 [c21 c23])
                    lives  (->> neighs (filter (partial = \#)) count)]
                (cond
                  (and (= c22 \#) (or (< lives 2) (> lives 3))) \space
                  (and (= c22 \#) (or (= lives 2) (= lives 3))) c22
                  (and (= c22 \space) (= lives 3)) \#
                  :else c22)))
            (partition-3 [line] (partition 3 1 line))]
      (->> cells
        (map seq)
        (map add-cells)
        (add-cells)
        partition-3
        (map #(map partition-3 %))
        (map #(apply map (fn [c1 c2 c3] (vector c1 c2 c3)) %))
        (map #(map change-state %))
        (map (partial apply str))))))

(defcheck solution-5b813492
  (fn [R E C b]
    (let [g #(nth (nth b % []) %2 \ )
          h #(R (fn [n [x y]] (+ n ({\  0 \# 1} (g (+ % x) (+ %2 y)))))
               0
               [[-1 -1] [-1 0] [-1 1]
                [0 -1] [0 1]
                [1 -1] [1 0] [1 1]])]
      (R (fn [u i]
           (conj u (R (fn [v j]
                        (str v ({2 (g i j) 3 \#} (h i j) " ")))
                     ""
                     (E (C (peek b))))))
        []
        (E (C b))))) reduce range count)

(defcheck solution-5bb0f1e9
  (fn [board]
    (let [width  (count (nth board 0))
          height (count board)]
      (letfn [(cell [row col] (get-in board [(mod row height) (mod col width)]))
              (live? [c] (= \# c))
              (live-neighbors [row col]
                (-
                 (count (for [r (range (dec row) (+ 2 row))
                              c (range (dec col) (+ 2 col))
                              :when (live? (cell r c))]
                          1))
                 (if (live? (cell row col)) 1 0)))]
        ;; Could memoize cell but these boards are small and we're only
        ;; calculating one generation.
        (map #(apply str %)
          (partition width
            (for [row (range height)
                  col (range width)
                  :let [is-live (live? (cell row col))
                        ln      (live-neighbors row col)]]
              (if is-live
                (cond (< ln 2) \space                       ; underpopulation
                      (or (= 2 ln) (= 3 ln)) \#             ; continue living
                      (> ln 3) \space)                      ; overpopulation
                (if (= ln 3)
                  \#                                        ; birth
                  \space)))))
        ))))

(defcheck solution-5c3d79a5
  (fn [I J [x & _ :as t]]
    (let [n (count x)
          m (count t)
          g #(get (get t %) %2)
          l (fn [a b]
              (if (= \# a)
                (if (< 1 b 4) \# \space)
                (if (= b 3) \# \space)))
          f (fn [i j] (count (filter #(= \# %) (map #(g (+ % i) (+ %2 j)) I J))))]
      (map #(apply str %)
        (partition n (for [i (range n) j (range m)]
                       (if (and (< 0 i (- n 1)) (< 0 j (- m 1)))
                         (l (g i j) (f i j))
                         \space)))))) [-1 0 1 -1 1 -1 0 1] [-1 -1 -1 0 0 1 1 1])

(defcheck solution-5c4c7720
  (fn [b]
    (let [r (count (first b))
          z (count b)]
      (map-indexed (fn [y s]
                     (apply str (map-indexed (fn [x v]
                                               (condp #(> %2 %1) (count (filter #(= \# %)
                                                                          (for [u (range (- y 1) (+ y 2))
                                                                                v (range (- x 1) (+ x 2))
                                                                                :when (and
                                                                                       (not= [x y] [u v])
                                                                                       (>= u 0) (< u z)
                                                                                       (>= v 0) (< v r))]
                                                                            (nth (nth b u) v))))
                                                 3 \space
                                                 2 \#
                                                 1 (if (= \# (get-in b [x y])) \# \space)
                                                 \space)) s)))
        b))))

(defcheck solution-5c5af62f
  (fn [rows]
    (letfn [
            (game-matrix [rows]
              (let [indices (for [x (range (count rows)) y (range (count (first rows)))] [x y])
                    cells   (flatten (for [r rows] (for [c r] (if (= c \#) :live :dead))))]
                (apply hash-map (interleave indices cells))))
            (cell-ranges [matrix]
              (let [ks (keys matrix)
                    n  (inc (apply max (distinct (map first ks))))
                    m  (inc (apply max (distinct (map second ks))))]
                [n m]))
            (to-strings [matrix]
              (let [[n m] (cell-ranges matrix)]
                (loop [ret [] i 0]
                  (if (< i n)
                    (let [a-row (apply str (map (fn [j]
                                                  (if (= :live (get matrix [i j])) \# \space)) (range m)))]
                      (recur (conj ret a-row) (inc i)))
                    ret))))
            (neighbours [cell matrix]
              (let [[i j] cell
                    [n m] (cell-ranges matrix)]
                (for [x (range (dec i) (+ i 2)) y (range (dec j) (+ j 2))
                      :when (and (not= [x y] [i j]) (>= x 0) (>= y 0) (< x n) (< y m))]
                  [x y])))
            (live-neighbours-count [cell matrix]
              (let [n      (neighbours cell matrix)
                    states (vals (select-keys matrix n))]
                (count (filter #(= % :live) states))))
            (in-next-gen [cell matrix]
              (let [ln (live-neighbours-count cell matrix)]
                (if (= (matrix cell) :live)
                  (cond
                    (< ln 2) :dead
                    (< ln 4) :live
                    :else :dead)
                  (if (= ln 3)
                    :live
                    :dead))))]

      (let [m          (game-matrix rows)
            ks         (keys m)
            m-next-gen (zipmap ks (map (fn [cell] (in-next-gen cell m)) ks))]
        (to-strings m-next-gen)))))

(defcheck solution-5c7630fb
  (fn [board]
    (let [h (count board)
          w (count (first board))
          m (fn [x y]                                       ; live cell?
              (and (<= 0 x) (< x w) (<= 0 y) (< y h)
                   (= \# (nth (nth board y) x))))
          n (fn [x y]                                       ; num of live neighbors
              (apply +
                (for [dx (range -1 2) dy (range -1 2)]
                  (if (and (zero? dx) (zero? dy)) 0
                                                  (if (m (+ x dx) (+ y dy)) 1 0)))))
          ]
      (map (partial apply str)
        (partition w
          (for [y (range h) x (range w)]
            (let [l (m x y) c (n x y)]
              (if (and l (< c 2)) \space
                                  (if (and l (< c 4)) \#
                                                      (if l \space
                                                            (if (= c 3) \# \space)))))))))))

(defcheck solution-5e6037bc
  (fn life [a]
    (let [m    (count a)
          n    (-> a first count)
          incx [-1 -1 -1 0 1 1 1 0]
          incy [-1 0 1 1 -1 0 1 -1]]
      (letfn [(is-alive [a x y]
                (and (>= x 0)
                     (>= y 0)
                     (< x (count a))
                     (< y (-> a first count))
                     (= \# (.charAt (a x) y))))
              (update [a x y]
                (let [current (.charAt (a x) y)
                      alive   (map #(if (is-alive a (+ x %1) (+ y %2)) 1 0) incx incy)
                      alive   (apply + alive)]
                  (if (= \# current)
                    (if (or (= 2 alive) (= 3 alive))
                      \#
                      " ")
                    (if (= 3 alive)
                      \#
                      " "))))]
        (for [x (range m)]
          (apply str (map #(update a x %) (range n))))))))

(defcheck solution-5e6f856c
  (fn [rows]
    (let [board            (vec (map #(vec (map {\  false \# true} %)) rows))
          get-cell         (fn [x y]
                             (if (and (contains? board y) (contains? (board y) x) ((board y) x)) 1 0))
          count-neighbours (fn [x y]
                             (+ (get-cell (dec x) (dec y)) (get-cell x (dec y)) (get-cell (inc x) (dec y))
                               (get-cell (dec x) y) (get-cell (inc x) y)
                               (get-cell (dec x) (inc y)) (get-cell x (inc y)) (get-cell (inc x) (inc y))))
          get-new          (fn [x y]
                             (let [alive           (= 1 (get-cell x y))
                                   live-neighbours (count-neighbours x y)]
                               (or (and alive (= live-neighbours 2))
                                   (= live-neighbours 3))))
          new-board        (for [y (range (count board))]
                             (for [x (range (count (board y)))]
                               (get-new x y)))]
      (map (fn [row] (apply str (map {false \  true \#} row))) new-board))))

(defcheck solution-60891842
  (fn game-of-life [board]
    (letfn [(cnt [[x y]] (count (for [dx [-1 0 1] dy [-1 0 1] :when (and (or (not= 0 dx)
                                                                             (not= 0 dy))
                                                                         (= (get-in board [(+ x dx) (+ y dy)]) \#))]
                                  1)))
            (update [new-board pos]
              (let [neibs (cnt pos)
                    live? (= \# (get-in board pos))]
                (assoc-in new-board pos (if (or (and live? (<= 2 neibs 3))
                                                (and (not live?) (= 3 neibs)))
                                          \#
                                          \space))))
            (n [] (count board))
            (m [] (count (first board)))]
      (->> (for [x (range 1 (dec (n))) y (range 1 (dec (m)))] [x y])
        (reduce update (vec (repeat (n) (vec (repeat (m) \space)))))
        (map #(apply str %))))))

(defcheck solution-60b82be8
  (fn game-oflife
    [grid]
    (let [old-grid         (map #(into [] %) grid)
          neighbours       (fn
                             [x y grid]
                             (let [xs (filter #(and (>= % 0) (< % (count grid))) (map #(+ x %) [-1 0 1]))
                                   ys (filter #(and (>= % 0) (< % (count (first grid)))) (map #(+ y %) [-1 0 1]))]
                               (filter #(not= % [x y])
                                 (for [x xs y ys]
                                   [x y]))
                               ))
          get-cell         (fn
                             [[x y] grid]
                             (nth (nth grid x) y))
          neighbours-count (fn
                             [x y grid]
                             (count (filter #(= % \#) (map #(get-cell % grid) (neighbours x y grid))))
                             )
          ]
      (map #(apply str %)
        (partition (count (first old-grid))
          (for [x (range (count old-grid))
                y (range (count (first old-grid)))]
            (let [cur    (get-cell [x y] old-grid)
                  status (neighbours-count x y old-grid)]
              (if (= cur \#)
                (if (< status 2)
                  \space
                  (if (< status 4)
                    \#
                    \space))
                (if (= status 3)
                  \#
                  \space)))
            ))))
    ))

(defcheck solution-6122cd77
  (fn gl [board]
    (let [neighbors (fn [x y c]
                      (apply + (for [dx [-1 0 1]
                                     dy [-1 0 1]]
                                 (if (and (zero? dx) (zero? dy)) 0
                                                                 (if (= \# (get-in board [(+ x dx)
                                                                                          (+ y dy)]))
                                                                   1 0)))))]
      (->> board
        (map-indexed
          (fn [x row]
            (->> row
              (map-indexed
                (fn [y c]
                  (case c
                    \# (if (< 1 (neighbors x y c) 4) \# \ )
                    \  (if (= 3 (neighbors x y c)) \# \ ))))
              (clojure.string/join ""))))))))

(defcheck solution-617a5e77
  (fn lifegame [in-arr]
    (let [get-at  (fn [arr x y] (nth (nth arr y) x))
          get-num (fn [arr x y]
                    (count
                      (filter #(= \# %)
                        (for [i (range (dec x) (inc (inc x))) j (range (dec y) (inc (inc y)))
                              :when (and (and (>= i 0) (>= j 0))
                                         (and (< i (count arr)) (< j (count arr)))
                                         (or (not (= x i)) (not (= y j))))]
                          (get-at arr i j)))))]
      (map #(apply str %) (partition (count (first in-arr)) (for [y (range (count in-arr))
                                                                  x (range (count (first in-arr)))]
                                                              (let [elem  (get-at in-arr x y)
                                                                    count (get-num in-arr x y)]
                                                                (if (= \# elem)
                                                                  (cond
                                                                    (< count 2) \space
                                                                    (> count 3) \space
                                                                    :else \#)
                                                                  (if (= count 3) \# \space)))))))))

(defcheck solution-619b96a0
  (fn conway [board]
    (let [curr-gen          (reduce (fn [coords [row cells]]
                                      (reduce (fn [coords' [col cell]]
                                                (if (= cell \#)
                                                  (conj coords' [row col])
                                                  coords'))
                                        coords
                                        (map vector (iterate inc 0) cells)))
                              #{}
                              (map vector (iterate inc 0) board))
          coords->neighbors (frequencies
                              (mapcat (fn [[row col]]
                                        (for [row' (map #(+ row %) (range -1 2))
                                              col' (map #(+ col %) (range -1 2))
                                              :when (and (not= [row col] [row' col']))]
                                          [row' col']))
                                curr-gen))
          neighbors->coords (group-by second coords->neighbors)
          three             (set (map first (get neighbors->coords 3)))
          two               (set (filter curr-gen (map first (get neighbors->coords 2))))
          next-gen          (clojure.set/union three two)]
      (map (fn [r]
             (apply str (map (fn [c]
                               (if (next-gen [r c]) \# \space))
                          (range (count (nth board r))))))
        (range (count board))))))

(defcheck solution-61b62c83
  (fn next-board [board]
    (let
     [lookup           (fn [x y]
                         (if (or (= x -1) (= y -1))
                           nil
                           (->> (drop y board) first (drop x) first)))

      alive            (fn ([x y] (= \# (lookup x y)))
                         ([cell] (= \# cell)))

      num-alive-around (fn [x y]
                         (apply +
                           (for [a (range (- x 1) (+ x 2))
                                 b (range (- y 1) (+ y 2))
                                 :when (not= [a b] [x y])]
                             (if (alive a b) 1 0))))

      next-cell        (fn [x y cell]
                         (let [num-alive (num-alive-around x y)]
                           (if (alive cell)
                             (if (#{2 3} num-alive) \# \space)
                             (if (= 3 num-alive) \# \space))))]

      (map-indexed
        (fn [y row] (clojure.string/join (map-indexed #(next-cell %1 y %2) row)))
        board))))

(defcheck solution-626a818a
  (fn gol [b]
    (let [live?          (fn [p] (= \# (get-in b p)))
          neighbor-count (fn [[r c]] (count (filter live? (for [i (range -1 2) j (range -1 2) :when (not= i j 0)] [(+ r i) (+ c j)]))))]
      (map (partial apply str)
        (map #(map str %)
          (for [r (range (count b))]
            (for [c (range (count (b r)))]
              (condp = (neighbor-count [r c])
                2 (get-in b [r c])
                3 \#
                \space))))))))

(defcheck solution-62ed2fc1
  (fn [board]
    (letfn [(conj-in
              [coll [k & ks] v]
              (if ks
                (assoc coll k (conj-in (get coll k []) ks v))
                (assoc coll k v)))
            (get-cell
              ([board x y]
               (get-cell board x y \space))
              ([board x y df]
               (let [c (count board)]
                 (if (or (< x 0)
                         (>= x c)
                         (< y 0)
                         (>= y c))
                   df
                   ((board x) y)))))
            (neirs
              [board x y]
              (for [dx '(-1 0 1)
                    dy '(-1 0 1)
                    :when (not (= 0 dx dy))]
                (get-cell board (+ x dx) (+ y dy))))
            (count-neirs [board x y]
              (count (filter #(= % \#) (neirs board x y))))
            (result
              [board x y]
              (if (or (= (count-neirs board x y) 3)
                      (and (= (count-neirs board x y) 2)
                           (= (get-cell board x y) \#)))
                \#
                \space))
            (get-all-cords [size]
              (for [x (range 0 size)
                    y (range 0 size)]
                (vector x y)))
            (step [board]
              (loop [coord (get-all-cords (count board))
                     res   board]
                (if (nil? coord)
                  res
                  (let [crd (first coord)
                        nw  (result board (first crd) (second crd))]
                    (recur (next coord)
                      (conj-in res crd nw)
                      )))))
            (parse [col]
              (into [] (map (comp (partial into []) seq) col)))
            (com [board]
              (into [] (map #(reduce str %) board)))
            (pre-final [board]
              (com (step (parse board))))]
      (pre-final board))))

(defcheck solution-62f25fdc
  (fn [b]
    (let [w (count (b 0))
          h (count b)
          l (fn [r c]
              (if (and (< -1 r h) (< -1 c h))
                (if (= \# (.charAt (b r) c)) 1 0)
                0))
          n (fn [r c]
              (apply + (map #(l (+ r %) (+ c %2))
                         [-1 -1 -1 0 0 1 1 1]
                         [-1 0 1 -1 1 -1 0 1])))]
      (for [r (range 0 h)]
        (apply str (for [c (range 0 w)]
                     (let [n (n r c)]
                       (if (or (= 3 n)
                               (and (= 1 (l r c)) (= 2 n)))
                         \# " "))))))))

(defcheck solution-63b7a1af
  (fn [b]
    (let [ib (->> b
               (map #(map vector (range) %))
               (map vector (range)))
          mb (reduce (fn [map [i r]]
                       (reduce (fn [map [j c]]
                                 (assoc map [i j] c)) map r)) {} ib)
          l  \#
          d  \space]
      (reduce (fn [b [i r]]
                (->> r
                  (reduce (fn [r [j c]]
                            (let [ir (range (dec i) (+ i 2))
                                  jr (range (dec j) (+ j 2))
                                  a  (->> ir
                                       (map (fn [i']
                                              (map (fn [j']
                                                     (if (and (= i' i) (= j' j))
                                                       nil
                                                       (get mb [i' j']))) jr)))
                                       flatten
                                       (filter #(= \# %))
                                       count)
                                  s  (get mb [i j])
                                  s' (condp = s
                                       \# (cond
                                            (< a 2) d
                                            (or (= a 2) (= a 3)) l
                                            (> a 3) d)
                                       \space (if (= a 3)
                                                l
                                                d))]
                              (str r s'))) "")
                  (conj b))) [] ib))))

(defcheck solution-643da109
  (letfn [
          ; Cells
          (live-cell [] \#)
          (dead-cell [] \space)
          (live? [cell] (= cell (live-cell)))
          (dead? [cell] (= cell (dead-cell)))
          (successor [cell neighbors]
            (let [ln (count (filter live? neighbors))]
              (if (live? cell)
                (cond (< ln 2) (dead-cell)
                      (<= ln 3) (live-cell)
                      (> ln 3) (dead-cell))
                (cond (= ln 3) (live-cell)
                      true (dead-cell)))))
          ; Boards
          (board-from-strings [strs] strs)
          (strings-from-board [board] board)
          ; Locations
          (get-cell [board location]
            (get-in board location (dead-cell)))
          (get-neighbor-cells [board location]
            (map #(get-cell board %)
              (for [i [-1 0 1]
                    j [-1 0 1]
                    :when (not= [i j] [0 0])]
                (map + location [i j]))))
          (get-successor [board location]
            (successor (get-cell board location)
              (get-neighbor-cells board location)))
          (map-board [f board]
            (vec (map
                   (fn [row] (apply str (map
                                          (fn [col]
                                            (f [row col]))
                                          (range (count (board row))))))
                   (range (count board)))))]
    (fn [x]
      (let [b (board-from-strings x)]
        (strings-from-board
          (map-board #(get-successor b %) b))))))

(defcheck solution-643dc272
  (fn [b]
    (map-indexed
      (fn [y r]
        (apply str
          (map-indexed
            (fn [x v]
              (let [n (count
                        (for [dx [-1 0 1]
                              dy [-1 0 1]
                              :when (and
                                     (not (= dx dy 0))
                                     (= (get-in b [(+ y dy)
                                                   (+ x dx)])
                                       \#))]
                          1))]
                (if (or (= n 3)
                        (and (= n 2)
                             (= (get-in b [y x]) \#)))
                  \#
                  " ")))
            r)))
      b)))

(defcheck solution-64b17a33
  (fn game-life [Board]
    (letfn [(neib-cells [l c n]
              (let [nei [[(dec l) (dec c)] [(dec l) c] [(dec l) (inc c)]
                         [l (dec c)] [l (inc c)]
                         [(inc l) (dec c)] [(inc l) c] [(inc l) (inc c)]]]
                (reduce #(if (or (< (%2 0) 0) (< (%2 1) 0)
                                 (> (%2 0) (dec n)) (> (%2 1) (dec n)))
                           %1
                           (conj %1 %2)) [] nei)))

            (alive? [l c Board]
              (= ((Board l) c) \#))

            (next-val [l c Board]
              (let [neib   (neib-cells l c (count Board))
                    nalive (reduce #(if (alive? (%2 0) (%2 1) Board)
                                      (inc %1)
                                      %1) 0 neib)]
                (if (alive? l c Board)
                  (if (< nalive 2)
                    \space
                    (if (and (>= nalive 2) (<= nalive 3))
                      \#
                      \space))
                  (if (= nalive 3)
                    \#
                    \space))))
            ]
      (let [Board (apply vector (map #(apply vector %) Board))
            n     (count Board)
            pos   (for [x (range n)
                        y (range n)] [x y])
            B2    (apply vector (for [x (range n)] (apply vector (take n (repeat \space)))))]
        (mapv #(apply str %)
          (reduce (fn [B p]
                    (let [l (p 0)
                          c (p 1)]
                      (assoc B l (assoc (B l) c (next-val l c Board))))) B2 pos))))))

(defcheck solution-64bbef6f
  (fn [board]
    (letfn
     [(alive? [cell] (= \# (get-in board cell)))
      (neighbors [[row col]] (count (filter alive? [[(dec row) (dec col)]
                                                    [(dec row) col]
                                                    [(dec row) (inc col)]
                                                    [row (dec col)]
                                                    [row (inc col)]
                                                    [(inc row) (dec col)]
                                                    [(inc row) col]
                                                    [(inc row) (inc col)]])))
      (lives? [cell] (if (alive? cell)
                       (contains? #{2 3} (neighbors cell))
                       (= 3 (neighbors cell))))]
      (for [row (range (count board))]
        (apply str
          (for [col (range (count (nth board row)))]
            (if (lives? [row col]) \# \space)))))))

(defcheck solution-64eda91c
  (fn next [board]
    (let [normalize      (fn [board] (mapv (fn [row] (mapv (fn [e] (if (= e \#) 1 0)) row)) board))
          shift          (fn [board x y]
                           (mapv (fn [row]
                                   (mapv (fn [col]
                                           (get-in board [(+ row y) (+ col x)] 0))
                                     (range (count (first board)))))
                             (range (count board))))
          neighbours     (fn [board]
                           (map (fn [[i j]] (shift board i j))
                             (for [x [-1 0 1] y [-1 0 1] :when (not (and (= x 0) (= y 0)))] [x y])))
          sum-neigh      (fn [boards]
                           (apply mapv (fn [& rows] (apply mapv (comp (partial reduce +) vector) rows)) boards))
          pair-mat       (fn [a b] (mapv (fn [arow brow] (mapv vector arow brow)) a b))
          ;; ---
          nboard         (normalize board)
          nbours         (neighbours nboard)
          decision-board (pair-mat nboard (sum-neigh nbours))]
      (mapv
        (fn [row]
          (apply str
            (mapv
              (fn [[l n]]
                (if (= l 1)
                  ;;currently alive
                  (cond (< n 2) \space
                        (= n 2) \#
                        (= n 3) \#
                        :else \space)
                  ;;currently dead
                  (cond (= n 3) \#
                        :else \space)))
              row)))
        decision-board))))

(defcheck solution-65019e1
  (fn game-of-life [board]
    (->> [(+ dx x)
          (+ dy y)]
      (get-in board)
      (for [x (range (count board))
            y (range (count (first board)))
            [dx dy] [[0 0] [1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1]]])
      (partition 9)
      (map (fn [xs]
             (if (= \space (first xs))                      ;if original element is whitespace
               (if (= 3 (count (filter #(= % \#) (rest xs))))
                 \# \space)
               (let [neighbours (count (filter #(= % \#) (rest xs)))]
                 (cond (> 2 neighbours) \space
                       (or (= 2 neighbours) (= 3 neighbours)) \#
                       (< 3 neighbours) \space)))))
      (partition (count (first board)))
      (map #(apply str %)))))

(defcheck solution-65927c4d
  (fn [world]
    (vec (map-indexed (fn [row-index row]
                        (apply str (map-indexed (fn [ele-index ele]
                                                  (let [row-vec (vec world)
                                                        above   (get (get row-vec
                                                                       (dec row-index)) ele-index)
                                                        d-t-l   (get (get row-vec
                                                                       (dec row-index)) (dec ele-index))
                                                        d-t-r   (get (get row-vec
                                                                       (dec row-index)) (inc ele-index))
                                                        below   (get (get row-vec
                                                                       (inc row-index)) ele-index)
                                                        d-b-l   (get (get row-vec
                                                                       (inc row-index)) (dec ele-index))
                                                        d-b-r   (get (get row-vec
                                                                       (inc row-index)) (inc ele-index))
                                                        left    (get (get row-vec row-index)
                                                                  (dec ele-index))
                                                        right   (get (get row-vec row-index)
                                                                  (inc ele-index))]
                                                    (let [live-neighbors (get
                                                                           (frequencies
                                                                             [below above
                                                                              left right
                                                                              d-t-l d-t-r
                                                                              d-b-l d-b-r]) \#)]
                                                      (if (= ele \space)
                                                        (if (= live-neighbors 3) "#" " ")
                                                        (if (or (= live-neighbors 3)
                                                                (= live-neighbors 2)) "#" " "))))
                                                  ) row))) world))))

(defcheck solution-65a5351b
  (fn [d]
    (let [bound_x    (count (nth d 0)) bound_y (count d) _get (fn [x y]
                                                                (if (or (< x 0) (>= x bound_x) (< y 0) (>= y bound_y)) nil
                                                                                                                       (nth (nth d y) x)))
          neighbours (fn [x y] (let
                                [d1 (_get (dec x) (dec y))
                                 d2 (_get x (dec y))
                                 d3 (_get (inc x) (dec y))
                                 d4 (_get (dec x) y)
                                 d5 (_get (inc x) y)
                                 d6 (_get (dec x) (inc y))
                                 d7 (_get x (inc y))
                                 d8 (_get (inc x) (inc y))] [d1 d2 d3 d4 d5 d6 d7 d8]))]
      (map #(apply str %) (partition bound_x (for [y (range bound_y) x (range bound_x)]
                                               (let [nbs (neighbours x y)
                                                     dpc (count (filter #(= \space %) nbs))
                                                     lpc (count (filter #(= \# %) nbs))
                                                     p   (_get x y)]
                                                 (if (= p \#)
                                                   (if (< lpc 2) \space
                                                                 (if (> lpc 3) \space
                                                                               \#
                                                                               )
                                                                 )
                                                   (if (= 3 lpc) \# \space)
                                                   )
                                                 )
                                               )))
      )
    ))

(defcheck solution-65ae27cc
  (fn [ss]
    (let
     [one       (reduce into [] (map #(map (fn [ch] (if (= ch \#) 1 0)) %) ss))
      size      (count (first ss))
      delta     [(- (- size) 1) (- size) (+ (- size) 1) -1 1 (- size 1) size (+ size 1)]
      neighbors (map (fn [idx] (apply + (map #(nth one (+ idx %) 0) delta))) (range (* size size)))
      flat      (map #(if (or (and (= %1 1) (or (= %2 2) (= %2 3)))
                              (and (= %1 0) (= %2 3))
                              ) \# \ ) one neighbors)
      ]
      (mapv #(apply str %) (partition size flat))
      )
    ))

(defcheck solution-66f42aa7
  (fn gol-step [b]
    (let [w         (count (first b))
          h         (count b)
          dirs      [[1 1] [1 0] [1 -1] [0 1] [0 -1] [-1 1] [-1 0] [-1 -1]]
          get-coord (fn [x y]
                      (if (and (>= x 0) (>= y 0) (< x w) (< y h))
                        (.charAt (b y) x)
                        \space))]
      (map (partial clojure.string/join "")
        (partition w
          (for [x (range w)
                y (range h)
                :let [c (get-coord y x)]]
            (let [live-neighbors
                  (count (filter (partial = \#)
                           (map (partial apply get-coord)
                             (map (fn [d] (map + [y x] d))
                               dirs))))]
              (if (= c \#)
                (if (or (< live-neighbors 2)
                        (> live-neighbors 3))
                  \space
                  \#)
                (if (= live-neighbors 3) \# \space))
              )))))))

(defcheck solution-671c1099
  (fn [m]
    (let [I (count m)
          J (count (m 0))]
      (->> (for [i (range (count m))
                 j (range (count (m 0)))]
             (let [cell
                   (get-in m [i j])
                   neighbour
                   (for [x (concat [i] (if (> i 0) [(dec i)]) (if (< i (dec I)) [(inc i)]))
                         y (concat [j] (if (> j 0) [(dec j)]) (if (< j (dec J)) [(inc j)]))]
                     [x y])
                   {live \# dead \space :or {live 0 dead 0}}
                   (->> neighbour (map #(get-in m %)) frequencies)]
               (if (= \  cell)
                 (if (= 3 live) \# \ )
                 (if (<= 3 live 4) \# \ ))))
        (partition J)
        (mapv #(apply str %))))))

(defcheck solution-68e0017
  (fn game-of-life [m]
    (let [pos   (fn [x y] (get (apply vector (get m y "")) x \space))
          dx    '(0 1 0 -1 1 1 -1 -1)
          dy    '(1 0 -1 0 1 -1 1 -1)
          nexts (fn [x y] (map #(pos (+ x %1) (+ y %2)) dx dy))
          nc    (fn [x y] (count (filter #(= \# %) (nexts x y))))
          next  (fn [x y]
                  (cond
                    (= (pos x y) \space) (if (= 3 (nc x y)) \# \space)
                    :else (cond
                            (< (nc x y) 2) \space
                            (> (nc x y) 3) \space
                            :else \#)))]
      (map (fn [y] (apply str (map #(next % y) (range (count (first m)))))) (range (count m))))))

(defcheck solution-68e1d889
  (letfn [
          (neighbors8
            ([height width yx] (neighbors8 [[0 1] [0 -1] [1 0] [-1 0] [-1 -1] [1 -1] [-1 1] [1 1]] height width yx))
            ([deltas height width yx] (filter (fn [[new-y new-x]] (and (< -1 new-y height) (< -1 new-x width))) (map #(vec (map + yx %)) deltas))))
          (board-width [board]
            (count (first board)))
          (board-height [board]
            (count board))
          (index-board [board yx]
            (get-in board yx))
          (count-live-neighbors [board yx]
            (count (filter #(= \# %) (map (partial index-board board) (neighbors8 (board-height board) (board-width board) yx)))))]

    (fn game-of-life [board]
      (let [new-board-flat (for [
                                 y (range (board-height board))
                                 x (range (board-width board))
                                 :let [cell (index-board board [y x]), num-live-neighbors (count-live-neighbors board [y x])]]
                             (if (= cell \#)
                               ; a live cell
                               (cond
                                 (< num-live-neighbors 2) \space ; the cell dies
                                 (<= num-live-neighbors 3) \# ; the cell lives on
                                 :else \space)              ; the cell dies
                               ; a dead cell
                               (if (= num-live-neighbors 3)
                                 \#                         ; the cell comes back to life
                                 \space)))]                 ; the cell remains dead
        (map (partial apply str) (partition (board-width board) new-board-flat))))))

(defcheck solution-691a5fab
  (fn next-state [board]
    (letfn [(count-neighbors-alive [x y]
              (reduce + (map-indexed (fn count-living-on-row [i row]
                                       (reduce + (map-indexed (fn [j p]
                                                                (if (and (<= i (inc x)) (>= i (dec x)) (<= j (inc y)) (>= j (dec y)) (or (not (= i x)) (not (= j y))) (= p \#)) 1 0)) row))) board)))]
      (map-indexed (fn [i row]
                     (apply str (map-indexed (fn [j p]
                                               (let [cnt (count-neighbors-alive i j)]
                                                 (if (= p \#)
                                                   (if (< cnt 2)
                                                     \space
                                                     (if (< cnt 4)
                                                       \#
                                                       (if (> cnt 3)
                                                         \space)))
                                                   (if (= cnt 3)
                                                     \#
                                                     \space)))) row))) board))))

(defcheck solution-6a32bceb
  (fn [rows]
    (let [neighbor-offsets (disj (set (for [y (range -1 2)
                                            x (range -1 2)]
                                        [y x]))
                             [0 0])]
      (letfn [(cells [b]
                (for [y (range (:h b))
                      x (range (:w b))]
                  [y x]))
              (find-life [b rows]
                (let [rows  (vec (map vec rows))
                      alive (fn [[y x]] (= \# ((rows y) x)))]
                  (set (filter alive (cells b)))))
              (parse [rows]
                (let [h (count rows)
                      w (count (first rows))
                      b {:h h, :w w}]
                  (assoc b :alive (find-life b rows))))
              (render [b]
                (map (partial apply str)
                  (partition (:w b)
                    (map #(if ((b :alive) %) \# \space) (cells b)))))
              (on-board? [b [y x]]
                (and (>= y 0) (>= x 0) (< y (:h b)) (< x (:w b))))
              (add [[y x] [dy dx]]
                [(+ y dy) (+ x dx)])
              (neighbors [board cell]
                (set (filter (partial on-board? board)
                       (map (partial add cell) neighbor-offsets))))
              (count-live-neighbors [board cell]
                (count (filter (:alive board) (neighbors board cell))))
              (cell-status [b cell]
                [((:alive b) cell)
                 (count-live-neighbors b cell)])
              (should-live [alive pals]
                (if alive
                  (and (> pals 1)
                       (< pals 4))
                  (= pals 3)))
              (cell-should-live [b cell]
                (apply should-live (cell-status b cell)))
              (round [b]
                (assoc b :alive (set (filter (partial cell-should-live b) (cells b)))))]
        (-> rows parse round render)))))

(defcheck solution-6ac9a3cc
  (fn [board]
    (let [rows (count board), cols (count (seq (first board)))]
      (map
        (fn [[r row]]
          (clojure.string/join
            (map
              (fn [[c cell]]
                (let [ns  (for [rr [-1 0 1] cc [-1 0 1]
                                :let [nr (+ r rr) nc (+ c cc)]
                                :when (and (>= nr 0) (>= nc 0) (< nr rows) (< nc cols) (not (= [r c] [nr nc])))]
                            [nr nc])
                      cnt (count (filter (fn [coord] (= \# (get-in board coord))) ns))]
                  (if (= \space cell)
                    (if (== cnt 3)
                      \#
                      \space)
                    (if (or (< cnt 2) (> cnt 3))
                      \space
                      \#))))
              (keep-indexed vector (seq row)))))
        (keep-indexed vector board)
        ))))

(defcheck solution-6b364d25
  (fn [coll]
    (let [width  (count (first coll))
          border [(apply str (repeat (+ 2 width) \-))]
          neighbours
                 (mapcat
                   (fn [rows]
                     (map (fn [area] (count (filter #(= \# %) (flatten area))))
                       (partition 3 (apply interleave (map #(partition 3 1 %) rows)))))
                   (partition 3 1
                     (concat border (map #(str " " % " ") coll) border)))]
      (map #(apply str %)
        (partition width
          (map (fn [[c n]]
                 (if (= \# c)
                   (if (or (= 3 n) (= 4 n)) \# \space)
                   (if (= 3 n) \# \space)))
            (partition 2 (interleave (apply str coll) neighbours))))))))

(defcheck solution-6b74a717
  (fn [board]
    (let [height               (count board)
          width                (count (first board))
          live?                (fn [[x y]] (= \# ((vec (board x)) y)))
          neighbors            (fn [[x y]] (for [[i j] [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]
                                                 :let [x' (+ x i) y' (+ y j)]
                                                 :when (and (>= x' 0) (< x' height) (>= y' 0) (< y' width))] [x' y']))
          live-neighbors-count (fn [coord] (count (filter live? (neighbors coord))))
          evolve-cell          (fn [coord] (let [live-count (live-neighbors-count coord)]
                                             (cond
                                               (< live-count 2) \space
                                               (= live-count 2) (if (live? coord) \# \space)
                                               (= live-count 3) \#
                                               :else \space)))]
      (for [i (range height)] (apply str (for [j (range width)] (evolve-cell [i j])))))))

(defcheck solution-6bec614c
  (fn game-of-life [board]
    (letfn [

            (get-pos [b x y]                                ;All good
              (cond
                (< x 0) \space
                (< y 0) \space
                (>= y (count b)) \space
                (>= x (count (nth b y))) \space
                :else (nth (nth b y) x)))

            (get-adjacent [b x y]
              (let [inc-lst [[1 1] [1 0] [0 1] [-1 -1]
                             [1 -1] [-1 1] [0 -1] [-1 0]]]
                (map #(get-pos b (first %) (second %))
                  (map #(identity [(+ (first %) x) (+ (second %) y)]) inc-lst))))

            (next-cell-state [b x y]
              (let [cell      (get-pos b x y)
                    adj-lst   (get-adjacent b x y)
                    num-alive (count (filter #(= \# %) adj-lst))]
                (case cell
                  \space (if (= num-alive 3) \# \space)
                  \# (cond
                       (< num-alive 2) \space
                       (> num-alive 3) \space
                       :else \#))))
            ]
      (loop [res [] y 0]
        (if (>= y (count board))
          res
          (recur (conj res (apply str (map #(next-cell-state board % y) (range 0 (count (nth board y)))))) (inc y)))))))

(defcheck solution-6cb47f80
  (fn [b]
    (let [rows (count b)
          cols (count (first b))
          bp   (apply str (concat [(apply str (repeat (+ 2 cols) " "))]
                            (map #(str " " % " ") b)
                            [(apply str (repeat (+ 2 cols) " "))]))
          f    (fn [i e]
                 (let [row  (quot i cols)
                       col  (mod i cols)
                       cols (+ 2 cols)
                       adj  [(+ (* row cols) col) (+ (* row cols) col 1) (+ (* row cols) col 2)
                             (+ (* (inc row) cols) col) (+ (* (inc row) cols) col 2)
                             (+ (* (+ 2 row) cols) col) (+ (* (+ 2 row) cols) col 1) (+ (* (+ 2 row) cols) col 2)
                             ]
                       adj  (count (filter (partial = \#) (map #(nth bp %) adj)))
                       ]
                   (if (= \# e)
                     (if (or (< adj 2) (> adj 3))
                       \space
                       \#)
                     (if (= adj 3)
                       \#
                       \space))))
          res  (map-indexed f (apply str b))]
      (map (partial apply str) (partition cols res))
      )))

(defcheck solution-6cdc8349
  (fn [x]
    (let [f  #(zipmap (range (count %)) %)
          m  (into {} (map #(f %) (list (map #(f %) x))))
          n  (fn [i j] (for [a [-1 0 1] b [-1 0 1] :when (not (= a b 0))] (get-in m [(+ i a) (+ j b)])))
          nc (for [i (range (count x))]
               (for [j (range (count (first x)))]
                 (count (filter #(= \# %) (n i j)))))]
      (map (fn [cell neighbors]
             (apply str (map #(if (= % \#)
                                (cond
                                  (< %2 2) \space
                                  (some #{2 3} [%2]) \#
                                  (> %2 3) \space)
                                (if (= %2 3) \# \space))
                          cell neighbors)))
        x nc))))

(defcheck solution-6da2026
  (fn [board]
    (letfn [(valid-idx? [idx x-len y-len]
              (let [row (first idx) col (last idx)]
                (and
                 (>= row 0) (< row y-len)
                 (>= col 0) (< col x-len))))
            (idx-value [board idx]
              (let [row (first idx) col (last idx)]
                (nth (nth board row) col)))
            (neighbour-idx [idx x-len y-len]
              (let [row (quot idx x-len)
                    col (rem idx x-len)
                    n1  [(- row 1) (- col 1)]
                    n2  [(- row 1) col]
                    n3  [(- row 1) (+ col 1)]
                    n4  [row (- col 1)]
                    n5  [row (+ col 1)]
                    n6  [(+ row 1) (- col 1)]
                    n7  [(+ row 1) col]
                    n8  [(+ row 1) (+ col 1)]]
                (filter #(valid-idx? % x-len y-len) [n1 n2 n3 n4 n5 n6 n7 n8])))
            (neighbours [board idx]
              (let [x-len (count (first board)) y-len (count board)]
                (flatten
                  (for [n-index (neighbour-idx idx x-len y-len)]
                    (idx-value board n-index)))))
            (target-value [board idx]
              (let [x-len   (count (first board))
                    col     (rem idx x-len)
                    row     (quot idx x-len)
                    value   (idx-value board [row col])
                    nvalues (neighbours board idx)
                    l-value (count (filter #(= \# %) nvalues))]
                (if (= value \#)
                  (if (or (= l-value 2) (= l-value 3)) \# \space)
                  (if (= l-value 3) \# \space))))]
      (let [b     (for [s board] (seq s))
            x-len (count (first b))
            y-len (count b)]
        (for [y (range y-len)]
          (apply str
            (for [x (range x-len)]
              (target-value b (+ (* y x-len) x)))))))))

(defcheck solution-6dd2727
  (fn gol [w]
    (let [x              (count (first w))
          y              (count w)
          living-cells   (set (filter (complement nil?) (mapcat identity (map-indexed #(map-indexed (fn [a b] (if (= b \#) [%1 a])) %2) w))))
          get-neighbours (fn [[a b]] (clojure.set/intersection living-cells
                                       (set [[(inc a) b] [(dec a) b] [a (inc b)] [a (dec b)] [(dec a) (dec b)]
                                             [(inc a) (dec b)] [(inc a) (inc b)] [(dec a) (inc b)]])))
          alive?         (fn [[a b]] (let [n (count (get-neighbours [a b]))]
                                       (case n
                                         3 true
                                         2 (contains? living-cells [a b])
                                         false)))
          print-gol      (fn [cells x y]
                           (let [board (repeat y (apply str (repeat x " ")))]
                             (map-indexed #(apply str (map-indexed (fn [k l] (if (contains? cells [%1 k]) "#" " ")) %2)) board)))
          coordinats     (mapcat identity (mapv #(map vec (partition 2 (interleave (repeat %) (range x)))) (range y)))
          next-step      (set (filter alive? coordinats))]
      (print-gol next-step x y))))

(defcheck solution-6e21c437
  (fn [board]
    (letfn [(live-neighbours [r c]
              (apply +
                (for [dr [-1 0 1] dc [-1 0 1] :when (not= [dr dc] [0 0])]
                  (if (= \# (get-in board [(+ r dr) (+ c dc)] \space)) 1 0))))]
      (for [r (range (count board))]
        (apply str
          (for [c (range (count (board r)))]
            (if (= \# (get-in board [r c]))
              (if (#{2 3} (live-neighbours r c)) \# \space)
              (if (= 3 (live-neighbours r c)) \# \space))))))))

(defcheck solution-6e60cacd
  (fn [x]
    (let [f2 (fn [j i c] [[j i] [c 0]])
          f1 (fn [j r] (into {} (map f2 (repeat j) (range) r)))
          m  (apply merge (map f1 (range) x))
          f4 (fn [j i]
               (let [s (first (m [j i]))
                     n (count (filter (partial not= \space)
                                (filter char?
                                  (map first
                                    [(m [j (dec i)])
                                     (m [j (inc i)])
                                     (m [(dec j) i])
                                     (m [(inc j) i])
                                     (m [(inc j) (inc i)])
                                     (m [(inc j) (dec i)])
                                     (m [(dec j) (inc i)])
                                     (m [(dec j) (dec i)])]))))]
                 (cond
                   (and (= s \space) (= n 3)) \#
                   (and (not= s \space) (< n 2)) \space
                   (and (not= s \space) (> n 3)) \space
                   :else s)))
          f3 (fn [j w] (apply str (map f4 (repeat j) (range w))))]

      (map f3 (range (count x)) (repeat (count (first x)))))))

(defcheck solution-6ec9c09f
  (fn [field]
    (let [xl (count field)
          yl (count (first field))
          at (fn [mx xl yl x y]
               (count
                 (filter #(not= \space %)
                   (for [i [(dec x) x (inc x)]
                         j [(dec y) y (inc y)] :when (or (not= i x) (not= j y))]
                     (cond
                       (or (< i 0) (>= i xl) (< j 0) (>= j yl)) \space
                       :else (nth (nth mx i) j))))))]
      (for [i (range xl)]
        (apply str
          (for [j (range yl) :let [p (nth (nth field i) j)
                                   n (at field xl yl i j)]]
            (if (= p \space)
              (if (= n 3) \# p)
              (if
               (or (< n 2) (> n 3)) \space
                                    p))))))))

(defcheck solution-6f2bfc37
  (fn [b]
    (let [g              (fn [b] (zipmap (range (count b)) b))
          h              (fn [m] (map #(get m %) (range (count m))))
          print-board    (fn [bm] (map (partial apply str) (map h (h bm))))
          bm             (g (map g b))
          live?          (fn [m x y] (= (get-in m [x y]) \#))
          live-neighbors (fn [m x y]
                           (count
                             (filter
                               #(apply live? m %)
                               [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
                                [x (dec y)] [x (inc y)]
                                [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]])))
          next-state     (fn [m x y]
                           (let [ln (live-neighbors m x y)]
                             (cond
                               (and (= 2 ln) (live? m x y)) \#
                               (= 3 ln) \#
                               :else \space)))]


      (loop [new-bm {} x 0 y 0]
        (cond
          (= x (count bm)) (recur new-bm 0 (inc y))
          (= y (count bm)) (print-board new-bm)
          :else (recur (assoc-in new-bm [x y] (next-state bm x y)) (inc x) y))))))

(defcheck solution-6fc3f5ff
  (fn [board]
    (letfn [(transform-point [point invalid? transform]
              (if (or (nil? point) (invalid? point))
                nil
                (transform point)))
            (up [point]
              (transform-point point
                (fn [[r c]] (<= r 0))
                (fn [[r c]] [(dec r) c])))
            (down [point size]
              (transform-point point
                (fn [[r c]] (>= r (dec size)))
                (fn [[r c]] [(inc r) c])))
            (left [point]
              (transform-point point
                (fn [[r c]] (<= c 0))
                (fn [[r c]] [r (dec c)])))
            (right [point size]
              (transform-point point
                (fn [[r c]] (>= c (dec size)))
                (fn [[r c]] [r (inc c)])))
            (neighbors [point size]
              (filter (complement nil?) [(up (left point)) (up point) (up (right point size))
                                         (left point) (right point size)
                                         (down (left point) size) (down point size) (down (right point size) size)]))
            (alive? [board [r c]]
              (= \# (nth (nth board r) c)))
            (neighbor-count [board point]
              (count (filter #(alive? board %)
                       (neighbors point (count board)))))
            (next-generation [board size]
              (for [r (range size)]
                (apply str (into []
                             (for [c (range size)]
                               (let [neighbor-count (neighbor-count board [r c])
                                     self-alive     (alive? board [r c])]
                                 (if self-alive
                                   (case neighbor-count
                                     (2 3) \#
                                     \space)
                                   (case neighbor-count
                                     3 \#
                                     \space))))))))]
      (next-generation board (count board)))))

(defcheck solution-700660ce
  (fn life [board]
    (let [w (count (board 0))
          h (count board)
          score
            (fn [[a b]]
              (let [c ((vec (board b)) a)]
                (if (= \# c) 1 0)))
          neighbors
            (fn [i j]
              (for [x [-1 0 1] y [-1 0 1] :when (not (and (zero? x) (zero? y)))]
                [(mod (+ i x) w) (mod (+ j y) h)]))
          n_count
            (fn [i j]
              (reduce + (map score (neighbors i j))))
          ]

      (for [y (range h)]
        (apply str
          (for [x (range w)]
            (let [s (n_count x y)
                  c ((vec (board y)) x)]
              (cond
                (< s 2) \space
                (> s 3) \space
                (= s 3) \#
                (= s 2) c)
              ))))
      )))

(defcheck solution-710d2536
  (fn [b]
    (let [m        (into {}
                     (for [[i y] (map list (range) b)
                           [j x] (map list (range) y)]
                       [[i j] x]))
          neigh    (fn [[i j]] (for [di [-1 0 1] dj [-1 0 1]
                                     :when (not (= [di dj] [0 0]))]
                                 [(+ i di) (+ j dj)]))
          counts   (into {}
                     (map #(vector
                             %
                             (count (filter (fn [x] (= \# x)) (map m (neigh %)))))
                       (keys m)))
          update   (fn [[k x]]
                     (let [c        (counts k)
                           newstate (if (= x \#)
                                      (cond
                                        (< c 2) \space
                                        (< c 4) \#
                                        :else \space)
                                      (cond
                                        (= c 3) \#
                                        :else \space))]
                       [k newstate]))
          newb     (map update m)
          vecboard (vec (map vec b))
          res      (reduce (fn [acc [k x]] (assoc-in acc k x)) vecboard newb)]
      (map #(apply str %) res))))

(defcheck solution-711b12e1
  (fn game-of-life [board]
    (let [height          (count board)
          width           (count (first board))
          all-cells       (for [i (range height), j (range width)] [i j])
          live-cells      (set (filter #(= \# (get-in board %)) all-cells))
          neighbors       (fn [[x y]]
                            (for [i [(dec x) x (inc x)], j [(dec y) y (inc y)]
                                  :when (not (and (= x i) (= y j)))]
                              [i j]))
          live-neighbors  #(filter live-cells (neighbors %))
          survive?        #(let [n (count (live-neighbors %))]
                             (or (= n 3) (and (live-cells %) (= n 2))))
          next-live-cells (filter survive? all-cells)]
      (->> next-live-cells
        (reduce (fn [rows cell] (assoc-in rows cell \#))
          (vec (repeat height (vec (repeat width \space)))))
        (map #(apply str %))))))

(defcheck solution-7241f899
  (fn step [lines]
    (let [height       (count lines) width (count (first lines))
          valid?       (fn [[i j]] (and (>= i 0) (< i height) (>= j 0) (< j width)))
          neighs       (fn [[i j]] (filter valid? [[(dec i) (dec j)] [(dec i) j] [(dec i) (inc j)]
                                                   [i (dec j)] [i (inc j)]
                                                   [(inc i) (dec j)] [(inc i) j] [(inc i) (inc j)]]))
          alive?       (fn [[i j]] (= \# (nth (nth lines i) j)))
          alive-neighs (fn [coord] (count (filter alive? (neighs coord))))]
      (map-indexed (fn [i row]
                     (apply str (map-indexed (fn [j col]
                                               (let [n (alive-neighs [i j])]
                                                 (if (= \# col)
                                                   (if (or (= n 2) (= n 3)) \# \space)
                                                   (if (= n 3) \# \space)))) row))) lines))))

(defcheck solution-73080163
  (fn [b]
    (map-indexed
      (fn [i r]
        (apply str
          (map-indexed
            (fn [j c]
              (->> (for [i [-1 0 1] j [-1 0 1] :when (not= [i j] [0 0])] [i j])
                (map (partial map + [i j]))
                (filter (fn [[k l]] (and (< -1 k (count b)) (< -1 l (count (first b))))))
                (map (fn [[k l]] ((vec (b k)) l)))
                (filter (partial = \#))
                (count)
                (#(if (= c \#)
                    (cond
                      (> % 3) \space
                      (< % 2) \space
                      :else \#)
                    (if (= % 3) \# \space)))
                ))
            r)))
      b)))

(defcheck solution-73139fc0
  (fn game-of-life [s]
    (letfn [
            (neighbors
              ([size yx] (neighbors [[-1 0] [1 0] [0 -1] [0 1] [-1 -1] [1 1] [-1 1] [1 -1]] size yx))
              ([deltas size yx]
               (filter (fn [new-yx]
                         (every? #(< -1 % size) new-yx))
                 (map #(vec (map + yx %)) deltas))
               )
              )
            (get-live-neighbors [s xy]
              (let [size (count s) neighbors-xy (neighbors size xy)]
                (count (filter #{\#} (for [[x y] neighbors-xy] (nth (nth s x) y))))
                )
              )
            (new-state [s [x y]]
              (let [cell           (nth (nth s x) y)
                    live-neighbors (get-live-neighbors s [x y])]
                (cond
                  (and (= cell \#) (< live-neighbors 2)) \space
                  (and (= cell \#) (>= live-neighbors 2) (<= live-neighbors 3)) \#
                  (and (= cell \#) (> live-neighbors 3)) \space
                  (and (= cell \space) (= live-neighbors 3)) \#
                  :else \space
                  )
                )
              )
            ]
      (let [size (count s)
            rows (partition size (for [x (range 0 size) y (range 0 size)] [x y]))]
        (for [row rows] (apply str (map #(new-state s %) row)))
        )
      )
    ))

(defcheck solution-731cc3a6
  (fn [board]
    (letfn [(map-board [f]
              (vec (map-indexed (fn [ridx row]
                                  (apply str (map-indexed (fn [cidx cell]
                                                            (f cell ridx cidx))
                                               row)))
                     board)))
            (format-cell [kw]
              (if (= :alive kw) "#" " "))
            (alive? [cell]
              (= cell \#))
            (neighbors-of [row col]
              (set (for [nbor-row (range (dec row) (+ 2 row))
                         nbor-col (range (dec col) (+ 2 col))
                         :when (not (and (= nbor-row row) (= nbor-col col)))]
                     [nbor-row nbor-col])))
            (in-board? [dim [row col]]
              (and
               (>= row 0)
               (>= col 0)
               (< row dim)
               (< col dim)))
            (valid-neighbors-of [dim row col]
              (set (filter (partial in-board? dim) (neighbors-of row col))))
            (count-alive-neighbors-of [row col]
              (count (filter alive? (for [[nbor-row nbor-col] (valid-neighbors-of (count board) row col)]
                                      (get-in board [nbor-row nbor-col])))))
            (cell-next-generation [cell row col]
              (let [alive-cell       (alive? cell)
                    alive-nbor-count (count-alive-neighbors-of row col)]
                (cond
                  (and alive-cell (< alive-nbor-count 2)) :dead
                  (and alive-cell (contains? #{2 3} alive-nbor-count)) :alive
                  (and alive-cell (> alive-nbor-count 3)) :dead
                  (and (not alive-cell) (= 3 alive-nbor-count)) :alive
                  :else :dead)))]
      (map-board
        (fn [cell row col]
          (format-cell (cell-next-generation cell row col)))))))

(defcheck solution-736d3f8e
  #(let [rows (count %)
         cols (count (first %))]
     (letfn [(board-to-set [board]
               (->> (for [row (range rows)
                          col (range cols)]
                      (when (= (nth (nth board row) col) \#)
                        [row col]))
                 (filter identity)
                 (into #{})))
             (set-to-board [cells]
               (for [row (range rows)]
                 (clojure.string/join
                   (for [col (range cols)]
                     (if (cells [row col]) \# \space)))))
             (neighbors [cells row col]
               (reduce +
                 (for [row-offset [-1 0 1]
                       col-offset [-1 0 1]
                       :when (not (= row-offset col-offset 0))]
                   (if (cells [(+ row row-offset) (+ col col-offset)])
                     1 0))))
             (advance [cells]
               (->> (for [row (range rows)
                          col (range cols)]
                      (let [live (neighbors cells row col)]
                        (if (or (= 3 live)
                                (and (= 2 live)
                                     (cells [row col])))
                          [row col])))
                 (filter identity)
                 (into #{})))]
       (-> % board-to-set advance set-to-board))))

(defcheck solution-743ea73e
  (fn [c m]
    (let [w (count m) s (c m)]
      (map c
        (partition w
          (map-indexed
            (fn [i x]
              (({\# {2 \# 3 \#}} x {3 \#})
               (apply + (map #({\# 1 \  0} (nth s (+ % i) \ ))
                          [(- -1 w) (- w) (- 1 w)
                           -1 1 (- w 1) w (+ w 1)]))
               \ ))
            s))))) #(apply str %))

(defcheck solution-7485ddbb
  (fn game-of-life [board]
    (let [deltas    (for [x [-1 0 1]
                          y [-1 0 1]
                          :when (not= 0 x y)]
                      [x y])
          neighbors (fn [yx]
                      (->> (map #(map + yx %) deltas)
                        (filter (fn [new-yx]
                                  (every? #(< -1 % (count board)) new-yx)))
                        (map #(get-in board %))))
          positions (map (fn [x]
                           (vec (map #(vector x %) (range (count board)))))
                      (range (count board)))
          live?     #(= \# (get-in board %))
          new-val   (fn [yx]
                      (let [c (count (filter (partial = \#) (neighbors yx)))]
                        (case (if (live? yx) :live :dead)
                          :live (cond
                                  (< c 2) \space
                                  (<= 2 c 3) \#
                                  :else \space)
                          :dead (if (= 3 c) \# \space))))]
      (map (partial reduce str)
        (map (fn [yxs]
               (map #(new-val %) yxs))
          positions)))))

(defcheck solution-74c4fdc8
  (fn prob94 [board]
    (letfn
     [(live? [board [row col]]
        (= \# (get-in board [row col])))
      (neighbors [board row col]
        (let [max-row (dec (count board))
              max-col (dec (count (first board)))]
          (filter #(not (= % [row col]))
            (for
             [r (range (dec row) (+ row 2))
              c (range (dec col) (+ col 2))
              :when (and (>= max-row r 0) (>= max-col c 0))]
              [r c]))))]
      (mapv
        #(apply str %)
        (partition
          (count board)
          (for
           [row (range (count board))
            col (range (count (first board)))]
            (let [ln (count (filter #(live? board %) (neighbors board row col)))]
              (if (live? board [row col])
                (cond
                  (< ln 2) \space
                  (> ln 3) \space
                  :else \#)
                (if (= ln 3) \# \space)))))))))

(defcheck solution-750d34b6
  (fn [board]
    (let [tripart   (partial partition 3 1)
          transpose (partial apply map vector)
          goto      (fn [[row1 [c- cell c+] row3]]
                      (-> [c- c+]
                        (into row1)
                        (into row3)
                        frequencies
                        (get \# 0)
                        int
                        (case 3 \# 2 cell \space)))
          wrap      (fn [pad s] (-> [pad] (into s) (conj pad)))]
      (->> board
        tripart
        (map #(->> %
                transpose
                tripart
                (map goto)
                (wrap \space)
                (apply str)))
        (wrap (first board))))))

(defcheck solution-757b3ee4
  (fn [board]
    (let [w    (count (first board))
          h    (count board)
          at   (fn [x y] (.charAt (board y) x))
          tree (->>
                 (for [x (range w), y (range h)]
                   (zipmap (map #(map + [x y] %)
                             [[1 0] [0 1] [-1 0] [0 -1]
                              [1 1] [-1 -1] [-1 1] [1 -1]])
                     (repeat (if (= \# (at x y))
                               [\#] nil))))
                 (apply merge-with concat))]
      (for [y (range h)]
        (apply str
          (for [x (range w)]
            (condp = (count (tree [x y]))
              2 (at x y)
              3 \#
              \space)))))))

(defcheck solution-75e77f4a
  (fn [board]
    (let [rows       (count board)
          cols       (count (first board))
          live?      (fn [r c]
                       (let [r (mod r rows)
                             c (mod c cols)]
                         (= \# (get-in board [r c]))))
          live-count (fn [r c]
                       (->> (for [r (range -1 2)
                                  c (range -1 2)
                                  :when (or (not= r 0) (not= c 0))]
                              [r c])
                         (filter #(live? (+ r (first %))
                                    (+ c (second %))))
                         count))
          next-cell  (fn [[r c]]
                       (let [live (live-count r c)]
                         (if (= \# (get-in board [r c]))
                           (if (or (< live 2) (> live 3))
                             \space
                             \#)
                           (if (= live 3) \# \space))))]
      (for [r (range rows)]
        (apply str (map next-cell
                     (for [c (range cols)]
                       [r c])))))))

(defcheck solution-76402794
  (fn [b]
    (let [X                      (-> b first count)
          Y                      (count b)
          cells                  (for [y (range Y) x (range X)] [x y])
          cell                   (fn [[x y]] (-> b (get y) vec (get x)))
          alive?                 (fn [c] (-> (cell c) #{\#} boolean))
          neighbours             (fn [[x y]] (for [a [-1 0 1] b [-1 0 1] :when (not (= a b 0))] [(+ x b) (+ y a)]))
          count-alive-neighbours (fn [c] (->> (neighbours c) (map alive?) (filter identity) count))
          next-gen               (->> (reduce #(assoc % %2 (count-alive-neighbours %2)) {} cells)
                                   (map (fn [[c alive-nbg]] [c (cond
                                                                 (= 3 alive-nbg) \#
                                                                 (= 2 alive-nbg) (cell c)
                                                                 :else \space
                                                                 )]))
                                   (into {})
                                   )]
      (map (partial apply str) (partition X (map next-gen cells))))))

(defcheck solution-76617cf6
  (fn f [s]
    (let [xs (vec (map #(vec (map (fn [x] x) %)) s))
          ]
      (->>
        (apply conj {}
          (map (fn [[x y]] [[x y] (map (fn [[nx ny]] (get-in xs [(mod (+ x nx) (count (first xs))) (mod (+ y ny) (count xs))])) (for [i (range -1 2) j (range -1 2) :when (not= i j 0)] [i j]))]) ((fn [xs] (for [x (range (count xs)) y (range (count (first xs)))] [x y])) xs)))
        (map (fn [[k v]] [k (count (filter #(= \# %) v))]))
        (reduce (fn [xs [k v]] (cond (or (< v 2) (> v 3)) (update-in xs k (fn [_] \space))
                                     (= v 3) (update-in xs k (fn [_] \#))
                                     :else xs
                                     )) xs)
        (map #(apply str %))
        )
      )
    ))

(defcheck solution-76ebe661
  (fn gameoflife [board]
    (let [rowsize    (count (first board))
          rows       (count board)
          simple-ind (keep-indexed #(when (= \# %2) %1)
                       (apply str board))
          live-cells (map #((juxt quot rem) % rowsize) simple-ind)
          neighbors  (fn [[a b]]
                       (for [x [-1 0 1]
                             y [-1 0 1]
                             :when (<= 0 (+ a x))
                             :when (> rowsize (+ a x))
                             :when (<= 0 (+ b y))
                             :when (> rows (+ b y))
                             :when (not= x y 0)]
                         [(+ a x) (+ b y)]))
          how-many   (frequencies (mapcat neighbors live-cells))
          easy-live  (->> (filter #(= 3 (val %)) how-many)
                       (map first))
          other-live (->> (filter #(= 2 (val %)) how-many)
                       (map first)
                       (filter (set live-cells)))]
      (map #(apply str %) (reduce (fn [s ind]
                                    (assoc-in s ind \#))
                            (vec (repeat rows (vec (repeat rowsize " "))))
                            (concat easy-live other-live))))))

(defcheck solution-7719a4fe
  (fn [board] (letfn [
                      (dimensions [rect] (vector (apply max (map count rect)) (count rect)))
                      (pairs [coll1 coll2] (map (fn [x] (map #(vector x %) coll2)) coll1))
                      (dimensions [rect] (vector (count rect) (apply max (map count rect))))
                      (positions [rect] (pairs (range (first (dimensions rect))) (range (second (dimensions rect)))))
                      (neighbour-positions [position] (remove #(= position %) (map (fn [diff] (map + diff position)) (mapcat identity (pairs [-1 0 +1] [-1 0 +1])))))
                      (on-board? [board [row col]] (and (>= row 0) (>= col 0) (<= row (dec (first (dimensions board)))) (<= col (dec (second (dimensions board))))))
                      (neighbour-positions-on-board [board position] (filter #(on-board? board %) (neighbour-positions position)))
                      (alive? [val] (= \# val))
                      (dead? [val] (= \space val))
                      (own-status [board position] (get-in board position))
                      (neighbour-statuses [board position] (map #(get-in board %) (neighbour-positions-on-board board position)))
                      (number-of-live-neighbours [board position] (count (filter alive? (neighbour-statuses board position))))
                      (next-round-status [board position] (cond (and (dead? (own-status board position)) (= (number-of-live-neighbours board position) 3)) \# (and (alive? (own-status board position)) (< (number-of-live-neighbours board position) 2)) \space (and (alive? (own-status board position)) (> (number-of-live-neighbours board position) 3)) \space :else (own-status board position)))
                      (next-round-board [board] (map (fn [row] (map #(next-round-status board %) row)) (positions board)))
                      (next-round [board] (map #(apply str %) (next-round-board board)))
                      ] (next-round board))))

(defcheck solution-78341f7d
  (let [alive? (fn [board x y] (= (nth (nth board y) x) \#))
        dead?  (comp not alive?)]
    (fn [board]
      (for [y (range (count board))]
        (->> (for [x (range (count (nth board y)))]
               (let [neighbours (reduce + 0 (for [dx [-1, 0, 1]
                                                  dy [-1, 0, 1]
                                                  :when (and (not (and (zero? dx) (zero? dy)))
                                                             (> (+ x dx) 0) (> (+ y dy) 0)
                                                             (< (+ x dx) (count (nth board y)))
                                                             (< (+ y dy) (count board)))]
                                              (if (alive? board
                                                    (+ x dx)
                                                    (+ y dy))
                                                1
                                                0)))]
                 (if (alive? board x y)
                   (cond
                     (< neighbours 2) \space
                     (= neighbours 2) \#
                     (= neighbours 3) \#
                     :else \space)
                   (if (= neighbours 3)
                     \#
                     \space))))
          (apply str))))))

(defcheck solution-78ca91e2
  (fn life-game
    [board]
    (let [nrow                  (count board)
          ncol                  (count (first board))
          gen-cell-at-next-step (fn [board pos]
                                  (let [current-cell          (get-in board pos)
                                        get-neighbours        (fn nbs [board [x y]]
                                                                (map #(get-in board %)
                                                                  (for [i '(-1 0 1)
                                                                        j '(-1 0 1)
                                                                        :when (or (not= i 0) (not= j 0))]
                                                                    [(+ x i) (+ y j)])))
                                        total-live-neighbours (count (filter #(= % \#) (get-neighbours board pos)))]
                                    (if (= current-cell \#)
                                      (cond
                                        (or (= total-live-neighbours 2) (= total-live-neighbours 3)) \#
                                        :else \space)       ;; live now
                                      (cond
                                        (= total-live-neighbours 3) \#
                                        :else \space))      ;; dead now
                                    ))
          -charlist             (for [x (range nrow)
                                      y (range ncol)
                                      :let [pos [x y]]]
                                  (gen-cell-at-next-step board pos))
          next-board            (map #(apply str %) (partition ncol -charlist))]
      next-board)))

(defcheck solution-795ec03b
  (fn next-generation
    [g]
    (letfn [(elem
              [i j]
              (nth (nth g i) j))
            (nbs
              [i j]
              (let [dirs [[1 0] [1 1] [1 -1]
                          [0 1] [0 -1]
                          [-1 1] [-1 0] [-1 -1]]
                    sz   (count g)]
                (reduce
                  (fn [res [a b]]
                    (let [ni (+ i a)
                          nj (+ j b)]
                      (cond
                        (or (>= ni sz) (>= nj sz)
                            (< ni 0) (< nj 0)) res
                        (= \# (elem ni nj)) (inc res)
                        :else res)))
                  0 dirs)))]
      (let [res (atom (vec (map vec g)))
            sz  (count g)]
        (doseq [i (range sz) j (range sz)]
          (let [e (elem i j)
                n (nbs i j)]
            (if (= \space e)
              (when (= 3 n)
                (swap! res assoc-in [i j] \#))
              (when-not (or (= 2 n) (= 3 n))
                (swap! res assoc-in [i j] \space)))))
        (vec (map #(apply str %) @res))))))

(defcheck solution-7a6aaa5b
  (fn [board]
    (let [x-max     (count (first board))
          y-max     (count board)
          neighbors (fn [x y]
                      (reduce #(if (= %2 \#) (inc %) %)
                        0
                        (for [i (range (max 0 (dec x))
                                  (inc (min x-max (inc x))))
                              j (range (max 0 (dec y))
                                  (inc (min y-max (inc y))))
                              :when (or (not= i x) (not= j y))]
                          (get-in board [j i]))))
          live      (fn [n]
                      (cond (< n 2) " "
                            (< n 4) "#"
                            :else " "))
          dead      (fn [n] (if (= 3 n) "#" " "))]
      (for [y (range y-max)]
        (apply str
          (for [x (range x-max)]
            ((if (= (get-in board [y x]) \#) live dead)
             (neighbors x y))))))))

(defcheck solution-7b378b60
  (fn game-of-life [vs]
    (letfn [
            (parse-str [s]
              (->> s
                (vec,,,)
                (replace {\space :dead \# :live},,,)))
            (parse-board [bs]
              (let [board   (vec (map parse-str bs))
                    height  (count board)
                    rowlens (vec (map count board))
                    width   (first rowlens)]
                (if (and (pos? height)
                         (every? #(= width (count %)) board)
                         (every? (fn [row] (every? #(#{:live :dead} %) row)) board))
                  board
                  (throw (ex-info "Parse error" {})))))
            (make-board-idx [height width]
              (for [y (range height), x (range width)] [y x]))
            (get-cell [board [r c]]
              (nth (nth board r) c))
            (idx-on-board? [height width [y x]]
              (and (not (neg? y))
                   (not (neg? x))
                   (> height y)
                   (> width x)))
            (neighb-idxs [height width [y x]]
              (for [yof [-1 0 1], xof [-1 0 1]
                    :let [yyy (+ y yof), xxx (+ x xof)]
                    :when (and (idx-on-board? height width [yyy xxx])
                               (not (and (zero? yof) (zero? xof))))]
                [yyy xxx]))

            (one-gen-pass [height width board vidx]
              (let [m-board
                                    (reduce (fn [m [i j :as k]]
                                              (let [cell (get-cell board k)
                                                    nbrs (map (partial get-cell board)
                                                           (neighb-idxs height width k))
                                                    [lives-nb, deads-nb]
                                                    (reduce (fn [[acclive accdead] state]
                                                              (if (= :live state)
                                                                [(inc acclive) accdead]
                                                                [acclive (inc accdead)]))
                                                      [0 0] nbrs)
                                                    new-state
                                                         (cond
                                                           (and (= :live cell)
                                                                (or (< lives-nb 2) (> lives-nb 3))) :dead
                                                           (and (= :live cell)
                                                                (or (= 2 lives-nb) (= 3 lives-nb))) :live
                                                           (and (= :dead cell) (= 3 lives-nb)) :live
                                                           :otherwise cell)]
                                                (assoc-in m [i j] new-state)))
                                      {} vidx)
                    mrows           (map #(get m-board %) (range height))
                    new-state-board (map (fn [mr] (map #(get mr %) (range width)))
                                      mrows)
                    str-form-rslt
                                    (vec (map (fn [rs] (->> rs
                                                         (replace {:dead \space :live \#},,,)
                                                         (apply str,,,)))
                                           new-state-board))]
                str-form-rslt))]
      (let [board  (parse-board vs)
            height (count board)
            width  (count (first board))
            vidx   (make-board-idx height width)]
        (one-gen-pass height width board vidx)))))

(defcheck solution-7b5a99db
  (fn [grid]
    (letfn [(count3s [lis]
              (map (fn [& args] (count (filter #(= % \#) args)))
                lis
                (cons \space lis)
                (concat (drop 1 lis) (list \space))))
            (sum3s [rowcounts]
              (map (fn [& rows] (apply map + rows))
                rowcounts
                (cons (repeat 0) rowcounts)
                (concat (drop 1 rowcounts) (list (repeat 0)))))
            (newval [ct vc]
              (if (= \# vc)                                 ; live
                (if (contains? #{3 4} ct) \# \space)
                (if (= 3 ct) \# \space)))]
      (let [rowcts  (map count3s grid)
            cellcts (sum3s rowcts)
            nvs     (map (fn [r1 r2] (map newval r1 r2))
                      cellcts grid)]
        (map (fn [r] (apply str r)) nvs)))))

(defcheck solution-7b5d2886
  (fn [inputboard]
    (let [neighbors (fn [[x y]]
                      (for [dx [-1 0 1] dy [-1 0 1] :when (not (and (= 0 dx) (= 0 dy)))]
                        [(+ x dx) (+ y dy)]))]
      (map (fn [x] (apply str x)) (reduce (fn [row x]
                                            (conj row (reduce (fn [col y]
                                                                (let [curneighbors (neighbors [x y])
                                                                      cntneighbors (count (filter #(= % \#)
                                                                                            (map (fn [[x y]]
                                                                                                   (get-in inputboard [x y] \space)) curneighbors)))
                                                                      live?        (= \# (get-in inputboard [x y] \space))]
                                                                  (if (or (= cntneighbors 3) (and live? (= cntneighbors 2)))
                                                                    (conj col \#)
                                                                    (conj col \space)))) [] (range (count inputboard))))) [] (range (count inputboard)))))))

(defcheck solution-7bb81b49
  (fn conway-life [b]
    (let [n     (count (first b))
          m     (count b)
          alive \#
          dead  \space]
      (letfn [(neighbours [x y]
                (frequencies
                  (keep (partial get-in b)
                    [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)] [(dec x) y]
                     [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)] [(inc x) y]])))
              (alive? [x y] (= alive (get-in b [x y])))]
        (for [x (range n)]
          (apply str
            (for [y (range m)]
              (let [neighb (neighbours x y)]
                (if (alive? x y)
                  (if (< 1 (neighb alive 0) 4) alive dead)
                  (if (= 3 (neighb alive 0)) alive dead))))))))))

(defcheck solution-7bfb33d6
  (fn gol-update [board]
    ;; Verifying that the board is in the proper format:
    {:pre [
           ;; The rows should be ordered.
           (sequential? board),
           ;; Each row should be a string.
           (every? string? board),
           ;; Each row should have the same length, but we do allow the empty board.
           (or (empty? board)
               (apply = (map count board))),
           ;; The only characters are #'s and spaces.
           (every? (partial every? #{\# \space}) board)
           ]}

    (let [
          ;; We begin by recording the dimensions of the board.
          max-rows    (count board),
          max-cols    (count (first board)),

          ;; The function count-adj counts the number of cells adjacent to the
          ;; cell at the specified row and column that match the supplied
          ;; character c.
          count-adj   (fn [c row col]
                        (->> (for [i (range max-rows), j (range max-cols)
                                   :when (< 0
                                           (max (Math/abs (- i row))
                                             (Math/abs (- j col)))
                                           2)]
                               (get-in board [i j]))
                          (filter (partial = c))
                          count)),

          ;; The function update-cell determines whether the cell at the specified
          ;; row and column should be alive or dead in the next iteration.
          update-cell (fn [row col]
                        (let [c              (get-in board [row col]),
                              live-neighbors (count-adj \# row col)]
                          (case live-neighbors
                            2 (if (= c \#) \# \space)
                            3 \#
                            \space)))]

      ;; With the helper functions defined above, we just need to "print" the
      ;; board.
      (for [i (range max-rows)]
        (apply str (for [j (range max-rows)]
                     (update-cell i j)))))))

(defcheck solution-7ca0b358
  (fn next-gen [grid]
    (let [cell?    (->> grid
                     (map-indexed (fn [i r]
                                    (map-indexed (fn [j c]
                                                   (if (= \# c) [i j] nil)) r)))
                     (apply concat)
                     (set))
          ns       '([-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1])
          n-count  (fn [r c]
                     (count (filter cell? (map (fn [[x y]]
                                                 [(+ r x) (+ c y)]) ns))))
          live-map (for [r (range (count grid))]
                     (for [c (range (count (first grid)))]
                       (let [n (n-count r c)]
                         (or (= n 3) (and (cell? [r c]) (= n 2))))))]
      (map (fn [row] (apply str (map #(if % \# \ ) row))) live-map))))

(defcheck solution-7ce8b23
  (let [neighbours
        (fn [x y minx miny maxx maxy]
          (let [u (+ y 1) d (- y 1) l (- x 1) r (+ x 1)]
            (let [naive
                  [[l u] [x u] [r u]
                   [l y] [r y]
                   [l d] [x d] [r d]]]
              (filter (fn [[x y]] (and (>= x minx) (>= y miny)
                                       (<= x maxx) (<= y maxy)))
                naive))))]
    (fn life-step
      [board]
      (let [alive \#
            dead  \space
            minx  0
            miny  0
            maxx  (- (count board) 1)
            maxy  (- (count (first board)) 1)
            get
                  (fn [[x y]] (nth (nth board x) y))
            nbrs
                  (fn [[x y]] (neighbours x y minx miny maxx maxy))
            live-count
                  (fn [cells] (count (filter #(= \# %) cells)))
            next-state
                  (fn [live-count current] (if (< live-count 2) dead
                                                                (if (= live-count 2) current
                                                                                     (if (= live-count 3) alive
                                                                                                          dead))))
            coords
                  (map (fn [x] (map (fn [y] [x y])
                                 (range (+ maxy 1))))
                    (range (+ maxx 1)))]
        (let [next-board (map #(map (fn [coord]
                                      (next-state (live-count (map get (nbrs coord)))
                                        (get coord)))
                                 %) coords)]
          (map #(clojure.string/join %) next-board))))))

(defcheck solution-7d0063bd
  (fn next-gen-gof [board]
    (let [live-cell              \#
          dead-cell              \space
          convert-to-vectors     (fn [board]
                                   "Converts vector of strings into vector of vectors of characters"
                                   (loop [b1 board, result []]
                                     (if (empty? b1)
                                       result
                                       (recur (rest b1) (conj result (vec (first b1))))
                                       )))
          gen-indexes            (fn [rows columns]
                                   "Generate map of indexes"
                                   (for [i (range 0 rows), j (range 0 columns)]
                                     {:row i, :col j}
                                     ))
          gen-idxs-of-neighbours (fn [idx rows columns]
                                   "Return indexes of 8 neighbours, filter out indexes outside of the board"
                                   (let [{r :row, c :col} idx
                                         dr   (dec r)
                                         dc   (dec c)
                                         ir   (inc r)
                                         ic   (inc c)
                                         r1   [{:row dr, :col dc}, {:row dr, :col c}, {:row dr, :col ic},
                                               {:row r, :col dc},, {:row r, :col ic},
                                               {:row ir, :col dc}, {:row ir, :col c}, {:row ir, :col ic}]
                                         pred (fn [{r :row, c :col}]
                                                (cond
                                                  (neg? r) false
                                                  (neg? c) false
                                                  (> r (dec rows)) false
                                                  (> c (dec columns)) false
                                                  :default true))]
                                     (filter pred r1)
                                     ))
          is-live?               (fn [c]
                                   (if (= live-cell c) true false))
          get-vals               (fn [board, indexes]
                                   "Get values from the board at indexes"
                                   (loop [idx1 indexes, result []]
                                     (if (empty? idx1)
                                       result
                                       (let [{r :row, c :col} (first idx1)
                                             v ((board r) c)]
                                         (recur (rest idx1) (conj result v))
                                         ))))
          how-many-is-alive      (fn [neighbours]
                                   "Return count of live cells from input"
                                   (count (filter is-live? neighbours)))
          evolve-for-live        (fn [neighbours]
                                   "Depending on number of live neighbours evolve actual live-cell according to rules"
                                   (let [alive (how-many-is-alive neighbours)]
                                     (cond
                                       (< alive 2) dead-cell
                                       (or (= alive 2) (= alive 3)) live-cell
                                       (> alive 3) dead-cell)
                                     ))
          evolve-for-dead        (fn [neighbours]
                                   "Depending on number of dead neighbours evolve actual dead-cell according to rules"
                                   (let [alive (how-many-is-alive neighbours)]
                                     (if (= alive 3)
                                       live-cell
                                       dead-cell)
                                     ))
          evolve-cell            (fn [cell neighbours]
                                   "Evolve the cell depening on type and number of neighbours according to rules"
                                   (if (is-live? cell)
                                     (evolve-for-live neighbours)
                                     (evolve-for-dead neighbours)
                                     ))
          evolve-board           (fn [board]
                                   (let [vectorized-board (convert-to-vectors board)
                                         rows             (count vectorized-board)
                                         columns          (count (first vectorized-board))
                                         indexes          (gen-indexes rows columns)]
                                     (loop [idxs1 indexes, result []]
                                       (if (empty? idxs1)
                                         result
                                         (let [idx            (first idxs1)
                                               r              (idx :row)
                                               c              (idx :col)
                                               cell           ((vectorized-board r) c)
                                               idxs-of-neighs (gen-idxs-of-neighbours idx rows columns)
                                               neighbours     (get-vals vectorized-board idxs-of-neighs)
                                               next-cell      (evolve-cell cell neighbours)]
                                           (recur (rest idxs1) (conj result next-cell)))
                                         ))))
          next-board             (evolve-board board)
          partitioned-board      (partition (-> board first count) next-board)]
      (map #(apply str %) partitioned-board)
      )))

(defcheck solution-7d2c1814
  (fn f [ss]
    (let [board        (mapv #(mapv char %) ss)
          neighborhood (juxt dec identity inc)]
      (letfn [(alive? [where] (= \# (get-in board where)))]
        (map
          #(reduce str %)
          (partition
            (-> ss first count)
            (for [y (-> ss count range) x (-> ss first count range)]
              (let [me             [y x]
                    live-neighbors (apply + (for [r (neighborhood y)
                                                  c (neighborhood x)
                                                  :let [neighbor [r c]]
                                                  :when (not= me neighbor)]
                                              (if (alive? neighbor) 1 0)))]
                (cond
                  (= 3 live-neighbors) \#
                  (and (alive? me) (= 2 live-neighbors)) \#
                  :else \space)))))))))

(defcheck solution-7d5b5944
  (fn game-of-life [coll]
    (let [height (count coll)
          width  (count (nth coll 0))]
      (letfn [(element-type [[i j]]
                (nth (nth coll i) j))
              (neighbour-no-neg [[x y]]
                (remove
                  #(some neg? %)
                  (for [dx [-1 0 1]
                        dy (if (zero? dx)
                             [-1 1]
                             [-1 0 1])]
                    [(+ x dx) (+ y dy)])))
              (neighbour-position [[x y]]
                (remove #(<= height (last %))
                  (remove #(<= width (first %)) (neighbour-no-neg [x y]))))
              (neighbour-type [[x y]]
                (map #(nth (nth coll (first %)) (last %)) (neighbour-position [x y])))
              (count-neighbours-alive [[x y]]
                (count (filter #(= % \#) (neighbour-type [x y]))))]
        (map
          #(apply str %)
          (for [i (range (count coll))]
            (for [j (range (count (nth coll i)))]
              (if (= \# (element-type [i j]))
                (cond
                  (> 2 (count-neighbours-alive [i j])) \space
                  (<= 2 (count-neighbours-alive [i j]) 3) \#
                  (< 3 (count-neighbours-alive [i j])) \space)
                (if (= 3 (count-neighbours-alive [i j]))
                  \#
                  \space)))))))))

(defcheck solution-7e6e590
  (fn [c]
    (letfn [(f [[a b]]
              (for [i [-1 0 1]
                    j [-1 0 1] :when (not= i j 0)]
                [(+ a i) (+ b j)]))]
      (map #(apply str %)
        (map-indexed
          (fn [i x]
            (map-indexed
              (fn [j y]
                (let [n (map #(get-in c %) (f [i j]))
                      a (count (filter #{\#} n))]
                  (cond (= a 3) \#
                        (= a 2) (get-in c [i j])
                        :else \space))
                ) x))
          c)))))

(defcheck solution-7f0bb82e
  (fn nextStep [maze]
    (letfn [(neighbours
              ([maze yx] (neighbours [[-1 -1] [-1 0] [-1 1]
                                      [0 -1] [0 1]
                                      [1 -1] [1 0] [1 1]]
                           maze
                           yx))
              ([deltas maze yx]
               (filter (fn [new-yx]
                         (every? #(< -1 % (count maze)) new-yx))
                 (map #(vec (map + yx %))
                   deltas))))
            (updateCell [x y maze]
              (let [neighs     (neighbours maze [x y])
                    isLive     (= (get-in maze [x y]) \#)
                    liveNeighs (count (filter (fn [[nx ny]] (= (get-in maze [nx ny]) \#)) neighs))]
                (cond
                  (and isLive (< liveNeighs 2)) \space
                  (and isLive (<= 2 liveNeighs 3)) \#
                  (and isLive (> liveNeighs 4)) \space
                  (and (not isLive) (= liveNeighs 3)) \#
                  :else \space)))]
      (for [x (range (count maze))]
        (clojure.string/join
          (for [y (range (count maze))]
            (updateCell x y maze)))))))

(defcheck solution-7f6a718a
  (fn nextStep [sq]
    (letfn [(access [sq x y]
              (nth (nth sq y) x))

            (neighbours [x y]
              (for [dx (range -1 2) dy (range -1 2) :when (not= [0 0] [dx dy])] [(+ x dx) (+ y dy)]))

            (countLivingNeighbours [sq x y]
              (apply + (map (fn [[x y]] (if (= \# (access sq x y)) 1 0)) (neighbours x y))))

            (gameRule [alive nbours]
              (or
               (and alive (or (= 2 nbours) (= 3 nbours)))
               (and (not alive) (= 3 nbours))))

            (sign->state [sign]
              (= \# sign))

            (state->sign [state]
              (if state \# \space))

            (evolve [sq x y]
              (state->sign
                (gameRule
                  (sign->state (access sq x y))
                  (countLivingNeighbours sq x y))))

            (addBorders [sq]
              (let [extra (apply str (repeat (+ 2 (count sq)) \space))]
                (vec (concat [extra]
                       (vec (map
                              #(apply str (concat " " % " "))
                              sq))
                       [extra]))))]
      (let [size (dec (count sq))]
        (addBorders (vec (map
                           (fn [y]
                             (apply str
                               (map
                                 #(evolve sq % y)
                                 (range 1 size))))
                           (range 1 size))))))))

(defcheck solution-7f9ba435
  (fn [a]
    (letfn [(alive [a r c]
              (and (>= r 0) (< r (count a)) (>= c 0)
                   (< c (count (a r))) (= (nth (a r) c) \#)))
            (to-r [x] (range (dec x) (+ x 2)))
            (neighbors [a r c]
              (apply + (for [rp (to-r r) cp (to-r c)]
                         (if (and (or (not= r rp) (not= c cp))
                                  (alive a rp cp))
                           1 0))))
            (to-c [r c v]
              (let [n (neighbors a r c)]
                (cond (or (< n 2) (> n 3)) \space
                      (= n 2) v
                      :else \#)))
            (cf [ri rv] (map-indexed #(to-c ri %1 %2) rv))]
      (map #(apply str %1) (map-indexed cf a)))))

(defcheck solution-7fbedd65
  (fn [h b]
    (map (partial apply str)
      (map-indexed
        (fn [r l] (map-indexed
                    (fn [c e]
                      ({3 \# 2 e}
                       (apply + (map
                                  (fn [[i j]] ({\# 1} (get-in b [(+ i r) (+ j c)] \ ) 0))
                                  h))
                       \ ))
                    l))
        b))) [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defcheck solution-8013eaaf
  (fn [b]
    (let [n (count (first b))]
      (->> (for [i (range n)
                 k (range n)
                 :let [c (->> [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
                           (map (fn [[y x]] [(+ y i) (+ x k)]))
                           (map (fn [[y x]] (nth (nth b y nil) x nil)))
                           (filter #(= \# %))
                           count)]]
             (cond (< c 2) \space
                   (= c 2) (nth (nth b i) k)
                   (= c 3) \#
                   :else \space))
        (partition n)
        (map #(apply str %))))))

(defcheck solution-80dd4646
  (fn [b]
    (let [neighborhood
          (partition 2
            (flatten
              (for [x [-1, 0, 1]]
                (for [y [-1, 0, 1] :when (not (and (= x 0) (= y 0)))] [x y]))))]
      (letfn [(bget [b x y]
                (get (get b y) x))
              (neighbors [b x y]
                (map (fn [[x1 y1]] (bget b (+ x x1) (+ y y1))) neighborhood))
              (num-neighbors [b x y]
                (count (filter #(= \# %) (neighbors b x y))))
              (new-state [b x y]
                (let [live (= (bget b x y) \#)
                      num  (num-neighbors b x y)]
                  (if live
                    (cond (< num 2) " "
                          (> num 3) " "
                          :else "#")
                    (if (= num 3) \# " "))))
              (line [b y]
                (let [l (for [x (range 1 (- (count b) 1))] (new-state b x y))]
                  (str " " (apply str l) " ")))]
        (conj (vec (cons (get b 0) (for [y (range 1 (- (count b) 1))] (line b y)))) (get b 0))))))

(defcheck solution-80eecb0b
  (fn life-iter [b]
    (letfn [(new-state [pos]
              (let [cell (get-in b pos) alive (= cell \#)
                    nb   (count (filter #{\#} (map #(get-in b % nil)
                                                (map (partial map + pos)
                                                  [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))))]
                (cond (and alive (< nb 2)) \space
                      (and alive (> nb 3)) \space
                      (and alive (<= 2 nb 3)) \#
                      (and (not alive) (= nb 3)) \#
                      :else cell)))]
      (map #(apply str %)
        (reduce #(assoc-in % %2 (new-state %2)) (mapv vec b)
          (for [x (range (count b)) y (range (count (first b)))] [x y]))))))

(defcheck solution-80f5c42a
  (fn game-of-life [board]
    (letfn [(find-cells [board]
              (map-indexed (fn [i row]
                             (map-indexed (fn [j ch]
                                            (let [ln (count (live-neighbors board [i j]))]
                                              (if (= ch \#)
                                                (if (or (< ln 2) (> ln 3)) \space ch)
                                                (if (= ln 3) \# ch))))
                               row))
                board))

            (valid-live-cell? [board [i j] max-i max-j]
              (and (>= i 0) (>= j 0) (< i max-i) (< j max-j) (= (get-in board [i j]) \#)))

            (live-neighbors [board [i j]]
              (let [max-i (count board)
                    max-j (count (first board))]
                (->> [[(dec i) j] [(dec i) (dec j)] [i (dec j)] [(inc i) (dec j)]
                      [(inc i) j] [(inc i) (inc j)] [i (inc j)] [(dec i) (inc j)]]
                  (filter #(valid-live-cell? board %1 max-i max-j)))))]
      (mapv #(apply str %1) (find-cells board)))))

(defcheck solution-80f94e0a
  (fn next-generation [board]
    (let [offsets           (clojure.set/difference (set (for [x (range -1 2) y (range -1 2)] (vector x y))) #{[0 0]})
          height            (count board)
          width             (count (first board))
          cell              (fn [x y] (get-in board [x y] \space))
          neighbours        (fn [x y] (map (fn [[dx dy]] (cell (+ x dx) (+ y dy))) offsets))
          living-neighbours (fn [x y] (count ((group-by identity (neighbours x y)) \#)))
          next-cell         (fn [x y] (if (= (cell x y) \#)
                                        (if (or (= (living-neighbours x y) 2) (= (living-neighbours x y) 3))
                                          \#
                                          \space)
                                        (if (= (living-neighbours x y) 3)
                                          \#
                                          \space)
                                        ))]
      (map #(apply str %)
        (partition width
          (for [x (range width) y (range height)]
            (next-cell x y)
            )
          )
        )
      )
    ))

(defcheck solution-813f362
  (fn gol [board]
    (let [row-n (count (first board))
          col-n (count board)
          v     #(if (and (<= 0 %1) (<= 0 %2) (< %1 col-n) (< %2 row-n)) (nth (seq (nth board %1)) %2))
          nbrs  #(for [m [0 1 -1] n [0 1 -1] :when (not (every? zero? [m n]))]
                   (v (+ %1 m) (+ %2 n)))
          lnrbs #(count (filter #{\#} (nbrs %1 %2)))]

      (for [m (range col-n)]
        (apply str
          (for [n (range row-n)]
            (let [l (lnrbs m n)]
              (cond
                (and (= (v m n) \#) (#{2 3} l)) \#
                (and (not= (v m n) \#) (= 3 l)) \#
                :else \space))))))))

(defcheck solution-818d156e
  (fn [rows]
    (map #(apply str %)
      (partition
        (count rows)
        (map (fn [[c n]]
               (if (or (and (= c \#) (= n 2)) (= n 3))
                 \# \ ))
          (for [x (range 0 (count rows))
                y (range 0 (count (nth rows x)))]
            (let [lim (fn [coll n] (if (< -1 n (count coll)) (nth coll n) " "))]
              [(lim (lim rows x) y)
               (count
                 (filter
                   (partial = \#)
                   [(lim (lim rows (dec x)) (dec y)) (lim (lim rows x) (dec y)) (lim (lim rows (inc x)) (dec y))
                    (lim (lim rows (dec x)) y) (lim (lim rows (inc x)) y)
                    (lim (lim rows (dec x)) (inc y)) (lim (lim rows x) (inc y)) (lim (lim rows (inc x)) (inc y))]))])))))))

(defcheck solution-81c669
  (fn gol [board]
    (letfn [(neighbors [board row col]
              (count (filter #(= % \#) (mapcat #(get (vec (partition 3 1 (extract-padded-row board %))) col) (map #(+ row %) [-1 0 1])))))
            (extract-padded-row [board row]
              (if (and (>= row 0) (< row (count board)))
                (concat [\space] (nth board row) [\space])
                []))]
      (map #(apply str %)
        (map-indexed
          (fn [r row]
            (map-indexed
              (fn [c cell]
                (let [live-neighs (neighbors board r c)]
                  (if (= cell \space)
                    ;; Dead cell
                    (if (= live-neighs 3)
                      \#
                      \space)
                    ;; Live cell (includes itself
                    (if (or (< live-neighs 3)
                            (> live-neighs 4))
                      \space
                      \#))))
              row)) board)))))

(defcheck solution-83018a2e
  (fn n94 [board] (letfn [(get-neighbors-coord [r c coord]
                            (filter (fn [[x y]] (and (not= [x y] coord) (<= 0 x) (< x r) (<= 0 y) (< y c)))
                              (map #(map + coord %) (for [dx [-1 0 1] dy [-1 0 1]] [dx dy]))))
                          (count-live-neighbors [nbs]
                            (count (filter (partial = \#) (map #(get-in board %) nbs))))
                          (next-gen [curr-cell live-nbs]
                            (if (= curr-cell \#)
                              (cond
                                (< live-nbs 2) \space
                                (> live-nbs 3) \space
                                :else \#)
                              (if (= live-nbs 3) \# \space)))]
                    (let [r     (count board)
                          c     (count (first board))
                          coord (for [x (range r) y (range c)] [x y])]
                      ;(for [cd coord])
                      ;(map #(get-in board %) coord)
                      (map #(apply str %)
                        (partition c
                          (map (fn [coord live-nbs] (next-gen (get-in board coord) live-nbs))
                            coord
                            (map #(count-live-neighbors (get-neighbors-coord r c %)) coord))))))))

(defcheck solution-843a8a70
  (fn [board]
    (let [
          pattern    (for [x (range -1 2) y (range -1 2) :when (not= y x 0)] [x y])
          cell       (fn [[x y]] (get (get board x) y))
          ngb        (fn [[x y]]
                       (map
                         (fn [[ox oy]] (cell
                                         [(mod (+ x ox) (count (first board)))
                                          (mod (+ y oy) (count board))]))
                         pattern))
          ngb-alive  (fn [p] (count (filter #{\#} (ngb p))))
          next-state (fn [cellstatus ngbs]
                       (if (= cellstatus \#)
                         (cond
                           (< ngbs 2) " "
                           (> ngbs 3) " "
                           :else \#)
                         (cond
                           (= ngbs 3) \#
                           :else " ")))]
      (for [x (range (count (first board)))]
        (apply str (for [y (range (count board))]
                     (next-state (cell [x y]) (ngb-alive [x y]))
                     ))
        ))))

(defcheck solution-84948f2
  (fn [b]
    (let [n #(apply +
               (for [i [-1 0 1]
                     j [-1 0 1]
                     :let [h (get-in b [(+ % i) (+ %2 j)])]
                     :when (and (= h \#) (not= i j 0))]
                 1))
          a #(let [l (n % %2)
                   h (get-in b [% %2])]
               (or (and (= h \#) (or (= l 2) (= l 3)))
                   (and (= h \ ) (= l 3))))
          s (range (count (b 0)))]
      (for [r s]
        (apply str (for [c s]
                     (if (a r c) \# \ )))))))

(defcheck solution-856d82ba
  (fn life [b]
    (let [bp                     (mapv (fn [v] (vec (seq v))) b)
          cell-indexes           (vec (for [x (range 1 (- (count bp) 1))
                                            y (range 1 (- (count (first bp)) 1))]
                                        [x y]))

          neighbor-fn            (fn [c]
                                   (let [f (first c)
                                         s (second c)]
                                     (keep-indexed (fn [idx v] (when (not= idx 4) v)) (for [x (range (dec f) (+ 2 f))
                                                                                            y (range (dec s) (+ 2 s))]
                                                                                        [x y]))))
          neighbor-indexes       (mapv neighbor-fn cell-indexes)
          neighbors              (mapv (fn [c] (map #(get-in bp %) c)) neighbor-indexes)
          neighbor-filter-counts (mapv (fn [cell n] [cell (count (filter #(= \# %) n))]) cell-indexes neighbors)
          new-vals               (reduce
                                   (fn [acc fc] (let [current (get-in bp (first fc))
                                                      newval  (if (= current \#)
                                                                (cond (< (second fc) 2) \space
                                                                      (< (second fc) 4) \#
                                                                      :othersise \space)
                                                                (if (= (second fc) 3) \# \space))]
                                                  (update-in acc (first fc) (fn [_] newval))))
                                   bp neighbor-filter-counts)]
      (mapv (fn [v] (apply str v)) new-vals)
      )
    ))

(defcheck solution-858b3537
  (fn game-of-life [ss]
    (let [lenlen (count ss)]
      (letfn [(ss->vs [sss]
                (flatten (map vec sss)))
              (vs->ss [vs]
                (map #(apply str %) (partition lenlen vs)))
              (neighbour-idx-n []
                (map (fn [x] (if (= 6 lenlen)
                               (remove #(or (neg? %) (> % 35))
                                 [(- x 7)
                                  (- x 6)
                                  (if (= 5 (rem x 6)) -1 (- x 5))
                                  (- x 1)
                                  (+ x 1)
                                  (if (zero? (rem x 6)) -1 (+ x 5))
                                  (+ x 6)
                                  (+ x 7)])
                               (remove #(or (neg? %) (> % 24))
                                 [(- x 6)
                                  (- x 5)
                                  (if (= 4 (rem x 5)) -1 (- x 4))
                                  (- x 1)
                                  (+ x 1)
                                  (if (zero? (rem x 5)) -1 (+ x 4))
                                  (+ x 5)
                                  (+ x 6)])))
                  (range (* lenlen lenlen))))
              (neighbour-elt-n []
                (map (fn [neighbour-idx]
                       (map #(nth (ss->vs ss) %) neighbour-idx))
                  (neighbour-idx-n)))]
        (vs->ss (map #(let [nb-elt-n (neighbour-elt-n)
                            elt      (nth (ss->vs ss) %)
                            live-nbs (remove #{\space} (nth nb-elt-n %))
                            live-len (count live-nbs)]
                        ;;                    live-len)
                        (if (= elt \#)
                          (if (<= 2 live-len 3)
                            \#
                            \space)
                          (if (= live-len 3)
                            \#
                            \space)))
                  (range (* lenlen lenlen))))))))

(defcheck solution-85ab297f
  (fn conway [state]
    (let [state  (mapv vec state)
          width  (count state)
          height (count (first state))]
      (map (partial apply str)
        (reduce
          (fn [next-state [y x]]
            (assoc-in next-state [y x]
              (case (->> (for [dx [-1 0 1]
                               dy [-1 0 1]
                               :when (not= 0 dx dy)]
                           [(+ y dy) (+ x dx)])
                      (map (partial get-in state))
                      (filter #{\#})
                      count)
                2 (get-in state [y x])
                3 \#
                \space)))
          state
          (for [y (range height)
                x (range width)]
            [y x]))))))

(defcheck solution-86027904
  (fn __ [se]
    (let [
          phase1         (map #(vec (map (fn [x] (if (= x \#) 1 0)) (seq %))) se)
          phase2         (map #(map + (drop-last (cons 0 %)) (rest (conj % 0)) %) phase1)
          transposed     (map vec (apply map list phase2))
          phase3         (map #(map + (drop-last (cons 0 %)) (rest (conj % 0)) %) transposed)
          transposedBack (apply map list phase3)
          final          (fn [sum orig]
                           (if (or (and (= orig 0) (= sum 3))
                                   (and (= orig 1) (or (= sum 3) (= sum 4))))
                             \#
                             \ )
                           )
          phase4         (map #(apply str (map final % %2)) transposedBack phase1)
          ]
      phase4)))

(defcheck solution-862261cb
  (fn [board]
    (let [get-cell       (fn [[x y]] (when (and (empty? (filter neg? [x y]))
                                                (< x (count (get board y))))
                                       (nth (get board y) x)))
          live-neighbors (fn [[x y]]
                           (loop [pointer [(dec x) (dec y)]
                                  n       0]
                             (if (nil? pointer)
                               n
                               (recur (cond (= (map inc [x y]) pointer) nil
                                            (= (first pointer) (inc x)) [(dec x) (inc (second pointer))]
                                            :else [(inc (first pointer)) (second pointer)])
                                 (if (and (= \# (get-cell pointer)) (not= [x y] pointer))
                                   (inc n)
                                   n)))))]

      (loop [pointer [0 0]
             acc     []]
        (if (nil? pointer)
          (map #(reduce str %) (partition (count board) acc))
          (let [cell-in-question (get-cell pointer)
                live-cell?       (= \# cell-in-question)
                neighbors        (live-neighbors pointer)]
            (recur (cond (= [(dec (count (first board))) (dec (count board))] pointer) nil
                         (= (dec (count (first board))) (first pointer)) [0 (inc (second pointer))]
                         :else [(inc (first pointer)) (second pointer)])

              (conj acc (cond (and live-cell? (< 3 neighbors)) \space
                              (and live-cell? (> 2 neighbors)) \space
                              (and live-cell? (or 2 (= neighbors) (= 3 neighbors))) \#
                              (and (not live-cell?) (= 3 neighbors)) \#
                              :else \space)))))))))

(defcheck solution-866e4d05
  (fn life [board]
    (letfn [(alive? [x y] (= \# (get-in board [y x])))
            (alive-neighbors [x y]
              (count (for [xx [x (dec x) (inc x)]
                           yy [y (dec y) (inc y)]
                           :when (alive? xx yy)] 1)))]
      (map #(apply str %)
        (for [y (range (count board))]
          (for [x (range (count (first board)))]
            (let [curr-nbos (alive-neighbors x y)]
              (if (or (= 3 curr-nbos) (and (alive? x y) (= 4 curr-nbos)))
                \#
                \space))))))))

(defcheck solution-869d5e21
  (fn [tabuleiro]
    (let [dimensao (count tabuleiro)]
      (->> (map vector
             (->> (for [linha-original  (range dimensao)
                        coluna-original (range dimensao)]
                    (remove #(let [lin (first %)
                                   col (second %)]
                               (or (neg? lin)
                                   (>= lin dimensao)
                                   (neg? col)
                                   (>= col dimensao)
                                   (= [linha-original coluna-original] %)))
                      (for [linha-vizinho  (range (dec linha-original) (inc (inc linha-original)))
                            coluna-vizinho (range (dec coluna-original) (inc (inc coluna-original)))]
                        [linha-vizinho coluna-vizinho])))
               (map #(->> (map (fn [c] (get-in tabuleiro c)) %)
                       (remove (fn [x] (= \space x)))
                       count)))
             (mapcat seq tabuleiro))
        (map (fn [[n cell]]
               (if (= \# cell)
                 (cond
                   (or (= n 2) (= n 3)) \#
                   :else \space)
                 (if (= n 3)
                   \#
                   \space))))
        (partition dimensao)
        (map #(apply str %))
        (vec)))))

(defcheck solution-87af0c3d
  (fn [rows]
    (let [nc                   (count (first rows))
          nr                   (count rows)
          c                    (* nc nr)
          b                    (apply str rows)
          live?                #(= \# (nth b % \ ))
          live-neighbors-count (fn [n] (count (filter live? [(- n nc 1) (- n nc) (- n nc -1)
                                                             (dec n) (inc n)
                                                             (+ n nc -1) (+ n nc) (+ n nc 1)])))
          next-gen             (fn [n]
                                 (let [nc (live-neighbors-count n)]
                                   (if (live? n)
                                     (if (#{2 3} nc) \# \ )
                                     (if (= 3 nc) \# \ ))))]
      (map #(apply str %) (partition nc (map next-gen (range c)))))))

(defcheck solution-8818b1b3
  (letfn
   [(f1 [w i j]
      (let [c (for [a [0 -1 1] b [0 -1 1]]
                (if (= \# (get-in w [(+ i a) (+ j b)])) 1 0))]
        (let [x (first c) y (apply + (rest c))]
          (if (= 0 x) (if (= 3 y) \# \space)
                      (if (or (< y 2) (> y 3)) \space
                                               \#)))))

    (f2 [w]
      (let [r (range 1 (dec (count w)))]
        (for [i r j r]
          [i j (f1 w i j)])))]

    (fn [w]
      (map #(apply str %)
        (reduce (fn [x [i j v]] (assoc-in x [i j] v))
          (vec (map vec w))
          (f2 w))))))

(defcheck solution-8887aac0
  (fn next-gen [b]
    (map clojure.string/join
      (let [board (vec (for [row b] (vec row))) rows (count board) cols (count (first board))]
        (loop [pos [0 0] result board]

          (if (>= (first pos) rows)
            result
            (let [
                  cell             (get-in b pos)
                  row              (first pos)
                  col              (last pos)
                  next-pos         [
                                    (if (= col (dec cols)) (inc row) row)
                                    (if (= col (dec cols)) 0 (inc col))
                                    ]
                  neighbour-coords [
                                    [(dec row) (dec col)]
                                    [(dec row) col]
                                    [(dec row) (inc col)]
                                    [row (dec col)]
                                    [row (inc col)]
                                    [(inc row) (dec col)]
                                    [(inc row) col]
                                    [(inc row) (inc col)]
                                    ]
                  alive-neighbours (reduce
                                     (fn [counter coord]
                                       (if (= \# (get-in board coord)) (inc counter) counter)
                                       )
                                     0 neighbour-coords)
                  ]

              (recur next-pos
                (assoc-in result pos
                  (cond
                    (and (= cell \space) (= alive-neighbours 3)) \#
                    (and (= cell \#) (< alive-neighbours 2)) \space
                    (and (= cell \#) (> alive-neighbours 3)) \space
                    :else
                    cell
                    )
                  )
                )
              )
            )
          )
        )
      )
    ))

(defcheck solution-88f58788
  (fn [board] (let [width          (count (first board))
                    height         (count board)
                    is-live?       (fn [row col bd] (= "#" (subs (bd row) col (inc col))))
                    num-neighbours (fn [row col w h bd]
                                     (count (filter true?
                                              (map
                                                (fn [k] (is-live? (first k) (second k) bd))
                                                (filter (fn [k] (not= k [row col]))
                                                  (for [r (range (max 0 (dec row)) (min (dec h) (+ row 2)))
                                                        c (range (max 0 (dec col)) (min (dec w) (+ col 2)))
                                                        ] [r c])
                                                  )
                                                )
                                              )
                                       )
                                     )
                    transition     (fn [row col bd]
                                     (let [n    (num-neighbours row col width height bd)
                                           live (is-live? row col bd)
                                           ]
                                       (if live
                                         (cond
                                           (< n 2) " "
                                           (> n 3) " "
                                           :else "#"
                                           )
                                         (if (= n 3) "#" " ")
                                         )
                                       )
                                     )
                    update-board   (fn [row col bd result]
                                     (assoc result row (str (subs (result row) 0 col) (transition row col bd) (subs (result row) (inc col))))
                                     )
                    ]
                (loop [result (reduce (fn [x y] (conj x (reduce (fn [a b] (str a " ")) "" (range width)))) [] (range height))
                       row    0 col 0
                       ]
                  (if (= row height) result
                                     (if (= col (dec width))
                                       (recur (update-board row col board result) (inc row) 0)
                                       (recur (update-board row col board result) row (inc col))
                                       )
                                     )
                  )
                )
    ))

(defcheck solution-8a08b835
  (fn [rows]
    (let [grid-size (count rows)]
      (letfn [(occupied? [i j]
                (cond
                  (or (< i 0) (>= i grid-size) (< j 0) (>= j grid-size)) false
                  (= \# (nth (nth rows i) j)) true
                  :else false))
              (count-neighbours [i j]
                (count (for [x [(dec i) i (inc i)]
                             y [(dec j) j (inc j)]
                             :when (or (not= x i) (not= y j))
                             :when (occupied? x y)]
                         true)))
              (alive? [i j]
                (let [neighbours (count-neighbours i j)]
                  (or (= neighbours 3) (and (= neighbours 2) (occupied? i j)))))]
        (let [next-step (for [i (range 0 grid-size)
                              j (range 0 grid-size)]
                          (alive? i j))]
          (->> next-step
            (map #(if % \# \space))
            (partition grid-size)
            (map (partial apply str))
            (vec)
            ))))))

(defcheck solution-8a36f457
  (fn [board]
    (let [cell-at    #(nth (nth board %1) %2)
          live?      (fn [[i j]]
                       (= (cell-at i j) \#))
          valid-idx? #(and
                       (< % (count board))
                       (>= % 0))
          neighbor-idx
                     #(for [i (range (dec %1) (+ 2 %1))
                            j (range (dec %2) (+ 2 %2))
                            :when (and (valid-idx? i)
                                       (valid-idx? j)
                                       (not (and (= i %1) (= j %2))))]
                        [i j])
          live-neighbors
                     #(count (filter live? (neighbor-idx %1 %2)))
          next-gen
                     (fn [i j]
                       (let [alive  (live? [i j])
                             living (live-neighbors i j)]
                         (if alive
                           (cond
                             (< living 2) \space
                             (> living 3) \space
                             :else \#)
                           (if (= living 3)
                             \#
                             \space))))]
      (map-indexed
        (fn [i row] (apply str (map-indexed
                                 (fn [j _] #_(println i j) (next-gen i j))
                                 row)))
        board))))

(defcheck solution-8a7ee87d
  (fn [b]
    (letfn [(g [x y] (nth (nth b y "") x \ ))]
      (for [y (range (count b))]
        (apply str
          (for [x (range (count (first b)))]
            (let [x- (dec x) x+ (inc x)
                  y- (dec y) y+ (inc y)
                  n  (count ((group-by identity
                               [(g x- y-) (g x y-) (g x+ y-)
                                (g x- y) (g x+ y)
                                (g x- y+) (g x y+) (g x+ y+)]) \#))]
              (if (= \  (g x y))
                (if (= 3 n) \# \ )
                (if (or (< n 2) (< 3 n)) \  \#)))))))))

(defcheck solution-8a92be70
  (fn [board]
    (let [expanded        (map #(str " " % " ") (cons "      " (conj board "      "))) ; expand the board by one to have the same number of neighbour cells
          neighbour-cells (map #(map (fn [x] (partition 3 1 x)) %) (partition 3 1 expanded)) ; partition the board into 3*3 cells
          neighbour-count (map #(apply map (fn [& cells] ((frequencies (flatten (vector cells))) \# 0)) %) neighbour-cells) ; number of neighbours
          cell-neighbours (map #(partition 2 %) (map interleave board neighbour-count)) ; each cell with its neighbours => (((\space 1) (\# 4) ...
          gol             (fn [[cell neighbours]]
                            (cond                           ; a live cell is also counted in the neighbours, so neighbour counts are off by one for live cells
                              (and (= cell \space) (= neighbours 3)) "#" ; new life by reproduction
                              (and (= cell \#) (< neighbours 3)) " " ; death by under-population
                              (and (= cell \#) (> neighbours 4)) " " ; death by over-population
                              (= cell \#) "#"               ; life goes on
                              :else " "))]
      (map #(apply str (map gol %)) cell-neighbours))))

(defcheck solution-8aa63f9
  (fn [m]
    (let [get   #(nth (nth %3 %1 nil) %2 \space)
          rg    #(range (dec %) (+ 2 %))
          neigh #(for [x (rg %1) y (rg %2)
                       :when (and (= \# (get x y %3)) (or (not= x %1) (not= y %2)))]
                   [x y])
          X     (count m)
          Y     (count (first m))]
      (map #(apply str %) (for [x (range X)]
                            (for [y (range Y) :let [e (get x y m) c (count (neigh x y m))]]
                              (if (= e \#)
                                (cond
                                  (< c 2) \space
                                  (> c 3) \space
                                  :else \#)
                                (if (= c 3) \# \space))))))))

(defcheck solution-8acc31fb
  (fn [c]
    (let [lc   (fn lc
                 [s x y]
                 (if (empty? s)
                   []
                   (if (= \# (first s))
                     (conj (lc (rest s) (inc x) y)
                       {:t 1 :x x :y y})
                     (conj (lc (rest s) (inc x) y)
                       {:t 2 :x x :y y}))))

          ln   (fn [c]
                 (mapcat #(lc %1 0 %2)
                   c
                   (range 0 (count c))))

          nbor (fn [pl mp]
                 (let [x (:x mp) y (:y mp)]
                   (->> (filter #(or (= (inc x) (:x %))
                                     (= (dec x) (:x %))
                                     (= x (:x %))) pl)
                     (filter #(or (= (inc y) (:y %))
                                  (= (dec y) (:y %))
                                  (= y (:y %))))
                     (remove #(= % mp)))))
          sts  (fn [pl mp]
                 (let [nb    (nbor pl mp)
                       lives (filter #(= 1 (:t %)) nb)
                       dies  (filter #(= 2 (:t %)) nb)]
                   (if (= 1 (:t mp))
                     (cond (< (count lives) 2) :die
                           (<= 2 (count lives) 3) :live
                           :else :die)
                     (if (= 3 (count lives))
                       :live
                       :die))))
          ts   (fn [pl i]
                 (loop [y 0 rs []]
                   (if (= i y)
                     rs
                     (recur (inc y)
                       (conj rs (apply str (map #(if (= (:t %) 1)
                                                   "#"
                                                   " ")
                                             (sort-by :x (filter #(= y (:y %)) pl)))))))))
          pl   (ln c)]
      (ts (loop [p pl rs []]
            (if (empty? p)
              rs
              (recur (rest p)
                (let [mp (first p)]
                  (if (= :die (sts pl mp))
                    (conj rs (assoc mp :t 2))
                    (conj rs (assoc mp :t 1)))))))
        (count c)))))

(defcheck solution-8afd3af6
  (fn [c]
    (let [l          (count c)
          alive?     (fn [x y] (= \# (get-in c [x y])))
          neighbours (fn [x y]
                       (reduce #(if %2 (inc %) %) 0 (for [i [-1 0 1] j [-1 0 1] :when (not= i j 0)]
                                                      (alive? (+ x i) (+ y j)))))
          son        (fn [x y n] (if (or (= n 3) (and (= n 2) (alive? x y))) \# " "))]
      (for [i (range l)]
        (apply str (for [j (range l)]
                     (son i j (neighbours i j))))))))

(defcheck solution-8b6c6a8c
  (fn [board]
    (letfn
     [(deltas []
        (for [y (range -1 2) x (range -1 2) :when (not= [y x] [0 0])] [y x]))

      (neighbour-coords
        ([size yx] (neighbour-coords (deltas) size yx))
        ([deltas size yx]
         (filter
           (fn [new-yx] (every? #(< -1 % size) new-yx))
           (map #(vec (map + yx %)) deltas))))

      (num-neighbours
        [board yx]
        (->> (neighbour-coords (count board) yx)
          (map (partial get-in board))
          (filter (partial = \#))
          (count)))

      (next-state
        [board yx]
        (let [alive (= (get-in board yx) \#)
              num   (num-neighbours board yx)]
          (if alive
            (if (<= 2 num 3) "#" " ")
            (if (= num 3) "#" " "))))

      (life
        [board]
        (let [size        (count board)
              next-states (for [x (range 0 size) y (range 0 size)]
                            (next-state board [x y]))
              next-rows   (partition size next-states)]
          (map #(apply str %) next-rows)))]

      (life board))))

(defcheck solution-8b97a3c7
  (fn [m]
    (letfn [(n [m i j]
              (for [v [-1 0 1]
                    h [-1 0 1]
                    :let [ni (+ i v)
                          nj (+ j h)]
                    :when (and (not= [ni nj] [i j])
                               (pos? (inc ni))
                               (pos? (inc nj))
                               (< ni (count m))
                               (< nj (count (first m)))
                               (= \# (get-in m [ni nj])))]
                \#))]
      (map-indexed
        (fn [i row]
          (apply str
            (map-indexed
              (fn [j v]
                (let [n (count (n m i j))]
                  (if (= \# v)
                    (cond
                      (< n 2) \space
                      (> n 3) \space
                      :else v)
                    (if (= n 3) \# v))))
              row))) m))))

(defcheck solution-8d020fca
  (fn [lf] (
             letfn [
                    (g [x y] (
                               if (and (>= x 0) (>= y 0) (< x (count (first lf))) (< y (count lf)))
                               (nth (nth lf y) x)
                               " "
                               ))
                    (c [x y] (count (filter #(= % \#) (map #(g (+ x (first %)) (+ y (last %))) [[-1 -1] [0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0]]))))
                    ]

             (map #(apply str %) (map-indexed (fn [y v] (
                                                          map-indexed (fn [x t] (
                                                                                 #(
                                                                                    if (= %2 \#)
                                                                                    (if (or (< % 2) (> % 3))
                                                                                      " "
                                                                                      "#"
                                                                                      )
                                                                                    (if (= % 3)
                                                                                      "#"
                                                                                      " "
                                                                                      )

                                                                                    ) (c x y) (g x y)
                                                                                 )) v

                                                          )) lf))

             )))

(defcheck solution-8d61bf7c
  (fn [board]
    (let [n   (count board) m (count (first board))
          add [[0 1] [0 -1] [1 0] [-1 0] [-1 1] [-1 -1] [1 1] [1 -1]]]
      (letfn [(in [[x y]] (and (>= x 0) (< x n) (>= y 0) (< y m)))
              (getV [[x y]] ((vec (board x)) y))
              (cn [pair]
                (count (filter #(= \# (getV %)) (filter in (map #(vec (map + pair %)) add)))))]
        (map-indexed (fn [x row] (apply str
                                   (map-indexed (fn [y v] (let [now (cn [x y])]
                                                            (if (= \# v)
                                                              (cond (= 2 now) \#
                                                                    (= 3 now) \#
                                                                    :else \space)
                                                              (cond (= 3 now) \#
                                                                    :else \space)))) row))) board)))))

(defcheck solution-8d6b40af
  (fn [board]
    (letfn [(remap [board]
              (->> (for [v board]
                     (->> (map (fn [c] (if (= (str c) " ") 0 1)) v)
                       (apply vector)))
                (apply vector)))
            (unmap [matrix]
              (->> (for [v matrix]
                     (->> (map (fn [b] (if (zero? b) " " "#")) v)
                       (apply str)))
                (apply vector)))
            (cneigh [matrix x y]
              (let [n     (count matrix)
                    left  (mod (dec x) n)
                    right (mod (inc x) n)
                    up    (mod (dec y) n)
                    down  (mod (inc y) n)]
                (->>
                  (for [[a b] [[left up] [x up] [right up]
                               [left y] [right y]
                               [left down] [x down] [right down]]]
                    ((matrix a) b))
                  (reduce +))))
            (nextgen [matrix]
              (let [n (count matrix)]
                (->>
                  (for [x (range n)]
                    (->>
                      (for [y (range n)]
                        (let [neighbors (cneigh matrix x y)]
                          (if (zero? ((matrix x) y))
                            (if (= neighbors 3) 1 0)
                            (cond
                              (or (= neighbors 2) (= neighbors 3)) 1
                              :else 0))))
                      (apply vector)))
                  (apply vector))))]
      (->> board (remap) (nextgen) (unmap)))))

(defcheck solution-8db77876
  (fn [rs]
    (let [r   [-1 0 1]
          g   #(get-in rs [% %2])
          h   #({\# 1} (g %2 %) 0)
          trs (for [y (range (count rs))]
                (for [x (range (count (rs 0)))]
                  [(g x y)
                   (apply + (for [j r, k r]
                              (h (+ x j) (+ y k))))]))
          f   (fn [[c s]] (if (or (= s 3) (and (= c \#) (= s 4))) "#" " "))]
      (map #(apply str (map f %)) trs))))

(defcheck solution-8dd7137a
  (fn [d]
    (for [i (range (count d))]
      (apply
        str
        (for [j (range (count (d i)))]
          (let [z (= \# (get-in d [i j]))
                v [-1 0 1]
                u (count (filter #(= \# (get-in d %)) (for [a v, b v] [(+ i a) (+ j b)])))]
            (if (or (== 3 u) (and z (== 4 u))) \# " ")))))))

(defcheck solution-8e1188c6
  (fn [prob] (let [alive             (first "#")
                   dead              (first " ")
                   life-or-death?    (fn [[x y] game] (nth (nth game y) x))
                   offsets           [-1 0 1]
                   neighbours        (fn [[x y] game] (->> (for [xo offsets]
                                                             (for [yo offsets]
                                                               [(+ x xo) (+ y yo)]))
                                                        identity
                                                        (apply concat)
                                                        (filter (fn [pair] (and (every? #(<= 0 % (dec (count game))) pair) (not= [x y] pair))))))
                   living-neighbours (fn [[x y] prob] (->> (neighbours [x y] prob)
                                                        (map #(life-or-death? % prob))
                                                        (filter #(= alive %))
                                                        count))
                   live-or-die?      (fn [[x y] prob]
                                       (let [neigh-count (living-neighbours [x y] prob)]
                                         (if (= alive (life-or-death? [x y] prob))
                                           (if (contains? #{2 3} neigh-count) alive dead)
                                           (if (= 3 neigh-count) alive dead))))]
               (->> (for [y (range (count prob))]
                      (for [x (range (count (first prob)))]
                        (live-or-die? [x y] prob)))
                 (map #(apply str %))))))

(defcheck solution-8ed08c69
  (fn __ [board]
    (let [b (apply str board) dims (count board)]
      (letfn [(live-neighbors [n]
                (let [W  (when (and (> (dec n) -1) (not= 0 (mod n dims)))
                           (dec n))
                      E  (when (and (< (inc n) (* dims dims)) (not= (dec dims) (mod n dims)))
                           (inc n))
                      N  (when (> (- n dims) -1)
                           (- n dims))
                      S  (when (< (+ n dims) (* dims dims))
                           (+ n dims))
                      NW (when (and N (> (dec N) -1) (not= 0 (mod N dims)))
                           (dec N))
                      NE (when (and N (< (inc N) (* dims dims)) (not= (dec dims) (mod N dims)))
                           (inc N))
                      SW (when (and S (> (dec S) -1) (not= 0 (mod S dims)))
                           (dec S))
                      SE (when (and S (< (inc S) (* dims dims)) (not= (dec dims) (mod S dims)))
                           (inc S))
                      ]
                  (count
                    (filter (partial = \#) (map #(nth b %)
                                             (filter identity [N E W S NW NE SW SE]))))))
              (step [idx v]
                (cond
                  ; reproduction
                  (and (= \space v)
                       (= 3 (live-neighbors idx))) \#
                  ; overcrowding
                  (and (= \# v)
                       (< 3 (live-neighbors idx))) \space
                  ; next gen
                  (and (= \# v)
                       (or (= 2 (live-neighbors idx)) (= 3 (live-neighbors idx)))) \#
                  ; underpopulation
                  (and (= \# v)
                       (> 2 (live-neighbors idx))) \space
                  :else \space
                  ))]
        (vec (map (partial apply str)
               (partition-all dims (map-indexed step b))))
        ))))

(defcheck solution-8f250de3
  (fn [board]
    (letfn [(cart-prod [l1 l2]
              (apply concat (map (fn [a]
                                   (map (fn [b] [a b]) l2)) l1)))
            (get-cell [i j board]
              (if (and (every? pos? [i j]) (< i (count board)) (< j (count (nth board i))))
                (nth (nth board i) j)))
            (is-hash [c]
              (= "#" (str c)))]
      (map (fn [lst] (reduce #(.concat %1 %2) lst))
        (map-indexed (fn [idx row]
                       (map-indexed (fn [i el]
                                      (let [live-neighbors (count (filter is-hash (map (fn [[x y]]

                                                                                         (get-cell (+ x idx) (+ y i) board))
                                                                                    (remove #(= [0 0] %) (cart-prod (range -1 2) (range -1 2))))))]
                                        (if (= (str el) "#")
                                          (if (or (= 2 live-neighbors) (= 3 live-neighbors))
                                            "#"
                                            " ")
                                          (if (= 3 live-neighbors) "#" " ")))) row)) board)))))

(defcheck solution-8f899c58
  (fn [board]
    (letfn [(cell [r c] (get-in board [r c] \space))
            (neighbors [r c]
              (for [dr [-1 0 1] dc [-1 0 1] :when (not (= 0 dr dc))]
                (cell (+ r dr) (+ c dc))))
            (info [r c] [(cell r c) (count (filter #{\#} (neighbors r c)))])
            (update [[cur ct]] (cond (= ct 2) cur (= ct 3) \# :else \space))]
      (map #(apply str %)
        (map-indexed #(map-indexed (fn [c _] (update (info %1 c))) %2) board)))))

(defcheck solution-90ac6af5
  (fn lifeX [t]
    (letfn
     [
      (cell [t y x]
        (get (get t y) x)
        )

      (alive [t y x]
        (= (cell t y x) \#)
        )

      (alive01 [t y x]
        (if (alive t y x)
          1
          0
          )
        )

      (aliveNeighbours [t y x]
        (+
          (alive01 t (dec y) (dec x))
          (alive01 t (dec y) x)
          (alive01 t (dec y) (inc x))
          (alive01 t y (dec x))
          (alive01 t y (inc x))
          (alive01 t (inc y) (dec x))
          (alive01 t (inc y) x)
          (alive01 t (inc y) (inc x))
          )
        )

      (toCell [t y x]
        (let
         [
          alivexy         (alive t y x)
          countNeighbours (aliveNeighbours t y x)
          ]
          (cond
            (and alivexy (< countNeighbours 2)) \space
            (and alivexy (or (= countNeighbours 2) (= countNeighbours 3))) \#
            (and alivexy (> countNeighbours 3)) \space
            (and (not alivexy) (= countNeighbours 3)) \#
            :else (cell t y x)
            )
          )
        )

      (toRow [t y]
        (let [columns (count (get t y))]
          (apply str
            (map (partial toCell t y) (range columns))
            )
          )
        )

      ]

      (let [rows (count t)]
        (map (partial toRow t) (range rows))
        )
      )
    ))

(defcheck solution-90e90575
  (fn [B]
    (let [N (count B)
          M (count (first B))
          O (fn [i j] (for [di [-1 0 1] dj [-1 0 1] :when (not= 0 di dj)] [(+ i di) (+ j dj)]))
          L (fn [i j] (count (filter #{\#} (map (partial get-in B) (O i j)))))]
      (for [i (range N)]
        (apply str (for [j (range M)] (condp = (L i j) 2 (get-in B [i j]) 3 \# \space)))))))

(defcheck solution-90e96d0f
  (fn self [in-board]
    (let [board                (mapv vec in-board)
          neighbor-rel-indices [[-1 1] [0 1] [1 1]
                                [-1 0] [1 0]
                                [-1 -1] [0 -1] [1 -1]]
          fate                 (fn [v neighor-type-counts]
                                 (case v
                                   \# (cond
                                        (#{0 1} (neighor-type-counts \#)) \space
                                        (#{2 3} (neighor-type-counts \#)) \#
                                        (> 3 (neighor-type-counts \#)) \space
                                        :else \space        ; no live neighbors
                                        )
                                   \space (if (= 3 (neighor-type-counts \#))
                                            \#
                                            \space)))
          next-board           (fn [b] (for [x (range (count (b 0)))
                                             y (range (count b))
                                             :let [v (get-in b [x y])]
                                             :let [neighbor-indices (mapv (fn [[i j]] [(+ x i) (+ y j)]) neighbor-rel-indices)]
                                             :let [neighbor-values (filterv (comp not nil?) (mapv (fn [i_j] (get-in b i_j)) neighbor-indices))]
                                             :let [neighor-type-counts (frequencies neighbor-values)]]
                                         (fate v neighor-type-counts)
                                         ))]
      (mapv (fn [x] (apply str x)) (partition (count board) (next-board board)))
      )))

(defcheck solution-9123e60f
  (fn __ [matrix]
    (let [nrow       (count matrix)
          ncol       (count (first matrix))
          nbor       (fn [x y dx dy]
                       (let [x' (+ x dx)
                             y' (+ y dy)]
                         (if (and (< -1 x' ncol)
                                  (< -1 y' nrow))
                           (cond
                             (= dx dy 0) 0
                             (= \# (get-in matrix [y' x'])) 1
                             :else 0)
                           0)))
          nbors      (fn [x y]
                       (apply + (for [dx [-1 0 1]
                                      dy [-1 0 1]]
                                  (nbor x y dx dy))))
          next-state (fn [state nb-count]
                       (cond
                         (= nb-count 3) true                ; rule #4
                         (< nb-count 2) false               ; rule #1
                         (< nb-count 4) state               ; rule #2
                         :else false))]                     ; rule #3
      (for [y (range nrow)]
        (apply str
          (for [x (range ncol)]
            (let [cur-state (= \# (get-in matrix [y x]))
                  nb-count  (nbors x y)]
              (if (next-state cur-state nb-count) \# \space))))))))

(defcheck solution-920331a1
  (fn [b]
    (let [
          mki
             #(map-indexed
                (fn [c row]
                  (keep-indexed
                    (fn [r v] (% c r v))
                    row))
                b)
          ac
             #(apply concat %)
          d  [-1 0 1]
          ds (for [x d, y d :when (not= x y 0)] [x y])
          neighb
             (frequencies
               (ac
                 (ac
                   (mki
                     (fn [c r v]
                       (when (= v \#)
                         (map #(map + [c r] %) ds)))))))
          ]
      (map
        #(apply str %)
        (mki
          (fn [c r v]
            (#(get {2 % 3 \#} %2 \ )
             v
             (neighb [c r]))))))))

(defcheck solution-921a4d4a
  (fn [v]
    (letfn ((r [v n a]
              (vec `(~@(take n v) ~a ~@(drop (+ n 1) v))))
            (g [b x y]
              (if (and (some #(= % y) (range (count b)))
                       (some #(= % x) (range (count (first b)))))
                (nth (nth b y) x)
                \space))
            (s [b x y v]
              (r b y (r (nth b y) x v)))
            (c [b x y]
              (reduce (fn [s a]
                        (cond (and (= (a 0) 0) (= (a 1) 0))
                              s
                              (= (g b (+ x (a 1)) (+ y (a 0))) \#)
                              (+ s 1)
                              :else
                              s))
                0
                (for [dy '(-1 0 1)
                      dx '(-1 0 1)]
                  [dy dx]))))
      (let [b (vec (map (fn [s]
                          (vec s))
                     v))]
        (vec (map #(apply str %)
               (reduce (fn [r a]
                         (let [y (a 1)
                               x (a 0)
                               c (c b x y)]
                           (cond (and (= (g b x y) \space)
                                      (= c 3))
                                 (s r x y \#)
                                 (= (g b x y) \space)
                                 r
                                 (< c 2)
                                 (s r x y \space)
                                 (> c 3)
                                 (s r x y \space)
                                 :else
                                 r)))
                 b
                 (for [y (range (count b))
                       x (range (count (first b)))]
                   [y x]))))))))

(defcheck solution-923defaf
  (fn step [board]
    (letfn [(s3 [arr] (map (partial reduce +)
                        (partition 3 1 (concat [0] arr [0]))))
            (life [o1 o9] (if (or (and (= 0 o1) (= 3 o9))
                                  (and (= 1 o1) (#{3 4} o9))) 1 0))
            (num->str [board] (for [row board] (apply str (map [" " "#"] row))))]
      (let [nboard (for [row board] (map {\# 1 \space 0} row))]
        (->> (map s3 nboard)
          (apply map vector)
          (map s3)
          (apply mapcat vector)
          (map life (flatten nboard))
          (partition (count (first board)))
          num->str)))))

(defcheck solution-92c1833
  (fn [coll]
    (letfn [(live-cells [coll]
              (->> coll
                (map (fn [x] (map-indexed #(when (= %2 \#) %) x)))
                (map #(remove nil? %))
                (map-indexed (fn [idx itm] (when itm (for [x itm] [idx x]))))
                (mapcat #(map flatten %))
                (map vec)
                set))
            (neighbours [[x y]]
              (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
                [(+ dx x) (+ dy y)]))
            (step [cells]
              (set (for [[loc n] (frequencies (mapcat neighbours cells))
                         :when (or (= n 3) (and (= n 2) (cells loc)))]
                     loc)))
            (empty-board [w h]
              (vec (repeat w (vec (repeat h \space)))))
            (populate [board living-cells]
              (reduce (fn [board coordinates]
                        (assoc-in board coordinates \#))
                board
                living-cells))
            ]
      (->> coll
        live-cells
        step
        (populate (empty-board (count coll) (count coll)))
        (map #(apply str %))))))

(defcheck solution-92e4648b
  (fn [world]
    (let [height (count world)
          width  (count (first world))]
      (map (partial apply str)
        (reduce (fn [matrix [y x :as coords]]
                  (let [live-neighbours
                        (count (filter #{\#}
                                 (map #(get-in world %)
                                   (for [dy [-1 0 1] dx [-1 0 1] :when (and (not= 0 dx dy)
                                                                            ((set (range height)) (+ y dy))
                                                                            ((set (range width)) (+ x dx)))]
                                     [(+ y dy) (+ x dx)]))))]
                    (assoc-in matrix
                      coords
                      (if (= \space (get-in world coords))
                        (if (= 3 live-neighbours) \# \space)
                        (if (or (< live-neighbours 2)
                                (> live-neighbours 3))
                          \space
                          \#))))
                  )
          (vec (map vec world))
          (for [y (range height) x (range width)] [y x]))))))

(defcheck solution-9306190
  (fn game-of-life [board-strs]
    (let
     [board          (vec (map #(vec %) board-strs))
      height         (count board)
      width          (count (first board))
      on-board?      (fn [[i j]]
                       (and (>= i 0) (>= j 0) (< i height) (< j width)))
      neighbors      (fn [[i j]]
                       (for [di [-1 0 1]
                             dj [-1 0 1]
                             :let [newi (+ i di)
                                   newj (+ j dj)]
                             :when (and (or (not= 0 di) (not= 0 dj))
                                        on-board? [newi newj])]
                         [newi newj]))
      live-neighbors (fn [square]
                       (count
                         (filter #(= \# (get-in board %)) (neighbors square))))
      alive?         (fn [square] (= \# (get-in board square)))

      next-gen       (fn [square]
                       (let
                        [is-alive?          (alive? square)
                         num-live-neighbors (live-neighbors square)]
                         (if is-alive?
                           (cond
                             (< num-live-neighbors 2) \space
                             (<= num-live-neighbors 3) \#
                             :else \space)
                           (if (= 3 num-live-neighbors) \# \space))))
      board-coords   (vec (for [i (range height)]
                            (vec (for [j (range width)] [i j]))))]
      (map (fn [row]
             (apply str (map next-gen row))
             ) board-coords))))

(defcheck solution-93b58fa8
  (fn [coll]
    (letfn [(nbs [x y]
              (->> [[0 1] [0 -1] [1 0] [-1 0] [1 1] [-1 -1] [-1 1] [1 -1]]
                (map (fn [[a b]] [(+ x a) (+ y b)]))
                (filter (fn [[a b]] (and (< -1 a (count coll)) (< -1 b (count coll)))))
                (map (fn [[a b]] (nth (nth coll a) b)))))]
      (map-indexed
        (fn [x a]
          (apply str (map-indexed
                       (fn [y b]
                         (let [dead    \space
                               live    \#
                               ns      (nbs x y)
                               live-ns (count (filter #(= live %) ns))
                               dead-ns (- (count ns) live-ns)]
                           (if (= live b)
                             (cond (< live-ns 2) dead
                                   (<= 2 live-ns 3) live
                                   :else dead)
                             (cond (= live-ns 3) live
                                   :else dead))))
                       a)))
        coll))))

(defcheck solution-93f9ad82
  (fn [ls] (letfn
            [(ij [x y] (nth (nth ls x ()) y nil))
             (mp [i j] (get (frequencies
                              [(ij (- i 1) (- j 1)) (ij (- i 1) j)
                               (ij (- i 1) (+ j 1))
                               (ij i (- j 1)) (ij i (+ j 1))
                               (ij (+ i 1) (- j 1)) (ij (+ i 1) j)
                               (ij (+ i 1) (+ j 1))]) \# 0))
             (f [m n] (cond
                        (= (ij m n) \space)
                        (if (= 3 (mp m n)) \# \space)
                        :else
                        (cond
                          (< (mp m n) 2) \space
                          (or (= 2 (mp m n)) (= 3 (mp m n)))
                          \#
                          (> (mp m n) 3) \space)))]
             (map #(apply str %)
               (partition (count ls)
                 (for [p (range (count ls))
                       q (range (count ls))]
                   (f p q))))
             )))

(defcheck solution-9455fe6d
  (fn [board]
    (let [width  (count (first board)) height (count board)
          states (map (fn [c] (if (= c \#) 1 0)) (apply concat board))]
      (letfn [(split-every [n l]
                (if (empty? l)
                  nil
                  (let [[x y] (split-at n l)]
                    (cons x
                      (split-every n y)
                      ))))
              (gen-new-board [new-states]
                (vec
                  (map
                    (fn [row] (apply str
                                (map (fn [c] (if (= c 1) \# \space)) row)))
                    (split-every width new-states))))
              (index-to-xy [i]
                [(quot i width) (rem i width)])
              (xy-to-index [i]
                (+ (* (first i) width) (second i)))
              (valid-xy [i]
                (and (<= 0 (second i) (dec width))
                     (<= 0 (first i) (dec height))))
              (live-neighbor-count-of-cell [n]
                (let [[x y] (index-to-xy n)]
                  (apply + (map (fn [z] (nth states (xy-to-index z)))
                             (filter valid-xy (list [(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
                                                [x (dec y)] [x (inc y)]
                                                [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]))))))
              ]
        (gen-new-board (map-indexed (fn [i o]
                                      (let [c (live-neighbor-count-of-cell i)]
                                        (if (or (and (= o 1) (<= 2 c 3))
                                                (and (= o 0) (= c 3)))
                                          1 0))) states))))))

(defcheck solution-94bd3a5
  (fn [b]
    (let [ct (fn [p] (count (filter #(= (get-in b (map + p %)) \#)
                              [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])))]
      (map-indexed (fn [y r]
                     (apply str (map-indexed
                                  (fn [x o]
                                    (let [n (ct [y x])]
                                      (if (or (= n 3) (and (= n 2) (= o \#)))
                                        \# \ )))
                                  r)))
        b))))

(defcheck solution-94d22fb3
  (fn conway [world]
    (letfn [
            (live? [x y]
              (= \# (nth (nth world x) y)))

            (nbrs [x y]
              (apply +
                (for [a [-1 0 1], b [-1 0 1]]
                  (let [i (+ a x) j (+ b y)]
                    (if (and (>= i 0)
                             (< i (count world))
                             (>= j 0)
                             (< j (count (first world)))
                             (not= [i j] [x y])
                             (live? i j))
                      1 0)))))
            (nxt [] (
                      map #(apply str %) (partition (count world)

                                           (for [a (range (count world)) b (range (count (first world)))]
                                             (let [N (nbrs a b) l (live? a b)]
                                               (cond
                                                 (and l (< N 2)) " "
                                                 (and l (or (= N 2) (= N 3))) "#"
                                                 (and l (> N 3)) " "
                                                 (and (not l) (= N 3)) "#"
                                                 :default " "))))))
            ]
      (nxt))))

(defcheck solution-94e0abf4
  (fn [b] (let [l      (dec (count b))
                b'     (vec (map vec b))
                newval (fn [a [i j]] (let [ns   (set (for [x [-1 0 1] y [-1 0 1]]
                                                       [(min l (max (+ x i) 0)) (min l (max 0 (+ y j)))]))
                                           live (count (filter (fn [[x y]] (= \# ((a x) y))) ns))]
                                       (if (or (and (= \# ((a i) j)) (#{3 4} live))
                                               (and (= \space ((a i) j)) (= 3 live))) \#
                                                                                      \space)))
                upd    (fn [a [i j]] (assoc a i (assoc (a i) j (newval b' [i j]))))]
            (map #(apply str %) (reduce upd b' (for [i (range l) j (range l)] [i j]))))))

(defcheck solution-95409744
  (fn [b]
    (let [bx (dec (count (first b))) by (dec (count b))]
      (letfn [(get-next [x y] (if (or (or (< x 0) (< y 0)) (or (> x bx) (> y by))) nil (if (= (nth (nth b y) x) \#) \# nil)))
              (neighbours [x y] (count (filter #(= % \#) (list (get-next (dec x) (dec y)) (get-next x (dec y))
                                                           (get-next (inc x) (dec y)) (get-next (dec x) y)
                                                           (get-next (inc x) y) (get-next (dec x) (inc y))
                                                           (get-next x (inc y)) (get-next (inc x) (inc y))))))
              (rep [x y]
                (cond (and (= (nth (nth b y) x) \space)
                           (= (neighbours x y) 3)) \#
                      (and (= (nth (nth b y) x) \#)
                           (or (< (neighbours x y) 2)
                               (> (neighbours x y) 3))) \space
                      :else (nth (nth b y) x)))]
        (map #(apply str %) (partition (count (first b))
                              (loop [x 0 y 0 res []]
                                (if (= y (count b)) res
                                                    (recur (if (= x bx) 0 (inc x))
                                                      (if (= x bx) (inc y) y)
                                                      (conj res (rep x y)))))))))))

(defcheck solution-95f286a6
  (fn [v]
    (letfn [(nc [x y w h]
              (filter
                #(and (>= (% 0) 0) (>= (% 1) 0) (< (% 0) w) (< (% 1) h) (not (= % [x y])))
                (for [i (range (dec x) (+ x 2)) j (range (dec y) (+ y 2))] [i j])))
            (n [x y] (count (filter #(= % \#) (map #(nth (nth v (% 1)) (% 0)) (nc x y (count (first v)) (count v))))))]
      (for [y (range 0 (count v))]
        (apply str (for [x (range 0 (count v))]
                     (let [c (nth (nth v y) x) ne (n x y)]
                       (if (= ne 3) \#
                                    (if (and (= ne 2) (= c \#)) \# \space)))))))))

(defcheck solution-95f7ba16
  (fn [xs]
    (let [w   (count (first xs))
          s   \space
          r0  (apply str (repeat (+ 2 w) s))
          xs' (concat [r0] (map #(str s % s) xs) [r0])
          bs  (for [rs (partition 3 1 xs')
                    cs (partition 3 1 (apply map str rs))]
                (apply str cs))
          c'  (fn [b]
                (let [a (= \# (first (drop 4 b)))
                      c (count (filter #{\#} b))]
                  (or (and a (<= 3 c 4)) (and (not a) (= 3 c)))))
          cs' (map #(if (c' %) \# s) bs)
          ]
      (map #(apply str %) (partition w cs')))))

(defcheck solution-960aab7
  (fn gol-next-generation [board]
    (let [live-cell \#
          dead-cell \space
          height    (count board)
          width     (count (first board))]
      (letfn [(neighbors [[x y]]
                (for [dx [-1 0 1]
                      dy (if (zero? dx) [-1 1] [-1 0 1])]
                  [(+ x dx) (+ y dy)]))
              (next-state [cells]
                (set (for [[cell n] (frequencies (mapcat neighbors cells))
                           :when (or (= n 3)
                                     (and (= n 2) (cells cell)))]
                       cell)))
              (generate-board [alive]
                (mapv #(apply str %)
                  (partition width
                    (for [y (range height)
                          x (range width)
                          :let [char (if (alive [x y]) live-cell dead-cell)]]
                      char))))]
        (let [live-cells (set (for [y (range height)
                                    x (range width)
                                    :when (= live-cell (get-in board [y x]))]
                                [x y]))]
          (generate-board (next-state live-cells)))))))

(defcheck solution-973ed224
  (fn [board]
    (let [m (count board)
          n (count (first board))
          s \space]
      (map #(apply str %) (partition m m (for [i (range m)
                                               j (range n)
                                               :let [neighbours (for [k [-1 0 1]
                                                                      l [-1 0 1]
                                                                      :when (not= [k l] [0 0])]
                                                                  (get-in board [(+ i k) (+ j l)]))
                                                     alive      (count (filter #{\#} neighbours))
                                                     cell       (= \# (get-in board [i j]))]]
                                           (cond
                                             (or (zero? i)
                                                 (zero? j)
                                                 (= i (dec m))
                                                 (= j (dec n))) s ; border
                                             (and cell (< alive 2)) s ; under-population
                                             (and cell (> alive 3)) s ; overcrowding
                                             cell "#"       ; lives on
                                             (= 3 alive) "#" ; reproduction
                                             :else s        ; stays dead
                                             )))))))

(defcheck solution-9783c34c
  (fn [in]
    (let [p #{[0 1] [0 -1] [1 0] [-1 0] [1 1] [1 -1] [-1 1] [-1 -1]}
          s (count (first in))
          n (fn [[x y]]
              (filter
                (fn [[a b]] (and (< -1 a s) (< -1 b s)))
                (map (fn [[a b]] [(+ a x) (+ b y)]) p)))]
      (for [x (range s)]
        (apply
          str
          (for [y (range s)]
            (let [c ((frequencies (map #(get-in in %) (n [x y]))) \#)]
              (if (= (get-in in [x y]) \space)
                (if (= c 3)
                  \#
                  \space)
                (cond
                  (> c 3) \space
                  (> c 1) \#
                  :else \space)))))))))

(defcheck solution-979f456e
  (fn [board]
    (let [
          width     (count (first board))
          height    (count board)
          cell      (fn [x y] (if (or (< x 0) (< y 0) (>= x width) (>= y height)) \space (nth (nth board y) x)))
          alive?    #(= \# %)
          dead?     #(not (alive? %))
          neighbors (fn [x y]
                      (count
                        (filter alive?
                          [(cell (dec x) (dec y)) (cell x (dec y)) (cell (inc x) (dec y))
                           (cell (dec x) y) (cell (inc x) y)
                           (cell (dec x) (inc y)) (cell x (inc y)) (cell (inc x) (inc y))])))
          result
                    (for [y (range 0 height) x (range 0 width)]
                      (let [c (cell x y)
                            n (neighbors x y)]
                        (if (dead? c)
                          (if (= n 3) \# \space)
                          (if (or (= n 2) (= n 3)) \# \space))))]
      (map #(apply str %) (partition width result)))))

(defcheck solution-98811b1b
  (fn [t]
    (let [w (count (first t)) h (count t)]
      (let [m [[0 1], [0 -1], [1 0], [-1 0], [1 1], [-1 -1], [1 -1], [-1 1]]]
        (letfn [
                (g [i j]
                  (cond
                    (< i 0) \space
                    (>= i h) \space
                    (< j 0) \space
                    (>= j w) \space
                    :else
                    (nth (nth t i) j)))

                ]
          (map #(apply str %)
            (for [i (range h)] (for [j (range w)]
                                 (let [c (count (filter #(= % \#) (for [k m] (g (+ i (k 0)) (+ j (k 1))))))]
                                   (cond (< c 2) \space
                                         (= c 2) (g i j)
                                         (= c 3) \#
                                         (> c 3) \space)
                                   ))
                               )))))))

(defcheck solution-99bd0188
  (fn life [raw-board]
    (let [board          (map seq raw-board)
          neighbors      (fn [o_r o_c] (for [r (range (dec o_r) (+ 2 o_r))
                                             c (range (dec o_c) (+ 2 o_c))
                                             :when (and (or (not= c o_c) (not= r o_r))
                                                        (<= 0 r (dec (count raw-board)))
                                                        (<= 0 c (dec (count (first raw-board)))))]
                                         (vector r c)))
          alive?         (fn [r c] (= \# (nth (nth board r) c)))
          neighbor-count (fn [r c] (apply + (map #(if (apply alive? %) 1 0) (neighbors r c))))
          next-step      (fn [r c]
                           (let [neighbors (neighbor-count r c)
                                 alive     (alive? r c)]
                             (cond
                               (< neighbors 2) " "
                               (and alive (<= 2 neighbors 3)) "#"
                               (< 3 neighbors) " "
                               (and (not alive) (= neighbors 3)) "#"
                               :else " ")))
          step-row       (fn [row-number]
                           (apply str (map (partial next-step row-number) (range (count (nth board row-number))))))
          ]
      (apply vector (map step-row (range (count board))))
      )))

(defcheck solution-9a539a54
  (fn next-gen [board]
    (letfn [(adjacent [i j]
              (for [ix (range (- i 1) (+ i 2))
                    jx (range (- j 1) (+ j 2))
                    :when (not= [ix jx] [i j])]
                (list ix jx)))
            (get-cell [i j]
              (get (get board i) j))
            (adjacent-cells [i j]
              (map #(get-cell (first %) (second %)) (adjacent i j)))
            (count-live [i j]
              (count (filter #(= \# %) (adjacent-cells i j))))
            (next-state [i j val]
              (let [c (count-live i j)]
                (if (= val \#)
                  (cond
                    (< c 2) \space
                    (or (= c 2) (= c 3)) \#
                    (> c 3) \space)
                  (cond
                    (= c 3) \#
                    :default \space))))]
      (for [ix (range (count board))]
        (apply str (map-indexed #(next-state ix %1 %2) (get board ix)))))))

(defcheck solution-9a7d719f
  (fn [board]
    (let [neighbours (fn [[x y]]
                       (for [dx [-1 0 1]
                             dy [-1 0 1]
                             :when (not= 0 dx dy)]
                         [(+ x dx) (+ y dy)]))

          step       (fn [cells]
                       (set (for [[loc n] (frequencies (mapcat neighbours cells))
                                  :when (or (= n 3) (and (= n 2) ((set cells) loc)))]
                              loc)))

          line       (fn [width s]
                       (apply str (map #(if (s %) \# \space) (range 0 width))))

          lines      (fn [hight width cells]
                       (map #(->> (get (group-by first cells) % [])
                               (map second)
                               set
                               (line width))
                         (range 0 hight)))]
      (->> (keep-indexed #(keep-indexed
                            (fn [n x] (if (= x \#) [% n] nil)) %2)
             board)
        (apply concat [])
        step
        (lines (count board) (count (first board)))))))

(defcheck solution-9ae7f457
  (fn gameoflife [b]
    (let [v (vec (mapcat vec b))
          n (count (first b))
          f (fn [w]
              (if (and (= (first w) \#) (< (peek w) 2))
                \space
                (if (and (= (first w) \#) (<= (peek w) 3))
                  \#
                  (if (and (= (first w) \#) (> (peek w) 3))
                    \space
                    (if (and (= (first w) \space) (== (peek w) 3))
                      \#
                      \space)))))]
      (vec (map #(apply str %) (partition n (map f (for [x (range (* n n))]
                                                     (vector (get v x) (apply + (for [y (range (* n n))
                                                                                      :when (and (or (== y (- x n)) (== y (+ x n))
                                                                                                     (and (== y (dec x)) (not= 0 (mod x n)))
                                                                                                     (and (== y (inc x)) (not= 0 (mod (inc x) n)))
                                                                                                     (and (== y (- x (inc n))) (not= 0 (mod x n)))
                                                                                                     (and (== y (- x (dec n))) (not= 0 (mod (inc x) n)))
                                                                                                     (and (== y (+ x (inc n))) (not= 0 (mod (inc x) n)))
                                                                                                     (and (== y (+ x (dec n))) (not= 0 (mod x n))))
                                                                                                 (= (get v y) \#))]
                                                                                  1)))))))))))

(defcheck solution-9b7080af
  (fn life [board]
    (let [r (count board)
          c (count (first board))]
      (letfn [(neighbors [i j]
                (remove #(= % [i j]) (for [x (filter #(and (>= % 0) (< % r)) (range (- i 1) (+ 2 i)))
                                           y (filter #(and (>= % 0) (< % c)) (range (- j 1) (+ 2 j)))]
                                       [x y])))
              (alive [cell]
                (let [[i j] cell]
                  (= \# (nth (nth board i) j))))
              (gen [cell]
                (let [[i j] cell
                      live-nb (count (filter true? (map alive (neighbors i j))))]
                  (cond (and (alive cell) (or (= 2 live-nb) (= 3 live-nb))) \#
                        (and (not (alive cell)) (= 3 live-nb)) \#
                        :else \space)))
              (gen-row [i]
                (apply str (map gen (for [j (range c)] [i j]))))]
        (map gen-row (range r))))))

(defcheck solution-9c1917fb
  (fn [grid]
    (let [offsets        (for [x [-1 0 1] y [-1 0 1] :when (not= 0 x y)] [x y])
          neighbors      (fn [coord]
                           (map (partial map +) (repeat coord) offsets))
          live?          (fn [coord]
                           (= \# (get-in grid coord)))
          neighbor-count (fn [coord]
                           (->> (neighbors coord)
                             (filter live?)
                             (count)))
          next-state     (fn [coord]
                           (let [n-count (neighbor-count coord)]
                             (if (live? coord)
                               (cond
                                 (< n-count 2) \space
                                 (> n-count 3) \space
                                 :else \#)
                               (if (= 3 n-count) \# \space))))]
      (for [r (range (count grid))]
        (clojure.string/join
          (for [c (range (count (first grid)))]
            (next-state [r c])))))))

(defcheck solution-9ce7336e
  (fn [initboard]
    (let [indices
                       (for [i (range (count initboard)) j (range (count (first initboard)))] [i j])

          update-board (fn [board [i j] char]
                         (let [s (vec (get board i))]
                           #_(println [i j] "to " char)
                           (assoc board i (apply str (assoc s j char)))))

          neighbours   (fn [[i j]]
                         (let [[xs ys] [(dec i) (dec j)]
                               [xe ye] [(+ 2 i) (+ 2 j)]]
                           (for [x (range xs xe) y (range ys ye)
                                 :when (not= [i j] [x y])
                                 :when (>= x 0)
                                 :when (>= y 0)]
                             (get-in initboard [x y]))))

          evolve-cell  (fn [board index]
                         (let [live?        (fn [c]
                                              (= \# c))

                               cell         (get-in board index)
                               live-neighbs (remove #(not (live? %))
                                              (neighbours index))
                               n            (count live-neighbs)]
                           (cond
                             (and (live? cell) (< n 2)) (update-board board index \space)
                             (and (live? cell) (> n 3)) (update-board board index \space)
                             (and (not (live? cell)) (= n 3)) (update-board board index \#)
                             :else board
                             )))]
      (reduce evolve-cell initboard indices))))

(defcheck solution-9ce8d441
  #(let [nx (juxt inc inc identity dec dec dec identity inc)
         ny (juxt identity inc inc inc identity dec dec dec)]
     (for [i (range (count %))]
       (apply str
         (for [j (range (count %))
               :let [curr           (get-in % [i j])
                     live-neighbors ((frequencies (map (comp (partial get-in %) vector) (nx i) (ny j))) \#)]]
           (case live-neighbors
             3 \#
             2 (if (= curr \#) \# \space)
             \space))))))

(defcheck solution-9dddf830
  (fn [board]
    (let [maxx (count (first board))
          maxy (count board)]
      (letfn [(alive? [[x y]] (= (get-in board [y x]) \#))
              (neighbours [[x y]] (for [dx [-1 0 1] dy [-1 0 1] :when (and (not (and (zero? dx) (zero? dy)))
                                                                           (< -1 (+ x dx) maxx)
                                                                           (< -1 (+ y dy) maxy))] [(+ x dx) (+ y dy)]))
              (next-gen [loc] (let [population (count (filter alive? (neighbours loc)))]
                                (cond (< population 2) " "
                                      (= population 2) (if (alive? loc) "#" " ")
                                      (= population 3) "#"
                                      (> population 3) " ")))
              (next-row-gen [y] (apply str (map next-gen (for [x (range 0 maxx)] [x y]))))]
        (into [] (map next-row-gen (range 0 maxy)))))))

(defcheck solution-9ec68533
  (fn [matrix-raw]
    (let [matrix (->> matrix-raw (map vec) vec)
          rows   (count matrix)
          cols   (count (first matrix))
          output (atom matrix)]
      (doseq [r (range rows), c (range cols)]
        (let [old-value      (get-in matrix [r c])
              neighbors      (for [row [(dec r) r (inc r)]
                                   col [(dec c) c (inc c)]]
                               (if (= [row col] [r c])
                                 nil
                                 (get-in matrix [row col])))
              live-neighbors (count (filter #{\#} neighbors))
              new-value      (cond
                               (< live-neighbors 2) \space
                               (= live-neighbors 2) old-value
                               (= live-neighbors 3) \#
                               (> live-neighbors 3) \space)]
          (swap! output assoc-in [r c] new-value)))
      (map #(apply str %) (deref output)))))

(defcheck solution-9f006464
  (fn [gen]
    (let [gen  (vec (map vec gen))
          w    (count gen)
          nbrs (fn [i j]
                 (reduce
                   + (for [x (range -1 2)
                           y (range -1 2)
                           :when
                           (and (not= [0 0] [x y])
                                (= \# (get-in gen [(+ i x)
                                                   (+ j y)])))]
                       1)))]
      (for [i (range w)]
        (apply str
          (for [j (range w)
                :let [x (nbrs i j)]]
            (if (some #{0 (dec w)} [i j])
              \space
              (condp > x
                2 \space
                3 (get-in gen [i j])
                4 \#
                \space))))))))

(defcheck solution-9f6431e6
  (fn f [b]
    (let [bb              (map seq b)
          dimx            (count b)
          dimy            (count (first b))
          surrounding     (fn [x y]
                            (for [x' [(dec x) x (inc x)]
                                  y' [(dec y) y (inc y)]
                                  :when (and (>= x' 0) (>= y' 0)
                                             (< x' dimx) (< y' dimy)
                                             (not (and (= x x') (= y y'))))]
                              (nth (nth bb x') y')))
          count-neighbors (fn [x y]
                            (count (filter #(= % \#) (surrounding x y))))
          nextgen         (fn [x y c]
                            (let [n (count-neighbors x y)]
                              (cond (and (= c \#) (= n 2)) \#
                                    (= n 3) \#
                                    :else \space)))]
      (map-indexed (fn [x row]
                     (apply str (map-indexed (partial nextgen x) row)))
        bb))))

(defcheck solution-9ff0f659
  (fn game-of-life [colls]
    (let [width (count (first colls))
          depth (count colls)]
      (letfn [(valid-pos? [x y] (and
                                 (<= 0 x (dec width))
                                 (<= 0 y (dec depth))))
              (get-status [x y] (if (valid-pos? x y)
                                  (get (get colls x) y)
                                  nil))
              (neigh-map [x y] (map (fn [p-x p-y] (get-status p-x p-y))
                                 [(dec x) x (inc x) (dec x) (inc x) (dec x) x (inc x)]
                                 [(dec y) (dec y) (dec y) y y (inc y) (inc y) (inc y)]))
              (live-neig-count [x y] (count (filter #(= % \#) (neigh-map x y))))
              (live-to-live [x y] (when (= (get-status x y) \#)
                                    (<= 2 (live-neig-count x y) 3)))
              (die-to-live [x y] (when (= (get-status x y) \space)
                                   (= (live-neig-count x y) 3)))]
        (into [] (for [x (range width)]
                   (apply str (for [y (range depth)]
                                (if (or (live-to-live x y)
                                        (die-to-live x y))
                                  \#
                                  \space)))))))))

(defcheck solution-a0afd2c2
  (fn [board]
    (letfn [(alive [[r c]]
              (when (< -1 r (count board))
                (let [row (nth board r)]
                  (when (< -1 c (count row))
                    (= \# (nth row c))))))
            (living-neighbors [r c]
              (- (count
                   (filter alive (for [dr (range -1 2) dc (range -1 2)]
                                   [(+ r dr) (+ c dc)])))
                 (if (alive [r c]) 1 0)))
            (step [r c]
              (let [n (living-neighbors r c)]
                (if (alive [r c])
                  (if (<= 2 n 3) \# " ")
                  (if (= 3 n) \# " "))))]
      (map-indexed
        (fn [r row]
          (apply str (map-indexed
                       (fn [c cell]
                         (step r c)) row))) board))))

(defcheck solution-a0baacad
  (fn [board]
    (let [max-y (-> board count dec)
          max-x (-> board first count dec)]
      (letfn [(matrix-add [[x y] [a b]] [(+ x a) (+ y b)])
              (neighbours [[x y]]
                (for [i (range -1 2) j (range -1 2) :when (not= 0 i j)]
                  (matrix-add [x y] [i j])))
              (valid? [[x y]] (and (<= 0 x max-x) (<= 0 y max-y)))
              (living [s] (count (filter #(= \# (get-in board %))
                                   (filter valid? s))))
              (life [me s]
                (cond
                  (and (= \  (get-in board me)) (= 3 (living s))) \#
                  (and (= \# (get-in board me)) (<= 2 (living s) 3)) \#
                  :else \ ))]
        (apply map str (partition (inc max-x) (for [i (range 0 (inc max-x)) j (range 0 (inc max-y))]
                                                (->> [j i] neighbours (life [j i])))))))))

(defcheck solution-a0cf1a3
  (fn
    [board]
    (let [maxi      (count board)
          maxj      (count (first board))
          living?   (fn [[i j]] (= \# (nth (nth board i) j)))
          neighbors (fn [i j]
                      (count (filter living? (for [i2 [(dec i) i (inc i)] :when (and (>= i2 0) (< i2 maxi))
                                                   j2 [(dec j) j (inc j)] :when (and (>= j2 0) (< j2 maxj))
                                                   :when (not (and (= i i2) (= j j2)))]
                                               [i2 j2]))))]
      (vec (for [i (range maxi)]
             (apply str
               (for [j (range maxj)]
                 (let [n (neighbors i j)]
                   (if (living? [i j])
                     (if (or (= n 2) (= n 3)) \# \space)
                     (if (= n 3) \# \space))))))))))

(defcheck solution-a0d5b7b5
  (fn [ss]
    (letfn [(next-gen [bm]
              (letfn [(count-neighbors [r c]
                        (+ (get-in bm [r (dec c)] 0)
                          (get-in bm [r (inc c)] 0)
                          (get-in bm [(dec r) c] 0)
                          (get-in bm [(inc r) c] 0)
                          (get-in bm [(dec r) (dec c)] 0)
                          (get-in bm [(inc r) (dec c)] 0)
                          (get-in bm [(dec r) (inc c)] 0)
                          (get-in bm [(inc r) (inc c)] 0)))]
                (map (fn [r]
                       (apply str
                         (map (fn [c]
                                (let [n (count-neighbors r c)]
                                  (cond (< n 2) \space
                                        (> n 3) \space
                                        (= n 2) (if (pos? ((bm r) c)) \# \space)
                                        (= n 3) \#)))
                           (range (count (bm r))))))
                  (range (count bm)))))]
      ; easier to work with binary matrix (of vectors)
      (next-gen (mapv (fn [s]
                        (mapv (fn [c]
                                ({\space 0, \# 1} c))
                          s))
                  ss)))))

(defcheck solution-a1319c83
  (fn [board]
    (letfn [(neighbours [board row col]
              (map #(get-in board %)
                (concat (map (fn [c] [(dec row) c]) [(dec col) col (inc col)])
                  (list [row (dec col)] [row (inc col)])
                  (map (fn [c] [(inc row) c]) [(dec col) col (inc col)]))))
            (live-neighbours [board row col]
              (count (filter #(= \# %) (neighbours board row col))))
            (next-state [board row col]
              (let [n (live-neighbours board row col)]
                (if (= (get-in board [row col]) \#)
                  (if (#{2 3} n) \# \space)
                  (if (= n 3) \# \space))))]
      (map (fn [row] (apply str (map (fn [col] (next-state board row col))
                                  (range (count (board row))))))
        (range (count board))))))

(defcheck solution-a195a38f
  (fn [board]
    (letfn [(live?
              [[x y :as coords] cell]
              (let [x-          (dec x) x+ (inc x) y- (dec y) y+ (inc y)
                    adjacent    [[x- y-] [x y-] [x+ y-] [x+ y] [x+ y+] [x y+] [x- y+] [x- y]]
                    n-neighbors (frequencies (map (partial get-in board) adjacent))]
                (case (get n-neighbors \# 0)
                  (0 1 4 5 6 7) \space
                  2 cell
                  3 \#)))
            (map-board
              [f]
              (map-indexed
                (fn [row line]
                  (apply str
                    (map-indexed
                      (fn [column cell]
                        (f [row column] cell))
                      line)))
                board))]
      (map-board live?))))

(defcheck solution-a195d196
  (fn [board]
    (->>
      (concat board '("      "))
      (list* "      ")
      (partition 3 1)
      (map
        (fn [row]
          (->>
            row
            (map #(partition 3 1 (str \space % \space)))
            (apply map list)
            (map
              #(->>
                 %
                 (apply concat)
                 (filter #{\#})
                 count
                 ((fn [cell n] (case (if (= cell \#) (dec n) n) 3 \# 2 cell \space)) (second (second %)))
                 ))
            (apply str)
            ))))))

(defcheck solution-a2a83e88
  (fn [grid]
    (let [nrows (count grid)
          ncols (count (grid 0))

          grid->set
                (fn [grid]
                  (loop [r 0 c 0 res #{}]
                    (let [cell   (nth (nth grid r) c)
                          next-c (mod (inc c) ncols)
                          next-r (if (zero? next-c) (inc r) r)]
                      (if (= nrows next-r)
                        res
                        (recur next-r next-c
                          (if (= \# cell)
                            (conj res [r c])
                            res))))))

          set->grid
                (fn [set]
                  (let [tmp (for [r (range nrows)
                                  c (range ncols)]
                              (some #{[r c]} set))]
                    (into [] (for [r (partition ncols
                                       (map #(if (nil? %) \space \#) tmp))]
                               (apply str (interpose "" r))))))

          neighbors
                (fn [[x y]]
                  (for [dx [-1 0 1]
                        dy (if (zero? dx)
                             [-1 1]
                             [-1 0 1])]
                    [(+ dx x) (+ dy y)]))

          live
                (fn [n alive?]
                  (or (= n 3)
                      (and (= n 2) alive?)))

          step
                (fn [world]
                  (set
                    (for [[cell n] (frequencies (mapcat neighbors world))
                          :when (live n (world cell))]
                      cell)))
          ]

      (-> grid
        grid->set
        step
        set->grid))))

(defcheck solution-a3901f7f
  (fn my-game-of-life
    [world]
    (let [width     (count (first world))
          height    (count world)
          world-map (zipmap (range) (apply str world))]
      (letfn [(get-neighbours-ids [id]
                (let [l (- id width)
                      u (+ id width)]
                  (vector (dec l) l (inc l) (dec id) (inc id) (dec u) u (inc u))))
              (get-neighbours [id]
                (filter (complement nil?) (map world-map (get-neighbours-ids id))))
              (calc-next-gen [neighbours id]
                (if (= (world-map id) \#)
                  (live-next-gen (count-of-live-neighbours neighbours))
                  (dead-next-gen (count-of-live-neighbours neighbours))))
              (count-of-live-neighbours [neighbours]
                (count (filter #(= % \#) neighbours)))
              (live-next-gen [live-count] (if (or (> live-count 3) (< live-count 2))
                                            \space
                                            \#))
              (dead-next-gen [live-count] (if (= live-count 3)
                                            \#
                                            \space))]
        (map #(apply str %) (partition width (map #(calc-next-gen (get-neighbours %) %) (keys (sort-by key world-map)))))))))

(defcheck solution-a4936c65
  (fn [data]
    (letfn [(d-coll [coord]
              (for [x (range -1 2) y (range -1 2)
                    :when (not= 0 x y)
                    :let [d (map + [x y] coord) ch (get-in data d)]]
                (if (= ch \#) 1 0)))
            (decide [r-i c-i ch]
              (condp #(%1 %2) (->> [r-i c-i] d-coll (apply +))
                #(and (= ch \#) (< % 2)) \space
                #(and (= ch \#) (#{2 3} %)) \#
                #(and (= ch \#) (> % 3)) \space
                #(and (= ch \space) (= % 3)) \#
                ch))]
      (->> data
        (map-indexed #(map-indexed (partial decide %1) %2))
        (map #(apply str %))))))

(defcheck solution-a4a867c4
  (fn n [b]
    (let [r      (count b)
          c      (count (first b))
          bl     (apply str (repeat c " "))
          ; create bigger board with empty boundaries
          bb     (cons (str " " bl " ") (map #(str " " % " ") (conj b bl)))

          alive? #(= \# (nth (apply concat b) %))
          ; count all live cells in 3x3 subquadrant of bb with center x,y (in b)
          sub9   #(let [x   (mod % r) y (quot % r)
                        sqr (apply str (for [i (range 3)]
                                         (subs (nth bb (+ y i)) x (+ x 3))))]
                    (get (frequencies sqr) \#))
          newc   #(if (or (and (alive? %) (= 4 (sub9 %))) (= 3 (sub9 %))) "#" " ")
          rs     (for [p (range (* r c))] (newc p))]

      (map #(apply str %) (partition c rs)))))

(defcheck solution-a4d7d8bc
  (fn runRound [board]

    (letfn [
            (update [m k f] (assoc m k (f (get m k))))

            (getNeighbors [coord] (filter (fn [c] (every? #(< -1 %) c)) (vector
                                                                          (update coord 0 inc)
                                                                          (update coord 1 inc)
                                                                          (update coord 0 dec)
                                                                          (update coord 1 dec)
                                                                          [(inc (first coord)) (dec (second coord))]
                                                                          (vector (dec (first coord)) (inc (second coord)))
                                                                          (map dec coord)
                                                                          (map inc coord)
                                                                          )))

            (lookup [coord board] (get (get board (second coord)) (first coord)))

            (countAliveNeigbors [coord board] ((frequencies (map #(lookup % board) (getNeighbors coord))) \#))

            (applyRules [coord board] (let [aliveNeighbors (countAliveNeigbors coord board)] (if (= (lookup coord board) \#) (cond
                                                                                                                               (< aliveNeighbors 2) " "
                                                                                                                               (< aliveNeighbors 4) "#"
                                                                                                                               :else " "
                                                                                                                               )
                                                                                                                             (if (= aliveNeighbors 3)
                                                                                                                               "#"
                                                                                                                               " "))))


            (getAllCoords [board] (for [x (range (count board)) y (range (count (first board)))] [y x]))

            ]
      (let [allCoords (getAllCoords board)] (into [] (map #(apply str %) (partition (count (first board)) (map #(applyRules % board) allCoords))))))))

(defcheck solution-a6f5b46a
  (fn [board]
    (let [board     (vec (map vec board))
          [h w] (map count ((juxt identity first) board))
          cell      (fn [y x]
                      (get-in board (map mod [y x] [h w])))
          alive?    (comp #{\#} cell)
          neighbors (let [offsets [-1 0 1]
                          deltas  (for [y offsets, x offsets
                                        :when (not= y x 0)]
                                    [y x])]
                      (fn [y x]
                        (count
                          (for [[dy dx] deltas
                                :when (alive? (+ y dy) (+ x dx))]
                            true))))
          new-state (fn [y x]
                      (let [nbr-count (neighbors y x)]
                        (if (or (= 3 nbr-count)
                                (and (= 2 nbr-count)
                                     (alive? y x)))
                          \#
                          \space)))]
      (for [y (range h)]
        (apply str (for [x (range w)]
                     (new-state y x)))))))

(defcheck solution-a6f9c802
  (fn [b] (let [O \space X \# s (+ 2 (count (first b))) p (map #(concat [O] % [O]) b) p (concat [(repeat s O)] p [(repeat s O)]) p (map #(partition 3 1 (apply map list %)) (partition 3 1 p))] (->> (for [r p] (for [[a [l c right] b] r :let [a? (= c X) n (->> (conj (concat a b) l right) (filter #{X}) count)]] (cond (< n 2) O (> n 3) O a? X (= n 3) X :else O))) (map #(apply str %))))))

(defcheck solution-a700b09d
  (fn [x]
    (let [cell                                              ;&#21028;&#23450;&#19968;&#20010;&#32454;&#32990;&#19979;&#19968;&#21051;&#30340;&#27515;&#27963;
                         (fn [self neighbours]
                           (let [lives (count (filter #{\#} neighbours))]
                             (cond (= \space self) (if (= 3 lives) "#" " ")
                                   (> 2 lives) " "
                                   (< 3 lives) " "
                                   :else "#")))
          ;&#33719;&#21462;&#19968;&#20010;&#32454;&#32990;&#65292;&#22914;&#26524;index&#36229;&#36807;&#33539;&#22260;&#65292;&#36820;&#22238;nil
          get-cell       #(if (or (> 0 %1) (> 0 %2) (<= (count x) %1) (<= (count (nth x %1)) %2)) nil (nth (nth x %1) %2))
          ;&#19968;&#32452;&#20989;&#25968;&#65292;&#29992;&#26469;&#35745;&#31639;&#21253;&#25324;&#33258;&#24049;&#30340;&#38468;&#36817;9&#20010;&#26684;&#23376;
          index9         (#(for [l % r %] [l r]) [#(dec %) #(identity %) #(inc %)])
          get-neighbours (fn [i j] (map (partial apply get-cell) (remove #{[i j]} (map #(vector ((first %) i) ((second %) j)) index9))))]
      (loop [i 0 j 0 result [] temp ""]
        (cond (= i (count x)) result
              (= j (count (nth x i))) (recur (inc i) 0 (conj result temp) "")
              :else (recur i (inc j) result (str temp (cell (get-cell i j) (get-neighbours i j))))))
      )))

(defcheck solution-a72e88f7
  (fn tick
    [state]
    (let [neighbors  (fn [x y]
                       (for [xinc (range -1 (inc 1))
                             yinc (range -1 (inc 1)) :when (not (= 0 xinc yinc))]
                         [(+ x xinc) (+ y yinc)]))
          map-matrix (fn [f matrix]
                       (map-indexed (fn [x row]
                                      (map-indexed (fn [y cell]
                                                     (f x y cell))
                                        row))
                         matrix))]
      (mapv (partial apply str)
        (map-matrix (fn [x y cell]
                      (let [n (count (filter #{\#}
                                       (map #(get-in state %)
                                         (neighbors x y))))]
                        (cond (= n 3) \#
                              (and (= cell \#) (= n 2)) \#
                              :else \space)))
          state)))))

(defcheck solution-a778e438
  (fn [m]
    (let [cell       (fn [m, x, y]
                       (cond
                         (or (< x 0) (> x (dec (count (m 0))))
                             (< y 0) (> y (count m)))
                         nil
                         :else
                         (get (m x) y))),
          live?      (fn [c] (= \# c)),
          dead?      (fn [c] (= \  c)),
          count-live (fn [v] (apply + (map #(if (live? %1) 1 0) v))),
          count-dead (fn [v] (apply + (map #(if (dead? %1) 1 0) v))),
          neighbors  (fn [m, x, y]
                       (vector
                         (cell m x (dec y))                 ; north
                         (cell m (inc x) (dec y))           ; north-east
                         (cell m (inc x) y)                 ; east
                         (cell m (inc x) (inc y))           ; south-east
                         (cell m x (inc y))                 ; south
                         (cell m (dec x) (inc y))           ; south-west
                         (cell m (dec x) y)                 ; west
                         (cell m (dec x) (dec y))           ; north-west
                         )),
          next-gen   (fn [m]
                       (for [i (range (count m))]
                         (for [j (range (count (m 0)))]
                           (let [c (cell m i j), n (count-live (neighbors m i j))]
                             (cond
                               (dead? c) (if (= 3 n) \# \ )
                               (live? c) (if (or (= 2 n) (= 3 n)) \# \ )
                               :else \ )
                             ))))]
      (into [] (for [v (next-gen m)] (apply str v)))
      )))

(defcheck solution-a7d1771e
  (fn [grid]
    (let [width              (count grid)
          height             (count (first grid))
          cells              (set
                               (for [x (range width)
                                     y (range height)
                                     :when (= \# (get-in grid [x y]))]
                                 [x y]))
          dirs               [[-1 -1] [0 -1] [1 -1]
                              [-1 0] [1 0]
                              [-1 1] [0 1] [1 1]]
          surrounding-coords (fn [coord] (map #(map + coord %) dirs))
          neighbours         (fn [coord] (filter #(contains? cells %) (surrounding-coords coord)))
          survivors          (filter #(let [n (count (neighbours %))] (or (= 2 n) (= 3 n))) cells)
          newbies            (filter #(let [n (count (neighbours %))] (= 3 n)) (clojure.set/difference (set (reduce #(concat %1 (surrounding-coords %2)) [] cells)) cells))
          next-gen           (set (concat survivors newbies))]
      (for [y (range height)]
        (apply str
          (for [x (range width)]
            (if (contains? next-gen [y x]) "#" " ")))))))

(defcheck solution-a92e19b5
  (fn [grid]
    (let [cellular-update
                  (fn [live-cells]
                    (let [get-neighbors (fn [[x y]] (for [i (range -1 2)
                                                          j (range -1 2)
                                                          :when (not (and (= i 0) (= j 0)))]
                                                      [(+ i x) (+ j y)]))]
                      (into #{}
                        (map first
                          (filter (fn [[cell freq]] (or (= freq 3)
                                                        (and (= freq 2) (live-cells cell))))
                            (frequencies (mapcat get-neighbors live-cells)))))))
          to-cell-list
                  (fn [grid]
                    (into #{}
                      (remove nil? (apply concat (map-indexed (fn [x row]
                                                                (map-indexed (fn [y v]
                                                                               (if (= v \#)
                                                                                 [x y]
                                                                                 nil))
                                                                  row))
                                                   grid)))))
          to-grid (fn [live-cells dim]
                    (for [x (range dim)]
                      (apply str (for [y (range dim)]
                                   (if (live-cells [x y]) \# \space)))))]
      (-> grid
        to-cell-list
        cellular-update
        (to-grid (count grid))))))

(defcheck solution-a9490e71
  (fn [board]
    (map-indexed
      (fn [y r]
        (apply str
          (map-indexed
            (fn [x c]
              (if (({\# #{2 3} \  #{3}} c)
                   (apply + (map-indexed #({\# 1 \  0}
                                           (get-in board (map + [y x] %2) \ ))
                              [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])))
                \#
                \ ))
            r)))
      board)))

(defcheck solution-a9acecd
  (fn [brd]
    (letfn [(getadjacent [[a b]]
              (for [n [-1 0 1]
                    m [-1 0 1]
                    :when (not= 0 n m)]
                (vector (+ a n) (+ b m))))
            (getstate [pos bd]
              (reduce #(conj % (get-in bd %2)) [] (getadjacent pos)))]
      (let [size (count brd)]
        (apply vector (map #(apply str %) (partition size (for [a (range size)
                                                                b (range size)]
                                                            (let [cnt (count (filter #{\#} (getstate [a b] brd)))]
                                                              (if (= \# (get-in brd [a b]))
                                                                (cond (< cnt 2) \space
                                                                      (or (= cnt 2) (= cnt 3)) \#
                                                                      :else \space)
                                                                (if (= cnt 3) \# \space)))))))))))

(defcheck solution-a9be3117
  (letfn [(remap [m f] (into {} (for [[k v] m] [k (f k v)])))
          (numbered [s] (map vector (range) s))
          (alive? [m pos] (get-in m [pos :alive?]))
          (neighbours [y x]
            (let [f (juxt identity dec inc)]
              (for [a (f y) b (f x) :when (not= [a b] [y x])] [a b])))
          (strings->map [s]
            (into {} (for [[row l] (numbered s)
                           [col c] (numbered l)]
                       [[row col] {:alive? (= \# c)}])))
          (live-neighbours [m row col]
            (->> (neighbours row col)
              (map (partial alive? m))
              (filter identity)
              count))
          (next-state [{live? :alive?} n]
            {:alive? (cond (and live? (#{0 1} n)) false
                           (and live? (#{2 3} n)) true
                           live? false
                           (= n 3) true
                           :else false)})
          (play-turn-in-map [m]
            (remap m (partial play-turn m)))
          (play-turn [m [row col] cell]
            (next-state cell (live-neighbours m row col)))
          (map->strings [m]
            (map (comp (partial apply str)
                   (partial map (comp {true \#, false \space}
                                  (partial alive? m))))
              (partition-by first (sort (keys m)))))]
    (fn [s]
      (-> s
        strings->map
        play-turn-in-map
        map->strings))))

(defcheck solution-a9ef1f01
  (fn [board]
    (let [width  (count (first board))
          height (count board)
          data   (vec (apply str board))
          size   (count data)
          wrap-  (fn [i d]
                   (mod (- (+ i size) d) size))
          wrap+  (fn [i d]
                   (mod (+ i d) size))
          n      (fn [i]                                    ; count the number of live cells around an index
                   (count
                     (filter
                       #(= \# %)
                       [
                        (nth data (wrap- i (+ width 1)))    ;nw
                        (nth data (wrap- i width))          ;n
                        (nth data (wrap- i (- width 1)))    ;ne
                        (nth data (wrap- i 1))              ;w
                        (nth data (wrap+ i 1))              ;e
                        (nth data (wrap+ i (- width 1)))    ;sw
                        (nth data (wrap+ i width))          ;s
                        (nth data (wrap+ i (+ width 1)))    ;se
                        ])))
          state  (map
                   #(if (= \# %1)
                      (let [c (n %2)]
                        (cond
                          (> c 3) \space
                          (< c 2) \space
                          :else \#))
                      (let [c (n %2)]
                        (cond
                          (= 3 c) \#
                          :else \space)))
                   data
                   (range))]
      (map
        #(apply str %)
        (partition width state)))))

(defcheck solution-aa369dea
  (fn [m]
    (let
     [d1   (count m) d2 (count (first m))
      nidx (fn [[x y] [xd yd]]
             (letfn [(n [a] (map #(+ a %) [-1 0 1]))]
               (for [i (n x) j (n y)
                     :when (and (< -1 i xd) (< -1 j yd) (not= [i j] [x y]))] [i j])))
      mv   (fn [m i] (if (= \# (nth (nth m (first i)) (last i))) 1 0))
      ]
      (for [i (range d1)]
        (apply str
          (for [j (range d2)]
            (let [nc (->> (nidx [i j] [d1 d2]) (map #(mv m %)) (apply +))
                  cv (mv m [i j])]
              (if (or (= [0 3] [cv nc]) (and (= 1 cv) (< 1 nc 4))) \# \space))))))))

(defcheck solution-ab72da3e
  (fn game-of-life [board]
    (letfn [(get-board-state [board x y]
              (nth (nth board x) y)
              )
            (neighbour [board x y]
              (if (or (< x 0)
                      (>= x (count board))
                      (< y 0)
                      (>= y (count (nth board 0)))
                      )
                0
                (if (= \# (get-board-state board x y))
                  1
                  0
                  )
                )
              )
            (live-neigbours-count [board x y]
              (+ (neighbour board (dec x) (dec y))
                (neighbour board (dec x) y)
                (neighbour board (dec x) (inc y))
                (neighbour board x (dec y))
                (neighbour board x (inc y))
                (neighbour board (inc x) (dec y))
                (neighbour board (inc x) y)
                (neighbour board (inc x) (inc y))
                )
              )
            (is-alive [board x y]
              (let [neigh-cnt (live-neigbours-count board x y)]
                (if (= \# (get-board-state board x y))
                  (cond
                    (< neigh-cnt 2) \space
                    (or (= neigh-cnt 2) (= neigh-cnt 3)) \#
                    (> neigh-cnt 3) \space
                    )
                  (if (= neigh-cnt 3)
                    \#
                    \space
                    )
                  )
                )
              )
            ]
      (let [x-size (count board) y-size (count (nth board 0))]
        (into [] (for [x (range x-size)]
                   (apply str
                     (for [y (range y-size)]
                       (is-alive board x y)
                       )
                     )
                   )
          )
        )
      )
    ))

(defcheck solution-ab97e365
  (fn life [board]
    (let [h (count board) w (count (first board)) join clojure.string/join]
      (letfn [(neighbor-coords [i j]
                (->> '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))
                  (map #(list (+ i (first %)) (+ j (second %))))
                  (filter #(and (< -1 (first %) h) (< -1 (second %) w)))))
              (neighbors [i j]
                (->> (neighbor-coords i j)
                  (map #(get-in board %))
                  (join)))
              (live-neighbors [i j]
                (->> (neighbors i j)
                  (filter #(= % \#))
                  (count)))
              (live? [i j]
                (= \# (get-in board [i j])))]
        (vec (for [i (range h)]
               (join (for [j (range w)]
                       (let [ln (live-neighbors i j) l (live? i j)]
                         (if (or (= ln 3) (and (= ln 2) l)) \# \space))))))))))

(defcheck solution-ada3d15b
  (fn [board]
    (let [n             (count board)
          m             (-> board first count)
          shifts        [[-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1]]
          plus          (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)])
          live?         #(= (get-in board %) \#)
          neighbors     (fn [pos]
                          (->> shifts
                            (map #(plus % pos))
                            (map #(get-in board %))
                            (filter #(= % \#))
                            count))
          will-be-live? (fn [pos]
                          (contains?
                            (if (live? pos) #{2 3} #{3})
                            (neighbors pos)))]
      (->> (reduce
             #(conj %1 (if (will-be-live? %2) \# \space))
             '()
             (for [x (range m) y (range n)] [x y]))
        (partition m)
        (map #(apply str %))))))

(defcheck solution-adbf30c3
  (fn [board]
    (letfn [(num-of-neignbors [r c b]
              (let [n (count b)]
                (count (filter #(= % \#)
                         (for [x [-1 0 1]
                               y [-1 0 1]
                               :let [r2 (+ r x)
                                     c2 (+ c y)]
                               :when (and (>= r2 0) (< r2 n)
                                          (>= c2 0) (< c2 n)
                                          (not (and (= x 0)
                                                    (= y 0))))]
                           (get-in b [r2 c2]))))))
            (cell-of-next-gen [r c b]
              (let [nb (num-of-neignbors r c b)]
                (cond
                  (or (< nb 2) (> nb 3)) " "
                  (= (get-in b [r c]) \#) "#"
                  (= nb 3) "#"
                  :else " ")))]
      (let [n (range (count board))]
        (for [r n]
          (apply str
            (for [c n]
              (cell-of-next-gen r c board))))))))

(defcheck solution-adc482c4
  (fn game-of-life [board]
    (let [n (count board)]
      (letfn [(board->coordinates [board]
                (set (for [x (range n) y (range n)
                           :when (= \# (get-in board [x y]))] [x y])))
              (neighbors [[x y]]
                (set (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
                       [(+ x dx) (+ y dy)])))
              (step [coordinates]
                (for [[cell n] (frequencies (mapcat neighbors coordinates))
                      :when (or (= n 3)
                                (and (= n 2) (coordinates cell)))]
                  cell))
              (coordinates->board [coordinates]
                (let [blank-board (vec (repeat n (vec (repeat n \space))))]
                  (->> coordinates
                    (reduce #(assoc-in % %2 \#) blank-board)
                    (map #(apply str %)))))]
        (->> board
          board->coordinates
          step
          coordinates->board)))))

(defcheck solution-adefaa3f
  (fn next-generation [board]
    (let [live                \#
          dead                \space
          directions          (remove #(= [0 0] %) (for [x (range -1 2) y (range -1 2)] [x y]))
          count-neighbor-live (fn [base-x base-y]
                                (count (filter #(= live %)
                                         (map (fn [[x y]]
                                                (get (get board (+ base-y y) [])
                                                  (+ base-x x)
                                                  dead))
                                           directions))))]
      (map-indexed (fn [y line]
                     (apply str
                       (map-indexed (fn [x cell]
                                      (let [live-count (count-neighbor-live x y)]
                                        (if (if (= cell live)
                                              (or (= live-count 2) (= live-count 3))
                                              (= live-count 3))
                                          live
                                          dead)))
                         line)))
        board))))

(defcheck solution-adfcdf5f
  (fn next-gen [cells]
    (let [cell-size         (count cells)
          cell-column       (count (first cells))
          neighbors         (fn [xy]
                              (let [delta [[-1 0] [1 0] [0 -1] [0 1] [-1 -1] [-1 1] [1 -1] [1 1]]]
                                (filter
                                  (fn [xy] (every? #(< -1 % cell-size) xy))
                                  (map #(map + % xy) delta))))
          neighbor-contents (fn [xy]
                              (map (partial get-in cells) (neighbors xy)))
          count-lives       (fn [lives]
                              (reduce (fn [[l d] c] (if (= c \#) [(inc l) d] [l (inc d)])) [0 0] lives))]
      (for [r (range 0 cell-size)]
        (apply str (for [c (range 0 cell-column)]
                     (let [content (get-in cells [r c])
                           [lives deaths] (count-lives (neighbor-contents [r c]))]
                       (if (= content \#) (cond
                                            (< lives 2) \space
                                            (< lives 4) \#
                                            :else \space)
                                          (if (= lives 3) \# \space)))))))))

(defcheck solution-ae514d28
  (fn [b]
    (let [r (count b)
          c (count (first b))
          n (reduce #(%2 %1)
              (vec (repeat (+ r 2) (vec (repeat (+ c 2) 0))))
              (for [i (range r) j (range c) :when (= \# (get-in b [i j]))
                    x [0 1 2] y [0 1 2] :when (not= [1 1] [x y])]
                #(update-in % [(+ i x) (+ j y)] inc)))
          f (comp rest butlast)]
      (map (fn [r n] (apply str (map
                                  #(condp = %2
                                     3 \#
                                     2 %1
                                     \ ) r (f n)))) b (f n)))))

(defcheck solution-aeef5193
  (fn [bd]
    (let [os [[-1 -1] [-1 0] [-1 1]
              [0 -1] [0 1]
              [1 -1] [1 0] [1 1]]
          h  (count bd)
          w  (count (first bd))
          gs (fn [[x y] [dx dy]]
               (let [c (+ x dx) r (+ y dy)]
                 (if (or (< c 0) (= c w) (< r 0) (= r h))
                   \space
                   (get-in bd [r c]))))
          cl (fn [p]
               (reduce + (map #(if (= \# (gs p %)) 1 0) os)))
          nx (fn [s p]
               (let [n (cl p)]
                 (if (or (= n 3)
                         (and (= s \#) (= n 2)))
                   \#
                   \space)))]
      (->> (for [y (range h) x (range w)]
             (nx (get-in bd [y x]) [x y]))
        (partition w)
        (map #(apply str %))
        (vec)))))

(defcheck solution-af43859f
  (fn [grid]
    (let [height-range                    (range (count grid))
          width-range                     (range (count (grid 0)))
          live-cells                      (set (for [y height-range
                                                     x width-range
                                                     :when (= (get-in grid [y x]) \#)]
                                                 [x y]))
          neighbors                       (fn [[x y]]
                                            (disj (set (for [i (range (- x 1) (+ x 2))
                                                             j (range (- y 1) (+ y 2))]
                                                         [i j]))
                                              [x y]))
          count-of-live-neighbors-matches (fn [pred]
                                            #(pred (count (filter live-cells (neighbors %)))))
          dead-cells-nearby               (filter (complement live-cells)
                                            (distinct (mapcat neighbors live-cells)))
          next-live-cells                 (set (concat
                                                 (filter (count-of-live-neighbors-matches #{2 3}) live-cells)
                                                 (filter (count-of-live-neighbors-matches #{3}) dead-cells-nearby)))]
      (for [y height-range]
        (apply str (for [x width-range]
                     (if (next-live-cells [x y])
                       "#"
                       " ")))))))

(defcheck solution-af4c67ac
  (fn [board]
    (map (partial apply str)
      (let [n     (count board)
            m     (count (board 0))
            dirs  [[0 1] [0 -1] [1 0] [-1 0] [-1 1] [1 -1] [-1 -1] [1 1]]
            cell  (fn [board [x y]]
                    (-> board (nth x) (nth y)))
            live? (fn [board [x y]]
                    (and
                     (>= x 0) (>= y 0) (< x n) (< y m)
                     (= \# (cell board [x y]))))]
        (for [x (range n)]
          (for [y (range m)]
            (let [neibs (count (filter (partial live? board)
                                 (map #(vec (map + [x y] %)) dirs)))
                  next  (if (live? board [x y])
                          (and (>= neibs 2) (<= neibs 3))
                          (= neibs 3))]
              (if next \# \space))))))))

(defcheck solution-af6d918a
  (fn [b]
    (let [c    (count b)
          b    (map (fn [l] (map #(if (= \# %) 1 0) l)) b)
          trps #(apply map vector %)
          rowc #(map
                  (fn [l]
                    (map
                      (fn [n]
                        (apply + (take (if (zero? n) 2 3) (drop (dec n) (nth % l)))))
                      (range c)))
                  (range c))]
      (map
        (fn [lb lp]
          (apply str
            (map
              (fn [nb np]
                (if (zero? nb)
                  (if (= np 3) "#" " ")
                  (if (or (= np 3) (= np 4)) "#" " ")))
              lb
              lp)))
        b
        ((comp trps rowc trps rowc) b)))))

(defcheck solution-afe29b46
  (fn life-game [coll]
    (letfn [(init [_coll]
              (reduce #(conj %1 (vec %2)) [] coll))

            (neighbers [_coll i j lm cm]
              (for [x [(dec i) i (inc i)]
                    y [(dec j) j (inc j)]
                    :when (and (<= 0 x) (<= 0 y)
                               (<= x lm) (<= y cm)
                               (not (and (= x i) (= y j))))]
                ((_coll x) y)))]

      (let [LMAX  (count coll)
            CMAX  (count (first coll))
            _coll (init coll)]

        (reduce #(conj %1 (apply str %2)) []
          (partition CMAX
            (for [i (range LMAX)
                  j (range CMAX)
                  :let [self ((_coll i) j)]]
              (let [nlist (group-by str (neighbers _coll i j (dec LMAX) (dec CMAX)))
                    lives (count (nlist "#"))
                    dies  (count (nlist " "))]
                (if (= (str self) "#")
                  ;live cell
                  (if (or
                       ; rule 1
                       (> 2 lives)
                       ; rule 2
                       (< 3 lives))
                    " "
                    "#")
                  (if (= 3 lives)
                    "#"
                    " "))
                ))))))))

(defcheck solution-b0254073
  (let [offsets        (-> (for [i [-1 0 1] j [-1 0 1]] [i j])
                         set
                         (disj [0 0]))
        live           \#
        dead           \space
        live?          #{live}
        dead?          #{dead}
        cell           (fn cell [m id]
                         (get-in m id))
        adjacents      (fn adjacents [rows cols [i j]]
                         ;; Not specified, so we make the world toroidal
                         (map (fn [[or oc]] [(mod (+ i or) rows)
                                             (mod (+ j oc) cols)])
                           offsets))
        neighbors      (fn neighbors [m id]
                         (for [coord (adjacents (count m)
                                       (count (first m))
                                       id)]
                           (cell m coord)))
        live-neighbors (fn live-neighbors [m id]
                         (->> (neighbors m id)
                           (filter live?)
                           count))
        successor      (fn successor [m id]
                         (let [c  (cell m id)
                               ns (live-neighbors m id)]
                           (if (live? c)
                             (if (<= 2 ns 3) live dead)
                             (if (= ns 3) live dead))))]
    (fn life-step [field]
      (let [m (mapv vec field)]
        (mapv #(apply str %)
          (for [i (range (count m))]
            (vec (for [j (range (count (first m)))]
                   (successor m [i j])))))))))

(defcheck solution-b102ccb4
  (fn prob94
    [strs]
    (letfn [
            ;; Decode a string representing a row consiting of live (#) and dead ( ) cells.
            ;;   Translate to nil for dead and :on for live
            ;;   (decode-row \" ##   \") => [nil :on :on nil nil nil]
            (decode-row [row]
              (vec (replace '{\space nil \# :on} (seq row))))

            ;; Reverse an encoded sequence
            ;;   (encode-row [nil :on :on nil nil nil]) => \" ##   \"
            (encode-row [xs]
              (apply str (replace '{nil \space :on \#} xs)))

            ;; Return the 'board' after decoding the string rows
            (load-from-strings [strs]
              ;; count is the height
              (vec (map #(decode-row %) strs)))

            (write-to-strings [xs]
              (vec (map #(encode-row %) xs)))

            ;; For a given 'board' return the set of live cells [r c]
            (get-live-cells [board]
              (for [r (range 6)
                    c (range 6)
                    :when (is-live board [r c])]
                [r c]))

            ;; Return the neighboring cells for a given cell [r c]
            (neighbors [[x y]]
              ;; https://en.wikipedia.org/wiki/Moore_neighborhood
              (for [dx [-1 0 1]
                    dy [-1 0 1]
                    :when (not= 0 dx dy)]                   ;; (not (= dx dy)) -> ignore dx=dy=0
                [(+ x dx) (+ y dy)]))

            ;; True if cell [r c] is live
            (is-live [board [r c]]
              (= :on (get-in board [r c])))                 ;; [row column] => [height width]

            ;; Find the number of live neighboars for a cell [r c]
            (live-neighbor-count [board [r c]]
              (let [neighbors (neighbors [r c])]
                (count (filter #(is-live board %) neighbors))))

            ;; Transform the board to the next step and return the set of new live cells
            (transform [board]
              ;; if 3 neighbors
              ;;     dead because live
              ;;     live lives
              ;;
              ;; if 2 neighbors
              ;;     live lives
              (filter
                #(not (nil? %))
                (let [live-cells (get-live-cells board)]
                  (for [r (range 6) c (range 6)]
                    (let [cnt (live-neighbor-count board [r c])]
                      (if (or (= 3 cnt)
                              (and (= 2 cnt)
                                   (is-live board [r c])))
                        [r c]))))))

            (empty-board
              [size]
              (vec (repeat size (vec (repeat size nil)))))

            ;; Build a new board from a set of live cells
            (build-board [size live-cells]
              (loop [b          (empty-board size)
                     live-cells live-cells]
                (if (empty? live-cells)
                  b
                  (recur (assoc-in b (first live-cells) :on) (rest live-cells)))))
            ]
      (let [board (load-from-strings strs)]
        (write-to-strings (build-board (count board) (transform board)))))))

(defcheck solution-b12c22a4
  (fn sol [xss]
    (letfn [
            (neibs [r c xss]
              (for [x [(dec r) r (inc r)]
                    y [(dec c) c (inc c)]
                    :when (and (or (not (= x r))
                                   (not (= y c))
                                   )
                               (and (< x (count xss))
                                    (< y (count (xss 0)))
                                    (>= x 0)
                                    (>= y 0)
                                    )
                               )
                    ]
                ((vec (xss x)) y)
                )
              )

            (countlife [xs]
              (count (filter #(= % \#) xs))
              )

            (nlife [r c xss]
              (let [n (countlife (neibs r c xss))]
                (cond (= n 2) \#
                      (= n 3) \#
                      :else \space
                      )
                )
              )

            (ndead [r c xss]
              (let [n (countlife (neibs r c xss))]
                (cond (= n 3) \#
                      :else \space
                      )
                )
              )


            (ngen [r c xss]
              (if (= ((vec (xss r)) c) \#)
                (nlife r c xss)
                (ndead r c xss)
                )
              )

            (trans [xss]
              (for [r (range (count xss))
                    c (range (count (xss 0)))
                    ]
                (ngen r c xss)
                )
              )

            ]



      (vec (map #(apply str %) (partition (count xss) (trans xss))))
      )))

(defcheck solution-b186e063
  (fn game-of-life [board]
    (let [new-cell    (fn [vv]
                        (let [alive? (= 1 (second (second vv)))
                              sum    (reduce + (flatten vv))]
                          (if alive?
                            (if (or (= 3 sum) (= 4 sum)) 1 0)
                            (if (= 3 sum) 1 0))))

          part-board  (fn [vv]
                        (map
                          (fn [[a b c]] (partition 3 1 (map vector a b c)))
                          (partition 3 1 vv)))

          parse-board (fn [c]
                        (map
                          (fn [a] (map #(if (= \# %1) 1 0) a))
                          c))

          frame-board (fn [vv]
                        (let [l       (repeat (count (first vv)) 0)
                              between (fn [c a] (conj (reverse (conj (reverse c) a)) a))]
                          (map #(between % 0) (between vv l))))

          draw-board  (fn [vv]
                        (map
                          (fn [l]
                            (apply str (map #(if (= % 1) \# \ ) l)))
                          vv))]
      (draw-board
        (map
          #(map new-cell %)
          (part-board (frame-board (parse-board board))))))))

(defcheck solution-b2c4fdf1
  #(let [r (range (count %))
         v [-1 0 1]
         a \#]
     (for [y r]
       (apply str (for [x r c [(count
                                 (for [j v
                                       k v
                                       :when (= a (get-in % [(+ y j) (+ x k)]))]
                                   1))]]
                    (if (or (= c 3) (and (= c 4) (= a (get-in % [y x]))))
                      a
                      \ ))))))

(defcheck solution-b3756b8
  (fn gen-next [table]
    (let [matrix         (->> table
                           (map (partial map (fn [char] (if (= char \#) 1 0))))
                           (map vec)
                           vec)
          max-x          (count (matrix 0))
          max-y          (count matrix)
          lazy-get       (fn [x y]
                           (if (or (< x 0) (= x max-x) (< y 0) (= y max-y)) 0 ((matrix y) x)))
          neighbours-sum (fn [x y]
                           (reduce + (for [xdiff (range -1 2)
                                           ydiff (range -1 2)
                                           :when (or (not= xdiff 0) (not= ydiff 0))]
                                       (lazy-get (+ x xdiff) (+ y ydiff)))))]
      (->> (for [y (range 0 max-y)
                 x (range 0 max-x)]
             (let [neighbours (neighbours-sum x y)]
               (if (= (lazy-get x y) 0)
                 (if (= 3 neighbours) \# \space)
                 (cond (< neighbours 2) \space
                       (> neighbours 3) \space
                       :else \#))))
        (partition max-x)
        (map (partial reduce str))))))

(defcheck solution-b3816592
  (fn [s]
    (let [h     (count s)
          w     (count (first s))
          dead  \space
          alive \#
          nnth  (fn nnth [s i j]
                  (cond (or (< i 0) (>= i (count s))
                            (< j 0) (>= j (count (first s))))
                        \space
                        :else
                        (nth (nth s i) j)))
          near  (fn near [s i j]
                  [(nnth s (inc i) j)
                   (nnth s (dec i) j)
                   (nnth s (inc i) (inc j))
                   (nnth s (dec i) (inc j))
                   (nnth s i (inc j))
                   (nnth s (inc i) (dec j))
                   (nnth s (dec i) (dec j))
                   (nnth s i (dec j))])]
      (vec (for [i (range h)]
             (apply str (for [j (range w)]
                          (if (= (nnth s i j) dead)
                            (if (= (reduce + (map (fn [x] (if (= x dead) 0 1)) (near s i j))) 3)
                              alive
                              dead)
                            (if (or (= (reduce + (map (fn [x] (if (= x dead) 0 1)) (near s i j))) 2)
                                    (= (reduce + (map (fn [x] (if (= x dead) 0 1)) (near s i j))) 3))
                              alive
                              dead)))))))))

(defcheck solution-b3a8b94a
  (fn [s]
    (let [nr              (count s)
          nc              (count (first s))
          live-neighbours (fn [y x]
                            (let [neighbours (rest (for [i [0 -1 1] j [0 -1 1]] [(+ y i) (+ x j)]))]
                              (count (filter #(= \# %) (map (partial get-in s) neighbours)))))
          new-state       (fn [y x]
                            (condp = (live-neighbours y x)
                              2 (get-in s [y x])
                              3 \#
                              \space))
          traverse        (for [y (range nr) x (range nc)] (new-state y x))]
      (map (partial apply str) (partition nc traverse)))))

(defcheck solution-b3babf93
  (fn [b]
    (let [nstate (fn [[x y]]
                   (let [fs   [(fn [x] x) inc dec]
                         v    (map #(get-in b %) (for [fx fs fy fs] [(fx x) (fy y)]))
                         I    (first v)
                         live (count (filter #{\#} (next v)))
                         ]
                     (if (#{\#} I)
                       (if (#{2 3} live) \# \ )
                       (if (= 3 live) \# \ ))))
          rc     #(range (count %))
          ]
      (vec (map (fn [r] (apply str (map #(nstate [r %]) (rc (first b))))) (rc b))))))

(defcheck solution-b4bac1fa
  (fn game-of-life [world]
    (let [living    (for [i (range (count world))
                          j (range (count (world i)))
                          :when (= (get-in world [i j]) \#)]
                      [i j])

          dead?     (fn [cell]
                      (= (get-in world cell) \space))

          living?   (fn [cell]
                      (= (get-in world cell) \#))

          neighbors (for [[i j] living
                          k (range -1 2)
                          l (range -1 2)
                          :when (not (= k l 0))]
                      [(+ i k) (+ j l)])

          counts    (reduce (fn [ret cell]
                              (update-in ret [cell] #(inc (or % 0))))
                      {}
                      neighbors)

          births    (reduce (fn [world cell]
                              (assoc-in world cell \#))
                      (mapv vec world)
                      (for [[cell cnt] counts
                            :when (and (dead? cell) (= cnt 3))]
                        cell))

          deaths    (reduce (fn [world cell]
                              (assoc-in world cell \space))
                      births
                      (for [[cell cnt] counts
                            :when (and (living? cell)
                                       (or (> cnt 3) (< cnt 2)))]
                        cell))]
      (mapv #(apply str %) deaths))))

(defcheck solution-b5dbb337
  (fn [g]
    (let [num-neighbors (fn [grid x y]
                          (let [xlen (count grid) ylen (count (first grid))]
                            (reduce +
                              (for [xc (range (max 0 (dec x)) (min xlen (+ 2 x)))
                                    yc (range (max 0 (dec y)) (min ylen (+ 2 y)))
                                    :when (and (not (and (= xc x) (= yc y))) (= \# (nth (nth grid xc) yc)))
                                    ] 1))))
          rules         {\# [{2 \#, 3 \#} \space], \space [{3 \#} \space]}
          new-cell      (fn [grid x y]
                          (let [cell      (nth (nth grid x) y)
                                neighbors (num-neighbors grid x y)
                                rule      (rules cell)
                                result    ((first rule) neighbors (last rule))
                                ]
                            result))
          new-line      (fn [grid x]
                          (apply str (for [y (range (count (nth grid x)))] (new-cell grid x y))))

          ]                                                 ;; let
      (map (partial new-line g) (range (count g))))))

(defcheck solution-b6816d2f
  (fn life-game-next
    ([v] (life-game-next v (count v) (count v)))
    ([v n m]
     (letfn [(getnm [v i j] (-> v (nth i) (nth j)))
             (count-around-cell [v n m x y]
               (->>
                 (for [i (range (- x 1) (+ x 2)) j (range (- y 1) (+ y 2))
                       :when (and
                              (not (and (= i x) (= j y)))
                              (<= 0 i) (<= 0 j)
                              (< i n) (< j m)
                              )]
                   (getnm v i j))
                 (filter #(= \# %))
                 count))
             (next-state [v n m x y]
               (let [cnt (count-around-cell v n m x y)]
                 (cond
                   (= cnt 3) \#
                   (= cnt 2) (getnm v x y)
                   :else \space)))]
       (for [i (range 0 n)]
         (->> (for [j (range 0 m)]
                (next-state v n m i j))
           (apply str)))))))

(defcheck solution-b714fe2a
  (fn game-of-life [rs]
    (let [row-length   (count (first rs))
          neighbors-fn (fn [r c] (count (filter (partial = \#)
                                          (for [x (range (dec r) (+ 2 r))
                                                y (range (dec c) (+ 2 c))]
                                            (get-in rs [x y])))))]
      (map (partial apply str)
        (partition row-length
          (for [r (range (count rs))
                c (range row-length)]
            (let [living-neighborhood (neighbors-fn r c)
                  is-live?            (= \# (get-in rs [r c]))]
              (cond (and is-live? (<= 3 living-neighborhood 4))
                    \#
                    (and (not is-live?) (= 3 living-neighborhood))
                    \#
                    :else
                    \space))))))))

(defcheck solution-b752477
  (fn [b]
    (letfn [(neighbours [[x y]]
              (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
                [(+ dx x) (+ dy y)]))
            (step [cells]
              (set (for [[loc n] (frequencies (mapcat neighbours cells))
                         :when (or (= n 3) (and (= n 2) (cells loc)))]
                     loc)))
            (lives [s]
              (let [w (count s)
                    h (count (first s))]
                (for [i (range w) j (range h) :when (= \# (get-in s [i j]))] [i j])))
            (populate [s]
              (map #(apply str %)
                (reduce #(assoc-in (vec (map vec %)) %2 \#)
                  (repeat (count b) (apply str (repeat (count (first b)) " "))) s)))]
      (-> b (lives) (set) (step) (populate)))))

(defcheck solution-b75f3ff0
  (fn game-of-live [cells]
    (let [w (count (first cells))
          h (count cells)]
      (->>
        (partition 3 1 (concat [(repeat (+ 2 w) \space)] (map #(concat [\space] % [\space]) cells) [(repeat (+ 2 w) \space)]))
        (mapcat (fn [[a b c]] (map list (partition 3 1 a) (partition 3 1 b) (partition 3 1 c))))
        (map (fn [[[a b c] [d e f] [g h i]]]
               (let [neighbor (count (filter #(= \# %) (list a b c d f g h i)))]
                 (cond
                   (= 3 neighbor) \#
                   (= 2 neighbor) e
                   :else \space))))
        (partition w)
        (map #(reduce str %))))))

(defcheck solution-b78acbac
  (fn life-full [board]
    (letfn [(dim-board [board]
              (hash-map :rows (count board) :cols (count (first board))))
            (extend-board [board]
              (let [{:keys [rows cols]} (dim-board board)
                    emp-row (apply str (repeat (+ 2 cols) " "))]
                (vec (concat [emp-row] (map #(str " " % " ") board) [emp-row]))))
            (sub-board [board x y]
              (let [sub-rows (take 3 (drop (dec y) board))]
                (map #(subs % (dec x) (+ 2 x)) sub-rows)))
            (calculate-neighbours [sub-board]
              (let [[a b c] sub-board
                    [b1 b2 b3] b]
                (hash-map
                  :state b2
                  :neighbours (count (filter #{\#} (concat a c [b1 b3]))))))
            (live-or-die [{:keys [state neighbours]}]
              (if (= state \#)
                (if (contains? #{2 3} neighbours) \# \space)
                (if (= 3 neighbours) \# \space)))]
      (let [ext-board (extend-board board)
            {:keys [rows cols]} (dim-board board)]
        (map
          (fn [ri]
            (apply str
              (map (fn [ci] (live-or-die (calculate-neighbours (sub-board ext-board ci ri))))
                (map inc (range cols)))))
          (map inc (range rows)))))))

(defcheck solution-b82ae0a9
  (fn [b]
    (let [h                (count b)
          w                (count (first b))
          str-board        (fn [h w s]
                             (map (fn [l] (apply str (mapcat (fn [c] (if (s [l c]) "#" " ")) (range w)))) (range h)))
          cells            (set (filter identity
                                  (apply concat
                                    (map-indexed (fn [i l] (map-indexed (fn [j c] (if (= c \#) [i j])) l)) b))))
          connexity        [[1 0] [-1 0] [0 1] [0 -1] [1 1] [-1 -1] [1 -1] [-1 1]]
          neighbors        (fn [[x y]] (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) connexity))
          update-neighbors (fn [m c] (reduce #(assoc %1 %2 (inc (m %2 0))) m (neighbors c)))
          neighbor-map     (reduce update-neighbors {} cells)
          apply-rules      (fn [[c n]]
                             (cond
                               (< n 2) nil
                               (= n 2) (cells c)
                               (= n 3) c
                               :else nil))
          new-cells        (set (filter identity (map apply-rules neighbor-map)))
          new-board        (str-board h w new-cells)
          ]
      #_(println cells)
      #_(println (neighbors [0 0]))
      #_(println neighbor-map)
      #_(println)
      #_(println)
      #_(println new-cells)
      #_(map println new-board)
      new-board
      )))

(defcheck solution-b8622e96
  (fn [input]
    (let [neighbours (fn [[y x]] (filter #(not= % [y x]) (for [i (range -1 2) j (range -1 2)] [(+ y i) (+ x j)])))
          isLive     (fn [[y x]] (and (>= y 0) (< y (count input)) (>= x 0) (< x (count (input y))) (not= (nth (input y) x) \space)))
          shouldLive (fn [p]
                       (let [liveNeighbours (count (filter isLive (neighbours p)))]
                         (or (= liveNeighbours 3) (and (isLive p) (= liveNeighbours 2)))))]
      (map
        (fn [i] (apply str (map #(if (shouldLive [i %]) \# \space) (range (count (input i))))))
        (range (count input))))))

(defcheck solution-b8cd6fd2
  (fn [strng]

    (let [mx    (count (first strng))
          mpa   (reduce (fn [map line]
                          (reduce (fn [mp column]
                                    (assoc mp (vector line column)
                                              (nth (nth strng line) column)))
                            map (range mx))) {} (range mx))
          score (fn [mp [line column]]
                  (if (nil? (#{[\# 2] [\# 3] [\space 3]}
                             (vector (mp [line column])
                               (->> (for [x (range -1 2) y (range -1 2)
                                          :when (or (not= x 0) (not= y 0))]
                                      (mp [(+ line x) (+ y column)]))
                                 (filter #(= \# %)) count))))
                    \space \#))
          nw    (reduce (fn [mp [[l c] v]] (assoc mp (vector l c)
                                                     (score mpa [l c]))) {} mpa)]

      (map (fn [l] (apply str (map #(nw [l %]) (range mx)))) (range mx)))))

(defcheck solution-ba2e9004
  (fn [board]
    (letfn [(in-board? [rowi coli]
              (and (contains? board rowi)
                   (contains? (nth board rowi) coli)))
            (hood-count [rowi coli]
              (->> [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
                (map (fn [[x y]] [(+ rowi x) (+ coli y)]))
                (filter (partial apply in-board?))
                (map (fn [[x y]] (-> board (nth x) (nth y))))
                (filter #{\#})
                (count)))
            (life [item neighbours]
              (if (= item \#)
                (if (or (= neighbours 2) (= neighbours 3)) \# \space)
                (if (= neighbours 3) \# \space)))]
      (->>
        (for [[rowi row] (map-indexed vector board)
              [coli col] (map-indexed vector row)]
          (life col (hood-count rowi coli)))
        (partition (count (first board)))
        (map (partial apply str))))))

(defcheck solution-baa2e940
  (fn game-of-life [b]
    (let [board     (vec (map (comp vec seq) b))
          xmax      (count (board 0))
          ymax      (count board)
          elem      (fn [{:keys [x y board]}] ((board x) y))
          neighbors (fn [{:keys [x y board]}]
                      (reduce +
                        (for [i (range (- x 1) (+ x 2))
                              j (range (- y 1) (+ y 2))
                              :when (and (not (and (= x i) (= y j)))
                                         (and (> xmax i) (< -1 i))
                                         (and (> ymax j) (< -1 j)))]
                          (if (= \space (elem {:x i :y j :board board})) 0 1))))
          new-state (fn [{:keys [x y board] :as args}]
                      (let [neigh (neighbors args)
                            el    (elem args)]
                        (cond
                          (and (= \# el) (> 2 neigh)) \space
                          (and (= \# el) (or (= 2 neigh) (= 3 neigh))) \#
                          (and (= \# el) (< 3 neigh)) \space
                          (and (= \space el) (= 3 neigh)) \#
                          :else el)))]
      (map #(apply str %)
        (partition xmax
          (for [i (range xmax) j (range ymax)]
            (new-state {:x i :y j :board board})))))))

(defcheck solution-baf6d74b
  (fn [board]
    (let [rows          (range (count board))
          cols          (range (count (first board)))
          indices       (set (for [i rows j cols] (vector i j)))
          entry         (fn [ij] (get (get board (first ij)) (second ij)))
          alive?        (fn [x] (= (entry x) \#))
          adjacent?     (fn [a b]
                          (= (max (Math/abs (- (first a) (first b))) (Math/abs (- (second a) (second b)))) 1))
          neighbours    (fn [x] (count (filter #(and (alive? %) (adjacent? x %)) indices)))
          survival-rule {0 false 1 false 2 true 3 true 4 false 5 false 6 false 7 false 8 false}
          update-rule   (fn [x] (if (alive? x) (survival-rule (neighbours x)) (= (neighbours x) 3)))
          new-board     (set (filter #(update-rule %) indices))]
      (map (fn [r] (apply str (map #(if (new-board [r %]) \# \space) cols))) rows))))

(defcheck solution-bb6ac817
  (fn [b]
    (letfn [(within-board? [b [x y]]
              (and (>= y 0)
                   (< y (count b))
                   (>= x 0)
                   (< x (count (nth b y)))))

            (is-live? [b [x y]]
              (and
               (within-board? b [x y])
               (= \# (nth (nth b y) x))))

            (adjacent [[x y]]
              (for [i (range (- x 1) (+ x 2))
                    j (range (- y 1) (+ y 2))
                    :when (not= [i j] [x y])]
                [i j]))

            (age-cell [b xy]
              (let [n (count
                        (filter (partial is-live? b) (adjacent xy)))]
                (cond
                  (= n 2) (if (is-live? b xy) \# \ )
                  (= n 3) \#
                  :else \ )))]

      (map-indexed (fn [y row]
                     (apply str (map-indexed (fn [x cell]
                                               (age-cell b [x y]))
                                  row)))
        b))
    ))

(defcheck solution-bc285d79
  (fn game-of-life [board]
    (letfn [(row-to-pos [c [i row]]
              (->> row
                (map-indexed vector)
                (filter (comp (partial = c) second))
                (map (comp (partial conj [i]) first))))
            (board-to-pos [board c]
              (->> board
                (map-indexed vector)
                (map (partial row-to-pos c))
                (apply concat)
                set))
            (clear-board [board]
              (into [] (repeat (count board)
                         (into [] (repeat (count (first board)) \space)))))
            (set-live [next-state [a b]]
              (update-in next-state [a b] (fn [x] \#)))
            (neighbours [[row col :as pos]]
              (set (for [del-row [-1 0 1]
                         del-col [-1 0 1]
                         :when (not= [del-row del-col] [0 0])]
                     [(+ row del-row) (+ col del-col)])))]
      (let [live (board-to-pos board \#)
            dead (board-to-pos board \space)]
        (letfn [(next-state [board-view valid-populations]
                  (into {}
                    (filter
                      #(valid-populations (val %))
                      (zipmap board-view
                        (map
                          (comp
                            count
                            (partial clojure.set/intersection live)
                            neighbours)
                          board-view)))))]
          (into []
            (map
              (partial apply str)
              (reduce set-live (clear-board board)
                (into [] (keys
                           (merge (next-state live #{2 3})
                             (next-state dead #{3})))))))
          )))))

(defcheck solution-bca62e8
  (fn al [x]
    (let [c
            (fn [x] (map (fn [l] (map {\space 0 \# 1} l)) x))
          s (fn [x] (map (fn [l] (apply str (map {false \space 0 \space 1 \# true \#} l))) x))
          l (fn [x] (map (fn [l] (concat (rest l) [0])) x))
          r (fn [x] (map (fn [l] (concat [0] l)) x))
          u (fn [x] (concat (rest x) [(map (constantly 0) (first x))]))
          d (fn [x] (concat [(map (constantly 0) (first x))] x))
          y (c x)]
      (s (map
           (partial map #(or (and (= %1 1) (or (= %2 2) (= %2 3)))
                             (and (= %1 0) (= %2 3))))
           y
           (map (partial map +)
             (l y) (r y)
             (u y) (d y)
             (l (u y)) (l (d y))
             (r (u y)) (r (d y))))))))

(defcheck solution-bd556b
  (fn [b]
    (let [is-alive? (fn [x] (= \# x))
          evolve    (fn [b x y]
                      (let [
                            cell-alive?    (is-alive? (-> b (nth x) (nth y)))
                            neighbor-count (- (count
                                                (filter
                                                  is-alive?
                                                  (flatten
                                                    (map
                                                      #(->> % (take (+ y 2)) (drop (dec y)))
                                                      (->> b (take (+ x 2)) (drop (dec x))))
                                                    )))
                                              (if cell-alive? 1 0)
                                              )]
                        (cond
                          (= 3 neighbor-count) \#
                          (and cell-alive? (= 2 neighbor-count)) \#
                          :default \space)))
          ]
      (for [x (range (count b))]
        (apply str
          (for [y (range (count (first b)))]
            (evolve b x y)
            ))))))

(defcheck solution-be200523
  (fn game-of-life [board]
    (let [cell                  (fn [i j] (.charAt (board i) j))
          live-cell             \# dead-cell \space
          m                     (count board) n (count (board 0))
          live-neighbours-count (fn [i j] (count (for [k (range (dec i) (+ i 2)) l (range (dec j) (+ j 2))
                                                       :when (and (not= [k l] [i j]) (<= 0 k (dec m)) (<= 0 l (dec n))
                                                                  (= live-cell (cell k l)))] 1)))
          next-state            (fn [i j] (let [cell-state (cell i j) live-count (live-neighbours-count i j)]
                                            (if (= live-cell cell-state) (if (or (= live-count 2) (= live-count 3)) live-cell dead-cell)
                                                                         (if (= live-count 3) live-cell dead-cell))))]
      (for [i (range m)] (apply str (for [j (range n)] (next-state i j)))))))

(defcheck solution-beae668e
  (fn [board]
    (map #(apply str %) (map
                          (fn [sub-board]
                            (apply
                              (partial
                                map
                                (fn [first-row second-row last-row]
                                  (case (count (filter #{\#} (concat first-row last-row [(first second-row) (last second-row)])))
                                    3 \#
                                    2 (second second-row)
                                    \space)))
                              (map #(partition 3 1 (concat [\space] % [\space])) sub-board)))
                          (partition 3 1 (concat ["      "] board ["      "]))))))

(defcheck solution-beef8a38
  (fn [ls]
    (letfn [
            (get-cell [ls i j]
              (map #(get-in ls %) [[(dec i) (dec j)] [(dec i) j] [(dec i) (inc j)]
                                   [i (dec j)] [i (inc j)]
                                   [(inc i) (dec j)] [(inc i) j] [(inc i) (inc j)]]))
            (count-alive [ls] (count (filter #{\#} ls)))
            (next-iter [alive? alive-n-cnt]
              (cond
                (and alive? (or (= alive-n-cnt 2) (= alive-n-cnt 3))) \#
                (and (not alive?) (= alive-n-cnt 3)) \#
                :else \space))
            (f [ls]
              (for [x (range (count ls)) y (range (count (first ls)))]
                (next-iter (= (get-in ls [x y]) \#) (count-alive (get-cell ls x y)))))]
      (map #(apply str %) (partition (count (first ls)) (f ls))))))

(defcheck solution-bf26562e
  (fn gol [w]
    (let [live       \#
          dead       \space
          r          (count w)
          c          (if (zero? r) (count w) (count (first w)))
          neighbours (fn [[a b]]
                       (for [x [-1 0 1]
                             y [-1 0 1]
                             :let [p (+ a x)
                                   q (+ b y)]
                             :when (and (<= 0 p)
                                        (< p r)
                                        (<= 0 q)
                                        (< q c)
                                        (not (and (= p a) (= q b))))]
                         [p q]))
          live?      (fn [[a b]]
                       (= live (get-in w [a b])))
          next       (fn [[a b]]
                       (let [ns            (neighbours [a b])
                             live-ns-count (count (filter live? ns))
                             dead-ns-count (- (count ns) live-ns-count)]
                         (if (live? [a b])
                           (if (or (= 2 live-ns-count)
                                   (= 3 live-ns-count))
                             live
                             dead)
                           (if (= 3 live-ns-count)
                             live
                             dead))))]
      (map #(apply str %)
        (partition c (map next (for [x (range r)
                                     y (range c)]
                                 [x y])))))))

(defcheck solution-bf358768
  (fn life-next-generation [b]
    (let [index-matrix         (fn [m]
                                 (apply
                                   concat
                                   (map-indexed
                                     (fn [row-idx row]
                                       (map-indexed #(list [row-idx %1] %2) row))
                                     m)))
          neighbors            (fn [indexed-matrix]
                                 (->>
                                   (group-by
                                     first
                                     (mapcat
                                       (fn [[[row col] value]]
                                         (for [x [-1 0 1]
                                               y [-1 0 1]
                                               :when (not (and (zero? x) (zero? y)))]
                                           (list [(+ x row) (+ y col)] value)))
                                       indexed-matrix))
                                   (map (fn [[idx cells]]
                                          (list
                                            idx
                                            (map #(second %) cells))))
                                   (filter
                                     (fn [[idx value]]
                                       (not-any? neg? idx)))))
          cells-with-neighbors (fn cells-with-neighbors [m]
                                 (let [indexed-matrix (index-matrix m)
                                       cell-neighbors (neighbors indexed-matrix)
                                       values-only    (reduce
                                                        #(assoc %1 (first %2) {:value (second %2) :neighbors ()})
                                                        {}
                                                        indexed-matrix)]
                                   (reduce
                                     (fn [matrix-hash [idx neighbor-values]]
                                       (update-in
                                         matrix-hash
                                         [idx :neighbors]
                                         #(concat % neighbor-values)))
                                     values-only
                                     cell-neighbors
                                     )
                                   ))
          next-generation      (fn [cells]
                                 (map
                                   (fn [[idx cell]]
                                     (let [live-neighbors (->>
                                                            (:neighbors cell)
                                                            (filter #(= \# %))
                                                            (count))]
                                       (if (= \# (:value cell))
                                         (if (and (< 1 live-neighbors)
                                                  (> 4 live-neighbors))
                                           (list idx \#)
                                           (list idx \space))
                                         (if (= live-neighbors 3)
                                           (list idx \#)
                                           (list idx \space)))))
                                   cells))
          form-board           (fn [cells]
                                 (->>
                                   cells
                                   (map
                                     (fn [[idx value]]
                                       (list (first idx) (list (second idx) value))))
                                   (group-by first)
                                   (sort-by first)
                                   (map (fn [[_ values]]
                                          (->>
                                            values
                                            (map second)
                                            (sort-by first)
                                            (map second)
                                            (drop-last)
                                            )))
                                   (map #(apply str %))
                                   ))]
      (->> b
        (map seq)
        (cells-with-neighbors)
        (next-generation)
        (form-board)
        (drop-last)
        ))))

(defcheck solution-bf3c7843
  (fn game-of-life [board]
    (let [rows           (count board)
          cols           (count (first board))
          cells          (for [i (range rows) j (range cols)] [i j])
          neighbors      (fn [[i j]] (for [di [-1 0 1]
                                           dj [-1 0 1]
                                           :when (not= di dj 0)] [(+ i di) (+ j dj)]))
          live-cell?     (fn [cell] (= (get-in board cell) \#))
          live-neighbors (fn [cell] (count (filter live-cell? (neighbors cell))))
          next-state     (fn [cell]
                           (let [ln (live-neighbors cell)]
                             (if (live-cell? cell)
                               (if (or (= ln 2) (= ln 3)) \# \space)
                               (if (= ln 3) \# \space))))]
      (map #(apply str %) (partition cols (map next-state cells))))))

(defcheck solution-bf600a39
  (fn next-game-of-life-board [board]
    (letfn [(neighbors-coordinates [[x y] size-x size-y]
              (let [deltas [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
                (filter (fn [[x y]]
                          (and (>= x 0)
                               (>= y 0)
                               (< x size-x)
                               (< y size-y)))
                  (map (fn [[dx dy]]
                         [(+ x dx) (+ y dy)])
                    deltas))))
            (neighbors [board xy]
              (let [size-x (count (first board))
                    size-y (count board)]
                (map #(get-in board %)
                  (neighbors-coordinates xy size-x size-y))))
            (cell-score [board xy]
              (count (filter #{\#}
                       (neighbors board xy))))
            (next-cell-value [board xy]
              (let [cell-val   (get-in board xy)
                    cell-score (cell-score board xy)]
                (condp = cell-val
                  \space (if (= cell-score 3) \# \space)
                  \# (if (some #{cell-score} [2 3]) \# \space))))]
      (let [size-x (count (first board))
            size-y (count board)]
        (->> (for [x (range size-x) y (range size-y)] [x y])
          (map #(next-cell-value board %))
          (partition size-x)
          (map #(apply str %)))))))

(defcheck solution-bf71ff81
  (fn [s]
    (map
      #(reduce str %)
      (let [n (count s)
            c (fn [i j] (if (and (>= i 0)
                                 (>= j 0)
                                 (< i n)
                                 (< j n))
                          (get (get
                                 (let [x (fn [t]
                                           (reduce #(conj % %2) [] t))]
                                   (reduce #(conj % (x %2)) [] s))
                                 i)
                            j)))]
        (partition n
          (for [i (range 0 n) j (range 0 n)]
            (let [q (c i j)
                  v (count (filter
                             #(= % \#)
                             (for [x [-1 0 1] y [-1 0 1]]
                               (if (and (= x 0) (= y 0)) nil (c (+ x i) (+ y j))))
                             )
                      )
                  ]
              (if (or
                   (and (= q \#) (or (= v 2) (= v 3)))
                   (and (= q \space) (= v 3)))
                \#
                \space))
            )
          )
        )
      )
    ))

(defcheck solution-bf882871
  (fn [b]
    (letfn [(n [r c]
              (for [i (range -1 2)
                    j (range -1 2)
                    :when (not= 0 i j)]
                (get-in b [(+ r i) (+ c j)])))]
      (let [r (count b) c (count (first b))]
        (->> (for [i (range r)
                   j (range c)]
               (let [l (count (filter #(= \# %) (n i j)))]
                 (cond
                   (and (= l 2)
                        (= \# (get-in b [i j]))) \#
                   (= l 3) \#
                   :else \space)))
          (partition c)
          (map #(apply str %)))))))

(defcheck solution-bfe21e68
  (fn [board]
    (letfn [(map2 [op xs ys] (loop [x xs y ys z (empty xs)]
                               (if (or (empty? x) (empty? y))
                                 z
                                 (recur (rest x) (rest y) (conj z (op (first x) (first y)))))))

            (neightbour [xs d]
              (filter (fn [t] (and (every? #{-1 1 0} (map2 - d t))
                                   (not-every? #(= 0 %) (map2 - d t)))) xs))
            (posset [board]
              (for [x (range (count board)) y (range (count (first board)))] [x y]))
            (pos-val [board p]
              ((apply vector (board (first p))) (second p)))
            ]

      (let [pset (posset board)]

        (map #(apply str %) (partition (count (first board))
                              (for [p pset]
                                (let [Ln (count (filter #(= \# %) (map #(pos-val board %) (neightbour pset p))))]
                                  (do                       ;(println (pos-val board p))
                                    (cond
                                      (and (= (pos-val board p) \#) (< Ln 2)) \space
                                      (and (= (pos-val board p) \#) (or (= Ln 2) (= Ln 3))) \#
                                      (and (= (pos-val board p) \#) (> Ln 3)) \space
                                      (and (= (pos-val board p) \space) (= Ln 3)) \#
                                      :else \space
                                      ))))

                              ))))))

(defcheck solution-c03b8681
  (fn game-of-life
    [board]
    (let [rows        (count board)
          columns     (count (first board))
          cell        (fn [x y] (if (and (<= 0 x (dec columns))
                                         (<= 0 y (dec rows)))
                                  (nth (board y) x)))
          neighbor-xy (list [0 -1] [0 1] [-1 0] [1 0] [-1 -1] [-1 1] [1 -1] [1 1])
          neighbors   (fn [x y]
                        (map #(cell (+ x (% 0)) (+ y (% 1))) neighbor-xy))]
      (->> (for [y (range 0 rows)
                 x (range 0 columns)]
             (vector (cell x y) (count (filter #{\#} (neighbors x y)))))
        (map #(if (or (and (= \# (% 0)) (<= 2 (% 1) 3))
                      (and (= \space (% 0)) (= 3 (% 1)))) \# \space))
        (partition columns)
        (map #(apply str %))))))

(defcheck solution-c0c0988b
  (fn game-of-life [board]
    (let [width  (count (first board))
          height (count board)]
      (letfn [(read []
                (vec (map (fn [line]
                            (vec (map (fn [c] ({\space :d \# :a} c)) line)))
                       board)))
              (at [game i j]
                (if (or (>= i width) (>= j height) (< i 0) (< j 0))
                  :d
                  ((game j) i)))
              (neighs [game i j]
                (for [x (range (dec i) (+ i 2))
                      y (range (dec j) (+ j 2))
                      :when (not (and (= x i) (= y j)))]
                  (at game x y)))
              (alive-neighs [game i j]
                (count (filter #{:a} (neighs game i j))))
              (to-str [game]
                (map (fn [line]
                       (apply str (map {:a "#" :d " "} line))) game))
              (step [game]
                (partition width
                  (for [y (range height)
                        x (range width)
                        :let [alive (alive-neighs game x y)]]
                    (if (= :a (at game x y))
                      (if (#{2 3} alive) :a :d)
                      (if (= 3 alive) :a :d)))))]
        (let [game (read)
              next (step game)]
          (to-str next)
          )))))

(defcheck solution-c0f9e7fe
  (fn [b]
    (letfn [(neighs [b i j]
              (for [u (map #(+ i %) (range -1 2))
                    v (map #(+ j %) (range -1 2))
                    :when (and (< -1 u (count b))
                               (< -1 v (count (first b)))
                               (not (and (= u i) (= v j))))]
                (get-in b [u v])))

            (lives [b i j]
              (->> (neighs b i j)
                (filter #(= \# %))
                count))

            (next-cell [b i j]
              (let [alive? (= \# (get-in b [i j]))
                    ls     (lives b i j)]
                (cond
                  (and alive? (< ls 2)) \space
                  (and alive? (<= 2 ls 3)) \#
                  (and alive? (> ls 3)) \space
                  (and (not alive?) (= ls 3)) \#
                  :else \space)))

            (next-row [b i]
              (clojure.string/join "" (map #(next-cell b i %)
                                        (range (count (first b))))))]

      (mapv #(next-row b %)
        (range (count b))))))

(defcheck solution-c15a6547
  (fn conway [sarray]
    (let
     [dim         (count sarray)
      neighbours  (fn [[x y]]
                    (map (fn [a] (map #(mod % dim) a))
                      (for [dx [-1 0 1] dy [-1 0 1] :when (not= [0 0] [dx dy])]
                        (map - [x y] [dx dy]))))
      is-alive    (fn [[x y]] (= \# (nth (nth sarray y) x)))
      alive-cells (filter is-alive (for [x (range dim) y (range dim)] [x y]))]
      (map #(apply str %)
        (partition dim
          (for [y (range dim) x (range dim)]
            (let [count-alive-neighbours
                  (count (clojure.set/intersection (set alive-cells)
                           (set (neighbours [x y]))))]
              (cond (> count-alive-neighbours 3) \space
                    (< count-alive-neighbours 2) \space
                    (= count-alive-neighbours 3) \#
                    :else (nth (nth sarray y) x)))))))))

(defcheck solution-c16d1b09
  (fn life [generation]
    (let [gensize   (count generation)
          alive?    (fn [x y] (and (> x -1) (> y -1) (< x gensize) (< y gensize) (= (nth (nth generation y) x) \#)))
          neighbors (fn [x y]
                      (reduce #(if (alive? (+ x (first %2)) (+ y (last %2)))
                                 (inc %) %)
                        0 [[-1 -1] [0 -1] [1 -1]
                           [-1 0] [1 0]
                           [-1 1] [0 1] [1 1]]))]
      (reduce #(conj %1 (apply str %2)) []
        (partition gensize
          (apply str
            (for [y (range gensize)
                  x (range gensize)
                  :let [a?   (alive? x y)
                        sibs (neighbors x y)]]
              (cond (and a? (or (< sibs 2) (> sibs 3))) " "
                    (and a? (or (= 2 sibs) (= 3 sibs))) "#"
                    (and (not a?) (= 3 sibs)) "#"
                    :else " "))))))))

(defcheck solution-c222d933
  (fn [board]
    (let
     [width       (count board)
      height      (count (first board))
      coords      (set
                    (for [x (range width)
                          y (range height)
                          :when (= (get-in board [x y]) \#)]
                      [x y]))
      neighbours  (fn [[x y]] (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)] [(+ x dx) (+ y dy)]))
      new-coords  (set
                    (for [[loc n] (frequencies (mapcat neighbours coords))
                          :when (or (= n 3) (and (= n 2) (coords loc)))]
                      loc))
      empty-board (vec (repeat width (vec (repeat height \ ))))]
      (mapv (partial apply str) (reduce #(assoc-in % %2 \#) empty-board new-coords)))))

(defcheck solution-c2b6db78
  (fn generate-new [board]
    (let
     [get-at           (fn [board [x y]]
                         (-> board
                           (nth x)
                           (nth y)))
      board-size       (fn [board]
                         [(dec (count board)) (dec (count (first board)))])
      neighbour-coords (fn [board [x y]]
                         (let [[sz-x sz-y] (board-size board)]
                           (->> (for [ix (range (dec x) (+ 2 x))
                                      iy (range (dec y) (+ 2 y))]
                                  [ix iy])
                             (remove #(or
                                       (neg? (first %))
                                       (neg? (second %))
                                       (> (first %) sz-x)
                                       (> (second %) sz-y)
                                       (= [x y] [(first %) (second %)]))))))
      get-neighbours   (fn [board [x y]]
                         (map #(get-at board %) (neighbour-coords board [x y])))
      count-neighbours (fn [board [x y]]
                         (count (filter #(not= \space %) (get-neighbours board [x y]))))
      decide-fate      (fn [board [x y]]
                         (let [curval   (get-at board [x y])
                               is-alive (not= \space curval)
                               nbrs     (count-neighbours board [x y])]
                           (cond
                             (and is-alive (< nbrs 2)) \space
                             (and is-alive (<= 2 nbrs 3)) \#
                             (and is-alive (> nbrs 3)) \space
                             (and (not is-alive) (= nbrs 3)) \#
                             :else curval)))]
      (->>
        (let [[sz-x sz-y] (board-size board)]
          (for [x (range (inc sz-x))]
            (for [y (range (inc sz-y))]
              (decide-fate board [x y]))))
        (mapv #(apply str %))))))

(defcheck solution-c38790fb
  (fn life [board]
    (let [num-rows (count board)
          num-cols (count (board 0))
          legal    (fn [[y x]] (and (< -1 y num-rows) (< -1 x num-cols)))
          deltas   [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
          live-neighbors
                   (fn [[y x]]
                     (->> deltas
                       (map (fn [[dy dx]] [(+ y dy) (+ x dx)]))
                       (filter legal)
                       (filter #(= \# (get-in board %)))
                       count
                       ))
          ]
      (for [y (range num-rows)]
        (apply str
          (for [x (range num-cols)]
            (let [live (live-neighbors [y x])]
              (if (= \# (get-in board [y x]))
                (cond
                  (< live 2) \space
                  (< live 4) \#
                  :else \space)
                (if (= 3 live) \# \space)
                ))))))))

(defcheck solution-c421776b
  (fn [d]
    (let [model       (vec (map vec d))
          r-e         (range (count model))
          r-i         (range (count (get model 0)))
          places      (fn [data r c]
                        (let [e   (> c 0)
                              w   (< c (dec (count (get data 0))))
                              n   (> r 0)
                              s   (< r (dec (count data)))
                              nw  (and n w)
                              sw  (and s w)
                              se  (and e s)
                              ne  (and n e)
                              res (->>
                                    (map (fn [one two]
                                           (when one two))
                                      [n nw w sw s se e ne]
                                      [[(dec r) c]
                                       [(dec r) (inc c)]
                                       [r (inc c)]
                                       [(inc r) (inc c)]
                                       [(inc r) c]
                                       [(inc r) (dec c)]
                                       [r (dec c)]
                                       [(dec r) (dec c)]
                                       ])
                                    (filter (complement nil?))
                                    (map (partial get-in data))
                                    )]
                          res))
          conway-laws (fn [r c] (let [current   (get (get model r) c)
                                      the-lives (count (filter (partial = \#) (places model r c)))]
                                  (if (= \# current)
                                    (if (< the-lives 2)
                                      \space
                                      (if (<= the-lives 3)
                                        \#
                                        \space
                                        ))
                                    (if (= 3 the-lives)
                                      \#
                                      \space
                                      )
                                    )
                                  ))]
      (vec (map (fn [r] (apply str (map (fn [c] (conway-laws r c)) r-i))) r-e))
      )
    ))

(defcheck solution-c4233c23
  (fn next-gen
    [state]
    (let [h               (count state)
          w               (count (first state))
          binary-state    (mapv (fn [row] (mapv #(if (= % \space) 0 1) row))
                            state)
          catern-product  (fn [l1 l2] (for [i l1 j l2] [i j]))
          cords           (catern-product (range h) (range w))
          deltas          (filter #(not= % [0 0]) (catern-product [-1 0 1] [-1 0 1]))
          get-value       (fn [x y] (get (get binary-state x) y 0))
          neighbor-number (fn [i j] (apply + (for [[di dj] deltas] (get-value (+ i di) (+ j dj)))))
          has-life        (fn [i j] (if (= (get-value i j) 1)
                                      (<= 2 (neighbor-number i j) 3)
                                      (= 3 (neighbor-number i j))))]
      (mapv (fn [i] (apply str (map (fn [j] (if (has-life i j) \# \space)) (range w))))
        (range h))
      )))

(defcheck solution-c44c30e5
  (fn [board]
    (letfn [(count-alive-neighbors [y x]
              (->> (for [dy [-1 0 1]
                         dx [-1 0 1] :when (not= 0 dy dx)]
                     [(+ y dy) (+ x dx)])
                (map (partial get-in board))
                (filter #{\#})
                (count)))]
      (let [h (count board)
            w (count (first board))]
        (map (fn [i]
               (apply str (map (fn [j]
                                 (let [n    (count-alive-neighbors i j)
                                       live (= (get-in board [i j]) \#)]
                                   (cond
                                     (and live (< n 2)) \space
                                     (and live (<= 2 n 3)) \#
                                     (and live (> n 3)) \space
                                     (and (not live) (= n 3)) \#
                                     :else \space)))
                            (range w))))
          (range h))))))

(defcheck solution-c4903f8a
  (fn [i-board]
    (let [
          c-count          (count (first i-board))
          r-count          (count i-board)
          in-board         (fn [i-coor]
                             (let [[i-row i-col] i-coor]
                               (and
                                (>= i-row 0)
                                (< i-row r-count)
                                (>= i-col 0)
                                (< i-col c-count))))
          has-life         (fn [i-coor]
                             (= \# (nth (nth i-board (first i-coor)) (last i-coor))))
          board-coor       (reduce
                             concat
                             '()
                             (map
                               (fn [my-row]
                                 (map #(vector my-row %) (range c-count)))
                               (range r-count)))
          live-list        (filter has-life board-coor)
          dead-list        (filter #(not (has-life %)) board-coor)
          neighbour        (fn [i-coor]
                             (let [[i-row i-col] i-coor]
                               (filter
                                 #(let [[my-row my-col] %] (not (and (= my-row i-row) (= my-col i-col))))
                                 (reduce
                                   concat
                                   '()
                                   (map
                                     (fn [i-coors]
                                       (filter in-board i-coors))
                                     (map
                                       (fn [my-row]
                                         (map
                                           (fn [my-col]
                                             (vector my-row my-col))
                                           (range (dec i-col) (+ i-col 2))))
                                       (range (dec i-row) (+ i-row 2))))))))
          living-neighbour (fn [i-coor] (filter has-life (neighbour i-coor)))
          lives-on         (fn [i-coor]
                             (let [n-count (count (living-neighbour i-coor))]
                               (or (= 2 n-count) (= 3 n-count))))
          reproducible     (fn [i-coor]
                             (= 3 (count (living-neighbour i-coor))))
          new-lives        (group-by
                             #(first %)
                             (concat
                               (filter lives-on live-list)
                               (filter reproducible dead-list)))
          draw-row         (fn plot-lives
                             ([i-row] (plot-lives [] 0 (sort (map last (get new-lives i-row)))))
                             ([result i-pos i-lives]
                              (if (= c-count i-pos)
                                (apply str result)
                                (let [c-life (first i-lives) n-pos (inc i-pos)]
                                  (if (= i-pos c-life)
                                    (plot-lives (conj result \#) n-pos (rest i-lives))
                                    (plot-lives (conj result \space) n-pos i-lives))))))
          ]
      (map draw-row (range r-count)))))

(defcheck solution-c52693e
  (fn update-board [b]
    (letfn [(cell [x, y, b]
              (nth (nth b y) x))
            (alive [x, y, b]
              (and
               (> y 0)
               (> x 0)
               (< y (count b))
               (< x (count (nth b y)))
               (= \# (cell x y b))))
            (nbrs [x, y, b]
              (count (filter true?
                       [(alive (dec x) (dec y) b)
                        (alive x (dec y) b)
                        (alive (inc x) (dec y) b)
                        (alive (dec x) y b)
                        (alive (inc x) y b)
                        (alive (dec x) (inc y) b)
                        (alive x (inc y) b)
                        (alive (inc x) (inc y) b)])))
            (update-row [width, b, y]
              (apply str
                (map (fn [x]
                       (let [c (nbrs x y b)]
                         (if (or
                              (= c 3)
                              (and (= c 2) (= \# (cell x y b))))
                           \#
                           \ )))
                  (range width))))]
      (let [height (count b)
            width  (count (nth b 0))]
        (map (partial update-row width b) (range height))))))

(defcheck solution-c56f60c4
  (fn [board]
    (let [dead            \space
          live            \#
          height          (count board)
          width           (count (first board))
          possible-coords (fn [x y]
                            (let [xs [(dec x) x (inc x)]
                                  ys [(dec y) y (inc y)]]
                              (mapcat (fn [a] (map #(vec [a %]) ys)) xs)))
          inbounds?       (fn [[x y]]
                            (and (>= x 0) (< x width) (>= y 0) (< y height)))
          neighbors       (fn [x y]
                            (filter inbounds? (possible-coords x y)))
          live?           (fn [coords]
                            (= (get-in board coords) live))
          live-neighbors  (fn [x y]
                            (count (filter live? (neighbors x y))))
          next-gen        (fn [x y]
                            (let [ln (live-neighbors x y)]
                              #_(if (= x 2) (println [x y ln]))
                              (if (live? [x y])
                                (cond (< ln 3) dead
                                      (> ln 4) dead
                                      :else live)
                                (if (= ln 3) live dead))))
          xs              (range 0 width)
          ys              (range 0 height)]
      (map (fn [x]
             (apply str (map (fn [y] (next-gen x y))
                          ys)))
        xs))))

(defcheck solution-c764c587
  (letfn [(k [c x] (<= 0 x c))
          (g [b c [x y]] (if (and (k c x) (k c y) (= \# (get-in b [x y]))) 1 0))
          ]
    (fn [b]
      (let [b (vec (map vec b))
            c (count (first b))
            g (partial g b c)]
        (map #(apply str %)
          (reduce
            (fn [a k]
              (let [
                    l
                      (reduce #(+ % (g %2)) 0
                        (map
                          #(map + % k)
                          (for [i (range -1 2) j (range -1 2) :when (or (not= i 0) (not= j 0))] [i j])
                          )
                        )
                    ? (= 1 (g k))]
                (assoc-in a k
                  (cond
                    (< l 2) " "
                    (and (#{2 3} l) ?) \#
                    (and (> l 3) ?) " "
                    (and (= l 3) (not ?)) \#
                    :else
                    (if ? \# " ")
                    )
                  )
                )
              )
            b
            (for [x (range c) y (range c)] [x y])
            ))))))

(defcheck solution-c7f0fb05
  (fn game [b]
    (letfn [(foo [b] (fn [c] (let [r (get-in b c) s (= r \#)] (if s 1 0))))
            (r2 [x] (range (dec x) (+ 2 x)))
            (circ [[x y]] (for [i (r2 x) j (r2 y) :when (or (not= i x) (not= j y))] [i j]))
            (n-alive [b c] (reduce + (map (foo b) (circ c))))
            (surv [b c] (let [n (n-alive b c)]
                          (cond
                            (and (= ((foo b) c) 1) (#{2, 3} n)) \#
                            (= 3 n) \#
                            :else \space)))
            (row [i r] (apply str (map-indexed (fn [j _]
                                                 (surv b [i j]))
                                    r)))]
      (map-indexed row b))))

(defcheck solution-c80b1b61
  (fn game-of-life [input]
    (let [board      (mapv vec input)
          height     (count board)
          width      (count (first board))
          all-coords (partition width
                       (for [y (range height)
                             x (range width)]
                         [x y]))]
      (letfn [(board-to-str [board]
                (mapv (partial apply str) board))

              (neighbours-coords [[x y]]
                (for [xs [(dec x) x (inc x)]
                      ys [(dec y) y (inc y)]
                      :when (not (and (= xs x) (= ys y)))]
                  [xs ys]))

              (neighbours [[x y]]
                (map #(get-in board %) (neighbours-coords [y x])))

              (neighbours-count [[x y]]
                (count
                  (filter #(= \# %) (neighbours [x y]))))

              (alive? [[x y]]
                (= \#
                  (get-in board [y x])))

              (lives? [[x y]]
                (let [n (neighbours-count [x y])]
                  (if (alive? [x y])
                    (cond
                      (< n 2) \space                        ;; Lonely
                      (> n 3) \space                        ;; Overpopulation
                      :else \#)
                    (cond
                      (= 3 n) \#                            ;; Reproduction
                      :else \space))))]
        (board-to-str
          (mapv
            (fn [row] (mapv (fn [cell] (lives? cell)) row))
            all-coords))))))

(defcheck solution-c80c503d
  (fn life [b]
    (let [swell   (fn [b]
                    (reduce into [] [[(apply str (repeat (+ 2 (count (first b))) " "))]
                                     (map #(str " " % " ") b)
                                     [(apply str (repeat (+ 2 (count (first b))) " "))]]))
          strings (mapv vec (swell b))
          rows    (count strings)
          columns (count (first strings))]
      (map #(apply str %)
        (partition (- rows 2)
          (for [i (range 1 (dec rows))
                j (range 1 (dec columns))]
            (let [cnt (reduce
                        +
                        (for [x [(dec i) i (inc i)]
                              y [(dec j) j (inc j)]
                              :when (not (and (= x i) (= y j)))]
                          (if (= ((strings x) y) \space) 0 1)))]
              (if (= ((strings i) j) \space)
                (if (= cnt 3) \# \space)
                (if (or (= cnt 2) (= cnt 3)) \# \space)))))))))

(defcheck solution-c830f54e
  (fn next-gen [board]
    (let [cartesian (fn [coll1 coll2]
                      (for [x coll1 y coll2] [x y]))
          nei-alive (fn [board cell]
                      (reduce + (for [dx [-1 0 1]
                                      dy [-1 0 1]
                                      :when (not= 0 dx dy)
                                      :let [nei-cell  (map + cell [dx dy])
                                            nei-value ({\# 1} (get-in board nei-cell))]
                                      :when nei-value]
                                  nei-value)))
          height    (count board)
          width     (count (first board))]
      (->> (map (partial map list)
             (->> board
               (map (partial map {\space 0 \# 1})))
             (->> (cartesian (range height)
                    (range width))
               (map (partial nei-alive board))
               (partition width)))
        (map (partial map (fn [[alive nei-alive]]
                            (if (or (and (= 0 alive)
                                         (= 3 nei-alive))
                                    (and (= 1 alive)
                                         (<= 2 nei-alive 3)))
                              1
                              0))))
        (map (partial map {0 \space 1 \#}))
        (map (partial apply str))
        vec))))

(defcheck solution-c8e2e6b6
  (fn solve [m]
    (letfn [(transpose [m]
              (apply map list m))
            (ext [m]
              (let [empty-line [(repeat \.)]]
                (transpose (concat empty-line
                             (transpose (concat empty-line
                                          m
                                          empty-line))
                             empty-line))))
            (windows [m]
              (transpose
                (map (fn [ys] (partition 3 1 ys))
                  (transpose
                    (map (fn [xs] (partition 3 1 xs)) (ext m))))))
            (freqs [wm]
              (map (fn [xs]
                     (map (fn [ys]
                            ((frequencies (flatten ys)) \#)
                            ) xs))
                wm))
            (next-gen [m]
              (map (fn [xs cs]
                     (map (fn [x c]
                            (if (= x \#)
                              (if (#{3 4} c) \# \space)
                              (if (= 3 c) \# \space)))
                       xs cs))
                m (freqs (windows m))))]
      (map #(apply str %) (next-gen m)))))

(defcheck solution-c989f99e
  (fn [board]
    (map #(apply str %)
      (let [bvboard (apply vector
                      (for [row board]
                        (apply vector
                          (map #(if (= \# %) 1 0) (seq row)))))
            size    (count bvboard)]
        (letfn [(check [x y]
                  (if (or (< x 0) (< y 0) (> x (dec size)) (> y (dec size)))
                    0
                    ((bvboard x) y)))]
          (for [x (range size)]
            (for [y (range size)]
              (let [nc   (+ (check (dec x) y)
                           (check (dec x) (inc y))
                           (check x (inc y))
                           (check (inc x) (inc y))
                           (check (inc x) y)
                           (check (inc x) (dec y))
                           (check x (dec y))
                           (check (dec x) (dec y)))
                    live (= ((bvboard x) y) 1)]
                (cond
                  (and live (< nc 2)) \space
                  (and live (= nc 2)) \#
                  (and live (= nc 3)) \#
                  (and (not live) (= nc 3)) \#
                  :else \space)))))))))

(defcheck solution-ca25e1e9
  (fn [b]
    (let [h  (count b)
          hr (range 0 h)
          w  (count (first b))
          wr (range 0 w)]
      (for [hi hr]
        (apply str
          (for [wi wr]
            (let [live     \#
                  dead     \space
                  live?    #(= live %)
                  dead?    #(not (live? %))
                  live-nbs #(count (filter live? %))
                  cell     (get (get b hi) wi)
                  nbs      (map
                             (fn [[aw ah]] [(+ wi aw) (+ hi ah)])
                             [[-1 -1] [-1 0] [-1 1] [0 1] [0 -1] [1 -1] [1 0] [1 1]])
                  nbs2     (filter
                             (fn [[aw ah]]
                               (not (or (neg? aw)
                                        (neg? ah)
                                        (>= aw w)
                                        (>= ah h))))
                             nbs)
                  nbs3     (map (fn [[wi hi]]
                                  (get (get b hi) wi))
                             nbs2)]
              (cond
                (and (dead? cell)
                     (= 3 (live-nbs nbs3))) live
                (and (live? cell)
                     (or (= 2 (live-nbs nbs3))
                         (= 3 (live-nbs nbs3)))) live
                :default dead
                ))))))))

(defcheck solution-ca30fe24
  (fn [board-in]
    (letfn [(indices [el coll] (map first (filter #(= el (second %)) (map-indexed vector coll))))
            (to-tuples [idx-coll] (let [[i coll] idx-coll] (map vector (repeat i) coll)))
            (get-board-dims [board-list] (vector (count board-list) (count (first board-list))))
            (get-all-cells [dims] (let [[nrow ncol] dims rows (range nrow) cols (range ncol) tups (map #(vector % cols) rows)] (apply concat (map to-tuples tups))))
            (parse-rows [el board-list] (apply concat (map to-tuples (map-indexed (fn [i coll] (vector i (indices el (map str (seq coll))))) board-list))))
            (dist [cell-one cell-two] (let [[x1 y1] cell-one [x2 y2] cell-two] (Math/sqrt (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2)))))
            (cell-dists [cell cell-list] (map (partial dist cell) cell-list))
            (count-live-neighbors [live-cells cell] (count (filter #(and (< 0 %) (> 2 %)) (cell-dists cell live-cells))))
            (will-live [cell live-neighbors is-live] (when (or (and is-live (or (= 2 live-neighbors) (= 3 live-neighbors))) (and (complement is-live) (= 3 live-neighbors))) cell))
            (get-live-cells [cells live-neighbors is-live] (into [] (filter (complement nil?) (map will-live cells live-neighbors is-live))))
            (build-board [dims live-cells] (let [[nrow ncol] dims row-groups (group-by first live-cells)] (loop [row 0 col 0 live-row (map second (get row-groups 0)) this-row "" out []] (if (and (= row nrow) (= col ncol)) out (cond (= col ncol) (recur (inc row) 0 (map second (get row-groups (inc row))) "" (conj out this-row)) (= col (first live-row)) (recur row (inc col) (next live-row) (str this-row "#") out) :else (recur row (inc col) live-row (str this-row " ") out))))))
            ]
      (let [dims           (get-board-dims board-in)
            cells          (get-all-cells dims)
            live-now       (parse-rows "#" board-in)
            live-neighbors (map (partial count-live-neighbors live-now) cells)
            is-live        (map #(if (seq (indices % live-now)) true false) cells)
            live-next      (get-live-cells cells live-neighbors is-live)
            ]
        (build-board dims live-next)
        ))))

(defcheck solution-ca7f7bf4
  (fn [board]
    (let [next'       (fn [board y x]
                        (let [ns
                              (reduce
                                (fn [s [y' x']]
                                  (+ s ({\# 1} (aget board (+ y y') (+ x x')) 0)))
                                0
                                [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])]
                          ((if (= \# (aget board y x)) {2 \# 3 \#} {3 \#}) ns \space)))
          size        (+ (count board) 2)
          empty-row   [(apply str (into [] (repeat size \space)))]
          padded-rows (map #(str \space % \space) board)
          board'      (to-array-2d (concat empty-row padded-rows empty-row))]
      (mapv
        #(apply str %)
        (for [y (range 1 (dec size))]
          (for [x (range 1 (dec size))]
            (next' board' y x)))))))

(defcheck solution-ca9b1716
  (letfn [(split-3 [s]
            (for [i (range (- (count s) 2))]
              (.substring s i (+ i 3))))
          (split-rows [ss]
            (partition 3 1 ss))
          (life [a b c]
            (let [live? (= \# (second b))
                  total (count (filter #(= % \#) (str a b c)))]
              (cond
                (and live? (<= 3 total 4)) "#"
                (and (not live?) (= total 3)) "#"
                :else " ")))
          (expand [ss]
            (let [len (+ 2 (count (first ss)))]
              (concat [(apply str (repeat len " "))]
                (map #(str " " % " ") ss)
                [(apply str (repeat len " "))])))]
    #(map (partial apply str)
       (map (partial apply map life)
         (split-rows (map split-3 (expand %)))))))

(defcheck solution-cb0a22d
  (fn [b]
    (let [board-width (count (first b))
          board       (for [x (range board-width) y (range board-width)] [x y])
          board-map   (zipmap board (->> b (apply str) vec))]
      (for [r (partition board-width
                (for [cursor board]
                  (->> (filter (fn [[k v]] (and (contains? #{1 0 -1} (- (first k) (first cursor)))
                                                (contains? #{1 0 -1} (- (last k) (last cursor)))
                                                (not= k cursor))) board-map)
                    vals
                    frequencies
                    ((fn [neighbors] (if (or (and (= (get board-map cursor) \#) (contains? #{2 3} (get neighbors \#)))
                                             (and (= (get board-map cursor) \space) (= 3 (get neighbors \#))))
                                       "#" " "))))))]
        (clojure.string/join r)))))

(defcheck solution-cb234263
  (fn [b]
    (letfn
     [
      (v [x y]
        (if
         (and
          (>= y 0)
          (< y (count b))
          (>= x 0)
          (< x (count b))
          (= (nth (nth b y) x) \#)
          )
          1
          0
          )
        )
      (n [x y]
        (+ (v (dec x) (dec y)) (v x (dec y)) (v (inc x) (dec y)) (v (dec x) y) (v (inc x) y) (v (dec x) (inc y)) (v x (inc y)) (v (inc x) (inc y)))
        )
      ]
      (for [y (range (count b))]
        (apply str
          (for [x (range (count b))]
            (case (n x y)
              2 (if (= (v x y) 1) \# \space)
              3 \#
              \space
              )
            )
          )
        )
      )
    ))

(defcheck solution-cb28eefc
  (letfn [
          (gamestr->vec [g]
            (let [xf {\space :dead \# :live}]
              (vec (map #(vec (map xf (seq %))) g))))
          (vec->gamestr [v]
            (let [xf {:dead \space :live \#}]
              (map (fn [row] (apply str (map xf row))) v)
              ))
          (nb-idxs [x y]
            [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]
             [(dec x) y] [(inc x) y]
             [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]])
          (count-nbs [game x y status]
            (count
              (for [[nx ny] (nb-idxs x y)
                    :when (#{status} (get-in game [ny nx]))] 1)))
          (summarize-nbs [game x y]
            [(count-nbs game x y :live)
             (count-nbs game x y :dead)])]
    (fn [game]
      (let [gvec   (gamestr->vec game)
            height (count gvec)
            width  (count (gvec 0))]
        (vec->gamestr (reduce
                        (fn [v [x y st]]
                          (update-in v [y x] (fn [_ b] b) st))
                        gvec
                        (for [x (range 1 (dec width))
                              y (range 1 (dec height))
                              :let [live? (#{:live} (get-in gvec [y x]))
                                    [nlive ndead] (summarize-nbs gvec x y)]]
                          [x y (if live?
                                 (cond
                                   (< nlive 2) :dead
                                   (> nlive 3) :dead
                                   :else :live)
                                 (cond
                                   (= nlive 3) :live
                                   :else :dead))])))))))

(defcheck solution-cb3ff434
  (fn [text]
    (letfn [(text->full-board [text]
              (apply concat
                (map-indexed (fn [y r]
                               (map-indexed (fn [x c]
                                              [x y c])
                                 r))
                  text)))
            (input-alive? [[x y c]]
              (not= \space c))
            (max-x [full-board]
              (apply max (map first full-board)))
            (max-y [full-board]
              (apply max (map second full-board)))
            (board->text [board max-x max-y]
              (map (partial apply str)
                (for [y (range 0 (inc max-y))]
                  (for [x (range 0 (inc max-x))]
                    (if (contains? board [x y]) \# \space)))))
            (neighbor-positions [[x y]]
              (for [x-delta [-1 0 1]
                    y-delta [-1 0 1]
                    :when (not= 0 x-delta y-delta)]
                [(+ x x-delta) (+ y y-delta)]))
            (alive? [board pt]
              (contains? board pt))
            (living-neighbors [board pt]
              (count (filter
                       (partial alive? board)
                       (neighbor-positions pt))))
            (fringe [board]
              (let [local-area (set (mapcat neighbor-positions board))]
                (clojure.set/difference local-area board)))]
      (let [full-board  (text->full-board text)
            board       (->> full-board
                          (filter input-alive?)
                          (map (partial take 2))
                          set)
            stays-alive (filter #(<= 2 (living-neighbors board %) 3) board)
            comes-alive (filter #(= 3 (living-neighbors board %)) (fringe board))
            next-board  (set (concat stays-alive comes-alive))]
        (board->text next-board (max-x full-board) (max-y full-board))))))

(defcheck solution-cbea358b
  (fn [board]
    (letfn [(to-living [brd]
              (into #{}
                (for [x (range) :while (< x (count brd))
                      y (range) :while (< y (count (brd x)))
                      :when (= \# (get (brd x) y))]
                  [x y])))
            (to-board [n living]
              (for [r (range n)]
                (apply str
                  (for [c (range n)] (if (living [r c]) \# \space)))))
            ;; neighbors and cells are copied verbatim from C. Legrand's solution
            ;; http://clj-me.cgrand.net/2011/08/19/conways-game-of-life/
            (neighbours [[x y]]
              (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])]
                [(+ dx x) (+ dy y)]))
            (step [cells]
              (set (for [[loc n] (frequencies (mapcat neighbours cells))
                         :when (or (= n 3) (and (= n 2) (cells loc)))]
                     loc)))]
      (to-board (count board) (step (to-living board))))))

(defcheck solution-cec6546c
  (fn [board]
    (letfn [

            (is-hash [c] (= c \#))

            (slice [board i j]
              (subs (board i) (max 0 (dec j)) (min (count board) (+ 2 j)))) ;keep borders in mind

            (count-neighbours [board i j]
              (let [border (dec (count board))]
                (+
                  (if (= 0 i) 0                             ;top row has no other cells on top
                              (count (filter is-hash (slice board (dec i) j))))
                  (if (= 0 j) 0                             ;if there is no left field
                              (if (is-hash (nth (board i) (dec j))) 1 0))
                  (if (= border j) 0                        ;if there is no right field
                                   (if (is-hash (nth (board i) (inc j))) 1 0))
                  (if (= border i) 0                        ;bottom row has no other cells below
                                   (count (filter is-hash (slice board (inc i) j)))))))

            (select-state [board i j]
              (let [neighbours (count-neighbours board i j)
                    self       (get-in board [i j])]
                (if (is-hash self)
                  (cond
                    (< neighbours 2) \space                 ;dies of lonelyness
                    (<= 2 neighbours 3) \#                  ;lives on
                    :else \space                            ;overcrowded
                    )
                  (if (= neighbours 3) \# \space))))]       ;dead and 3 neighbours

      (for [i (range (count board))]
        (apply str (for [j (range (count board))]
                     (select-state board i j)))))))

(defcheck solution-cf23c896
  (fn gol [board]
    (let [ch-board          (map seq board)
          y-len             (count ch-board)
          x-len             (count (first ch-board))
          neigh-fn          (fn [x y]
                              (->>
                                (for [dx (range -1 2)
                                      dy (range -1 2)
                                      :when (not (and (= 0 dx) (= 0 dy)))]
                                  [dx dy])
                                (map #(vector (+ x (first %)) (+ y (second %))))
                                (filter #(and (<= 0 (first %) x-len) (<= 0 (second %) y-len)))))
          cell-fn           (fn [x y]
                              (get-in board [y x]))
          num-live-neigh-fn (fn [x y]
                              (count (filter #(= \# (apply cell-fn %)) (neigh-fn x y))))
          cell-next-gen-fn  (fn [x y]
                              (condp = (cell-fn x y)
                                \# (cond
                                     (> 2 (num-live-neigh-fn x y)) \space
                                     (<= 2 (num-live-neigh-fn x y) 3) \#
                                     (< 3 (num-live-neigh-fn x y)) \space
                                     :default \space)
                                \space (cond (= 3 (num-live-neigh-fn x y)) \#
                                             :default \space)
                                \space))
          new-ch-board      (for [y (range y-len)]
                              (for [x (range x-len)]
                                (cell-next-gen-fn x y)))
          board-fn          (fn [ch-board]
                              (for [ch-row ch-board]
                                (apply str ch-row)))]
      (board-fn new-ch-board))))

(defcheck solution-d0657ed8
  (fn life [board]
    (let [cells     (set (apply concat (keep-indexed
                                         (fn [x row]
                                           (keep-indexed
                                             (fn [y item]
                                               (if (= \# item) [x y] nil))
                                             row))
                                         board)))
          neighbors (fn [[x y]]
                      (for [dx [-1 0 1]
                            dy [-1 0 1]
                            :when (not= 0 dx dy)]
                        [(+ dx x) (+ dy y)]))
          cells'    (set (for [[loc n] (frequencies (mapcat neighbors cells))
                               :when (or (= n 3) (and (<= 2 n 3) (cells loc)))]
                           loc))
          height    (count board)
          wat       (count cells')]
      (map (partial apply str) (partition height (for [x (range height) y (range height)]
                                                   (if (cells' [x y]) \# \space)))))))

(defcheck solution-d06a5dba
  (fn [b]
    (let
     [is-live?   (fn [cs i j]
                   (-> cs
                     (get i)
                     (get j)
                     (= \#)))
      dij        [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]
      next-live? (fn [i j]
                   (let [c (count (filter
                                    (fn [[di dj]] (is-live? b (+ di i) (+ dj j)))
                                    dij))
                         l (is-live? b i j)]
                     (or (and l (<= 2 c 3))
                         (and (not l) (= c 3)))))
      next-cell  (fn [i j]
                   (if (next-live? i j) \# \space))]
      (for [i (range (count b))]
        (apply str
          (for [j (range (count (get b i)))]
            (next-cell i j)))))))

(defcheck solution-d0c6d31c
  (fn [b]
    (let [si    (count b)
          neigh (fn [b x y] (map (fn [[q w]] #_(println [x y] [q w]) (nth (nth b q) w)) (for [q (range (dec x) (inc (inc x))) w (range (dec y) (inc (inc y))) :when (and (not (neg? q)) (not (neg? w)) (< q si) (< w si) (not (and (= q x) (= w y))))] [q w])))
          alive (fn [b x y] (let [c  (nth (nth b x) y)
                                  ns (count (filter #(= \# %) (neigh b x y)))]
                              #_(println [x y] ns)
                              (get {[\# 2] \# [\# 3] \# [\  3] \#} [c ns] \ )))]
      (map (fn [x] (apply str (map #(alive b x %) (range si)))) (range si)))))

(defcheck solution-d1bfb12f
  (fn [g]
    (letfn [
            (ncount [g r c]
              (let [n (apply str (conj (map #(subs (g (+ r %)) (- c 1) (+ c 2)) [-1 1])
                                   (get-in g [r (dec c)])
                                   (get-in g [r (inc c)])))
                    c (count (filter #(= % \#) n))] c
                                                    ))

            (rules [c n]
              (cond
                (and (< n 2) (= c \#)) \space
                (and (> n 3) (= c \#)) \space
                (and (= n 3) (= c \space)) \#
                :otherwise c
                ))
            ]
      (let [d   (count g)
            pad (apply str (repeat d \space))
            ng  (map #(str " " % " ")
                  (apply map str (partition (- d 2) (map
                                                      #(rules (get-in g [(second %1) (first %1)]) (ncount g (second %1) (first %1)))
                                                      (for [r (range 1 (dec d)) c (range 1 (dec d))] [r c]))
                                   )))]
        (concat [pad] ng [pad])
        ))))

(defcheck solution-d1ff255c
  (fn [board]
    (letfn [
            (row-to-ints [row] (map #(if (= \# %) 1 0) row))
            (row-to-bools [row] (map #(= \# %) row))
            (shift-row-right [row] (cons 0 row))
            (shift-row-left [row] (conj (vec (rest row)) 0))
            (shift-board-up [rows] (conj (vec (rest rows)) (map (partial * 0) (first rows))))
            (shift-board-down [rows] (cons (map (partial * 0) (first rows)) rows))
            (add-boards [a b] (map (partial map +) a b))
            (count-neighbors [board]
              (reduce add-boards
                [(shift-board-up board)
                 (shift-board-down board)
                 (map shift-row-right board)
                 (map shift-row-left board)
                 (shift-board-up (map shift-row-right board))
                 (shift-board-up (map shift-row-left board))
                 (shift-board-down (map shift-row-right board))
                 (shift-board-down (map shift-row-left board))]))
            (simulate [cell neighbor-count]
              (cond
                (and cell (< neighbor-count 2)) false
                (and cell (< neighbor-count 4)) true
                (and (not cell) (= neighbor-count 3)) true
                :else false))
            (print-board [board]
              (map (fn [row] (apply str (map #(if % \# " ") row))) board))]
      (print-board
        (map (partial map simulate)
          (map row-to-bools board)
          (count-neighbors (map row-to-ints board)))))))

(defcheck solution-d26cb
  (fn [board]
    (let [living-neighbor-count (fn [pos]
                                  (->> [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
                                    (map #(map + pos %))
                                    (filter (partial every? #(>= % 0)))
                                    (map (partial get-in board))
                                    (filter #{\#})
                                    count))
          next-cell             (fn [v c pos]
                                  (cond (= 3 c) \#
                                        (and (= \# v) (= 2 c)) \#
                                        :else \space))]
      (for [row (range (count board))]
        (->> (for [col (range (count (board row)))]
               (next-cell (get-in board [row col])
                 (living-neighbor-count [row col])
                 [row col]))
          (apply str))))))

(defcheck solution-d2c1841
  (fn life [board]
    (let [size      (count board)
          grid      (flatten (map seq board))
          indices   (for [a (range size)
                          b (range size)]
                      [a b])
          squares   (zipmap indices grid)
          neighbors (fn [index]
                      (let [cx (first index)
                            cy (second index)]
                        (filter #(not (nil? %)) (for [x [(dec cx) cx (inc cx)]
                                                      y [(dec cy) cy (inc cy)]]
                                                  (if (or (and (= x cx) (= y cy))
                                                          (or (> 0 x) (> 0 y))
                                                          (or (<= size x) (<= size y)))
                                                    nil
                                                    (squares [x y]))))))
          nextgen   (fn [index]
                      (let [alive-neighbors (count (filter #(= \# %) (neighbors index)))]
                        (if (= \# (squares index))
                          (cond (> 2 alive-neighbors) \space
                                (< 3 alive-neighbors) \space
                                :else \#)
                          (cond (= 3 alive-neighbors) \#
                                :else \space))))
          nextgrid  (map nextgen indices)
          nextboard (partition size nextgrid)]

      (map clojure.string/join nextboard))))

(defcheck solution-d2f88e39
  (fn [board]
    (let [
          board   (vec (map #(vec (replace {\space 0 \# 1} %)) board))
          nr      (count board)
          nc      (count (board 0))
          deltas  (fn [[r c]]
                    (for [dr [-1 0 1]
                          dc (if (= 0 dr)
                               [-1 1]
                               [-1 0 1])] [(+ r dr) (+ c dc)]))

          inrange (fn [i b]
                    (< -1 i b))

          neighbors
                  #(reduce +
                     (map (fn [[r c]]
                            (if (and (inrange r nr) (inrange c nc))
                              (get-in board [r c])
                              0))
                       (deltas %)))

          new-gen #({[1 3] \#
                     [1 2] \#
                     [0 3] \#} % \space)]

      (map (partial apply str)
        (partition nr
          (for [r (range nr)
                c (range nc)]
            (new-gen [(get-in board [r c]) (neighbors [r c])])))))))

(defcheck solution-d3027368
  (fn [t]
    (for [y (range (count t))]
      (apply str
        (for [x (range (count (first t)))]
          (let [t #(get-in t [% %2])
                n
                  (count (for [[a b] [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
                               :when (= \# (t (+ a y) (+ b x)))] 1))]
            (cond
              (= n 3) "#"
              (= n 2) (t y x)
              :else " ")))))))

(defcheck solution-d336dd46
  (fn [board]
    (let [size       (count board)
          alive?     (fn [x y] (= (get-in board [x y]) \#))
          neighbours (fn [x y]
                       (reduce #(if %2 (inc %) %) 0
                         (for [i [-1 0 1]
                               j [-1 0 1]
                               :when (not= i j 0)]
                           (alive? (+ x i) (+ y j)))))
          cell       (fn [x y n] (if (or (= n 3) (and (= n 2) (alive? x y))) \# \space))]
      (for [x (range size)]
        (apply str (for [y (range size)]
                     (cell x y (neighbours x y))))))))

(defcheck solution-d42932d2
  (fn gameoflife [b]
    (let [w  (count (first b))
          h  (count b)
          m  (into {} (apply concat (map-indexed (fn [y row] (map-indexed (fn [x c] [[x y] (if (= c \#) 1 0)]) row)) b)))
          nb (fn [m x y] (+ (m [(dec x) (dec y)] 0) (m [(dec x) y] 0) (m [(dec x) (inc y)] 0)
                           (m [x (dec y)] 0) (m [x (inc y)] 0)
                           (m [(inc x) (dec y)] 0) (m [(inc x) y] 0) (m [(inc x) (inc y)] 0)))]
      (into [] (map (fn [y] (apply str (map (fn [x]
                                              (let [lc (nb m x y)
                                                    l  (m [x y])]
                                                (if ({[1 2] true, [1 3] true, [0 3] true} [l lc] false) \# \space)
                                                )) (range w)))) (range h))))))

(defcheck solution-d432f258
  (fn q94 [board]                                           ;similar q117
    (let [rows            (count board)
          cols            (count (first board))
          dirs            [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
          live-neighbours (fn [pos]
                            (->>
                              (map #(map + pos %) dirs)
                              (filter (fn [[r c]] (and (< -1 r rows) (< -1 c cols))))
                              (filter #(= \# (get-in board %)))
                              count))]
      (->>
        (for [r (range rows) c (range cols)
              :let [ls         (live-neighbours [r c])
                    cell-live? (= \# (get-in board [r c]))
                    next-gen   (if cell-live?
                                 (if (or (= 2 ls) (= 3 ls)) \# \space)
                                 (if (= 3 ls) \# \space))]]
          next-gen)
        (partition cols)
        (map #(apply str %))))))

(defcheck solution-d4de3e82
  (fn [lines]
    (let [ALIVE          \#
          DEAD           \space
          alive?         (fn [ch] (= ALIVE ch))
          cell           (fn [x y] (get (get lines y) x))
          life-rule      (fn [me living-neighbours]
                           (if (alive? me)
                             (and (>= living-neighbours 2) (<= living-neighbours 3))
                             (= living-neighbours 3)))
          neighbours     (fn [x y]
                           (let [x-1                   (- x 1)
                                 x+1                   (+ x 1)
                                 y-1                   (- y 1)
                                 y+1                   (+ y 1)
                                 neighbour-coordinates [[x-1 y-1] [x y-1] [x+1 y-1]
                                                        [x-1 y] [x+1 y]
                                                        [x-1 y+1] [x y+1] [x+1 y+1]]]
                             (reduce (fn [acc c] (if (alive? c) (inc acc) acc)) 0
                               (map (fn [[ix iy]] (cell ix iy)) neighbour-coordinates))))
          calculate-line (fn [y]
                           (loop [x         0
                                  next-line ""]
                             (let [content (cell x y)]
                               (if (nil? content)
                                 next-line
                                 (recur
                                   (inc x)
                                   (str next-line
                                     (if (life-rule content (neighbours x y)) ALIVE DEAD)))))))
          ]
      (loop [y      0
             result []]
        (if (nil? (get lines y))
          result
          (recur (inc y) (conj result (calculate-line y))))))))

(defcheck solution-d5062a72
  (fn [b]
    (map #(apply str %)
      (let [b (vec (map vec b))
            n (count b)
            m (count (first b))
            d [[-1 1] [-1 -1] [1 -1] [1 1] [0 1] [0 -1] [1 0] [-1 0]]]
        (for [i (range n)]
          (for [j (range m)]
            (let [x (apply + (map #(let [i (+ i (first %))
                                         j (+ j (second %))]
                                     (if (and (>= i 0) (>= j 0) (< i n) (< j m) (= \# ((b i) j)))
                                       1
                                       0)) d))
                  c ((b i) j)]
              (if (= c \#)
                (if (or (= 2 x) (= 3 x))
                  \#
                  \ )
                (if (= 3 x)
                  \#
                  \ )))))))))

(defcheck solution-d54272e6
  (fn walk-board3 [b]
    (let [w    (count (first b)) h (count b)
          cell (fn [x y] (if (and (> x -1) (> y -1) (< x w) (< y h)) (nth (nth b y) x)))
          [ox oy] [[-1 0 1 -1 1 -1 0 1] [-1 -1 -1 0 0 1 1 1]]]
      (map #(apply str %)
        (for [y (range h)]
          (for [x (range w)]
            (let [n (count (filter #(= \# %) (map #(cell (+ x %1) (+ y %2)) ox oy)))]
              (if (= \# (cell x y))
                (cond (< n 2) \space (> n 3) \space :else \#)
                (if (= n 3) \# \space)))))))))

(defcheck solution-d5eda762
  (fn [board]
    (letfn [(neighbor-idxs [r c]
              (disj (set (mapcat (fn [x] (map #(vec [x %]) [(dec c) c (inc c)]))
                           [(dec r) r (inc r)]))
                [r c]))
            (neighbor-count [r c]
              (count (filter #(= \# %) (map #(get-in board %) (neighbor-idxs r c)))))
            (curr-live? [r c] (= \# (get-in board [r c])))
            (next-live? [r c]
              (or (and (curr-live? r c) (<= 2 (neighbor-count r c) 3))
                  (and (not (curr-live? r c)) (= 3 (neighbor-count r c)))))]
      (map (fn [r] (apply str (map (fn [c] (if (next-live? r c) \# \ ))
                                (range (count (first board))))))
        (range (count board))))))

(defcheck solution-d69f3b4e
  (fn game-of-life [xseq] (let [
                                x (vec (map vec xseq))
                                h (count x)
                                w (count (first x))]
                            (for [hi (range h)]
                              (apply str (for [wi (range w)] (let [
                                                                   b3x3       (for [hd [0 -1 1] wd [0 -1 1]] (get (get x (+ hi hd)) (+ wi wd)))
                                                                   self       (first b3x3)
                                                                   near       (rest b3x3)
                                                                   self-alive (= \# self)
                                                                   near-lifes (count (filter (partial = \#) near))
                                                                   alive1     (and self-alive (#{2 3} near-lifes))
                                                                   alive2     (= 3 near-lifes)
                                                                   alive      (or alive1 alive2)]
                                                               (if alive \# \space))))))))

(defcheck solution-d7213447
  (fn [b]
    (let [w (count (last b))
          v [-1 0 1]
          t (fn [[i j]] (nth (b i) j))
          a (fn [i j] (for [x v
                            y v
                            :let [m (+ x i) n (+ y j)]
                            :when (and (contains? b m)
                                       (contains? (last b) n)
                                       (not= x y 0))]
                        [m n]))
          n (fn [i j] (->> (a i j)
                        (map t)
                        (filter #(= % \#))
                        (count)))
          f (fn [l n] (or (and l (#{2 3} n)) (and (not l) (= n 3))))
          ]
      (->> (for [i (range 0 (count b)) j (range 0 w)
                 :let [l (= \# (t [i j]))
                       n (n i j)
                       f (if (f l n) \# " ")]
                 ]
             f)
        (partition w)
        (map (partial apply str))))))

(defcheck solution-d77be267
  (fn [g]
    (let [r [-1 0 1]
          n
            #(count
               (for [i r j r
                     :when (= \# (get (get g (+ % i)) (+ %2 j)))]
                 1))]
      (map-indexed
        (fn [i r]
          (apply str
            (map-indexed
              #(condp = (n i %)
                 3 "#"
                 4 %2
                 " ")
              r)))
        g))))

(defcheck solution-d7fd3d40
  (fn [board]
    (letfn [(neighbours [universe rows cols [i j]]
              (for [x (map (partial + i) [-1 0 1])
                    y (map (partial + j) [-1 0 1])
                    :when (and (not= [x y] [i j])
                               (<= 0 x (dec rows))
                               (<= 0 y (dec cols)))]
                (nth (nth universe x) y)))]
      (let [universe (map (partial map {\space 0 \# 1}) board)
            rows     (count universe)
            cols     (count (first universe))]
        (map (comp (partial apply str)
               (partial map {0 \space 1 \#}))
          (for [i (range rows)]
            (for [j (range cols)]
              (condp = (vector (nth (nth universe i) j)
                         (apply + (neighbours universe rows cols [i j])))
                [1 2] 1
                [1 3] 1
                [0 3] 1
                0))))))))

(defcheck solution-da05670b
  (fn [rows]
    (let
     [mp
          (map
            (fn [row]
              (map #(= \# %) row)
              )
            rows
            )
      off
          [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
      alv {0 \  1 \  2 \# 3 \# 4 \  5 \  6 \  7 \  8 \  9 \ }
      ded {0 \  1 \  2 \  3 \# 4 \  5 \  6 \  7 \  8 \  9 \ }
      ]
      (letfn
       [
        (nm
          [a b]
          (if (or (< a 0) (>= a (count mp)) (< b 0) (>= b (count (first mp))))
            0
            (if (nth (nth mp a) b) 1 0)
            )
          )
        ]
        (map
          (fn [a]
            (apply
              str
              (map
                (fn [b]
                  (let [x (reduce + (map #(nm (+ a (first %1)) (+ b (second %1))) off))]
                    (if (nth (nth mp a) b)
                      (alv x)
                      (ded x)
                      )
                    )
                  )
                (range (count (first mp))))
              )
            )
          (range (count mp))
          )
        )
      )
    ))

(defcheck solution-db396fc1
  (fn gol [b]
    (let [coordseq   (fn [b] (for [y (range (count b)) x (range (count (nth b 0)))] [x y])) ; all coordinates of a 2D array
          getb       (fn [b [x y]] ((b y) x))               ; get value of 2D Array
          setb       (fn [b [x y] v] (assoc b y (assoc (b y) x v))) ; set value of 2D Array
          str2bool   (fn [b] (vec (map (fn [r] (vec (map #(if (= \# %) true false) r))) b)))
          bool2str   (fn [b] (map (fn [r] (apply str (map #(if % "#" " ") r))) b))
          world      (str2bool b)                           ; GOL board as booleans
          coords     (coordseq world)                       ; seq of all coordinates to talk to
          neighbours (fn [[x y]]                            ; all valid neighbour coords of given coord on board
                       (let [rng  #(set (range (dec %) (+ 2 %)))
                             has? #(not (nil? (% %2)))]
                         (filter #(and (not= [x y] %) (has? (rng x) (% 0)) (has? (rng y) (% 1))) coords)))
          nextstep   (fn [c]
                       (let [vl    (getb world c)           ; value of coord
                             vls   (map (partial getb world) (neighbours c)) ; values of neighbours
                             alive #(count (filter true? %))
                             dead  #(count (filter false? %))]
                         (if vl                             ; cell was alive?
                           (cond                            ; what happens with living cell
                             (< (alive vls) 2) false
                             (> (alive vls) 3) false
                             :else true)
                           (if (= 3 (alive vls)) true false))))] ; 3 living neighbours of dead cell? resurrect
      (bool2str (reduce #(setb % %2 (nextstep %2)) world coords)))))

(defcheck solution-db6c5097
  (fn [board]
    (let [at             (fn [x y]
                           (cond
                             (< x 0) false
                             (>= x (count (first board))) false
                             (< y 0) false
                             (>= y (count board)) false
                             :else (= (nth (nth board y) x) \#)))
          live-neighbors (fn [x y]
                           (count (filter identity (for [x' [(dec x) x (inc x)]
                                                         y' [(dec y) y (inc y)]
                                                         :when (not (and (= x x') (= y y')))]
                                                     (at x' y')))))
          ]
      (concat (for [y (range 0 (count board))
                    :let [row (nth board y)]]
                (apply str (map #(if % \# \space)
                             (for [x (range 0 (count row))
                                   :let [n    (live-neighbors x y)
                                         live (at x y)]]
                               (cond
                                 (and live (or (= n 2) (= n 3))) true
                                 (and (not live) (= n 3)) true
                                 :else false)))))))))

(defcheck solution-dc1f33f5
  (fn gol [board]
    (letfn [(neighbors [i j]
              (let [nbs  (filter identity (map (partial get-in board)
                                            [[(dec i) (dec j)] [(dec i) j] [(dec i) (inc j)] [i (dec j)] [i (inc j)] [(inc i) (dec j)] [(inc i) j] [(inc i) (inc j)]]))
                    live (count (filter (partial = \#) nbs))
                    dead (- (count nbs) live)]
                [live, dead]))
            (nextState [i j]
              (let [isLive (= \# (get-in board [i j]))
                    [ln dn] (neighbors i j)]
                (cond
                  (and isLive (< ln 2)) false
                  (and isLive (<= 2 ln 3)) true
                  (and isLive (> ln 3)) false
                  (and (not isLive) (= 3 ln)) true
                  :else false)))]

      (for [i (range (count board))]
        (apply str (map #(if (nextState i %) "#" " ") (range (count (first board)))))))))

(defcheck solution-dc834a11
  (fn [board]
    (letfn [(update-cell-fn [alive?]
              (fn [empty-board pos]
                (assoc-in empty-board
                  pos
                  (step alive? pos))))

            (step [alive [y x]]
              (let [{adj-alive \#} (frequencies
                                     (neighbours x y))]
                (if (or (and alive
                             (<= 2 adj-alive 3))
                        (= adj-alive 3))
                  \#
                  \space)))

            (neighbours [x y]
              (let [y-  (dec y)
                    y+  (inc y)
                    x-  (dec x)
                    x+  (inc x)
                    adj [[y- x-]
                         [y- x]
                         [y- x+]
                         [y x+]
                         [y+ x+]
                         [y+ x]
                         [y+ x-]
                         [y x-]]]
                (map (partial get-in board) adj)))

            (index-if-x [x i item]
              (if (= x
                    item)
                i))
            (pos-or-nil [i indexes]
              (if-not (empty? indexes)
                (map (partial conj [i]) indexes)))

            (get-coordinates-of [cell-type]
              (->> board
                (map (partial keep-indexed
                       (partial index-if-x cell-type)))
                (keep-indexed pos-or-nil)
                (apply concat)))]

      (let [board-size    (count board)
            empty-board   (->> \space
                            (repeat board-size)
                            vec
                            (repeat board-size)
                            vec)
            alive         (get-coordinates-of \#)
            alive-updated (reduce (update-cell-fn true)
                            empty-board alive)
            dead          (get-coordinates-of \space)
            dead-updated  (reduce (update-cell-fn false)
                            alive-updated dead)]
        (vec (map (partial clojure.string/join "") dead-updated))))))

(defcheck solution-dc9d4ef3
  (fn gol
    [b]
    (letfn
     [(rows
        [board]
        (count board))
      (cols
        [board]
        (count (first board)))
      (bounded
        [board row col]
        (and (< row (rows board))
             (< col (cols board))
             (>= row 0)
             (>= col 0)))
      (cell
        [board row col]
        (if (bounded board row col)
          (get (get board row) col)
          \space))
      (neighbors
        [board row col]
        [(cell board (dec row) (dec col))
         (cell board (dec row) col)
         (cell board (dec row) (inc col))
         (cell board row (dec col))
         (cell board row (inc col))
         (cell board (inc row) (dec col))
         (cell board (inc row) col)
         (cell board (inc row) (inc col))])
      (lives-on
        [board row col]
        (let [live-neighbors (count
                               (filter #(= % \#) (neighbors board row col)))
              cell-alive     (= \# (cell board row col))]
          (if cell-alive
            (or (= live-neighbors 2)
                (= live-neighbors 3))
            (= live-neighbors 3))))]
      (map (fn [r]
             (apply str (map
                          (fn [c]
                            (if (lives-on b r c)
                              \#
                              \space))
                          (range (cols b)))))
        (range (rows b))))))

(defcheck solution-dd3c24b2
  (fn next-life [board]
    (let [neighbour-coords (fn [x y]
                             (for [nx (range (dec x) (+ x 2))
                                   ny (range (dec y) (+ y 2))
                                   :when (not (or (neg? nx) (neg? ny)
                                                  (and (= nx x) (= ny y))))]
                               [nx ny]))
          alive?           (partial = \#)
          future-cell      (fn [cell neighbours]
                             (let [live?    (alive? cell)
                                   live-nbs (count (filter alive? neighbours))]
                               (if (or (and live? (#{2 3} live-nbs))
                                       (and (not live?) (= live-nbs 3)))
                                 \#
                                 \space)))
          L                (count board), H (count (first board))]
      (map #(apply str %)
        (partition H
          (for [x (range L), y (range H)]
            (future-cell (get-in board [x y])
              (map (partial get-in board)
                (neighbour-coords x y)))))))))

(defcheck solution-de55056e
  (fn [c]
    (let [limit (count c)
          pos   (for [x (range limit) y (range limit)] [x y])
          round (remove #(= [0 0] %) (for [x [-1 0 1] y [-1 0 1]] [x y]))
          ret   (fn [[x y]] (if (or (neg? x) (neg? y) (> x (dec limit)) (> y (dec limit))) nil (nth (nth c x) y)))]
      (map #(apply str %)
        (partition limit
          (for [i pos j [(map #(map + % i) round)]]
            (let [rd (count (filter #(= \# %) (map ret j)))]
              (if (or (= 3 rd)
                      (and (= 2 rd) (= \# (ret i))))
                \# \space)
              )))))))

(defcheck solution-de92f2db
  (fn [board]
    (let [height     (count board)
          width      (count (first board))
          live?      (fn [cell]
                       (= cell \#))
          live-cell? (fn [row col]
                       (and (<= 0 row) (< row height) (<= 0 col) (< col width) (live? (nth (nth board row) col))))
          live-count (fn [row col]
                       (if (live-cell? row col) 1 0))
          neighbors  (fn [row col]
                       (+ (live-count (dec row) (dec col))
                         (live-count (dec row) col)
                         (live-count (dec row) (inc col))
                         (live-count row (dec col))
                         (live-count row (inc col))
                         (live-count (inc row) (dec col))
                         (live-count (inc row) col)
                         (live-count (inc row) (inc col))))
          trans      (fn [row col cell]
                       (let [neighbors (neighbors row col)]
                         (cond
                           (and (live? cell) (< neighbors 2)) \space
                           (and (live? cell) (> neighbors 3)) \space
                           (and (not (live? cell)) (= neighbors 3)) \#
                           :else cell)))]
      (map-indexed (fn [row cells]
                     (apply str (map-indexed (fn [col cell]
                                               (trans row col cell))
                                  cells)))
        board))))

(defcheck solution-de9fb87e
  (fn next-gen [world]
    (let [
          lines         (count world)
          cols          (count (first world))

          indexate      #(map vector % (range))

          parse-cells   (fn [world]
                          (set
                            (for [[line nline] (indexate world),
                                  [ch ncol] (indexate line)
                                  :when (= ch \#)]
                              [nline ncol])))

          unparse-cells (fn [cells]
                          (for [i (range lines)]
                            (apply str
                              (map #(if (contains? cells [i %]) "#" " ") (range cols)))))

          neighbours    (fn [cell]
                          (map
                            (fn [delta] (map + cell delta))
                            [[-1 -1] [-1 0] [-1 1]
                             [0 -1] [0 1]
                             [1 -1] [1 0] [1 1]]))

          survive?      #(and (>= % 2) (<= % 3))

          newborn?      #(== % 3)

          step          (fn [cells]
                          (let [neigs     (frequencies (mapcat neighbours cells))
                                survivors (filter (fn [cell] (survive? (get neigs cell 0))) cells)
                                newborns  (map first (filter #(newborn? (second %)) neigs))]
                            (set (concat survivors newborns))))

          ] (unparse-cells (step (parse-cells world))))))

(defcheck solution-dead4c22
  (fn [b]
    (let [w (count b)
          z (range w)
          l [-1 0 1]
          a apply
          g get-in]
      (map #(a str %)
        (partition w
          (for [x z y z
                :let [n (a + (for [o l p l]
                               (if (= (g b [(+ x o) (+ y p)]) \#)
                                 1
                                 0)))]]
            (if (= (g b [x y]) \#)
              (if (<= 3 n 4) \# " ")
              (if (= n 3) \# " "))))))))

(defcheck solution-dede3cde
  (fn gol [b]
    (let [n
            [[-1 -1] [-1 0] [-1 1] [0 1] [1 1] [1 0] [1 -1] [0 -1]]
          h (count b) w (count (first b))]
      (letfn [(live? [c] (= (get-in b c) \#))
              (valid? [[y x]] (and (< -1 y h) (< -1 x w)))
              (live_n [c]
                (count (filter #(and (valid? %) (live? %))
                         (map #(map + c %) n))))]
        (for [y (range h)]
          (apply str
            (for [x (range w)]
              (if (live? [y x])
                (if (<= 2 (live_n [y x]) 3) \# \space)
                (if (= (live_n [y x]) 3) \# \space)))))))))

(defcheck solution-deeb2313
  (fn [g]
    (let [m {\space 0 \# 1}
          d (count g)
          b (vec (map #(vec (map m %)) g))
          n (rest (for [x [0 -1 1] y [0 -1 1]] [x y]))]
      (map #(apply str %)
        (for [x (range d)]
          (for [y (range d)]
            (let [c (apply + (map (fn [[k l]] (get-in b [(+ k x) (+ l y)] 0)) n))
                  s (get-in b [x y])]
              (cond
                (= c 3) \#
                (and (= c 2) (= s 1)) \#
                :else " "))))))))

(defcheck solution-e169e894
  (fn life [board]
    (let [y-max (count board)
          x-max (count (first board))
          adj-coords
                (fn [[x y]]
                  (for [x-n [(dec x) x (inc x)]
                        y-n [(dec y) y (inc y)]
                        :when (not= [x-n y-n] [x y])]
                    [x-n y-n]))
          count-adj
                (fn [[x y] cells]
                  (let [adj (adj-coords [x y])]
                    (reduce #(if (contains? cells %2) (inc %1) %1) 0 adj)))
          lives?
                (fn [[x y] cells]
                  (let [num-live (count-adj [x y] cells)]
                    (if (contains? cells [x y])
                      (boolean (#{2 3} num-live))
                      (boolean (#{3} num-live)))))
          step
                (fn [cells]
                  (for [x-n (take x-max (range))
                        y-n (take y-max (range))
                        :when (lives? [x-n y-n] cells)]
                    [x-n y-n]))
          collapse-str
                (fn [s y]
                  (->> s
                    (map #(if (= \# %2) [%1 y] nil) (take (count s) (range)))
                    (filter #(not= % nil))
                    (into #{})))
          collapse-vec
                (fn [v]
                  (->> v
                    (map #(vector %1 %2) (reverse (take (count v) (range))))
                    (reduce #(concat %1 (collapse-str (last %2) (first %2))) [])
                    (into #{})))
          explode-set
                (fn [s]
                  (let [spaces-list (into [] (take x-max (repeat \space)))
                        y-range     (take y-max (range))
                        live-map
                                    (reduce (fn map-set-to-rows [mp st]
                                              (assoc mp
                                                (last st)
                                                (conj (get mp (last st)) (first st))))
                                      (zipmap y-range (take y-max (repeat [])))
                                      s)
                        explode-str
                                    (fn [s-e]
                                      (->> s-e
                                        (reduce #(assoc %1 %2 \#) spaces-list)
                                        (apply str)))]
                    (->> y-range
                      (into [])
                      (map #(explode-str (get live-map %)))
                      (reverse))))]
      (explode-set (step (collapse-vec board))))))

(defcheck solution-e1b1d2bb
  (fn [board]
    (letfn [(new-cell [cell ns]
              (let [live-neighbours (get (frequencies ns) \# 0)]
                (if (= (get-in board cell) \space)
                  (if (= 3 live-neighbours)
                    \#
                    (get-in board cell))
                  (cond
                    (< live-neighbours 2) \space
                    (#{2 3} live-neighbours) \#
                    (> live-neighbours 3) \space))))
            (neighbours [[x y]]
              (for [i [-1 0 1]
                    j [-1 0 1]
                    :when (not= i j 0)
                    :let [[nx ny :as nxy] [(+ x i) (+ y j)]]]
                (get-in board nxy)))]
      (for [i (range (count board))]
        (clojure.string/join
          (for [j (range (count (board i)))
                :let [cell [i j]]]
            (new-cell cell (neighbours cell))))))))

(defcheck solution-e1e73ad
  (fn [board]
    (letfn [(cells-with-s [b s]
              (let [lcinrow (fn [rn] (filter #(= s (nth (nth b rn) %)) (range (count (nth b rn)))))
                    rowsl   (map #(vector % (lcinrow %)) (range (count b)))
                    cellslr (fn [rl] (if (empty? (last rl)) nil
                                                            (map #(vector (first rl) %) (last rl))))]
                (apply concat (map cellslr rowsl))))
            (live-cells [b] (cells-with-s b \#))
            (dead-cells [b] (cells-with-s b \space))
            (my-abs [n] (if (neg? n) (- n) n))
            (in-nbd [n1 n2 dist] (<= (my-abs (- n1 n2)) dist))
            (neighbours? [cell1 cell2]
              (cond
                (= cell1 cell2) false
                (and (in-nbd (first cell1) (first cell2) 1) (in-nbd (last cell1) (last cell2) 1)) true
                :else false))
            (count-live-neighbours [c lcs] (count (filter #(neighbours? c %) lcs)))
            (live-cell-rules [c lcs]
              (let [cln (count-live-neighbours c lcs)]
                (cond (< cln 2) false
                      (or (= cln 2) (= cln 3)) true
                      :else false)))
            (dead-cell-rules [c lcs]
              (let [cln (count-live-neighbours c lcs)]
                (cond (= cln 3) true
                      :else false)))
            (new-live-cells [b]
              (let [lcs     (live-cells b)
                    dcs     (dead-cells b)
                    lcslive (filter #(live-cell-rules % lcs) lcs)
                    dcslive (filter #(dead-cell-rules % lcs) dcs)
                    newlcs  (concat lcslive dcslive)]
                newlcs))
            (newboard [b]
              (let [nlb    (new-live-cells b)
                    strels (for [x (range (count b))
                                 y (range (count (first b)))]
                             (if (empty? (filter #(= [x y] %) nlb))
                               " "
                               "#"))
                    strb   (partition (count (first b)) strels)]
                (map #(apply str %) strb)))]
      (newboard board))))

(defcheck solution-e1f3c12f
  (fn [b]
    (let [
          w          (count (nth b 0)) h (count b)
          vecs       [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
          vecadd     (fn [a b] [(+ (first a) (first b)) (+ (second a) (second b))])
          getCell    (fn [x y] (if (and (>= x 0) (>= y 0) (< x w) (< y w)) (nth (nth b y) x)))
          neighbours (fn [x y] (filter identity (map #(apply getCell %) (map #(vecadd [x y] %) vecs))))
          nAlive     (fn [x y] (count (filter #(= \# %) (neighbours x y))))
          nextGen    (fn [s n] (if (= s \space) (if (= n 3) \# \space)
                                                (if (or (= n 2) (= n 3)) \# \space)))
          doRow      (fn [y] (map-indexed (fn [x s] (nextGen s (nAlive x y))) (nth b y)))]
      (vec (map #(apply str %) (map doRow (range h)))))))

(defcheck solution-e20d3d3e
  (fn [board]
    (let [h (count board)
          w (count (first board))]
      (for [i (range h)]
        (apply str (for [j (range w)]
                     (let [n  (for [di [-1 0 1]
                                    dj (if (= 0 di) [-1 1] [-1 0 1])]
                                (get-in board [(+ i di) (+ j dj)]))
                           nc (count (filter #{\#} n))]
                       (if (= \# (get-in board [i j]))
                         (cond
                           (< nc 2) \space
                           (or (= nc 2) (= nc 3)) \#
                           (> nc 3) \space)
                         (if (= nc 3)
                           \#
                           \space)))))))))

(defcheck solution-e20da399
  (fn next-gen [board]
    (letfn [(neighbours [i j]
              (for [x [(dec i) i (inc i)]
                    y [(dec j) j (inc j)]
                    :when (and (>= (dec (count board)) x 0)
                               (>= (dec (count (first board))) y 0)
                               (not (and (= x i) (= y j))))]
                [x y]))
            (live-neighours [i j]
              (filter (fn [[x y]] (= \# (get-in board [x y]))) (neighbours i j)))]
      (vec
        (for [x (range (count board))]
          (reduce
            str
            (for [y (range (count (first board)))
                  :let [n (count (live-neighours x y))]]
              (cond
                (= n 3) \#
                (= n 2) (get-in board [x y])
                :die \space))))))))

(defcheck solution-e2f73f8b
  (fn [board]
    (let [old-board (mapv vec board)
          x-size    (count (first old-board))
          y-size    (count old-board)]
      (->>
        (for [x (range x-size)
              y (range y-size)]
          (let [alive?          (= (get-in old-board [x y]) \#)
                neighbor-coords (for [x-offset (range (dec x) (+ x 2))
                                      y-offset (range (dec y) (+ y 2))
                                      :when (not= [x-offset y-offset] [x y])]
                                  [x-offset y-offset])
                alive-neighbors (->> neighbor-coords
                                  (map #(get-in old-board %))
                                  (filter #(= % \#))
                                  (count))]
            (cond
              (< alive-neighbors 2) \space
              (and alive? (< 1 alive-neighbors 4)) \#
              (and alive? (< 3 alive-neighbors)) \space
              (and (not alive?) (= alive-neighbors 3)) \#
              :else \space)))
        (partition x-size)
        (mapv #(apply str %))))))

(defcheck solution-e348077a
  (fn eval-board [b]
    (let [state-dead \space
          state-live \#
          w          (count b)
          h          (count (first b))]
      (letfn [
              (cell [i j]
                (nth (nth b i) j))

              (live? [c]
                (= c state-live))

              (neighbour [i j acc]
                (if (or (< i 0) (< j 0) (>= i w) (>= j h))
                  acc
                  (cons (cell i j) acc)))

              (neighbours [i j]
                (->>
                  (neighbour (dec i) (dec j) [])
                  (neighbour i (dec j))
                  (neighbour (inc i) (dec j))
                  (neighbour (dec i) j)
                  (neighbour (inc i) j)
                  (neighbour (dec i) (inc j))
                  (neighbour i (inc j))
                  (neighbour (inc i) (inc j))))

              (eval-cell [i j]
                (let [ns (neighbours i j)
                      lc (count (filter live? ns))
                      c  (cell i j)]
                  (if (live? c)
                    (cond
                      (< lc 2) state-dead
                      (<= lc 3) state-live
                      :else state-dead)
                    (if (= lc 3)
                      state-live
                      state-dead))))

              (eval-row [i]
                (reduce
                  (fn [acc j] (conj acc (eval-cell i j)))
                  [] (range h)))]

        (reduce
          (fn [acc i]
            (conj acc (apply str (eval-row i))))
          [] (range w))))))

(defcheck solution-e39394b3
  (fn [board]
    (let [h                   (count board)
          w                   (count (first board))
          live-neighbor-count (fn [board [y x]]
                                (count (filter #(= % \#) (for [dx [-1 0 1] dy [-1 0 1] :when (not= dx dy 0)]
                                                           (get-in board [(+ y dy) (+ x dx)])))))]
      (map #(apply str %) (partition w (for [y (range h) x (range w)]
                                         (let [coords  [y x]
                                               letter  (get-in board coords)
                                               n-count (live-neighbor-count board coords)]
                                           (if (or
                                                (= n-count 3)
                                                (and
                                                 (= letter \#)
                                                 (= n-count 2)))
                                             \#
                                             \space))))))))

(defcheck solution-e397bffb
  (letfn [(neighbor-indexes [i j]
            (for [x (range (dec i) (+ i 2))
                  y (range (dec j) (+ j 2))
                  :when (not (and (= x i) (= y j)))]
              [x y]))
          (get-val [b i j] (nth (nth b i) j))
          (neighbor-val [b [i j]]
            (cond
              (< i 0) 0
              (< j 0) 0
              (>= i (count b)) 0
              (>= j (count (first b))) 0
              :else (if (= (get-val b i j) \#) 1 0)))
          (num-neighbors [b i j]
            (apply + (map (partial neighbor-val b) (neighbor-indexes i j))))
          (next-val [b i j]
            (case (num-neighbors b i j)
              2 (str (get-val b i j))
              3 "#"
              " "))]
    (fn [b]
      (map (fn [row] (apply str (map #(next-val b row %) (range (count (first b)))))) (range (count b))))))

(defcheck solution-e49636c
  (letfn [(transpose [m] (apply map vector m))
          (translate [m] (map (partial map {\space 0 \# 1}) m))
          (pad [m] (let [r (repeat (count (first m)) 0)] (concat [r] m [r])))
          (expand [m] (map (comp #(partition 3 1 %)) m))
          (squash [mm] (map #(map (partial apply +) %) mm))]
    (fn [b]
      (let [f  (comp squash expand transpose pad)
            bb (f (f (translate b)))]
        (map (comp (partial apply str) (fn [rb rbb] (map #(condp = %1
                                                            0 (if (= 3 %2) \# \space)
                                                            1 (cond
                                                                (or (> %2 4) (< %2 3)) \space
                                                                :e \#)
                                                            ) rb rbb))) (translate b) bb)))))

(defcheck solution-e51973ba
  (fn [b]
    (let [c (count (nth b 0))
          d (count b)]
      (->>
        (for [y (range d)
              x (range c)
              :let [i   (nth (nth b y) x)
                    n   (max 0 (- y 1))
                    e   (min c (+ x 2))
                    s   (min d (+ y 2))
                    w   (max 0 (- x 1))
                    sub (map
                          #(subs % w e)
                          (subvec b n s))
                    a   (reduce #(if (= %2 \#) (+ % 1) %) (if (= i \#) -1 0) (apply str sub))
                    ]]
          (if (= i \#)
            (cond
              (< a 2) \space
              (< a 4) \#
              1 \space)
            (if (= a 3) \# \space)))
        (partition c)
        (map #(apply str %))
        vec
        ))))

(defcheck solution-e5d2a00
  (fn next-gen [board]
    (letfn [
            (alive? [[cy cx]]
              (if (= (get-in board [cy cx]) \#)
                true false))

            (neighbors [cy cx]
              (count
                (filter alive?
                  (remove (partial some #(< % 0))
                    (for [y (range (dec cy) (+ 2 cy))
                          x (range (dec cx) (+ 2 cx))
                          :when (not= [cy cx] [y x])]
                      [y x])))))

            (next-state [cy cx]
              (let [nbr  (neighbors cy cx)
                    cell [cy cx]]
                (if (alive? cell)
                  (cond
                    (< nbr 2) :die
                    (> nbr 3) :die
                    :else :live)
                  ; dead cell
                  (if (= nbr 3) :live
                                :die))))

            (update-cell [cy cx]
              (case (next-state cy cx)
                :live \#
                :die \space))]

      (let [bheight (count board)
            bwidth  (count (first board))]

        (vec (for [y (range bheight)]
               (clojure.string/join
                 (for [x (range bwidth)]
                   (update-cell y x)))))))))

(defcheck solution-e6370834
  (letfn
   [(neighbors [size yx]
      (filter (fn [new-yx]
                (every? #(< -1 % size) new-yx))
        (map #(vec (map + yx %))
          [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])))
    (next-generation [board yx]
      (let [cel (get-in board yx)]
        (-> (map #(get-in board %) (neighbors (count board) yx))
          (frequencies)
          (get \# 0)
          (#(cond (and (= cel \#) (< 1 % 4)) \#
                  (and (= cel \space) (= % 3)) \#
                  :defauld \space)))))]

    (fn [board]
      (->>
        (for [y (range (count board)) x (range (count (get board y)))] [y x])
        (map #(next-generation board %))
        (partition (count board))
        (map #(apply str %))))))

(defcheck solution-e63cf908
  (fn [bd] (let [h    (count bd)
                 w    (count (first bd))
                 offs (remove #{[0 0]} (for [i [-1 0 1] j [-1 0 1]] [i j]))
                 m    (zipmap (for [i (range h) j (range w)] [i j])
                        (for [r bd b r] b))
                 nm   (reduce (fn [nm ps]
                                (assoc nm ps
                                          (let [ns (map m (map #(map + ps %) offs))
                                                ns (count (filter #{\#} ns))]
                                            (cond (= ns 3) \#
                                                  (= ns 2) (m ps)
                                                  :else \space))))
                        {} (keys m))]
             (for [i (range h)] (apply str (for [j (range w)] (nm [i j])))))))

(defcheck solution-e64cfbe5
  (fn [board]
    (let [vboard       (vec (map vec board))
          trans        (fn [b f]
                         (vec (for [y (range (count b))]
                                (vec (for [x (range (count (get b y)))]
                                       (f y x (get-in b [y x])))))))
          neib-indexes (for [x [-1 0 1] y [-1 0 1] :when (not (= 0 x y))] [x y])
          delta-neib?  (fn [x y [dx dy]]
                         (= \# (get-in vboard [(+ x dx) (+ y dy)])))
          new-gen      (fn [x y d]
                         (let [neibs     (filter (partial delta-neib? x y) neib-indexes)
                               abs-count (count neibs)
                               ncount    (if (= d \#) abs-count (- abs-count))]
                           (if (#{-3 2 3} ncount) \# \space)))
          new-board    (trans vboard new-gen)]
      (map (partial apply str) new-board))))

(defcheck solution-e70e6bf7
  (fn [s]
    (let [s (mapv vec s), t {\# \space, \space \#}
          g [inc dec identity], x (count (first s)), y (count s)]
      (letfn [(c [p q]
                (for [a (map #(% p) g) b (map #(% q) g)
                      :when (and (< -1 a x) (< -1 b y) (not= [a b] [p q]))]
                  ((s a) b)))
              (j [f s]
                (for [a (range x) b (range y)]
                  [((s a) b) (count (filter #{\#} (f a b)))]))
              (k [[o p]]
                (if (= o \#)
                  (if (#{2 3} p) o (t o))
                  (if (= 3 p) (t o) o)))]
        (mapv #(apply str %) (partition x (map k (j c s))))))))

(defcheck solution-e7354d94
  (fn [b]
    (let [Y (count b) X (count (first b))]
      (letfn [(b2i [b]
                (apply concat (keep-indexed (fn [y r] (keep-indexed (fn [x v] [y x (= \# v)]) r)) b)))
              (i2b [i]
                (vec (map (fn [r] (apply str (map (fn [[_ _ v]] (if v \# \ )) r))) (partition-by first i))))
              (around [y x]
                (set
                  (for [a [(dec y) y (inc y)]
                        b [(dec x) x (inc x)]
                        :when (and (not (and (= a y) (= b x)))
                                   (some #{a} (range 0 Y))
                                   (some #{b} (range 0 X)))]
                    [a b])))
              (live? [i y x]
                (first (map (fn [[_ _ v]] v) (filter (fn [[a b v]] (and (= y a) (= x b))) i))))
              (live-count [i y x]
                (count (filter (fn [[y x]] (live? i y x)) (around y x))))]
        (let [o (b2i b)]
          (i2b (map (fn [[y x v]] [y x (or (= 3 (live-count o y x)) (and v (= 2 (live-count o y x))))]) o)))))))

(defcheck solution-e8e334ba
  (let [a apply
        m map
        c #({3 \# 4 %} %2 \ )
        s #(a + %)
        n #(m s (partition 3 1 [0] (cons 0 %&)))
        l #(a str (m c % %2))
        t #(a n (m {\  0 \# 1} %&))]

    #(m l % (a m n (a m t %)))))

(defcheck solution-e9359aa0
  (fn [b]
    (let [w  (count (first b))
          h  (count b)
          n  (fn [x y]
               (let [off [-1 0 1]]
                 (partition 2 (flatten (map (fn [yoff]
                                              (map (fn [xoff] (if (= 0 xoff yoff) [] [(mod (+ x xoff) w) (mod (+ y yoff) h)])) off)) off)))))
          a? (fn [x y] (= "#" (str (nth (nth b y) x))))
          ac (map (fn [y]
                    (map (fn [x]
                           (let [neigh (n x y)
                                 ct    (count (filter (fn [cc] (a? (first cc) (second cc))) neigh))]
                             (cond
                               (and (a? x y) (< ct 2)) " "
                               (and (a? x y) (< ct 4)) "#"
                               (and (a? x y) (> ct 3)) " "
                               (and (not (a? x y)) (= ct 3)) "#"
                               :else " ")
                             ))
                      (range w)))
               (range h))
          ]
      (map (fn [r] (apply str r)) ac))))

(defcheck solution-e9e8443a
  #(for [[i r] (map list (range) %)]
     (apply
       str
       (for [[j v] (map list (range) r)]
         (let [n (reduce +
                   (for [k [(- i 1) i (+ i 1)]
                         l [(- j 1) j (+ j 1)]]
                     ({\  0 \# 1}
                      (get-in % [k l] \ ))))]
           ({true \# false \ }
            (boolean
              (if (= v \#)
                (#{3 4} n)
                (= n 3)))))))))

(defcheck solution-e9f12ba1
  (fn [rows]
    (let [rows (mapv #(mapv #{\#} %) rows)]
      (for [i (range (count rows))]
        (apply str
          (for [j (range (count (first rows)))
                :let [neighbors
                      (apply +
                        (for [i' [-1 0 1]
                              j' [-1 0 1]
                              :when (not (= 0 i' j'))]
                          (if (get-in rows [(+ i i') (+ j j')]) 1 0)))]]
            (if (or (= neighbors 3)
                    (and (= neighbors 2) (get-in rows [i j])))
              \# \space)))))))

(defcheck solution-eadc5722
  (fn [board]
    ;; c for count
    ;; nboard for nemerify board
    (letfn [(empty-row [board]
              (take (count (first board)) (repeat 0)))
            (numerify [board]
              (map #(map {\space 0, \# 1} %) board))
            (cleft [nboard]
              (map #(cons 0 (butlast %)) nboard))
            (cright [nboard]
              (map #(conj (vec (rest %)) 0) nboard))
            (cup [nboard]
              (cons (empty-row nboard) (butlast nboard)))
            (cdown [nboard]
              (conj (vec (rest nboard)) (empty-row nboard)))
            (cneighbours [nboard]
              (apply map #(apply map + %&)
                ((apply juxt (conj (for [x (list cup cdown) y (list cleft cright)]
                                     (comp x y))
                               cup cdown cleft cright))
                 nboard)))
            (live [l c]                                     ;l for live, c for count
              (cond (= c 3) \#
                    (and (= c 2) (= l 1)) \#                ;which is space
                    :else \space
                    ))
            (next-gen [board]
              (let [cboard (numerify board)]
                (map #(clojure.string/join (apply map live %&)) cboard (cneighbours cboard)))
              )]

      (next-gen board))))

(defcheck solution-eb23999f
  (fn [game-state]
    (let [cells                (vec (mapcat vec game-state))
          rows                 (count game-state)
          cols                 (count (first game-state))
          up-row               (partial map #(- % cols))
          down-row             (partial map #(+ % cols))
          neighbouring         (comp
                                 set
                                 (partial map #(mod % (* rows cols)))
                                 #(apply concat %)
                                 (juxt up-row identity down-row)
                                 (juxt dec identity inc))
          alive?               #{\#}
          dead?                (complement alive?)
          surrounding-contents (fn [index]
                                 (map #(get cells %) (disj (neighbouring index) index)))
          surrounding-live     #(count (filter alive?
                                         (surrounding-contents %)))
          inc-cell             (fn [index state]
                                 (let [live-count (surrounding-live index)]
                                   (cond
                                     (and (alive? state) (<= 2 live-count 3)) \#
                                     (and (dead? state) (= 3 live-count)) \#
                                     :else \space)
                                   ))
          ]
      (map #(apply str %)
        (partition cols
          (map inc-cell (range (* rows cols)) cells)))
      )))

(defcheck solution-eb7702d4
  (fn [rs]
    (let
     [
      count-rows (count rs)
      count-cols (count (first rs))
      bitmap     {\# 1 \space 0}
      rows       (reduce #(concat %1 [(map bitmap %2)]) [] rs)
      neighbors
                 (concat
                   [(first rows)]
                   (map
                     (fn [[pred row succ]]
                       (map-indexed
                         (fn [i x]
                           (let
                            [
                             h (max 0 (dec i))
                             j (min (dec count-cols) (inc i))
                             ]
                             (+
                               (nth pred h)
                               x
                               (nth pred j)
                               (nth row h)
                               (nth row j)
                               (nth succ h)
                               (nth succ i)
                               (nth succ j)
                               )
                             )
                           )
                         pred
                         )
                       )
                     (map (fn [i] [(nth rows (dec i)) (nth rows i) (nth rows (inc i))]) (range 1 (dec count-rows)))
                     )
                   [(last rows)]
                   )
      ]
      (map-indexed
        (fn [i row]
          (apply str
            (map-indexed
              (fn [j cell]
                (cond
                  (and (= 1 cell) (< (nth (nth neighbors i) j) 2)) \space
                  (and (= 1 cell) (> (nth (nth neighbors i) j) 3)) \space
                  (= 1 cell) \#
                  (and (zero? cell) (= (nth (nth neighbors i) j) 3)) \#
                  :else \space
                  )
                )
              row
              )
            )
          )
        rows
        )
      )
    ))

(defcheck solution-ec215812
  (fn [brd]
    (let [brd   (vec (map vec brd))
          nr    (count brd)
          nc    (count (brd 0))
          neigh (fn [r c]
                  (count (filter identity
                           (for [rr [-1 0 1] cc [-1 0 1]
                                 :when (or (not= rr 0) (not= cc 0))]
                             (= \# (get-in brd [(+ r rr) (+ c cc)]))))))]
      (->>
        (for [r (range nr)
              c (range nc)]
          (let [n (neigh r c)]
            (if (= \# (get-in brd [r c]))
              (cond
                (< n 2) \space
                (<= 2 n 3) \#
                :else \space)
              (if (= n 3) \# \space))))
        (partition nc) (map #(apply str %))))))

(defcheck solution-ec38cda1
  (fn [c] (letfn [
                  (unpack [v] (mapv #(vec %) v))
                  (pack [v] (mapv #(apply str %) v))
                  (neighbours [m y x]
                    ((frequencies (for [i [-1 0 1] k [-1 0 1] :when (not= 0 i k)]
                                    (get-in m [(+ y i) (+ x k)]))) \#))
                  (next-gen [old m y x] (condp = [old (neighbours m y x)] [\# 2] \# [\# 3] \# [\space 3] \# \space))]
            (let [o (unpack c)]
              (pack
                (reduce
                  (fn [m [y x]]
                    (update-in m [y x] next-gen o y x))
                  o
                  (for [i (range (count c)) k (range (count (first c)))] [i k])))))))

(defcheck solution-ec518ad8
  #(letfn [
           (line-degree [n s]
             "return the number of live neighbours within the line"
             (count
               (filter (fn [x] (= x \#))
                 (if (empty? s) [] (take (if (= n 0) 2 3) (drop (dec n) s))))))

           (matrix-degree [n s1 s2 s3]
             "return the number of live neighbours"
             (+ (line-degree n s1) (- (line-degree n s2) (if (= (nth s2 n) \#) 1 0)) (line-degree n s3)))

           (next-gen-char [n s1 s2 s3]
             (let [deg (matrix-degree n s1 s2 s3)]
               (if (< deg 2)
                 \space
                 (if (= deg 2)
                   (nth s2 n)
                   (if (= deg 3)
                     \#
                     \space)))))
           ]

     (loop [s1 [] s2 % s3 (next s2) ret []]
       (if (empty? s2)
         ret
         (recur s2 s3 (next s3)
           (conj ret (apply str (for [n (range (count (first s2)))] (next-gen-char n (first s1) (first s2) (first s3))))))))))

(defcheck solution-ec6769cb
  (fn [board]
    (let [max_r        (count board)
          max_c        (count (first board))
          positions    (fn [r c]
                         (for [x (range (dec r) (+ r 2)), y (range (dec c) (+ c 2))
                               :when (and (not (= [x y] [r c])) (<= 0 x max_r) (<= 0 y max_c))]
                           [x y]))

          output       (fn [cells]
                         (map #(apply str %1) (partition max_r cells)))

          process_cell (fn [r c]
                         (let [cell     (get (get board r) c)
                               ne       (positions r c)
                               ne_cells (map (fn [coord] (get-in board coord)) ne)
                               live     (count (filter (partial = \#) ne_cells))]

                           (if (= cell \space)
                             (if (= live 3)
                               \#
                               \space)
                             (cond
                               (> live 3) \space
                               (< live 2) \space
                               :else \#))))]

      (output
        (for [r (range 0 max_r), c (range 0 max_c)]
          (process_cell r c))))))

(defcheck solution-ec97cf54
  (fn game-of-life
    [board]
    (let [context   (fn [s]
                      (concat [[(first s) (nth s 1)]]
                        (map vector
                          (drop-last 2 s)
                          (butlast (next s))
                          (nthnext s 2))
                        [[(nth s (- (count s) 2)) (last s)]]))
          transpose (fn [s]
                      (for [n (range (count (first s)))]
                        (map #(nth % n) s)))]
      (let [neighbors (flatten (map (fn [row]
                                      (map #(apply + (map {\# 1 \space 0} (flatten %)))
                                        row))
                                 (map context (map transpose (context board)))))
            contents  (flatten (map seq board))]
        (map (partial apply str) (partition (count (first board))
                                   (map (fn [live-cells contents]
                                          (if (= contents \#)
                                            (if (< 2 live-cells 5)
                                              \#
                                              \space)
                                            (if (= 3 live-cells)
                                              \#
                                              \space)))
                                     neighbors
                                     contents)))))))

(defcheck solution-eed20c63
  (fn GoL [textboard]
    (letfn
     [(row->v [r] (vec (map #(if (= \# %) 1 0) r)))
      (textboard->vv [b]
        (vec (map row->v b)))
      (get-cell [b i j] ((b i) j))
      (neighbors [b i j]
        (let [N (count b)]                                  ;NOTE assumes board is square
          (for [ii (map #(mod % N) [(dec i) i (inc i)])
                jj (map #(mod % N) [(dec j) j (inc j)])
                :when (not= [i j] [ii jj])]
            (get-cell b ii jj))))
      (dead? [v] (= 0 v))
      (live? [v] (= 1 v))
      (map-board [f b]
        (vec (map vec
               (map-indexed (fn [i r]
                              (map-indexed (fn [j v]
                                             (f b i j v))
                                r))
                 b))))
      (next-state [b i j v]
        (let [live-nbrs (count (filter live? (neighbors b i j)))
              live      1 dead 0]
          (if (live? v)
            (cond
              (< live-nbrs 2) dead                          ;underpopulation
              (> live-nbrs 3) dead                          ;overpopulation
              :else live)
            (if (= live-nbrs 3)
              live                                          ; reproduction
              dead))))
      (vv->textboard [vv]
        (map-board (fn [b i j v] (if (= v 0) \space \#)) vv))]
      (->> textboard
        textboard->vv
        (map-board next-state)
        vv->textboard
        (map #(apply str %))))))

(defcheck solution-eefc0930
  (fn [b]
    (let [n (count b)
          g (fn [x y]
              (if (and (< -1 x n) (< -1 y n))
                (nth (nth b x) y) \space))
          c (fn [x y]
              (count (filter #(= \# %)
                       [(g (inc x) (inc y)) (g x (inc y)) (g (dec x) (inc y)) (g (dec x) y)
                        (g (dec x) (dec y)) (g x (dec y)) (g (inc x) (dec y)) (g (inc x) y)])))]
      (map #(apply str %) (partition n
                            (for [x (range n) y (range n)]
                              (let [a (= \# (g x y))
                                    c (c x y)]
                                (cond
                                  (false? a) (if (= c 3) \# \space)
                                  (< c 2) \space
                                  (<= 2 c 3) \#
                                  (> c 3) \space))))))))

(defcheck solution-ef8084dd
  (fn life [v]
    (letfn [(get-live-locs [v]
              (set (filter (comp not nil?)
                     (for [i (range (count v)) j (range (count (first v)))]
                       (if (= (get-in v [i j]) \#) [i j])))))
            (shift-shape [s loc]
              (set (map (fn [v] [(+ (first v) (first loc)) (+ (last v) (last loc))]) s)))
            (count-env [v cell]
              (let [s [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
                (count
                  (filter #(= % \#)
                    (map #(get-in v %)
                      (map (fn [x] [(+ (x 0) (cell 0)) (+ (x 1) (cell 1))]) s))))))
            (check-cell [v live-locs cell]
              (let [live? (if (nil? (live-locs cell)) false true)
                    count (count-env v cell)]
                (if live?
                  (cond (< count 2) \space
                        (< count 4) \#
                        :else \space)
                  (cond (= count 3) \#
                        :else \space))))
            (next-gen [v]
              (let [live-locs (get-live-locs v)]
                (set (for [i (range (count v)) j (range (count (first v)))
                           :when (= (check-cell v live-locs [i j]) \#)] [i j]))))
            (print-locs [locs n m]
              (vec
                (map clojure.string/join
                  (partition m
                    (clojure.string/join
                      (for [i (range n) j (range m)]
                        (if (nil? (locs [i j])) \space \#)))))))]
      (print-locs (next-gen v) (count v) (count (v 0))))))

(defcheck solution-f0045cd3
  (fn game-of-life [board]
    (let [height             (count board)
          width              (count (first board))
          valid-sq           (fn [[x y]] (and (not (neg? x)) (not (neg? y)) (< x height) (< y width)))
          neighbors          (fn [x y]
                               (filter valid-sq
                                 [
                                  [(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
                                  [x (dec y)] [x (inc y)]
                                  [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]
                                  ]))
          num-live-neighbors (fn [x y] (count (filter #(= \# (get-in board %)) (neighbors x y))))]
      (for [x (range height)]
        (apply str
          (for [y (range width)]
            (if (= \# (get-in board [x y]))                 ; live cell
              (if (#{2, 3} (num-live-neighbors x y)) \# \space)
              (if (= 3 (num-live-neighbors x y)) \# \space))))))))

(defcheck solution-f0ef3dcc
  (fn [m A I b]
    (I (fn [i r]
         (A str (I (fn [j c]
                     ({3 \#
                       2 c}
                      (A + (map #({\# 1} (get-in b (map + [i j] %)) 0)
                             [[m m] [m 0] [m 1]
                              [0 m] [0 1]
                              [1 m] [1 0] [1 1]]))
                      \ ))
                  r)))
      b)) -1 apply map-indexed)

(defcheck solution-f0fc0a3
  (fn gol [board]
    (letfn [(get-live-in-row [row]
              (reduce (fn [acc [k v]]
                        (if (= v \#)
                          (conj acc k)
                          acc))
                []
                (zipmap (range (count row)) row)))

            (get-live-indices [board]
              (reduce
                (fn [acc [r cols]]
                  (if (empty? cols)
                    acc
                    (into acc (map (fn [c] [r c]) cols))))
                #{}
                (zipmap
                  (range (count board))
                  (map get-live-in-row board))))

            (get-surrounding [row col]
              (for [rows (range (dec row) (+ row 2))
                    cols (range (dec col) (+ col 2))
                    :let [pair [rows cols]]
                    :when (not= pair [row col])]
                pair))

            (live-neighbors [live-set [row col]]
              (reduce #(if (contains? live-set %2) (inc %) %)
                0
                (get-surrounding row col)))

            (cell-coords [board]
              (partition (count (first board))
                (for [rows (range (count board))
                      cols (range (count (first board)))]
                  [rows cols])))
            (live-or-not [live neighbors]
              (if live
                (cond
                  (< neighbors 2) " "
                  (or (= neighbors 2) (= neighbors 3)) "#"
                  (> neighbors 3) " "
                  :else "?")
                (if (= neighbors 3) "#" " ")))]
      (let [live-set   (get-live-indices board)
            next-state (map (fn [coord-row]
                              (map
                                #(live-or-not
                                   (contains? live-set %)
                                   (live-neighbors live-set %))
                                coord-row))
                         (cell-coords board))]
        (vec (map #(apply str %) next-state))))))

(defcheck solution-f2c8f032
  (fn [m]
    (let [w (count (first m))
          h (count m)
          p #(and (<= 0 %1)
                  (< %1 w)
                  (<= 0 %2)
                  (< %2 h)
                  (= \# (nth (m %2) %1)))]
      (vec (for [y (range h)]
             (apply str (for [x (range w)
                              :let [c (count (for [dx [-1 0 1]
                                                   dy [-1 0 1]
                                                   :when (p (+ x dx) (+ y dy))]
                                               1))]]
                          (if (if (p x y) (#{3 4} c) (= 3 c)) \# \space))))))))

(defcheck solution-f2e8e02b
  (fn run [g]
    (let [get-wrapped-cell-position (fn [row-position column-position offset grid]
                                      (let [row-pos         (+ row-position (:row offset))
                                            col-pos         (+ column-position (:column offset))
                                            wrapped-row-pos (cond (< row-pos 0)
                                                                  (- (count grid) 1)
                                                                  (> row-pos (- (count grid) 1))
                                                                  0
                                                                  :else
                                                                  row-pos)
                                            wrapped-col-pos (cond (< col-pos 0)
                                                                  (- (count (first grid)) 1)
                                                                  (> col-pos (- (count (first grid)) 1))
                                                                  0
                                                                  :else
                                                                  col-pos)]
                                        {:row wrapped-row-pos
                                         :col wrapped-col-pos}))
          offsets                   (remove nil? (for [row-offset (range -1 2) column-offset (range -1 2)]
                                                   (when (or (not= 0 row-offset) (not= 0 column-offset))
                                                     {:row row-offset :column column-offset})))
          get-neighbouring-cells    (fn
                                      [row-position column-position grid]
                                      (map #(get-wrapped-cell-position row-position column-position % grid) offsets))
          get-cell-contents         (fn
                                      [grid {:keys [col row]}]
                                      (nth (nth grid row) col))

          update-cell               (fn [cell row-position column-position grid]
                                      (let [neighbour-cell-locations (get-neighbouring-cells row-position column-position grid)
                                            neighbours               (map (partial get-cell-contents grid) neighbour-cell-locations)
                                            neighbour-count          (count (filter #(= \# %) neighbours))]
                                        (if (= cell \#)
                                          (or (= neighbour-count 2)
                                              (= neighbour-count 3))
                                          (= neighbour-count 3))))
          modify-row                (fn [row row-position grid] (map-indexed (fn [column-position cell] (update-cell cell row-position column-position grid)) row))]
      (map (fn [row] (apply str (map #(if % "#" " ") row)))
        (map-indexed (fn [position row] (modify-row row position g)) g)))))

(defcheck solution-f35ac1db
  (fn [cells]
    (letfn [
            (nbh [cells r c]
              (let [pr-r (if (zero? r) (dec (count cells)) (dec r))
                    n-r  (if (= (dec (count cells)) r) 0 (inc r))
                    pr-c (if (zero? c) (dec (count (first cells))) (dec c))
                    n-c  (if (= (dec (count (first cells))) c) 0 (inc c))]
                (-> []
                  (conj (get-in cells [pr-r pr-c]))
                  (conj (get-in cells [pr-r c]))
                  (conj (get-in cells [pr-r n-c]))
                  (conj (get-in cells [r pr-c]))
                  (conj (get-in cells [r n-c]))
                  (conj (get-in cells [n-r pr-c]))
                  (conj (get-in cells [n-r c]))
                  (conj (get-in cells [n-r n-c])))))
            (nbs [cells r c] (count (filter #(= \# %) (nbh cells r c))))]
      (let [mtx (map (fn [n] [n (range (count cells))]) (range (count cells)))]
        (map
          (fn [mtx-it]
            (apply str (map
                         (fn [row-it]
                           (if (= \# (get-in cells [(first mtx-it) row-it]))
                             (cond (or (< (nbs cells (first mtx-it) row-it) 2) (> (nbs cells (first mtx-it) row-it) 3)) \space
                                   :else \#)
                             (if (= 3 (nbs cells (first mtx-it) row-it)) \# \space)))
                         (second mtx-it))))
          mtx)))))

(defcheck solution-f449ef82
  (fn next-frame [board]
    (let [height (count board)
          width  (count (first board))]
      (letfn [(get-neighbors [[x y]]
                (remove (fn [[x y]]
                          (or (some identity (map #(= -1 %) [x y]))
                              (= x height) (= y width)))
                  (map (fn [[a b]] [(+ a x) (+ b y)])
                    [[0 1] [1 0] [0 -1] [-1 0]
                     [1 1] [1 -1] [-1 1] [-1 -1]])))
              (get-position [[x y]] (= \# (nth (board x) y)))
              (living-neighbors [pos] (->> (get-neighbors pos)
                                        (map get-position)
                                        (filter true?)
                                        (count)))]
        (->> (for [x (range height)]
               (->> (for [y (range width)]
                      (let [living?        (get-position [x y])
                            live-neighbors (living-neighbors [x y])]
                        (if (or (and living? (< 1 live-neighbors 4))
                                (= 3 live-neighbors))
                          \# \space)))
                 (reduce str)))
          (vec))))))

(defcheck solution-f4a509d2
  (fn [m]
    (let [around     [[-1 -1] [0 -1] [1 -1]
                      [-1 0] [1 0]
                      [-1 1] [0 1] [1 1]]
          state      (fn [m i j]
                       (if (and (< -1 i (count m)) (< -1 j (-> m first count)))
                         (nth (nth m i) j)
                         \space))
          live?      #(= % \#)
          live-count (fn [m i j]
                       (count (filter live? (map #(apply state m (map + [i j] %))
                                              around))))
          r          (count m)
          c          (-> m first count)
          mr         (for [i (range r)]
                       (for [j (range c) :let [lc (live-count m i j) e (state m i j)]]
                         (if (live? e)
                           (cond (or (< lc 2) (< 3 lc)) \space
                                 (<= 2 lc 3) \#)
                           (if (= lc 3) \# \space))))]
      (map #(apply str %) mr))
    ))

(defcheck solution-f553fa0c
  (fn [x]
    (let [bv             (mapv (comp vec seq) x)
          is-live?       (fn [[i j]] (= \# ((bv i) j)))
          maxi           (dec (count bv))
          maxj           (dec (count (first bv)))
          gen-neighbours (fn [ij] (remove (fn [[i j]] (or (neg? i) (neg? j) (> i maxi) (> j maxj))) (mapv #(mapv + ij %) [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])))]
      (vec
        (for [i (range (inc maxi))]
          (apply
            str
            (for [j (range (inc maxj))]
              (let [nc (count (filter is-live? (gen-neighbours [i j])))]
                (if (is-live? [i j])
                  (if (or (< nc 2) (> nc 3)) \space \#)
                  (if (not= nc 3) \space \#))))))))))

(defcheck solution-f5785a36
  (fn life [board]
    (let [size (count board)]
      (letfn [
              (char-at [board [x y]] (nth (seq (board y)) x))
              (next-state [live-neighbours c]
                (cond
                  (and (= c \#) (= live-neighbours 2)) \#
                  (= live-neighbours 3) \#
                  true \space))
              (neighbours [pos]
                (let [
                      dirs       [[-1 -1] [-1 0] [-1 1] [1 -1] [1 0] [1 1] [0 -1] [0 1]]
                      add-points (fn [& pts] (vec (apply map + pts)))
                      on-board?  (fn [[x y]] (and (>= x 0) (>= y 0) (< x size) (< y size)))
                      ]
                  (count
                    (filter (partial = \#)
                      (map
                        (fn [np] (char-at board np))
                        (filter on-board? (map #(add-points pos %) dirs)))))
                  )
                )
              ]
        (vec (map #(apply str %) (partition size (for [y (range size) x (range size)]
                                                   (next-state (neighbours [x y]) (char-at board [x y]))
                                                   ))))
        ))
    ))

(defcheck solution-f61b32e6
  (fn [board]
    (let [board (vec (map vec board))
          xs    (range 0 (count (first board)))
          neighbor-offsets
                (-> (for [dy (range -1 +2)
                          dx (range -1 +2)]
                      [dy dx])
                  (set)
                  (disj [0 0]))
          dead  \space
          live  \#]
      (letfn [(old-state [coords]
                (get-in board coords dead))
              (neighbor-coords [[y x]]
                (map (fn [[dy dx]] [(+ y dy) (+ x dx)]) neighbor-offsets))
              (count-live-neighbors [coords]
                (count (filter #(= live (old-state %)) (neighbor-coords coords))))
              (new-state [coords]
                (let [os (old-state coords)
                      n  (count-live-neighbors coords)]
                  (if (= live os)
                    (cond (< n 2) dead
                          (= n 2) live
                          (= n 3) live
                          (> n 3) dead
                          :else os)
                    (if (= n 3) live os))))
              (row-coords [y] (map vector (repeat y) xs))
              (add-cell [row coords] (conj row (new-state coords)))
              (add-row [b y] (conj b (apply str (reduce add-cell [] (row-coords y)))))]
        (reduce add-row [] (range 0 (count board)))))))

(defcheck solution-f654bc11
  (letfn
   [
    (get-elem [b [x y]]
      (nth (nth b x) y))
    (is-valid [b [x y]]
      (let [rows (count b)
            cols (count (first b))]
        (and
         (>= x 0)
         (>= y 0)
         (< x rows)
         (< y cols))))
    (get-neighbors [b [x y]]
      (filter #(is-valid b %1)
        (for [x1 [(dec x) x (inc x)]
              y1 [(dec y) y (inc y)]
              :when (not= [x y] [x1 y1])]
          [x1 y1])))
    (get-all [b]
      (for [x (range 0 (count b))
            y (range 0 (count (first b)))]
        [x y]))
    (get-state [b xy]
      [xy, (map #(get-elem b %) (get-neighbors b xy)), (get-elem b xy)])
    (get-next-state [[xy ns own]]
      (let [nc (count (filter #(= \# %1) ns))]
        [xy
         (cond (= own \space) (if (= 3 nc) \# \space)
               :else (if (or (> nc 3) (< nc 2)) \space \#))]))
    (get-next-board [b]
      (map get-next-state (map #(get-state b %) (get-all b))))
    (compose-board [bs]
      (map
        (comp clojure.string/join #(map (comp str second) %))
        (vals (group-by ffirst bs))))]
    (comp compose-board get-next-board)))

(defcheck solution-f73a33b1
  (fn [cells]
    (let [w           (count cells)
          h           (count (first cells))
          live?       (fn [x y] (= \# (get-in cells [y x])))
          count-lives (fn [cx cy] (count (for [x [(dec cx) cx (inc cx)]
                                               y [(dec cy) cy (inc cy)]
                                               :when (live? x y)]
                                           1)))
          row         (fn [y] (->> (range w)
                                (map (fn [x]
                                       (let [num-lives (count-lives x y)]
                                         (if (live? x y)
                                           (if (#{3 4} num-lives) "#" " ")
                                           (if (#{3} num-lives) "#" " ")))))
                                (apply str)))]
      (map row (range h)))))

(defcheck solution-f88a7815
  (letfn [(yx [m] (for [y (range 0 (count m)) x (range 0 (count (first m)))] [y x]))
          (lget [[y x] m] (if (and (< -1 y (count m)) (< -1 x (count (first m))))
                            (nth (m y) x)
                            \space))
          (lput [[y x] m v] (update-in m [y] #(reduce str (assoc (vec %) x v))))
          (lnb [[y x] b] (reduce (fn [r [y0 x0]] (if (= (lget [(+ y y0) (+ x x0)] b) \#)
                                                   (inc r)
                                                   r))
                           0
                           (remove #(= [0 0] %) (for [y (range -1 2) x (range -1 2)] [y x]))))]
    (fn [b] (reduce (fn [r i] (let [c (lnb i b)]
                                (lput i r (if (= (lget i b) \#)
                                            (if (<= 2 c 3) \# \space)
                                            (if (= 3 c) \# \space)))))
              b
              (yx b)))))

(defcheck solution-f9ee7de4
  (fn [board]
    (letfn [(count-neighbours [x y]
              (->> (for [dx [-1 0 1]
                         dy [-1 0 1]
                         :let [x' (+ x dx)
                               y' (+ y dy)]
                         :when (or (not= x x') (not= y y'))]
                     (get-in board [x' y']))
                (filter (partial = \#))
                count))]
      (for [x (range (count board))]
        (apply str (for [y (range (count (board x)))]
                     (let [cnt  (count-neighbours x y)
                           cell (get-in board [x y])]
                       (cond
                         (and (= cell \#) (#{2 3} cnt)) \#
                         (and (= cell \space) (= cnt 3)) \#
                         (= cell \#) \space
                         :else \space))))))))

(defcheck solution-faf0277
  (fn [board]
    (let [neighbors [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
          xdim      (count (first board))
          ydim      (count board)]
      (->> (for [y (range ydim)
                 x (range xdim)]
             (let [cell (nth (board y) x)
                   num  (->> (filter (fn [[x y]] (and (< -1 x xdim) (< -1 y ydim))) (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) neighbors))
                          (map (fn [[x y]] (nth (board y) x)))
                          (filter #{\#})
                          count)]
               (if (= cell \#)
                 (if (< 1 num 4) \# \space)
                 (if (= num 3) \# \space))))
        (partition xdim)
        (map (partial apply str))
        ))))

(defcheck solution-fb3aa51d
  (fn life [board] (let [x-dim (count (first board)) y-dim (count board)]
                     (map (partial apply str)
                       (partition x-dim
                         (for [x (range x-dim) y (range y-dim)]
                           (let [neighbors           (for [sub-x (range (dec x) (+ x 2)) sub-y (range (dec y) (+ y 2))]
                                                       (cond
                                                         (or (and (= sub-x x) (= sub-y y)) (< sub-x 0) (< sub-y 0) (>= sub-x x-dim) (>= sub-y y-dim)) nil
                                                         :else (get-in board (vector sub-x sub-y))))
                                 live-neighbor-count (count (filter #{\#} neighbors))
                                 this-cell           (get-in board (vector x y))]
                             (cond
                               (and (= this-cell \#) (< live-neighbor-count 2)) \space
                               (and (= this-cell \#) (< live-neighbor-count 4)) \#
                               (= this-cell \#) \space
                               (and (= this-cell \space) (= 3 live-neighbor-count)) \#
                               :else \space
                               )
                             )))))))

(defcheck solution-fb84f7d
  (fn next-iter [f]
    (let [
          cell-value (fn [f coord]
                       (let [[x y] coord]
                         (get (get f x) y)))
          neighbours (fn [f n m]
                       (->>
                         (for [a [-1 0 1] b [-1 0 1]]
                           [a b])
                         (remove #(= [0 0] %))
                         (map #(map + [n m] %))
                         (map #(cell-value f %))
                         (filter #(= \# %))
                         count
                         ))
          X          (count f)
          Y          (->> f first count)
          ]
      (->>
        (for [x (range X) y (range Y)]
          (case [(cell-value f [x y]) (neighbours f x y)]
            [\space 3] \#
            [\# 2] \#
            [\# 3] \#
            \space))
        (partition Y)
        (map #(apply str %))
        vec))))

(defcheck solution-fc1cabd5
  (fn [b]
    (let [bc         (mapcat (partial map vector) b (for [x (range)] (for [y (range)] [x y])))
          dim        (map inc (last (last bc)))
          coords     (map second (filter #(= \# (first %)) bc))
          adj        (fn [[x y]] (let [x- (dec x) x+ (inc x) y- (dec y) y+ (inc y)]
                                   [[x- y-] [x y-] [x+ y-] [x- y] [x+ y] [x- y+] [x y+] [x+ y+]]))
          adj-freqs  (frequencies (mapcat adj coords))
          keep?      (fn [[loc freq]] (if ((set coords) loc) (#{2 3} freq) (#{3} freq)))
          new-coords (set (map first (filter keep? adj-freqs)))]
      (map (partial apply str)
        (for [x (range (first dim))]
          (for [y (range (second dim))]
            (if (new-coords [x y]) \# \space)))))))

(defcheck solution-fde1ecd7
  (fn [board]
    (let [rows      (count board)
          cols      (count (board 0))
          neighbors (fn [[r c]] (for [i (range -1 2) j (range -1 2) :when (and (not= [0 0] [i j]) (< -1 (+ r i) rows) (< -1 (+ c j) cols))] [(+ r i) (+ c j)]))
          ln        (fn [[r c]] (count (filter #(= \# (get-in board %)) (neighbors [r c]))))
          nxtgen    (fn [[r c]] (let [lvn (ln [r c]) ch (get-in board [r c])]
                                  (cond (and (= ch \#) (< lvn 2)) \space
                                        (and (= ch \#) (<= 2 lvn 3)) \#
                                        (and (= ch \#) (< 3 lvn)) \space
                                        (and (= ch \space) (= 3 lvn)) \#
                                        :else \space)))]
      (vec (for [r (range rows)]
             (apply str (for [c (range cols)]
                          (nxtgen [r c]))))))))

(defcheck solution-fe02bffb
  (fn [rows]
    (letfn [(live-nbors [y x] (for [dy [-1 0 1]
                                    dx [-1 0 1]
                                    :let [n (get-in rows [(+ y dy) (+ x dx)])]
                                    :when (and (not= 0 dy dx)
                                               (= \# n))]
                                n))]
      (map-indexed
        (fn [y row] (clojure.string/join
                      (map-indexed
                        (fn [x c] (let [nbors (live-nbors y x)]
                                    (if (= c \#)
                                      (cond (< (count nbors) 2) \space
                                            (<= (count nbors) 3) \#
                                            :else \space)
                                      (if (= (count nbors) 3)
                                        \#
                                        \space))))
                        row)))
        rows))))

(defcheck solution-ff217141
  (fn next-gen [board]
    (let [rows         (count board)
          cols         (count (first board))
          cell         (fn [[row col]] (nth (nth board row) col))
          value        #(if (= \# %) 1 0)
          left         (fn [[row col]] [row (dec col)])
          right        (fn [[row col]] [row (inc col)])
          top          (fn [[row col]] [(dec row) col])
          bottom       (fn [[row col]] [(inc row) col])
          top-left     #(top (left %))
          top-right    #(top (right %))
          bottom-left  #(bottom (left %))
          bottom-right #(bottom (right %))
          valid?       (fn [[row col]] (and (>= row 0) (>= col 0) (< row rows) (< col cols)))
          cell-value   #(if (valid? %) (value (cell %)) 0)
          neighbours   [left right top bottom top-left top-right bottom-left bottom-right]
          score        (fn [[row col]] (apply + (map (fn [f] (cell-value (f [row col]))) neighbours)))]

      (reduce (fn [acc-rows r] (let [row (count acc-rows)]
                                 (concat acc-rows [
                                                   (reduce (fn [acc-cols c] (let [col (count acc-cols)
                                                                                  sc  (score [row col])
                                                                                  new (if (= (cell [row col]) \#)
                                                                                        (cond (< sc 2) \space (< sc 4) \# :else \space)
                                                                                        (if (= sc 3) \# \space))]

                                                                              (str acc-cols new)

                                                                              )) "" r)])
                                 )) [] board))))

(defcheck solution-ff317db3
  (fn f [b]
    (let [get-live-cells  (fn [b]
                            (let [h (count b)
                                  w (count (first b))]
                              (for [i (range 0 h)
                                    j (range 0 w) :when (= (get-in b [i j]) \#)]
                                [i j])))
          count-neighbors (fn [b i j]
                            (let [lc (get-live-cells b)]
                              (count
                                (for [i' (range (dec i) (+ i 2))
                                      j' (range (dec j) (+ j 2))
                                      :when (let [neighbor (some #{[i' j']} lc)]
                                              (if (and neighbor (not= neighbor [i j]))
                                                neighbor))]
                                  [i' j']))))
          next-cell       (fn [b i j]
                            (let [n (count-neighbors b i j)
                                  c (get-in b [i j])]
                              (cond
                                (and (= c \#) (< n 2)) \space
                                (and (= c \#) (<= n 3)) \#
                                (= c \#) \space
                                (= n 3) \#
                                :else \space)))
          h               (count b)
          w               (count (first b))
          flat-board      (for [i (range 0 h)
                                j (range 0 w)]
                            (next-cell b i j))
          b'              (map #(apply str %) (partition h flat-board))
          ]
      b')))

(defcheck solution-ff6a0c96
  (fn next-gen [board]
    (let [
          m            (count (first board))
          n            (count board)
          ; TODO - investigate bijections in clojure
          alive?       {\# true, \space false}
          encode-cell  {true \#, false \space}
          in-bounds    (fn [lower upper]
                         (partial filter #(when (and (<= lower %) (< % upper)) %)))
          >>=          (fn [xs f]
                         (apply concat (map f xs)))
          flip         (fn [f b a]
                         (f a b))
          neighborhood (fn [x y]
                         (let [xs ((in-bounds 0 m) (range (- x 1) (+ x 2)))
                               ys ((in-bounds 0 n) (range (- y 1) (+ y 2)))]
                           (->> (>>= xs #(for [y ys] [% y]))
                             (set)
                             (flip disj [x y]))))
          board-value  (fn [[x y]]
                         (nth (board y) x))
          new-cell     (fn [y]
                         (fn [x]
                           (let [neighbors (->> (neighborhood x y) (map board-value) (filter alive?) (count))
                                 alive     (alive? (board-value [x y]))]
                             (encode-cell (cond
                                            (< 3 neighbors) false
                                            (== 3 neighbors) true
                                            (== 2 neighbors) alive
                                            (< neighbors 2) false)))))
          new-row      (fn [y]
                         (->> (map (new-cell y) (range m)) (apply str)))
          ]
      (->> (map new-row (range n)) (apply vector)))))

(defcheck solution-ffc91e34
  (fn game-of-life-step-v2 [x]
    (let [w                   (count (first x))
          h                   (count x)
          to-int              (fn [board pos] (if (contains? board pos) 1 0))
          get-from-row        (fn [idx row] (filter #(not= (second %) \space) (map-indexed #(vector [idx %1] %2) row)))
          taken               (set (map first (apply concat (map-indexed #(get-from-row %1 %2) x))))
          get-neighbours      (fn [[r0 c0] other]
                                (for [dr [-1 0 1]
                                      dc [-1 0 1]
                                      :when (not (= dr dc 0))
                                      :let [r (+ r0 dr)
                                            c (+ c0 dc)]]
                                  (to-int other [r c])))
          get-neighbour-count (fn [pos other] (reduce + (get-neighbours pos other)))
          neighbour-and-this  (for [r (range h)
                                    c (range w)]
                                [(get-neighbour-count [r c] taken) (to-int taken [r c])])
          new-vals            (partition w (map #(cond
                                                   (and (pos? (second %)) (< (first %) 2)) 0
                                                   (and (pos? (second %)) (> (first %) 3)) 0
                                                   (pos? (second %)) 1
                                                   (= (first %) 3) 1
                                                   :else 0)
                                             neighbour-and-this))
          row-to-string       (fn [x] (apply str (map #(if (zero? %) \space \#) x)))
          as-string           (map row-to-string new-vals)
          ]
      as-string)))
