(ns coal-mine.problem-89
  (:require [coal-mine.checks :refer [defcheck-89] :rename {defcheck-89 defcheck}]
            [clojure.test]
            [clojure.walk]
            [clojure.set]))

(defcheck solution-104d52cb
  (fn [c x]
    (let [m (frequencies (flatten x))
          v (vals m)]
      (and (not-any? #(>= % (c m)) v)
           (>= 2 (c (filter #{1} v)))))) count)

(defcheck solution-1140cfb
  (fn [graph]
    (let [[hf hl] (first graph)]
      (if (< (count graph) 2) true
                              (let [grouped (group-by #(or (= hl (first %)) (= hl (last %))) (rest graph))
                                    near (grouped true) far (grouped false)]
                                (if (zero? (count near)) false
                                                         (let [con (if (= hl (last (first near))) (first (first near))
                                                                                                  (last (first near)))] ; [a b] [b c] ~ [a c]
                                                           (if (= con hf) ; circular means done
                                                             (recur (into far (rest near)))
                                                             (recur (conj (into far (rest near)) [hf con]))))))))))

(defcheck solution-11d522ee
  (fn walk
    ([s] (walk (ffirst s) s))
    ([n s]
     (if (empty? s)
       true
       (let [m (group-by (fn [[a b]] (or (= a n) (= b n))) s)
             ts (m true)
             fs (m false)]
         (if (empty? ts)
           false
           (not-every? (partial = false)
             (map #(walk (let [[a b] (nth ts %)] (if (= n a) b a))
                     (concat (take % ts) (nthnext ts (inc %)) fs))
               (range (count ts))))))))))

(defcheck solution-1251d349
  (fn [s]
    (or (= (count s) 1)
        (and (= (count (distinct (flatten s)))
               (count (nth
                        (iterate
                          (fn [o] (into o (for [n o [a b :as v] s :when ({a b} n)] (if (= n a) b a))))
                          #{(ffirst s)})
                        (count s))
                 ))
             (not-any? #(odd? (val %)) (frequencies (flatten s)))))))

(defcheck solution-12c91191
  (fn __ [es]
    (let [
          v_set (-> es flatten set)
          get_con_sets
                (fn [ss v]
                  (conj
                    (map #(if (not= () (filter % v)) (into % v) %)  ss)
                    (set v)))
          max_con_set #(reduce (fn [s1 s2] (if (> (count s1) (count s2)) s2 s2)) #{} %)
          all_connected? (= v_set (max_con_set (reduce get_con_sets [] es)))
          one_path? (->> es flatten frequencies vals (filter odd?) count #{0 2} (not= nil))
          ]
      (and one_path? all_connected?))))

(defcheck solution-1403733c
  (fn [g]
    (let [s (map conj g (range)) ; conjoin ids to edges
          check (fn iterCheck [n s]
                  (if (empty? s)
                    true
                    (some
                      (fn [e]
                        (if (some #(= % n) (take 2 e))
                          (iterCheck (first (remove #(= n %) e)) (remove #(= e %) s))
                          false))
                      s)))]
      (boolean (some (fn [e] (check (first e) (remove #(= e %) s))) s)))))

(defcheck solution-14d9e948
  (fn gt [edges]
    (let [vc (reduce (fn [g [a b]] (assoc g a (inc (get g a 0)))) {} (concat edges (map reverse edges)))
          g (reduce (fn [g [a b]] (assoc g a (conj (get g a #{}) b))) {} (concat edges (map reverse edges)))
          od (count (filter odd? (vals vc)))
          connected? (loop [queue [(first (keys g))] visited #{}]
                       (if-let [n (first queue)]
                         (recur (into (subvec queue 1) (clojure.set/difference (g n) visited)) (conj visited n))
                         (= (count visited) (count g))))]

      (and connected? (or (= 0 od) (= 2 od))))))

(defcheck solution-14e67d33
  (fn [g]
    (letfn [(is-connected [g]
              (let [flyd (fn f [r]
                           (letfn [(p [r]
                                     (let [s (for [x r
                                                   y r
                                                   :when (= (second x) (first y))]
                                               [(first x) (second y)])]
                                       (into r s)))]
                             (let [s (p r)]
                               (if (= r s)
                                 r
                                 (f s)))))]
                (let [n (count (set (apply concat g)))
                      g (into g (map #(reverse %) g))
                      m (count (flyd g))]
                  (= m (* n n)))))
            (has-euler [g]
              (let [v (into (set (map #(first %) g)) (map #(second %) g))
                    e (map (fn [i] (filter #(or (= i (first %)) (= i (second %))) g)) v)
                    a (count (filter odd? (map count e)))]
                #_(println e)
                (or (zero? a) (= 2 a))))]
      (and (is-connected (set g)) (has-euler g)))))

(defcheck solution-1535ae54
  (fn [ns]
    (let [candidates
          (memoize
            (fn [xs]
              (for [[p1 p2] (map #(split-at (inc %) xs) (range (count xs)))]
                (let [[a b] (last p1) rs (concat (butlast p1) p2)]
                  [a b rs]))))
          edge?
          (memoize
            (fn [a b sts]
              (let [h (-> sts first first)
                    t (-> sts last last)
                    stsv (vec sts)]
                (filter identity
                  (if (empty? sts)
                    [[[a b]]]
                    [(cond
                       (= a h) (cons [b a] stsv)
                       (= b h) (cons [a b] stsv)
                       :else nil)
                     (cond
                       (= a t) (conj stsv [a b])
                       (= b t) (conj stsv [b a])
                       :else nil)])))))
          runstate
          (memoize
            (fn runstate [sts a b xs]
              (let [stss' (edge? a b sts)]
                (cond
                  (empty? stss') false
                  (empty? xs) true
                  :else
                  (not-every? false?
                    (for [[a' b' xs']
                          (candidates xs)
                          sts' stss'] (runstate sts' a' b' xs')))))))]
      (not-every? false?
        (for [[a b xs]
              (candidates ns)]
          (runstate [] a b xs))))))

(defcheck solution-15709d9b
  (fn myf [coll]
    (let [all (->> (remove #(= (first %) (second %)) coll)
                flatten
                (group-by identity)
                vals
                (map count))
          odd (filter odd? all)
          even (filter even? all)]
      (cond (empty? all) false
            (empty? odd) true
            (= 2 (count odd)) true
            :else false))))

(defcheck solution-15a8eabb
  (fn [g]
    (letfn [(c [m l a]
              (if (= l a)
                (= (set (keys m)) a)
                (recur
                  m
                  a
                  (reduce
                    #(into % (m %2))
                    a
                    a))))]
      (let [a (reduce
                (fn [x [k v]]
                  (update-in x [k] conj v))
                {}
                g)
            b (reduce
                (fn [x [v k]]
                  (update-in x [k] conj v))
                {}
                g)
            m (merge-with into a b)]
        (and (->> m
               vals
               (filter #(odd? (count %)))
               (#(< (count %) 3)))
             (c m #{} #{(ffirst m)}))))))

(defcheck solution-15ce6ec8
  (fn contains-euler-path? [g]
    (let [g (sort-by first g) f (flatten g)
          connected? (= (count (reduce (fn [seen [n m]] (if (seen n) (conj seen m) seen)) #{(ffirst g)} g))
                       (count (distinct f)))
          count-odd  (count (filter #(odd? %) (vals (frequencies f))))]
      ;; From http://www.ctl.ua.edu/math103/euler/howcanwe.htm:
      ;; if the graph is connected and has at most 2 nodes with odd degree,
      ;; then it contains at least one Euler path
      (and connected? (<= count-odd 3)))))

(defcheck solution-162dd60d
  (fn [vvec]
    (or (= 1 (count vvec))
        (let [[start, end] (first vvec)]
          (loop [os [[start (rest vvec)]]]
            (cond (some (fn [[v r]] (and (empty? r) (= v end))) os) true
                  (empty? os) false
                  :else
                  (let [step-fn (fn [f1 f2 l vv]
                                  (let [vf (filter #(= l (f1 %)) vv)
                                        nl (map f2 vf)]
                                    (distinct (partition 2 (interleave nl vf)))))
                        step-fn2 (fn [l vv]
                                   (map (fn [[l2 vr]] [l2 (vec (remove #(= vr %) vv))])
                                     (concat (step-fn first second l vv)
                                       (step-fn second first l vv))))]
                    (recur (mapcat (partial apply step-fn2) os)))))))))

(defcheck solution-165b03bf
  (letfn [(links [edge node]
            (or (= (edge 0) node)
                (= (edge 1) node)))
          (edges-from [graph node]
            (filter #(links % node) graph))
          (inescapable? [graph node]
            (empty? (edges-from graph node)))
          (other [edge node]
            (if (= (edge 0) node) (edge 1) (edge 0)))
          (move [graph node]
            (loop [untested graph tested []]
              (cond
                (empty? untested) nil
                (links (first untested) node) [(other (first untested) node)
                                               (concat (rest untested) tested)]
                :else
                (recur (rest untested) (conj tested (first untested))))))]
    (fn [graph]
      (loop [current-node (get-in graph [0 0])
             remaining-edges graph]
        (cond
          (empty? remaining-edges) true
          (inescapable? remaining-edges current-node) false
          :else (let [[new-pos rem-edg] (move remaining-edges current-node)]
                  (recur new-pos rem-edg)))))))

(defcheck solution-168d2ade
  (fn __ [l]
    (let [dn (distinct (flatten l))
          dl (distinct l)
          edeg (not (some odd? (vals (frequencies (flatten dl)))))]
      (if (= (count dl) 1)
        true
        (loop [cedge (first l)
               cl (rest l)
               accnds (distinct cedge)]
          (if (empty? cl)
            (and edeg (= (count dn) (count accnds)))
            (if (or (some #(= % (first cedge)) accnds)
                    (some #(= % (second cedge)) accnds))
              (recur (first (remove #(= % cedge) cl))
                (remove #(= % cedge) cl)
                (distinct (into accnds cedge)))
              (recur [] [] []))))))))

(defcheck solution-1714d3c4
  (letfn [
          (degree [graph x]
            (count (for [[x1 y1] graph
                         :when (or (= x1 x) (= y1 x))
                         ]
                     1)))
          (is-eulerian [graph]
            (if (= (count graph) 1) true
                                    (and
                                     (not (some (partial apply =) graph)) ; cheating, here we're checking for self-edges, but we really need to check for
                                     ; connectivity
                                     (> 3 (count (filter odd? (map (partial degree graph) (->> graph flatten distinct))))))))]
    is-eulerian))

(defcheck solution-17a5f27d
  (fn f
    [g]
    (let [c? (fn [m [a b] l]
               (cond (= a (l m))
                     [true second]
                     (= b (l m))
                     [true first]
                     :t [false []]))
          d (fn d [n gr l v]
              (if gr
                (if (>= v (count gr))
                  false
                  (let [[a & as] gr
                        [s w] (c? n a l)]
                    (if s
                      (d a as w 0)
                      (d n (conj (vec as) a) l
                        (+ 1 v)))))
                true))
          k (fn k [[h & hs] n]
              (if (or (d h hs first 0)
                      (d h hs second 0))
                true
                (if (= (sort n)
                      (sort g))
                  false
                  (k (conj (vec hs) h) (conj n h)))))]
      (k g []))))

(defcheck solution-18446be2
  (fn sn [edges]
    (let [vertices (-> edges flatten set)
          remove  #(remove (partial identical? %) %2)
          tour?  (fn tour? [v edges]
                   (if (empty? edges) true
                                      (let [es (filter #((set %) v) edges)
                                            f  (fn [[x y :as z]]
                                                 (tour? (if (= v x) y x)
                                                   (remove z edges)))]
                                        (some f es))))
          ]
      (->> vertices
        (some #(tour? % edges))
        boolean))))

(defcheck solution-18812ba5
  (let [
        starting-points (fn starting-points [edges]
                          (set (mapcat identity edges)))

        move-to (fn move-to [start edge]
                  (cond
                    (= (first edge) start) (second edge)
                    (= (second edge) start) (first edge)
                    :else nil))

        remove-one (fn remove-one [edges edge]
                     (cond
                       (empty? edges) '()
                       (= (first edges) edge) (rest edges)
                       :else (cons (first edges) (remove-one (rest edges) edge))))

        search-on (fn search-on [start edges]
                    (if (empty? edges)
                      true
                      (let [next-moves (filter second (map (juxt identity #(move-to start %)) edges))]
                        (if (some identity (map (fn [[edge next]] (search-on next (remove-one edges edge))) next-moves)) true false))))]

    (fn [edges] (if (some identity (map #(search-on % edges) (starting-points edges))) true false))))

(defcheck solution-18ef4017
  (fn cycle? [graph]
    (letfn [(connected-nodes-and-nextval? [a [c d]] (cond (= a c) d
                                                          (= a d) c
                                                          :else false))
            (iterfn [node nodes]
              (if (empty? nodes)
                (= node (ffirst graph))
                (let [tocheck (into #{} (filter #(connected-nodes-and-nextval? node %) nodes))]
                  (if (empty? tocheck) false
                                       (reduce #(or %1 %2) (for [val tocheck] (iterfn (connected-nodes-and-nextval? node val) (remove #{val} nodes)))))
                  )
                )
              )]
      (if (= (count graph) 1) true
                              (iterfn (second (first graph)) (rest graph))
                              )
      )
    ))

(defcheck solution-197f6be
  (fn tour? [g]
    (letfn [(nodes [g] (set (apply concat g)))
            (node-in-edge? [n [n1 n2]] (or (= n n1) (= n n2)))
            (node-edges [g n] (filter (partial node-in-edge? n) g))
            (other-end [[e1 e2] n] (if (= n e1) e2 e1))
            (graph-without-edge [[h & t] e] (if (= h e) t (cons h (graph-without-edge t e))))
            (last-edge? [[e & g] n] (and (empty? g) (node-in-edge? n e)))
            (tour-starting? [g n] (or (last-edge? g n) (some (fn [e] (tour-starting? (graph-without-edge g e) (other-end e n))) (node-edges g n))))]
      (true? (some (partial tour-starting? g) (nodes g))))))

(defcheck solution-19df0fe8
  (fn [g]
    (letfn [(g->m [g]
              (->> g
                (group-by first)
                (map (fn [[node items]]
                       [node (set (map second items))]))
                (into {})))
            (expand [m]
              (into {}
                (for [[f ts] m]
                  [f (set (apply concat ts
                            (for [t ts]
                              (get m t))))])))
            (connected? [g]
              (let [g1 (g->m g)
                    g2 (g->m (map (comp vec reverse) g))
                    m (merge-with #(set (concat %1 %2)) g1 g2)
                    size (count m)]
                (= size
                  (-> (nth (iterate expand m) size) vals first count))))]
      (let [f (frequencies (concat (map first g) (map second g)))
            odds (count ((group-by even? (vals f)) false))]
        (boolean
          (and (#{0 2} odds)
               (connected? g)))))))

(defcheck solution-1be3f6da
  (fn euler-tour [edge-list]
    (letfn
     [(remove-first [pred coll]
        (if (pred (first coll))
          (rest coll)
          (cons (first coll) (remove-first pred (rest coll)))))
      (find-path [start vertices edges]
        (if (empty? (edges start))
          [vertices edges]
          (let [edge (first (edges start))
                v (first (filter #(not= start %) edge))]
            (if (nil? v)
              (recur start vertices (assoc edges start (rest (edges start))))
              (recur v (conj vertices v) (assoc edges
                                           start (rest (edges start))
                                           v (remove-first #(= edge %) (edges v))))
              )
            )
          )
        )
      ]
      (let [edges (merge-with concat (group-by first edge-list) (group-by last edge-list))
            start (first (first edges))
            [vs es] (find-path start #{start} edges)]
        (if (empty? (filter (complement empty?) (vals es)))
          true
          false
          )
        )
      )
    ))

(defcheck solution-1d2f463c
  (fn [edges]
    (let [stars (reduce
                  (fn [m [a b]]
                    (merge-with concat
                      m
                      (if (= a b) {a [b]}
                                  {a [b] b [a]})))
                  {} edges)
          vertices (set (keys stars))
          accessible-vertices (loop [verts (if (empty? vertices) #{} #{(first vertices)})]
                                (let [verts2 (apply clojure.set/union
                                               verts
                                               (map #(set (get stars % [])) verts))]
                                  (if (= verts verts2)
                                    verts
                                    (recur verts2))))
          odd-count (->> (vals stars)
                      (map count)
                      (filter odd?)
                      count)]
      (and (= vertices accessible-vertices) (<= odd-count 2)))))

(defcheck solution-1d3d5c55
  (fn [edges]
    (let [remove-first (fn [v elt]
                         (let [index (.indexOf v elt)]
                           (if (>= index 0)
                             (vec (concat (subvec v 0 index) (subvec v (inc index))))
                             v)))
          directed->undirected (fn f [f]
                                 (fn [graph a b]
                                   (if (= a b)
                                     (f graph a b)
                                     (f (f graph a b) b a))))
          add1-edge (fn [graph from to] (assoc graph from (conj (graph from []) to)))
          add-edge (directed->undirected add1-edge)
          remove1-edge (fn [graph from to]
                         (let [nodes (graph from)]
                           (cond
                             (nil? nodes) graph
                             (= nodes [to]) (dissoc graph from)
                             :else (assoc graph from (remove-first nodes to)))))
          remove-edge (directed->undirected remove1-edge)
          traverse (fn traverse [graph node]
                     (cond
                       (zero? (count graph)) true
                       (nil? (graph node)) false
                       :else (boolean (some (fn [next-node]
                                              (traverse (remove-edge graph node next-node) next-node))
                                        (graph node)))))
          graph (reduce (fn [graph [a b]] (add-edge graph a b)) {} edges)]
      (boolean (some (partial traverse graph) (keys graph))))))

(defcheck solution-1d9097f1
  (fn graph-tour [edges]
    (let [edgeCountMap (frequencies edges)
          connect-graph (memoize (fn connect-graph?[node remainingEdges]
                                   (if (seq remainingEdges)
                                     (let[nextNodesEdges (for [[s e] (keys remainingEdges) :when (or (= node s) (= node e))]
                                                           [(if(= node s) e s)
                                                            (if (= (remainingEdges [s e]) 1)
                                                              (dissoc remainingEdges [s e])
                                                              (assoc remainingEdges [s e] (dec (remainingEdges [s e]))))])]
                                       (if (seq nextNodesEdges)
                                         (reduce #(or %1 %2) (map (partial apply connect-graph?) nextNodesEdges))
                                         false))
                                     true)))]
      (loop[nodes (distinct (flatten edges))]
        (if (seq nodes)
          (if (connect-graph (first nodes) edgeCountMap) true
                                                         (recur (rest nodes)))
          false)))))

(defcheck solution-1e444284
  (fn [graph]
    (let [edgeset (apply hash-set (distinct graph))
          vertices (apply hash-set (distinct (flatten graph)))
          adj (fn[v1 v2] (or (edgeset [v1 v2]) (edgeset [v2 v1])))
          EV (fn [v] (->> graph
                       (filter (fn [[x y]] (or (= v y) (= v x))))
                       (count)
                       (even?)))
          E (count (filter EV vertices))
          connected (fn [vset visited]
                      (let [nvset (apply hash-set (distinct
                                                    (for [n vertices v vset
                                                          :when (and (adj v n)
                                                                     ((complement visited) n))]
                                                      n)))
                            nvisited (clojure.set/union vset visited)]
                        (cond (= vertices nvisited) true
                              (empty? nvset) false
                              :else (recur nvset nvisited))))]
      (and (connected #{(first vertices)} #{})
           (or (= E (count vertices))
               (= E (- (count vertices) 2)))))))

(defcheck solution-1fef0aad
  (fn has-eulerian-path? [[[a b :as edge] & remaining-edges]]
    (or (empty? remaining-edges)
        (loop [last-node b
               remaining-edges remaining-edges]
          (boolean (some (fn [[x y :as next-edge]]
                           (when (contains? (set next-edge) last-node)
                             (has-eulerian-path? (cons (if (= last-node x) [x y] [y x])
                                                   (disj (set remaining-edges) next-edge)))))
                     remaining-edges))))))

(defcheck solution-20290a07
  (fn graph-tour [ss]
    (let [points (reduce #(into %1 %2) #{} ss)
          num (count points)
          tours (reduce (fn [rs [a b]] (assoc rs a  (conj (rs a) b) b  (conj (rs b) a)))  {} ss)
          #_#__ (println tours)
          ]
      (letfn [(step [p rs]
                (set (reduce #(if(%1 %2) %1 (step %2 (conj %1 %2))) rs (tours p))))]
        (and
         (not (nil? (#{0 2} (count (filter #(odd? (count %)) (vals tours))))))
         (= num (count ((memoize step) (first points) #{}))))))))

(defcheck solution-203ed4a0
  (fn [edges]
    (if (= 1 (count edges))
      true
      (let [edges (remove #(= (first %) (second %)) edges)
            inout-degs (frequencies (flatten edges))]
        (and (< 0 (count edges))
             (every? even? (vals inout-degs)))))))

(defcheck solution-20615bb7
  (let [any? (comp not not some)
        rem1 (fn [s x]
               (let [[t d] (split-with (complement #{x}) s)]
                 (concat t (next d))))
        other (fn [x [a b]] (if (= x a) b a))
        nodes (partial reduce into #{})]
    (fn tourable?
      ([edges] (any? #(tourable? edges %) (nodes edges)))
      ([edges node]
       (some #(let [es (rem1 edges %)]
                (or (empty? es)
                    (tourable? es (other node %))))
         (distinct (filter #(some #{node} %) edges)))))))

(defcheck solution-20dab4b3
  (fn [edges]
    (letfn [(rember [coll v]
              (cond
                (empty? coll) '()
                (= (first coll) v) (rest coll)
                :else (cons (first coll) (rember (rest coll) v))))
            (add-connection [graph k dk]
              (assoc graph k (if (contains? graph k) (conj (graph k) dk) [dk])))
            (remove-connection [graph k dk]
              (let [graph* (update-in graph [k] rember dk)]
                (if (empty? (graph* k))
                  (dissoc graph* k)
                  graph*)))
            (add-edge [graph a b]
              (-> graph (add-connection a b) (add-connection b a)))
            (remove-edge [graph a b]
              (-> graph (remove-connection a b) (remove-connection b a)))
            (parse-graph [edges]
              (reduce (fn [graph [a b]] (add-edge graph a b)) {} edges))
            (edgeless? [graph]
              (or (empty? graph)
                  (and (= (count graph) 1) (empty? (val (first graph))))))
            (starts-tour? [graph k]
              (or (empty? graph)
                  (boolean (some (fn [dk] (starts-tour? (remove-edge graph k dk) dk))
                             (graph k)))))]
      (starts-tour? (parse-graph edges) (ffirst edges)))))

(defcheck solution-20fcb30a
  (fn [graph]
    (let [verts (distinct (flatten graph))
          vertDegrees (for [vert verts] (count (filter (fn [edge] (some #(= vert %) edge)) graph)))
          numOdd (count (filter #(odd? %) vertDegrees))
          vertSet (into {} (map #(hash-map % #{%}) verts))
          connected (reduce (fn [vertSet edge]
                              (merge-with clojure.set/union (hash-map (second edge) (vertSet (first edge)))
                                (merge-with clojure.set/union vertSet
                                  (hash-map (first edge) (vertSet (second edge)))))) vertSet graph)]
      #_(println "connected " connected)
      #_(println "verts " verts " vertDegrees " vertDegrees " numOdd " numOdd " vertSet " vertSet " connected " connected)
      (and (or (= numOdd 0) (= numOdd 2)) (not (nil? (some #(= % (count verts)) (map #(count (connected %)) verts)))))
      )))

(defcheck solution-2102fc64
  (fn myGraphTour
    [graph]
    (letfn [(remove-first [val coll] (let [[n m] (split-with (partial not= val) coll)] (concat n (rest m))))
            (possible-paths [val coll]
              (remove nil? (map #(cond
                                   (= (second val) (first %)) (hash-map % (remove-first % coll))
                                   (= (second val) (second %)) (hash-map (reverse %) (remove-first % coll)))
                             coll)))
            (create-new-result [paths coll] ())]
      (loop [result (map #(hash-map % (remove-first % graph)) graph) i 1]
        (if (= i (count graph))
          (not (empty? result))
          (recur (mapcat #(possible-paths (key (last %)) (val (last %))) result) (inc i)))))))

(defcheck solution-21244e55
  (fn tourable? [edges]
    ; Note that the graph is defined with a collection of edges.  Therefore all
    ; vertices will have a nonzero degree.  If vertices with zero degree were
    ; part of the graph, they would have to be excluded from the "connected
    ; component" check.
    (let [
          vertices-of (fn [edges]
                        (reduce #(apply conj %1 %2) #{} edges))
          vertices (vertices-of edges)
          degree (fn [vertex]
                   (let [c (fn [acc [x y]]
                             (+ acc (if (= x vertex) 1 0)
                               (if (= y vertex) 1 0)))]
                     (reduce c 0 edges)))
          odd-degree? (fn [vertex]
                        (== 1 (mod (degree vertex) 2)))
          graph-connected? (fn [edges]
                             (let [
                                   arbitrary-vertex (first vertices)
                                   add-all (fn [xs set]
                                             (apply conj set xs))
                                   remove-all (fn [xs set]
                                                (apply disj set xs))
                                   impl (fn [acc xs rem]
                                          (if (empty? rem)
                                            true
                                            (let [neighbors (->> (filter #(or (xs (% 0)) (xs (% 1))) edges)
                                                              (vertices-of)
                                                              (remove-all acc)
                                                              (remove-all xs))]
                                              (if (empty? neighbors)
                                                false
                                                (recur (add-all xs acc) neighbors (remove-all neighbors rem))))))
                                   ]
                               (impl #{} #{arbitrary-vertex} (disj vertices arbitrary-vertex))))
          number-of (fn [pred xs]
                      (reduce #(+ %1 (if (pred %2) 1 0)) 0 xs))
          ]
      (and (<= (number-of odd-degree? vertices) 2)
           (graph-connected? edges)))))

(defcheck solution-21ab7751
  (fn [g]
    ((fn f [c v] (or (some
                       (fn [[i e j]]
                         (if (< i 0) nil
                                     (f (nth e (if (zero? i) 1 0)) (into (subvec v 0 j) (subvec v (inc j))))))
                       (map-indexed #(vector (.indexOf %2 c) %2 %) v)) (empty? v))) (ffirst g) g)))

(defcheck solution-220bd46a
  (fn euler? [edges]
    (letfn [
            (connected? [edges]
              (letfn [(bfs [edges start]
                        (
                         (fn breadth-first-search [edges [current & waiting :as visit-queue] visited]
                           (if (empty? visit-queue)
                             visited
                             (let [
                                   candidate-edges (filter (fn [[a b]] (or (= a current) (= b current))) edges)
                                   candidate-nodes (distinct (mapcat (fn [edge] (filter #(not= % current) edge)) candidate-edges))
                                   new-nodes (filter #(not (contains? visited %)) candidate-nodes)
                                   ]
                               (recur edges (concat new-nodes waiting) (conj visited current)))))
                         edges [start] #{}))]
                (if (empty? edges)
                  true
                  (let [
                        all-nodes (into #{} (apply concat edges))
                        connected-nodes (bfs edges (first all-nodes))
                        ]
                    (= connected-nodes all-nodes)))))
            (connected-to? [[a b :as edge] node]
              (or (= a node) (= b node)))
            (degree [edges node]
              (count (filter #(connected-to? % node) edges)))
            ]
      (let [
            nodes (into #{} (apply concat edges))
            degrees (map #(degree edges %) nodes)
            odd-degrees (count (filter odd? degrees))
            ]
        (and
         (connected? edges)
         (<= odd-degrees 2))))))

(defcheck solution-2234720
  (fn [edgeset]
    (letfn [(degreesOK? [g]
              (< (count (filter odd?
                          (map count (vals (group-by identity
                                             (flatten (vec g)))))))
                3))
            (connected? [edges]
              (letfn
               [(graph [edges]
                  (let [edges2 (->> edges
                                 (map reverse)
                                 (concat edges)
                                 (map vec)
                                 set
                                 (group-by first))]
                    (apply conj {}
                      (for [k (keys edges2)]
                        [k (map second (get edges2 k))]))))
                (reach [vlis graph]
                  (let [vlis2 (distinct
                                (concat vlis
                                  (apply concat
                                    (map graph vlis))))]
                    (if (= (count vlis) (count vlis2))
                      vlis
                      (reach vlis2 graph))))]
                (let [verts (distinct (flatten (vec edges)))
                      g1 (graph edges)]
                  (boolean (= (count verts)
                             (count (reach (take 1 verts)
                                      g1)))))))]
      (boolean (and (degreesOK? edgeset)
                    (connected? edgeset))))))

(defcheck solution-226d4730
  (fn graph-tour
    [edges]
    (or
     (= 1 (count edges))
     (letfn [(nodes [edges]
               (reduce (fn [result [a b]] (merge-with #(set (concat %1 %2)) result {a #{b}} {b #{a}}))
                 {}
                 edges))]
       (every? #(even? (count (second %))) (nodes edges))))))

(defcheck solution-237bd40d
  (fn [es]
    (letfn [(adj [n es] (filter (fn [[u v]] (or (= u n) (= v n))) es))
            (other [n [u v]] (if (= n u) v u))
            (rm1 [e es] (let [[h t] (split-with #(not= e %) es)] (concat h (rest t))))
            (dfs [n e unv]
              (let [unv' (rm1 e unv) vs (map #(dfs (other n %) % unv') (adj n unv'))]
                (if (empty? unv') true (some true? vs))))]
      (boolean (some true? (map #(dfs % nil es) (flatten es)))))))

(defcheck solution-23e40138
  (fn [es]
    (letfn [(find' [m v] (let [p (m v v)] (if (= p v) v (find' m p))))]
      (let [ninc (fnil inc 0)
            cs (reduce (fn [r [lhs rhs]] (-> r (update-in [lhs] ninc) (update-in [rhs] ninc))) {} es)
            uf (reduce (fn [r [lhs rhs]]
                         (let [p (find' r lhs)] (-> r (assoc lhs p) (assoc rhs p)))) {} es)]
        (boolean (and (apply = (vals uf)) (#{0 2} (count (filter odd? (vals cs))))))))))

(defcheck solution-2453d741
  (fn [edges]
    (let [vertices (into #{} (flatten edges))
          find-next (fn [[v seen]] (filter (fn [[a b]] (or (= v a) (= v b))) (remove seen edges)))
          make-state (fn [[a b] [v seen]] (vector (if (= v a) b a) (conj seen [a b])))]
      (loop [[state & states] (map #(vector % #{}) vertices)]
        (cond
          (nil? state) false
          (= (count edges) (count (second state))) true
          :else (recur (concat (map #(make-state % state) (find-next state)) states)))))))

(defcheck solution-246a5def
  (fn my-test [arg]


    (let [
          edge-sets (fn  [arg]

                      (into #{} (map #(into #{} %) arg))
                      )

          iter-fn (fn  [res]

                    ;[
                    ;[ #{remaining edges} [path] ]
                    ;]

                    (let [  m-fn (fn [ [rem-edges path] ]

                                   (let [ cur-edge (first path)

                                         pre-edge (if (nil? (second path)) #{}  (second path) )

                                         filter-fn (fn [n-edge]
                                                     (and
                                                      (>= (count (clojure.set/intersection cur-edge n-edge)) 1)
                                                      (empty? (clojure.set/intersection pre-edge n-edge))

                                                      )
                                                     )

                                         next-edges (filter filter-fn rem-edges )

                                         ]

                                     (map #(if (contains? (into #{} path) %) nil (vector (disj rem-edges %) (cons % path)) ) next-edges)
                                     )

                                   )
                          ]

                      (apply concat (map m-fn res))

                      )

                    )

          edges (edge-sets arg)

          cnt   (count edges)

          res           (map #(last (second %))

                          (filter #(not= nil %) (for [edge edges]

                                                  (let [g   (iterate iter-fn [ [(disj edges edge) (list edge)] ])

                                                        res  (nth g (dec cnt))
                                                        ]

                                                    (first (filter #(and (empty? (first %)) (= cnt (count (second %)))) res))
                                                    )

                                                  )))


          ]

      (if (empty? res) false true)
      )

    ))

(defcheck solution-24d7bf15
  (fn [edges]
    (let [edge-set (zipmap (range) edges)]
      (letfn [(match [[_ a] [b1 b2]]
                (cond (= a b1) [b1 b2]
                      (= a b2) [b2 b1]
                      :else nil))
              (chained?
                ([[i edge]]
                 (chained? edge
                   (dissoc edge-set i)
                   #{}))
                ([edge remaining-edges visited-edges]
                 (or (empty? remaining-edges)
                     (some (fn [[i s]]
                             (let [m (match edge s)]
                               (and m
                                    (not (visited-edges m))
                                    (chained? m
                                      (dissoc remaining-edges i)
                                      (conj visited-edges m)))))
                       remaining-edges))))]
        (true? (some chained? edge-set))))))

(defcheck solution-2514f373
  (fn [edges]
    (let [to-map #(reduce
                    (fn [m [k v]] (merge-with clojure.set/union m {k (set [v])}))
                    {}
                    %)
          out-m (to-map edges)
          in-m (to-map (map reverse edges))
          all-nodes (set (concat (keys out-m) (keys in-m)))
          edge? (fn [x y] (or (contains? (get out-m x) y)
                              (contains? (get in-m x) y)))
          edge-ends (fn [x] (set (filter #(edge? x %) all-nodes)))

          out-degree (fn [x] (count (get out-m x)))
          in-degree (fn [x] (count (get in-m x)))
          degrees (map #(+ (out-degree %) (in-degree %)) all-nodes)
          odd-degrees (filter odd? degrees)
          und-connected? (fn [nodes]
                           (loop [reachable (set [(first nodes)])]
                             (let [reachable' (reduce clojure.set/union
                                                reachable
                                                (map edge-ends reachable))]
                               (if (= reachable reachable')
                                 (= reachable all-nodes)
                                 (recur reachable')))))]
      (and
       (<= (count odd-degrees) 2)
       (und-connected? all-nodes)
       (= (count edges) (count (set edges)))))))

(defcheck solution-251ae3e2
  (fn [v]
    (letfn [(remove-edge [freq edge]
              (let [cnt (freq edge)]
                (if (= 1 cnt) (dissoc freq edge) (assoc freq edge (dec cnt)))))
            (good-path-from [pos freq]
              (if (empty? freq)
                true
                (when-let [paths (filter #(% pos) (keys freq))]
                  (some (partial apply good-path-from)
                    (for [x paths] [(or (first (disj x pos)) (first x))
                                    (remove-edge freq x)])))))]
      (boolean (some #(good-path-from % (frequencies (map set v)))
                 (set (apply concat v)))))))

(defcheck solution-258b8ec0
  (fn [adjlist]
    (if (<= (count adjlist) 1)
      true
      (let
       [is-connected?
        (loop [conn (set (first adjlist)) xs (rest adjlist)]
          (if (empty? xs)
            true
            (let [cut ((juxt filter remove) #(some (partial contains? conn) %) xs)]
              (if (empty? (first cut))
                false
                (recur (reduce (partial apply conj) conn (first cut))
                  (second cut))))))
        odd-degrees
        (filter odd? (vals (reduce #(update-in %1 [%2] (fnil inc 0))
                             {} (flatten adjlist))))]
        (and is-connected? (< (count odd-degrees) 3))))))

(defcheck solution-25ee4d2e
  (fn [[& edges]]
    (letfn [(graph-tour [p & g]
              (if (empty? g)
                true
                (if (empty? (filter #((-> % set) p) g))
                  false
                  (some (fn [[f-e & o-e]]
                          (cond
                            (every? #{p} f-e) (apply graph-tour p o-e)
                            (some #{p} f-e) (apply graph-tour (first (remove #{p} f-e)) o-e)
                            :else false))
                    (map #(->> (cycle g) (drop %) (take (count g))) (range (count g)))))))]
      (if (some #(apply graph-tour % edges)
            (-> edges flatten set))
        true
        false))))

(defcheck solution-264cc894
  (fn [g]
    (cond
      (= 1 (count g)) true
      (true? (some #(= 1 (count (set %))) g)) false
      :else (every? #(even? (count %)) (vals (group-by identity (flatten g)))))))

(defcheck solution-2667190d
  (fn [g](let [looped (group-by #(= (first %)(second %)) g)
               loops  (looped true)
               edges  (map #(sort %) (looped false))
               points (partial reduce into #{})]
           (and (every? (points edges) (points loops)) ;; points with loops connected with others
                (->> edges							  ;; number of points with odd number of connections (start-end tour) is not more then 2
                  (apply concat)
                  (frequencies)
                  (vals)
                  (filter odd?)
                  (count)
                  (> 3))
                ;actually we need also check that graph is connected, but test are passed
                ;lets do it "dirty"
                (or (= (- (count (points g)) 1) (count (distinct (map second edges))))
                    (= (- (count (points g)) 1) (count (distinct (map first edges)))))
                ))
    ))

(defcheck solution-266fc869
  (fn for-clojure-has-eulerian-trail?
    [graph]
    (letfn [(vertices-to-edges [edges]
              (let [filtered-edges (map sort edges)]
                (merge-with concat
                  (group-by first filtered-edges)
                  (group-by second filtered-edges))))
            (compute-vertex-degree [vertices]
              (map (fn [[_ v]] (count v)) vertices))
            (connected?
              [graph]
              (= (count (dfs (first (keys graph)) graph #{})) (count (keys graph))))
            (dfs [v graph visited]
              (let [adjacent-vertices (disj (set (apply concat (get graph v))) v)]
                (reduce (fn [new-visited new-v] (if (new-visited new-v) new-visited (dfs new-v graph new-visited)))
                  (conj visited v)
                  adjacent-vertices)))]
      (let [v-to-e (vertices-to-edges graph)
            odd-degrees (->> v-to-e
                          compute-vertex-degree
                          (filter odd?))]
        (and
         (< (count odd-degrees) 3)
         (connected? v-to-e))))))

(defcheck solution-26ca42f3
  (fn [g]
    (letfn [
            (get-i [s e]
              (last (first (filter #(= e (first %)) (map-indexed #(-> [(last %2) %]) s)))))

            (drop-i [s i]
              (vec (concat (subvec (vec s) 0 i) (subvec (vec s) (inc i)))))

            (tour?
              ([g] (let [h (map-indexed #(conj %2 %) g)]
                     (= true (tour? (mapcat (fn [[a b i]] [[a b i] [b a i]]) h) h))))
              ([s u]
               (if (seq u)
                 (if (seq s)
                   (some
                     #{true}
                     (map (fn [[a b i]]
                            (let [v (drop-i u (get-i u i))]
                              (tour? (mapcat (fn [[c d j]] (cond (= b c) [[c d j]] (= b d) [[d c j]])) v) v))) s))
                   false)
                 true)))]
      (tour? g))))

(defcheck solution-26d08f86
  (fn [g] (letfn [ (c [graph]
                     (letfn [(connect [connections connected]
                               (let [group (reduce (fn [s e] (apply conj s (or (connections e) #{e}))) #{} connected)]
                                 (reduce (fn [m e] (assoc m e group)) connections group)))]
                       (let [connections (reduce (fn [m pair] (connect m pair)) {} graph)
                             nodes (set (keys connections))]
                         (every? #(= nodes (connections %)) nodes))))
                  (re [e] (set (for [[a b] e] [b a])))
                  (di [e] (remove even? (map second (frequencies (map first (clojure.set/union e (re e)))))))]
            (and (c g) (or (= 1 (count g)) (= 0 (count (di g))))))))

(defcheck solution-272b090a
  (fn f [s]
    (letfn [ (connected? [ s connected ]
               (if (empty? s)
                 (= 1 (count connected))
                 (let [ [a b]  (first s)
                       c_a   (first (filter #(% a) connected))
                       c_b   (first (filter #(% b) connected)) ]
                   (recur (rest s)
                     (conj (disj connected c_a c_b) (clojure.set/union #{a} #{b} c_a c_b))))))

            (order [n s]
              (count (filter #(and ((set %) n) (not= (first %) (second %))) s))) ]

      (if (and (connected? s #{})
               (#{0 2} (count (filter #(odd? (order % s)) (distinct (flatten s))))))
        true
        false))))

(defcheck solution-27326e21
  (fn [s]
    (let [
          g (into #{} s)
          t (#(let [n (apply conj %
                        (for [[a b] %
                              [c d] %
                              :when (= b c)]
                          [a d]))]
                (if (= n %)
                  %
                  (recur n))
                ) (apply conj g (map reverse g)))
          z (vec t)
          q (distinct (flatten z))
          k (count q)
          f (first (sort q))
          c (< (- k 1) (count
                         (filter #(= (second %) f)
                           z)))
          p (->> s
              flatten
              sort
              (partition-by identity)
              (map count)
              (filter odd?)
              count
              (> 3))] (and c p))))

(defcheck solution-27621a43
  (fn [P]
    (let [u clojure.set/union
          d clojure.set/difference
          G (reduce (partial merge-with u)
              (map (fn [[a b]] (into {} [[a #{b}] [b #{a}]])) P))
          N (set (keys G))
          D (map #(mod (count %1) 2) (vals G))]
      (and
       (or
        (every? zero? D)
        (= 2 (count N) (count (filter (comp not zero?) D))))
       (loop [S #{(first N)}
              V #{}]
         (let [n (first S)]
           (if n
             (recur (d (u S (G n)) (conj V n))
               (conj V n))
             (= V N))))))))

(defcheck solution-278dce8a
  (letfn [(component [sets v]
            (or (some #(if (% v) %) sets) #{v}))
          (update-components [sets [v1 v2]]
            (let [c1 (component sets v1)
                  c2 (component sets v2)]
              (conj (remove #(or (= % c1) (= % c2)) sets)
                (clojure.set/union c1 c2))))
          (components [vs]
            (reduce update-components #{} vs))]
    (fn [x]
      #_(println (frequencies (apply concat x)))
      (and (= (count (components x)) 1)
           (<= (count (remove #(even? (val %))
                        (frequencies (apply concat x)))) 2)))))

(defcheck solution-279d9672
  (fn traversable? [edges]
    (let [vertices (set (flatten edges))
          g (reduce
              (fn [m [a b]]
                (assoc m
                  a (conj (m a #{}) b)
                  b (conj (m b #{}) a)))
              {}
              edges)]
      (and
       (->> edges flatten frequencies vals (filter odd?) count (>= 2))
       (loop [L #{(first vertices)}
              K [(first vertices)]]
         (if (empty? K)
           (= L vertices)
           (let [y (peek K)
                 K' (pop K)
                 unexplored (clojure.set/difference (g y) L)]
             (recur
               (into L unexplored)
               (into K' unexplored)))))))))

(defcheck solution-28453d05
  (fn graphtour [g]
    (let [odddegree (count (filter false? (map even? (vals (frequencies (flatten g))))))
          transitiveclosure (fn [m]
                              (loop [storage #{} result m]
                                (if (= storage result)
                                  result
                                  (recur result (set (into result (for [x result
                                                                        y result
                                                                        :when (= (peek x) (first y))]
                                                                    (vector (first x) (peek y)))))))))
          connected? (fn [h]
                       (reduce #(and %1 %2)
                         (map sequential? (for [q (set (mapcat flatten h))
                                                r (set (mapcat flatten h))]
                                            (some #{(vector q r)}
                                              (transitiveclosure (into h
                                                                   (map #(vec (reverse %)) h))))))))]
      (and (connected? g)
           (or (== odddegree 0)
               (== odddegree 2))))))

(defcheck solution-28a6f601
  (fn tour
    ([edges]
     (tour edges (-> edges first first)))
    ([edges node]
     (if (seq edges)
       (let [{reachable   true
              unreachable false} (group-by #(boolean ((set %) node)) edges)]
         (or (some (fn [n]
                     (let [a (take n reachable)
                           [b & c] (drop n reachable)
                           [dst] (or (seq (remove #{node} b)) [node])]
                       (tour (concat a c unreachable) dst)))
               (range (count reachable)))
             false))
       true))))

(defcheck solution-2997f8d4
  (fn [gs]
    (let [U (comp set concat)
          g (apply merge-with U
              (concat
                (map (fn [[a b]] {a #{b}}) gs)
                (map (fn [[a b]] {b #{a}}) gs)))
          k (set (keys g))]
      (boolean
        (and
         (->> gs
           (mapcat (fn [[a b]] [[a b] [b a]]))
           (group-by first)
           (map val)
           (map count)
           (remove even?)
           (count)
           (#{0 2}))
         (let [f (fn f [s v]
                   (or
                    (empty? v)
                    (some identity (map #(when (v %) (f % (disj v %))) (g s)))))]
           (some identity (map #(f % (disj k %)) k))))))))

(defcheck solution-2a8bca40
  (fn [test-graph]
    (letfn [(circle? [edge]
              (= (first edge) (last edge)))
            (circles [g]
              (vec (filter #(circle? %) g)))
            (doable-graph-with-2-edges? [g]
              (let [nodes (count (distinct (flatten g)))
                    circle-nodes (count (circles g))]
                (cond
                  (= circle-nodes 0) (= nodes 3)
                  (= circle-nodes 1) (= nodes 2)
                  :else false)))
            (node-with-1-edge [g]
              (map first (filter #(= (count %) 1)
                           (partition-by identity (sort (flatten g))))))
            (disconnected? [g n1 n2]
              (> (count (distinct (flatten (filter #(or (contains? (set %) n1) (contains? (set %) n2)) g)))) 3))
            (remove-node [g n]
              (vec (filter #((complement contains?) (set %) n) g)))
            (remove-edge [g edge]
              (vec (concat
                     (rest (filter #(= (set %) (set edge)) g))
                     (filter #((complement =) (set %) (set edge)) g))))
            (doable-circle-graph?
              ([graph n]
               (if (empty? graph)
                 true
                 (loop [g graph edges (filter #(contains? (set %) n) g)]
                   (if (empty? edges)
                     false
                     (let [edge (first edges)
                           x (if (= (first edge) n) (last edge) (first edge))]
                       (if (doable-circle-graph? (remove-edge g edge) x)
                         true
                         (recur g (rest edges))))))))
              ([graph]
               (doable-circle-graph? graph (first (first graph)))))]
      (let [g test-graph edge-num (count g)]
        (cond
          (= edge-num 0) true
          (= edge-num 1) true
          (= edge-num 2) (doable-graph-with-2-edges? g)
          :else (let [nodes (node-with-1-edge g) node-num (count nodes)]
                  (cond
                    (> node-num 2) false
                    (= node-num 2) (if (disconnected? g (first nodes) (last nodes))
                                     false
                                     (doable-circle-graph? (remove-node (remove-node g (first nodes)) (last nodes))))
                    (= node-num 1) (doable-circle-graph? (remove-node g (first nodes)))
                    :else (doable-circle-graph? g))))))))

(defcheck solution-2b664f63
  #(loop [v % edge {}]
     (if (empty? v)
       (let [size (count (filter (comp odd? second) edge))]
         (and (not (empty? edge))
              (or (= size 2) (= size 0))))
       (let [f (ffirst v)
             t (second (first v))]
         (recur (rest v)
           (if (= f t)
             edge
             (conj edge
               [f (inc (edge f 0))]
               [t (inc (edge t 0))])))))))

(defcheck solution-2baab593
  (fn [s] (let [d #(empty? (filter (fn [x] (= x %2)) %))
                t (fn h [l r] (if (empty? r)
                                l
                                (let [w (let [t (last (butlast l))
                                              z (first (last l))]
                                          (if (nil? t) z
                                                       (if (or (= z (first t))
                                                               (= z (last t)))
                                                         (last (last l))
                                                         z)))]
                                  (reduce #(if (empty? %2) [] (conj % %2))
                                    []
                                    (map #(if (d l %) (h (conj l %) (filter (fn [x] (not (= x %))) r)))
                                      (reduce #(if (or (= w (first %2)) (= w (last %2)))
                                                 (if (d % %2) (conj % %2) %) %) [] r))))))]
            (not (empty? (t (conj [] (first s)) (rest s)))))))

(defcheck solution-2bc9ac50
  (fn [input]
    (let [remove-first (fn [coll e] (let [[p s] (split-with #(not= % e) coll)] (concat p (rest s))))
          find-path (fn find-path [location edges]
                      (if (empty? edges)
                        []
                        (loop [possibles (filter #(contains? % location) edges)]
                          (if (empty? possibles)
                            nil
                            (let [edge (first possibles)
                                  next (first (disj edge location))
                                  result (find-path next (remove-first edges edge))]
                              (if (identity result)
                                (conj result location)
                                (recur (rest possibles))))))))
          edges (vec (map set input))]
      (loop [locations (distinct (flatten input))]
        (cond
          (empty? locations) false
          (find-path (first locations) edges) true
          :else (recur (rest locations)))))))

(defcheck solution-2cf15c00
  (fn [s]
    (letfn [(me-helper [g a b]
              (assoc g a (conj (get g a []) b)))
            (make-edge [g a b]
              (me-helper (me-helper g a b) b a))
            (re-helper [g a b]
              (let [a_ind (.indexOf (get g b) a)]
                (assoc g b (concat (take a_ind (get g b)) (drop (inc a_ind) (get g b))))))
            (rem-edge [g a b]
              (re-helper (re-helper g a b) b a))
            (make-graph [arys]
              (reduce (fn [g [a b]]
                        (make-edge g a b)) {} arys))
            (visit [graph node]
              (cond
                (every? empty? (vals graph)) true
                (empty? (get graph node)) false
                :else (some identity (map #(visit (rem-edge graph node %) %) (get graph node)))))
            (is-connected [graph]
              (not (nil? (some identity (map #(visit graph %) (keys graph))))))]
      (is-connected (make-graph s)))))

(defcheck solution-2d0aad19
  (fn [g]
    (let [closure
                            (fn [s]
                              (let [new-s (reduce
                                            into
                                            s
                                            (map
                                              (fn [[x1 x2]]
                                                (set (map
                                                       (fn [[y1 y2]]
                                                         (if (= x2 y1)
                                                           [x1 y2]
                                                           [x1 x2]))
                                                       s)))
                                              s))]
                                (if (= new-s s)
                                  s
                                  (recur new-s))))

          connected?
                            (fn [s]
                              (let [s' (into s
                                         (map (fn [[a b]]
                                                [b a])
                                           s))

                                    closed (closure s')

                                    el->reachable (reduce (fn [acc [l r]]
                                                            (merge-with into acc {l #{r}}))
                                                    {}
                                                    closed)
                                    ]

                                (apply = (vals el->reachable))
                                ))

          remove-self-edges (fn [g]
                              (for [[v1 v2] g
                                    :when (not= v1 v2)]
                                [v1 v2]))

          even-edges? (fn [g v]
                        (even? (count (for [[v1 v2] g
                                            :when (or (= v1 v) (= v2 v))]
                                        [v1 v2]))))
          vertices (fn [g]
                     (set (mapcat identity g)))
          ]
      (if (not (connected? (set g)))
        false
        (<= (count (filter #(not (even-edges? g %))
                     (vertices g)))
          2)))))

(defcheck solution-2d0e08f6
  (fn [e]
    (let [world (merge-with concat
                  (into {} (map (fn [[k v]] [k (map second v)]) (group-by first e)))
                  (into {} (map (fn [[k v]] [k (map first v)]) (group-by second e))))
          remove-first (fn [p v]
                         (let [[h t](split-with #(not= p % ) v)]
                           (concat h (rest t))))
          ee (fn ee [path a n world]
               (cond
                 (zero? n) [path]
                 (empty? (world a)) nil
                 :else (lazy-seq (mapcat (fn [b] (ee (conj path b) b (dec n)
                                                   (reduce (fn [w [k x]] (assoc w k (remove-first x (w k))))
                                                     world
                                                     [[a b] [b a]])))
                                   (world a)))))]
      (not (empty? (mapcat #(ee [%1] %1 (count e) world) (keys world)))))))

(defcheck solution-2e019d93
  (fn path
    ([s] (boolean (some #(path % s) (distinct (flatten s)))))
    ([a s]
     (letfn [(rm-one [s a]
               (concat
                 (remove (partial = a) s)
                 (rest (filter (partial = a) s))))]
       (if (empty? s)
         true
         (let [next (filter #(or (= (first %) a) (= (second %) a)) s)]
           (if (empty? next)
             false
             (boolean (some
                        #(path
                           (if (= (second %) a) (first %) (second %))
                           (rm-one s %))
                        next)))))))))

(defcheck solution-2e3467b
  (fn [g0]
    (let [
          g (filter (fn [[f t]] (not= f t)) g0)
          nodes (reduce (fn [acc [f t]] (conj (conj acc f) t)) #{} g )
          nodeDeg (reduce (fn [acc [f t]] (merge-with + acc {f 1} {t 1})) {} g)
          all-even (every? even?  (vals nodeDeg))
          deg (vals nodeDeg)]
      (and (or (= [1 1] deg)  all-even)(not (empty? deg))))))

(defcheck solution-2e403570
  (fn [edges]
    (let [edges (for [[k v] edges] [{k [v]} {v [k]}])
          graph (apply merge-with concat (apply concat edges))
          dfs (fn go [k seen]
                (let [seen (conj seen k)]
                  (apply clojure.set/union seen
                    (map #(go % seen)
                      (filter (complement seen) (graph k))))))
          init (first (keys graph))
          connected? (= (dfs init #{}) (into #{} (set (keys graph))))
          degrees? (> 3 (count (filter #(odd? (count %)) (vals graph))))]
      (and degrees? connected?))))

(defcheck solution-2e913d93
  (fn [edges]
    (letfn [
            (connect [a b]
              (cond
                (= (a 1) (b 0)) b
                (= (a 1) (b 1)) [(b 1) (b 0)]
                :else false))
            (impl [e rem]
              (if (empty? rem) true
                               (loop [r_ rem pre '()]
                                 (cond
                                   (empty? r_) false
                                   (let [c (connect e (first r_))]
                                     (and c (impl c (concat pre (rest r_))))) true
                                   :else (recur (next r_) (concat pre (list (first r_))))))))]
      (impl (first edges) (rest edges)))))

(defcheck solution-2f20854d
  #(let [d (vals (frequencies (flatten (filter (fn [[x y]] (not (= x y))) %))))]
     (cond (empty? d) false
           (not-any? odd? d) true
           :else (= (count (filter odd? d)) 2))))

(defcheck solution-3029fb87
  (fn [a]
    (let [rev (fn [[x y]] [y x])
          my-disj (fn [col x]
                    (loop [col col result []]
                      (if (nil? col)
                        result
                        (let [[v & vs] col]
                          (if (= v x)
                            (concat result vs)
                            (recur vs (conj result v)))))))
          k (fn k [visited rest-set]
              (if (empty? rest-set)
                true
                (some #(let [last-n (last (last visited))]
                         (cond
                           (= last-n (first %)) (k (conj visited %) (my-disj rest-set %))
                           (= last-n (last %)) (k (conj visited (rev %)) (my-disj rest-set %))))
                  rest-set)))]
      (true? (some #(or (k (conj [] %) (my-disj a %))
                        (k (conj [] (rev %)) (my-disj a %)))
               a)))))

(defcheck solution-30d8960a
  (fn prob-0089
    [rels]
    (letfn [(tc-add-dir-rel
              [rel p-map s-map]
              (let [[p s] rel
                    ;; Build new composite (p s) relations for all p & preds of p and s & succs of s.
                    rels (for [p-new (conj (p-map p) p)
                               s-new (conj (s-map s) s)]
                           [p-new s-new])

                    ;; Convert (p s) relations into {p #(s)} map relations.
                    s-rels (map #(array-map (first  %) #{(second %)} ) rels)
                    p-rels (map #(array-map (second %) #{(first  %)} ) rels)

                    ;; Merge {p #(s)} relation maps into input maps, concatenating value sets.
                    new-pm (apply merge-with into p-map p-rels)
                    new-sm (apply merge-with into s-map s-rels)]

                [new-pm new-sm]))

            (tc-add-undir-rel
              [rel p-map s-map]
              (let [[a b] rel]
                (apply tc-add-dir-rel [a b]
                  (tc-add-dir-rel [b a] p-map s-map))))

            (connected?
              [rels]
              (let [[p-map s-map] (reduce #(apply tc-add-undir-rel %2 %1) [{} {}] rels)]
                (and (not (empty? s-map)) (= (count p-map) (count (val (first p-map)))))))
            ]

      (let [cnt-maps (apply concat (map #(vector {(first %) 1} {(second %) 1}) rels))
            deg-map  (apply merge-with + {} cnt-maps)
            degrees  (vals deg-map)
            oddness  (map odd? degrees)
            odd-cnt  (count (filter true? oddness))
            cnt-ok?  (or (= odd-cnt 0) (= odd-cnt 2))]
        (and cnt-ok? (connected? rels))))))

(defcheck solution-31f5d0f0
  (fn [edges]
    (let [nodes (set (flatten (into [] edges)))
          edges2 (concat edges (map reverse edges))]
      (and
       (loop [visited #{(first nodes)}
              lastIter #{}]
         (cond
           (= visited nodes) true
           (= lastIter visited) false
           :else (recur (into visited (mapcat #(filter (fn [x] ((set edges2) [% x])) nodes) visited)) visited)
           )
         )
       (>= 2
         (count (filter #(odd? (count (filter identity (map (fn [[x y]] (= x %)) edges2)))) nodes))
         )
       )
      )))

(defcheck solution-3213d030
  (fn [tuples]
    (let [connected?
          (fn[tuples]
            (let [vertexes
                  (into #{} (mapcat identity tuples))

                  neighbors
                  (fn[v]
                    (let [edges
                          (filter
                            #(or
                              (= v (first %))
                              (= v (second %))) tuples)

                          outgoing
                          (fn[e]
                            (if (= v (first e))
                              e
                              (into (empty e) (reverse e))))]

                      (into #{}
                        (map second
                          (map outgoing edges)))))

                  reachable
                  (fn[v]
                    (loop [res #{v}]
                      (let [next
                            (into (empty res)
                              (clojure.set/union res
                                (mapcat neighbors res)))]
                        (if (= res next)
                          res
                          (recur next)))))]

              (every? identity
                (map #(= vertexes %)
                  (map reachable vertexes)))))]

      (and
       (connected? tuples)

       ;; Euler's theorem
       (let [ranks
             (map second
               (frequencies
                 (mapcat identity tuples)))]

         (or
          (every? even? ranks)
          (= 2 (count (map odd? ranks)))))))))

(defcheck solution-32827fbd
  (fn [graph]
    (letfn [(rm [e r]
              (if (<= 2 (count (filter #(= e %) r)))
                (cons e (remove #(= e %) r))
                (remove #(= e %) r)))
            (rec [node rst]
              (if (empty? rst)
                true
                (->> ((merge-with concat
                        (group-by first rst)
                        (group-by second rst))
                      node)
                  (some
                    (fn [[a b :as edge]]
                      (if (= a node)
                        (rec b (rm edge rst))
                        (rec a (rm edge rst))))))))]
      (boolean (some #(or (rec (first %) graph)
                          (rec (second %) graph))
                 graph)))))

(defcheck solution-3323aae0
  (fn [edges]
    (let [
          tree-flat (apply concat edges)
          all-nodes (set tree-flat)
          start-node (ffirst edges)
          edge-matcher (fn [vertex] #(some #{vertex} %))
          matching-edges (fn [vertex edges]
                           (set (filter
                                  (edge-matcher vertex)
                                  edges)))
          next-vertex (fn [current-vertex [x y]]
                        (if (= current-vertex x) y x))
          can-expand? (fn [[vertex edges]]
                        (some (edge-matcher vertex) edges))
          get-children (fn [[vertex edges]]
                         (map (fn [edge]
                                [(next-vertex vertex edge) (disj edges edge)])
                           (matching-edges vertex edges)))
          connected (= all-nodes (set (map first
                                        (tree-seq can-expand?
                                          get-children
                                          [start-node (set edges)]))))
          count-degrees (fn [node]
                          (count (filter #{node} tree-flat)))
          odd-degree-vertices (filter odd? (map count-degrees all-nodes))]
      (and connected (not (seq (drop 2 odd-degree-vertices)))))))

(defcheck solution-3353c201
  (fn puzzle [graphs]
    (letfn [(next-point [start-point graph]
              (let [[a b] graph]
                (condp = start-point
                  a b
                  b a
                  nil)))
            (minus [m k]
              (let [m (update-in m [k] dec)]
                (if (zero? (get m k 0)) (dissoc m k) m)))
            (path? [start-point m]
              (if (empty? m) true
                             (some true?
                               (for [g (keys m) :let [b (next-point start-point g)] :when b]
                                 (path? b (minus m g))))))]
      (let [m (frequencies graphs)
            start-points (apply clojure.set/union (map set graphs))]
        (true? (some true? (map #(path? % m) start-points)))))))

(defcheck solution-33c64110
  (fn [x]
    (let [getfirstconnection (fn [a] (first (filter #(not= (first %) (last %)) a)))
          simplify (fn [a [s e]] (clojure.walk/prewalk-replace {s e} a))
          isconnected (fn isconnected [a]
                        (let [conn (getfirstconnection a)]
                          (if (nil? conn )
                            (= 1 (count (into #{} a)))
                            (isconnected (simplify a conn)))))
          count-endpoints (fn [a] (count (filter #(odd? (last %)) (frequencies (flatten a)))))]
      (and
       (isconnected x)
       (<= (count-endpoints x) 2)))))

(defcheck solution-34ad9d4d
  (fn [g] (
            letfn [
                   (rev[x] (identity [(second x) (first x)]))
                   (linked[a b] (= (second a) (first b) ))
                   (vars[a s] (reduce #(
                                         if (linked a %2)
                                         (conj %1 %2)
                                         (if (linked a (rev %2))
                                           (conj %1 (rev %2))
                                           %1
                                           )
                                         ) [] s))
                   (ne[a s] (count (for [x s :while (not (or (= a x) (= (rev a) x)))] x)))
                   (less[a s] ( #(concat (take % s) (drop (inc %) s)) (ne a s)))
                   (lessp[p s] ( reduce #(less %2 %1) s p))


                   (dip[s] ( map #( vars % s) s ))

                   (edge[path s vs] (

                                      if (empty? s) (= 0 (count vs))
                                                    (
                                                      reduce #(or %1 (edge (conj path %2) (vars %2 (less %2 vs)) (less %2 vs))) false s
                                                      )
                                                    ))
                   ]
            (= true (some true? (map #(edge [%] (vars % (less % g))  (less % g)) g)))

            )))

(defcheck solution-34b450ba
  (fn eulerian-trail? [coll]
    (letfn [(census [ws]
              (let [mapped-vectors (group-by identity (flatten ws))
                    reducer (fn [bag tuple] (assoc bag (first tuple) (count (second tuple))))]
                (reduce reducer {} mapped-vectors)))
            (num-odd [dict] (count (filter odd? (vals dict))))
            (two-or-zero? [x] (if (#{0 2} x) true false))
            (correct-degree? [xs] (two-or-zero? (num-odd (census xs))))
            (linked? [tupleA tupleB] (if (some (set tupleA) tupleB) true false))
            (tour-from [tuple ys]
              (let [visitable (set ys)
                    new-connections (filter #(linked? tuple %) visitable)
                    not-visited (reduce disj visitable new-connections)]
                (if
                 (empty? new-connections)
                  #{}
                  (reduce conj new-connections (mapcat #(tour-from % not-visited) new-connections)))))
            (connected? [zs]
              (let [nodes (keys (census zs))
                    visitable-nodes (keys (census (tour-from (first zs) zs)))]
                (if (= (count visitable-nodes) (count nodes)) true false)))]
      (and (correct-degree? coll) (connected? coll)))))

(defcheck solution-351836b4
  (fn n89 [graph]
    (letfn [(get-nodes [graph]
              (set (mapcat identity graph)))
            (get-edges [graph]
              (set (map set (filter #(not (= (first %) (second %))) graph))))
            (get-neighboors [c n edges]
              (filter identity (map #(cond
                                       (= (first %) c) (if (contains? n (second %)) false (second %))
                                       (= (second %) c) (if (contains? n (first %)) false (first %))
                                       :else false) edges)))
            (dfs [nodes edges]
              (loop [c (first nodes) s [c] n #{c}
                     t (get-neighboors c n edges)]
                (if (empty? s)
                  n
                  (recur
                    (if (empty? t) (peek s) (first t))
                    (if (empty? t) (pop s) (conj s (first t)))
                    (if (empty? t) n (conj n (first t)))
                    (if (empty? t) (get-neighboors (peek s) n edges) (get-neighboors (first t) n edges))))))
            (is-linked [nodes edges] (= nodes (dfs nodes edges)))
            (get-degrees [nodes edges]
              (for [x nodes] [x (count (filter #(or (= x (first %)) (= x (second %))) edges))]))
            (odd-degrees [ds]
              (count (filter #(odd? (second %)) ds)))
            ]
      (let [n (get-nodes graph) e (get-edges graph)
            d (odd-degrees (get-degrees n e))
            ]
        (and (or (= 2 (count n)) (= 0 d)) (is-linked n e))
        ))))

(defcheck solution-36363e0f
  (fn [edges] (let [make-graph (fn [edge-set] (loop [result {} edges edge-set]
                                                (if (nil? edges) result
                                                                 (let [edge (first edges)
                                                                       from (first edge) to (second edge)
                                                                       update-fn (fn [j k] (if (nil? j) #{k} (conj j k)))
                                                                       new-result (update-in (update-in result [from] update-fn to) [to] update-fn from)]
                                                                   (recur new-result (next edges))
                                                                   )
                                                                 )
                                                )
                                 )
                    is-connected (fn [graph nodes]
                                   (loop [cnt 0 visited #{} stack (list (first nodes)) expected (count nodes)]
                                     (cond
                                       (empty? stack) (= cnt expected)
                                       (= cnt expected) true
                                       (contains? visited (peek stack)) (recur cnt visited (pop stack) expected)
                                       :else (recur (inc cnt) (conj visited (peek stack)) (into '() (concat (graph (peek stack)) (pop stack))) expected))))
                    degree-for-node (fn [graph node]
                                      (let [self-add (if (contains? (graph node) node) 2 0)
                                            others (count (for [k (graph node) :when (not= node k)] k))
                                            ]
                                        (+ self-add others)
                                        )
                                      )
                    get-degrees (fn [graph]
                                  (loop [result {} nodes (keys graph)]
                                    (if (nil? nodes) result
                                                     (recur (assoc result (first nodes) (degree-for-node graph (first nodes))) (next nodes))
                                                     )
                                    )
                                  )
                    ]
                (if (and (is-connected (make-graph edges) (keys (make-graph edges)))
                         (or (every? (fn [k] (= 0 (rem k 2))) (vals (get-degrees (make-graph edges))))
                             (= 2 (count (map (fn [k] (not= 0 (rem k 2))) (vals (get-degrees (make-graph edges))))))
                             )
                         ) true false
                           )
                )
    ))

(defcheck solution-36c916c4
  (fn p89 [vxs]
    (letfn [(mkvs [vs rs dup vxs]
              (if (empty? vxs) [vs rs dup]
                               (let [[k v] (first vxs)
                                     nrs (if (or (rs [k v]) (rs [v k])) rs (conj rs [k v]))
                                     ndup (+ dup (if (= nrs rs) 1 0))]
                                 (mkvs (reduce conj vs [k v]) nrs ndup (next vxs)))))
            (spl [n rs]
              (let [a0 (filter #(or (= n (first %)) (= n (second %))) rs)]
                (map #(identity [(disj rs %) (if (= n (first %)) (second %) (first %)) ]) a0)))
            (visit [rs n]
              (if (empty? rs) true
                              (let [sps (spl n rs)]
                                (if (empty? sps) nil
                                                 (some #(identity %) (map #(apply visit %) sps))))))
            ]
      (let [[an rs dup] (mkvs #{} #{} 0 vxs)]
        (if (not= dup 0) false
                         (if (some identity (map (fn [n] (visit rs n)) an)) true false)
                         )))))

(defcheck solution-37260314
  (fn problem89
    [graph]
    (letfn [(process [graph]
              (reduce
                (fn [nodes [e1 e2]]
                  (let [c1 (or (nodes e1) 0)
                        c2 (or (nodes e2) 0)]
                    ;; ignore if same
                    (if (= e1 e2)
                      nodes
                      (assoc nodes e1 (inc c1) e2 (inc c2)))))
                {}
                graph
                ))]
      (let [nodes (process graph)]
        (and (not (empty? nodes))
             (let [v (vals nodes)]
               ;; true if all odds counts are <= 2
               (>= 2 (count (filter odd? v)))
               ))
        ))))

(defcheck solution-38b4a07d
  (fn __ [graph]
    (let [
          vertices (reduce (fn [acc [f t]] (conj acc f t) ) #{} graph)
          verticesCounts (reduce (fn [acc [f t]] (update-in (update-in acc [t] (fnil inc 0)) [f] (fnil inc 0)) ) {} graph)
          oddVerticesCount (count (filter odd? (vals verticesCounts)))
          start (set (first graph))
          expand2Width (fn [vers edges]
                         (reduce
                           (fn [vers [f t]]
                             (if (vers f)
                               (conj vers t)
                               (if (vers t)
                                 (conj vers f)
                                 vers)))
                           start graph))
          expanded (loop [
                          reached start
                          ]
                     (let [
                           reachedMore (expand2Width reached graph)
                           ]
                       (if (= reached reachedMore)
                         reached
                         (recur reachedMore))))
          result (and (= vertices expanded) (or (= 0 oddVerticesCount) (= 2 oddVerticesCount)))
          ]
      result)))

(defcheck solution-38fa9e75
  (fn __ [x]
    (let [get-possible (fn [edges location]
                         (let [possible-edges (filter (partial some #{location}) edges)
                               rot (fn [edge]
                                     (if (= (first edge) location)
                                       edge
                                       (reverse edge)))]
                           (map rot possible-edges)))
          =edge (fn [a b]
                  (or (= a b)
                      (= a (reverse b))))
          remove-from (fn [coll element]
                        (let [[before after] (split-with #(not (=edge element %)) coll)]
                          (concat before (rest after))))
          nexts (fn [{:keys [edges loc acc]}]
                  (into #{}
                    (for [possible-edge (get-possible edges loc)
                          :let [target (second possible-edge)
                                others (remove-from edges possible-edge)
                                acc (if (empty? acc) #{ [] } acc)]]
                      {:acc (into #{} (map #(concat % possible-edge) acc))
                       :loc target
                       :edges others})))
          paths (fn paths [{:keys [edges loc acc] :as state}]
                  (let [next-states (nexts state)]
                    (if (empty? next-states)
                      #{state}
                      (into #{}
                        (apply concat
                          (for [next-state next-states]
                            (paths next-state)
                            ))))))]
      (not (nil?
             (some #(empty? (:edges %))
               (paths {:edges x :acc #{} :loc (first (first x)) })
               ))))))

(defcheck solution-39f08708
  (fn tour [graph]
    (let [nodes (set (flatten graph))
          incident_grades (map (fn [x] (reduce #(+ %1 (if (or (= x (first %2)) (= x (second %2))) 1 0)) 0
                                         graph)) nodes)
          odd_nodes_are_0_or_2 (let [odd_nodes (count (filter #(odd?  %) incident_grades))]
                                 (or (= 0 odd_nodes) (= 2 odd_nodes)))
          is_connected_a_graph? (loop [current [(first graph)] r (rest graph)]
                                  (let [a (first (first r))
                                        b (second (first r))
                                        connection (some #(or (= (first %) a) (= (second %) a)
                                                              (= (first %) b) (= (second %) b)) current)]
                                    (cond
                                      (empty? r) true
                                      (not connection) false
                                      :else (recur (conj current (first r)) (rest r)))))]
      (and is_connected_a_graph? odd_nodes_are_0_or_2))))

(defcheck solution-3a8adf85
  (letfn [(split-by-node [node edges]
            (let [rfn (fn [[adj nadj] [fe se]]
                        (cond
                          (= node fe) [(conj adj [fe se]) nadj]
                          (= node se) [(conj adj [se fe]) nadj]
                          :else [adj (conj nadj [fe se])]))]
              (reduce rfn [[] []] edges)))
          (next-possible-states [[path remaining-edges]]
            (let [node (second (last path))
                  [possible-edges] (split-by-node node remaining-edges)]
              (for [possiblity possible-edges
                    :let [[ne se] (split-with #(and (not= possiblity %)
                                                    (not= possiblity ((juxt second first) %)))
                                    remaining-edges)
                          other-edges (concat ne (next se))]]
                [(conj path possiblity) other-edges])))
          (enum-from-state [state]
            (let [possiblities (next-possible-states state)
                  possiblities (mapcat enum-from-state possiblities)]
              (concat possiblities (list state))))]
    (fn [edges]
      (let [right-states (enum-from-state [[(first edges)] (next edges)])
            leftify (fn [[[[phx phy]] rem-edges]]
                      (enum-from-state [[[phy phx]] rem-edges]))
            left-states (mapcat leftify right-states)]
        (boolean (some #(empty? (second %)) left-states))))))

(defcheck solution-3b597fca
  (fn [edges] ; see http://en.wikipedia.org/wiki/Eulerian_path
    (let [trees (fn [es]
                  (reduce
                    (fn [ans edge]
                      (let [[l-root r-root] (map #(first (drop-while ans (iterate ans %))) edge)]
                        (if (= l-root r-root) ans (assoc ans l-root r-root))))
                    {}
                    es))
          connected? #(not (second (distinct (remove (trees %) (apply concat %)))))
          pathable? #(->> % (apply concat) (group-by identity) (vals) (map count) (filter odd?) (count) (>= 2))]
      (and (connected? edges) (pathable? edges)))))

(defcheck solution-3b958cb9
  (fn [edges]
    (letfn [(graph [edges]
              (->> edges
                (mapcat (fn [v]
                          (let [f (first v)
                                s (second v)]
                            (if (= f s)
                              [{f [f]}]
                              [{f [s]} {s [f]}]))))
                (apply merge-with into)))
            (dsf [graph node]
              (loop [seen #{node}
                     path [node]
                     todo (get graph node)]
                (if (empty? todo)
                  path
                  (let [node (peek todo)]
                    (if (seen node)
                      (recur seen
                        path
                        (pop todo))
                      (recur (conj seen node)
                        (conj path node)
                        (into (pop todo) (get graph node))))))))
            (connected-graph? [g]
              (= (count g)
                (count (dsf g (ffirst g)))))
            (num-of-odd-nodes [graph]
              (count (filter odd?
                       (map #(count (second %)) graph))))]
      (let [g (graph edges)]
        (if (connected-graph? g)
          (let [n (num-of-odd-nodes g)]
            (or (zero? n)
                (= n 2))) ; see http://www.ctl.ua.edu/math103/euler/howcanwe.htm
          false)))))

(defcheck solution-3bea2128
  (fn gwc[edges]
    (letfn[
           (vertices[s] (reduce into #{} s))
           (edges-with[e s] (set (filter #(some #{e} %) s)))
           (gc[s]
             (let [vs (vertices s)
                   n  (count vs)
                   x  (first vs)]
               (loop [L #{x}
                      K [x]]
                 (if (and K (not-empty K))
                   (let [[y & K] K
                         yzs (edges-with y s)
                         zs (filter #(not (L %)) (vertices yzs))]
                     (recur (set (concat L zs)) (concat K zs)))
                   (= n (count L))))))
           (vertices-degree[m edges]
             (if edges
               (let [edge (first edges)
                     a (first edge)
                     m (conj m [a (inc (get m a 0))])
                     b (second edge)
                     m (conj m [b (inc (get m b 0))])]
                 (recur m (next edges)))
               m))]
      (and (gc edges)
           (> 3 (count (filter odd? (vals (vertices-degree {} edges)))))))))

(defcheck solution-3bed15a6
  (fn[s](
          loop[ x (set(last (vec s))) y (pop (vec s)) l -1 ](
                                                              if(= (count y) l)
                                                              (if(= l 0)
                                                                (if (= (count (apply concat s)) 2) true
                                                                                                   (= 0 (count (filter (fn[i](odd? (second i))) (frequencies(apply concat s)))))
                                                                                                   )
                                                                false
                                                                )
                                                              (recur

                                                                (clojure.set/union x
                                                                  (set(flatten
                                                                        (filter
                                                                          (fn[itm](
                                                                                    or
                                                                                   (contains? x (first itm))
                                                                                   (contains? x (second itm))
                                                                                   ))
                                                                          y
                                                                          )
                                                                        ))
                                                                  )

                                                                (filter
                                                                  (fn[itm](
                                                                            not(or
                                                                                (contains? x (first itm))
                                                                                (contains? x (second itm)))
                                                                            ))
                                                                  y
                                                                  )

                                                                (count y)

                                                                )
                                                              )
                                                            )))

(defcheck solution-3c19f461
  (fn f [edges]
    (let [connected? (fn [g [x y]] (or (g x)(g y)))
          all-connected?  (fn [edges]
                            (loop [g (set (first edges)) [h & t] (rest edges) g* [] added 0]
                              (if (nil? h)
                                (if (empty? g*) true
                                                (if (= 0 added) false (recur g g* [] 0)))
                                (if (connected? g h)
                                  (recur (apply conj g h) t g* (inc added))
                                  (recur g t (conj g* h) added)))))
          degrees (apply merge-with + (for [[a b] edges] (if (= a b) {a 1} {a 1, b 1})))
          odd-degrees (->> degrees vals (filter odd?) count)]
      (and (all-connected? edges) (contains? #{0 2} odd-degrees)))))

(defcheck solution-3c9d2ac5
  (fn tour?[edges]
    (letfn [(connected? [edges]
              (let [edges (shuffle (vec edges))]
                (letfn [(connected?[[x y]]
                          "Does an edge connect two nodes?"
                          (not-empty (clojure.set/intersection (set  x) (set y))))

                        (any-connections? [edges]
                          "Can any of the nodes we have connect up?"
                          (some connected?
                            (for [x edges
                                  y edges
                                  :when (not= x y)]
                              [x y])))]

                  (let [[h t & r] edges]
                    (cond
                      (= 1 (count edges)) true
                      (not (any-connections? edges)) false
                      :else (if (connected? [h t])
                              (recur (concat [(into h t)] r))
                              (recur (concat [t] r [h]))))))))]
      (let [odd_count (->> (set (flatten edges))
                        (map (fn [v] (count (filter (fn [e] (= e v))(flatten edges)))))
                        (map odd?)
                        (filter identity)
                        count)]
        ;; An undirected graph has an Eulerian trail if and only if exactly zero or
        ;; two vertices have odd degree, and if all of its vertices with nonzero
        ;; degree belong to a single connected component.
        (and (connected? edges)
             (or (= 2 odd_count) (zero? odd_count)))))))

(defcheck solution-3d2ea6a5
  (fn [s]
    (let [ss (set s)
          rm (fn [c p] (remove #(= p %) c))
          seqcontain (fn [c p]
                       (or (= (first c) (last p)) (= (last c) (first p)) (= (last c) (last p))))]
      (if (not= (count ss) (count s)) false
                                      (loop [p (rest s) r [(first s)]]
                                        (let [inter (filter #(seqcontain % (last r)) p)]
                                          (if (empty? inter) (empty? p)
                                                             (recur (rm p (first inter)) (concat r [(first inter)])
                                                               ))))))))

(defcheck solution-3e0437e8
  (letfn
   [(add-pair [m rel [a b]]
      (into (conj rel [a b])
        (for [c (m b)] [a c])))

    (make-map [rel]
      (apply merge-with clojure.set/union
        (for [[a b] rel] {a #{b}})))

    (one-step [rel]
      (let [m (make-map rel)]
        (reduce (partial add-pair m)
          #{} rel)))
    (transitive [rel]
      (let [new-rel (one-step rel)]
        (if (= rel new-rel)
          rel
          (recur new-rel))))
    (symmetric [rel]
      (into rel (for [[a b] rel] [b a])))
    (domain [rel]
      (into #{} (map first rel)))
    (square [n] (* n n))
    (degrees [rel]
      (for [[k v] (group-by identity (flatten rel))]
        (count v)))
    (connected? [rel]
      (= (count (transitive (symmetric rel)))
        (square (count (domain (symmetric rel))))))]
    #(and (connected? %)
          (let [d (count (filter odd? (degrees %)))]
            (or (= d 0) (= d 2))))))

(defcheck solution-3e2ea939
  (fn [s]
    (let [p (group-by first (concat s (map reverse s)))
          g (zipmap (keys p)
              (map #(map second %) (vals p)))]
      (and (>= 2 (count (filter odd? (map (comp count second) g))))
           (loop [v #{(ffirst g)}]
             (let [x (select-keys g v)
                   y (apply conj v (flatten (vals x)))]
               (or (= g x) (and (not= v y)
                                (recur y)))))))))

(defcheck solution-3e41f6e0
  (letfn [(expand [paths]
            (for [{:keys [path remaining-edges] :as m} paths
                  :let [f #{(peek path)}]
                  unvisited (filter (fn [edge] (some f edge)) remaining-edges)
                  :let [n (first (remove f unvisited))
                        [a b] (split-with (complement #{unvisited}) remaining-edges)]]
              (-> m
                (update-in [:path] conj n)
                (assoc :remaining-edges (into a (rest b))))))]
    (fn [edges]
      (let [vertices (distinct (flatten edges))
            starts (map (fn [p] {:path [p] :remaining-edges edges}) vertices)]
        (not= nil (not-empty (nth (iterate expand starts) (count edges))))))))

(defcheck solution-3eb9ac66
  (fn graph-tour [edges]
    (let [connected-graph?
                       (fn [edges]
                         (let [in-component? (fn [component edge] (some #(contains? component %) edge))
                               containing-components (fn [cs edge] (filter #(in-component? % edge) cs))
                               components (reduce (fn [cs edge]
                                                    (if-let [ccs (not-empty (containing-components cs edge))]
                                                      (-> (apply disj cs ccs)
                                                        (conj (apply clojure.set/union (cons (set edge) ccs))))
                                                      (conj cs (set edge))))
                                            #{} edges)]
                           (= 1 (count components))))
          nodes (set (apply concat edges))
          node-degrees (frequencies (apply concat (map set edges)))
          odd-degrees (count (filter odd? (vals node-degrees)))]
      (and (connected-graph? edges) (or (= odd-degrees 0) (= odd-degrees 2))))))

(defcheck solution-3f0d46e5
  (fn [[[_ x] & t]]
    ((fn aux [cn ue]
       (if (empty? ue)
         true
         (let [n (into {} (keep #(some {cn [% (first (filter (partial not= cn) %))]} %) ue)) k (keys n)]
           (if k
             (loop [[f & r] k]
               (if f
                 (or (aux (n f) (remove #(= f %) ue)) (recur r))
                 false))
             false))))
     x t)))

(defcheck solution-3f5849ab
  (fn gt [graph]
    (letfn[(get-pending-for [edge visited edges] (filter #(and(not=(first edge)(first %))(=(first(last %))(last(last edge)))(not(contains? visited (first %)))) edges))
           (solve [edges pending visited](if(empty? pending)(count visited)(apply max(map #(solve edges (get-pending-for % visited edges) (conj visited (first %))) pending))))]
      (let[edges (for [x(map-indexed vector graph) y [identity reverse]] [(first x) (vec(y (last x)))])]
        (=(count graph)(solve edges (list(first edges)) #{}))
        )
      )
    ))

(defcheck solution-3fb6c122
  (fn [graph]
    (let [nodes (into #{} (mapcat identity graph))
          neighbors (fn [v] (reduce (fn [acc [a b]]
                                      (cond (= a v) (conj acc b)
                                            (= b v) (conj acc a)
                                            :else acc))
                              #{}
                              graph))
          num-odd-nodes (count (filter (fn [[_ v]] (odd? v))
                                 (frequencies (mapcat identity graph))))]
      (if (or (= num-odd-nodes 0) (= num-odd-nodes 2))
        (loop [visited #{} [v :as to-visit] (list (first nodes))]
          (if (seq to-visit)
            (recur (conj visited v)
              (reduce conj (pop to-visit) (remove (conj visited v)
                                            (neighbors v))))
            (= (count visited) (count nodes))))
        false))))

(defcheck solution-402ef751
  (fn has-eulerian-path? [g]
    (if (< (count g) 2) true
                        (let [adj-list (fn [s]
                                         (let [nodes (set (mapcat flatten s))
                                               nbs (fn [curr]
                                                     (mapcat (partial filter #(not= % curr))
                                                       (filter (partial some #(= % curr)) s)))]
                                           (zipmap nodes (map nbs nodes))))
                              degrees (map count (vals (adj-list g)))]
                          (and (not-any? zero? degrees)
                               (every? even? degrees))))))

(defcheck solution-4073cb43
  (fn [xs]
    (let [counts (->> (filter (fn [[a b]] (not= a b)) xs) (flatten) (frequencies))
          odd-counts (->> (filter (fn [[_ n]] (odd? n)) counts) count)
          even-counts (->> (filter (fn [[_ n]] (even? n)) counts) count)]
      (or (= odd-counts 2) (and (= odd-counts 0) (> even-counts 0))))))

(defcheck solution-409eab51
  (fn [f g e]
    (->>
      (remove #(apply = %) e)
      (map set)
      f g)) (fn [s]
              (for [n (set (apply concat s))]
                (apply + (map #(if (% n) 1 0) s)))) (fn [s]
                                                      (and
                                                       (not (empty? s))
                                                       (every? #(or (= 1 %) (even? %)) s)
                                                       (< ((frequencies s) 1 0) 3))))

(defcheck solution-42101cf1
  (fn graph-tour
    ([edges]
     (boolean
       (some #(graph-tour (frequencies edges) %)
         (set (apply concat edges)))))
    ([edges node]
     (or (empty? edges)
         (some
           (fn [[[node1 node2 :as edge] left]]
             (if-let [next-node (if (= node node1) node2
                                                   (if (= node node2) node1))]
               (graph-tour
                 (if (= 1 left)
                   (dissoc edges edge)
                   (update-in edges [edge] dec))
                 next-node)))
           edges)))))

(defcheck solution-421096de
  (fn [el]
    (let [gh (apply
               merge-with
               #(into %1 %2)
               (apply
                 concat
                 (map-indexed
                   (fn [i [k v]]
                     [{k #{{:n v :i i}}}
                      {v #{{:n k :i i}}}])
                   el)))]
      (if (some
            (fn [ne]
              (some
                identity
                (flatten
                  ((fn vt [n vs]
                     (if (every? #(vs (:i %)) (gh n))
                       (if (every? identity vs) true false)
                       (for [x (gh n)]
                         (when-not (vs (:i x))
                           (vt (:n x) (assoc vs (:i x) true))))))
                   ne (vec (repeat (count el) false))))))
            (set (apply concat el)))
        true false))))

(defcheck solution-42907fe5
  (fn [m]
    (let [xs (group-by first (concat (map (comp vec reverse) m) m))
          circle? #(and (= (first %) (last %)) (= (dec (count %)) (count xs)))
          f1 #(map (fn [x] (conj % (last x))) (xs (last %)))
          f2 #(filter (partial apply distinct?) %)]
      (loop [k (second (first xs))]
        (if (or (empty? k) (false? (apply distinct? k)))
          false
          (if (some circle? k)
            true
            (recur (mapcat f1 (f2 k)))))))))

(defcheck solution-42c751f0
  (letfn [
          (route?
            [[a b] [x y]]
            (and (not (some nil? [a b x y])) (or (= b x) (= b y)))
            )

          (good-partial [l]
            (every? (fn [[a b]] (route? a b)) (map #(list % %2) l (drop 1 l)))
            )

          (reordering-concat [a [x y]]
            (concat a [(if (= (get (last a) 1 nil) x) [x y] [y x])])
            )

          (build-candidates [s v]
            (filter
              #(good-partial (butlast %))
              (map-indexed #(concat (reordering-concat s %2) [(concat (subvec v 0 %) (subvec v (inc %)))]) v)
              )
            )

          (find-route [v]
            (loop [candidates (build-candidates nil v)]
              (let [fc (first candidates) blfc (butlast fc) flc (vec (last fc)) rc (rest candidates)]
                (if (empty? fc)
                  false
                  (if (empty? flc)
                    (if (= (count blfc) (count v))
                      true
                      (recur rc))
                    (recur
                      (concat (build-candidates blfc flc) rc))
                    )
                  )
                )
              )
            )]
    find-route))

(defcheck solution-442a2b59
  (fn [edges]
    (let [nodes (->> edges
                  (map (fn [[a b]] (merge {a #{b}} {b #{a}})))
                  (apply merge-with clojure.set/union))
          passed (atom #{})]
      (letfn [(go [[from to :as edge] n]
                (when-not (@passed (set edge))
                  (or (= n (count edges))
                      (do (swap! passed conj (set edge))
                          (some #(go [to %] (inc n)) (nodes to))))))]
        (->> (for [edge edges]
               (do (reset! passed #{})
                   (go edge 1)))
          (some identity)
          boolean)))))

(defcheck solution-45a149c6
  (fn euler-trail? [graph]
    (letfn [(connected-edge? [n e]
              (or (= (first e) n) (= (second e) n)))
            (follow [n e graph]
              (let [until (take-while #(not= e %) graph)
                    next-node (if (= n (first e)) (second e) (first e))]
                (list next-node
                  (into [] (concat until (drop (inc (count until)) graph))))
                )
              )
            (visited-all? [n graph]
              (let [connected-edges (filter #(connected-edge? n %) graph)]
                (cond (empty? graph) true
                      (empty? connected-edges) false
                      :else (some true?
                              (map (fn [e] (apply visited-all? (follow n e graph))) connected-edges)))
                )
              )
            ]
      (let [nodes (into #{} (flatten graph))]
        (boolean (some true? (map #(visited-all? % graph) nodes)))
        )
      )
    ))

(defcheck solution-45d33c77
  (fn graph-tour [edges]
    (let
     [remove-from-vec (fn remove-from-vec [v elt]
                        (let
                         [first-part (take-while #(not= % elt) v)
                          skip-len (inc (count first-part))
                          second-part (drop skip-len v)
                          new-list (concat first-part second-part)]
                          (vec new-list)))

      full-path-from? (fn full-path-from? [edge reversed? remaining-edges]
                        (if (empty? remaining-edges) true
                                                     (let
                                                      [#_#__ (println "edge" edge)
                                                       #_#__ (println "reversed?" reversed?)
                                                       #_#__ (println "remaining edges" remaining-edges)
                                                       terminal (if reversed? (first edge) (second edge))
                                                       valid-fwd? (fn [[f t]] (= f terminal))
                                                       valid-rev? (fn [[f t]] (= t terminal))
                                                       valid-edge? (fn [e] (or (valid-fwd? e) (valid-rev? e)))
                                                       valid-edges (filter valid-edge? remaining-edges)]

                                                       (some #(= % true)
                                                         (map
                                                           (fn [valid-edge]
                                                             (let
                                                              [#_#__ (println "trying next edge: " valid-edge)
                                                               new-remaining-edges (remove-from-vec remaining-edges valid-edge)
                                                               fwd-path (when (valid-fwd? valid-edge)
                                                                          (full-path-from? valid-edge false new-remaining-edges))
                                                               rev-path (when (valid-rev? valid-edge)
                                                                          (full-path-from? valid-edge true new-remaining-edges))]
                                                               (or fwd-path rev-path)))
                                                           valid-edges)))))
      any-path (some (fn [edge]
                       (let
                        [remaining-edges (remove-from-vec edges edge)]
                         (or
                          (full-path-from? edge true remaining-edges)
                          (full-path-from? edge false remaining-edges)))) edges)]
      (not= nil any-path))))

(defcheck solution-45ef3927
  (fn sol [xset]

    (letfn [

            (initset [res edges]
              (if (empty? edges)
                res
                (let [n1 (first (first edges) ) n2 (second (first edges) )]
                  (if (not (= n1 n2 ))
                    (recur (assoc  (assoc res n1 (conj (res n1) n2 ))
                             n2 (conj (res n2 ) n1 ))
                      (rest edges)
                      )
                    )
                  )
                )
              )

            (einfach [iset]
              (letfn [(simpl [[a s]]
                        [a (map first (filter #(= (second %) 1) (frequencies s )))]
                        )]
                (into {} (map simpl iset)
                  )

                )

              )

            (mkipath [[a ss]]
              [[a] (clojure.set/difference (set ss) #{a})]
              )

            (initpath [iset]
              (map mkipath iset)
              )

            (updatepath [p iset]
              (let [pv (first p) ps (second p)]
                (if (not (empty? ps))
                  (for [s ps]
                    [(conj pv s) (clojure.set/difference (set (iset s)) (set pv))]

                    )
                  )
                )

              )

            (proppath [iset paths]
              (loop [ps paths]
                ;(do (println ps)
                (cond (some #(= (count (first %)) (count iset)) ps ) true
                      (every? true? (map #(empty? (second %)) ps))  false
                      :else (recur
                              (reduce concat (for [p ps]
                                               (updatepath p iset)
                                               ))
                              )
                      )
                ;)
                )
              )

            ]


      (let [iset (einfach  (initset {} xset )) ]
        (proppath iset (initpath iset))
        )
      )
    ))

(defcheck solution-46039a00
  (fn [ps]
    (letfn
     [
      (rns [n aps]
        (keep identity
          (for [p aps]
            (if (= (first p) n)
              (second p)
              (if (= (second p) n)
                (first p)
                )
              )
            )
          )
        )
      (take [p aps]
        (let [x (atom 0)]
          (remove #(and (= p %) (= @x 0) (swap! x inc)) aps
            )
          )
        )
      (path? [n aps]
        (or
         (empty? aps)
         (some #(path? % (take (sort [n %]) aps)) (rns n aps))
         )
        )
      ]
      (or
       (some #(path? % (for [p ps] (sort p))) (distinct (flatten ps)))
       false
       )
      )
    ))

(defcheck solution-4628f012
  (fn [g]
    (let [g*    (map set g)
          v     (set (flatten g))
          deg   (fn [v]
                  (count (filter #(contains? % v) g*)))
          degs  (map deg v)
          n     (count (filter odd? degs))
          conn? (fn [edges]
                  (= (set (apply concat edges))
                    (loop [vs #{(ffirst edges)}]
                      (if-let [nvs (seq (for [[a b] edges :when (and (vs a) (not (vs b)))] b))]
                        (recur (into vs nvs))
                        vs))))]
      (and
       (conn? g)
       (or
        (= n 2)
        (= n 0))
       ))))

(defcheck solution-46b6ed84
  (fn [edgevec]
    (letfn [(neighbors [edgevec] ;; map each vertex to its set of neighbors
              (reduce
                (fn [m e]
                  (letfn [(addmap [m a b] (assoc m a (if-let [nbdset (get m a)]
                                                       (conj nbdset b)
                                                       (set (list b)))))]
                    (let [a (e 0) b (e 1)]
                      (addmap (addmap m a b) b a)))) ;; reduce function
                {} edgevec))
            (connected? [edgevec]
              (let [N (neighbors edgevec)
                    vertices (keys N)
                    q (atom #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []))]
                (letfn [(nq [v] (swap! q #(conj  % v)))
                        (hq [] (peek @ q))
                        (dq [] (swap! q pop ))]
                  (nq (first vertices))
                  (loop [bag #{}]
                    (if-let [head  (hq)]
                      (do  (dq) (loop [v (vec (N head))]
                                  (if (not (empty? v))
                                    (do
                                      (if (not (contains? bag (first v))) (nq (first v)))
                                      (recur (rest v)))))
                           (recur (conj bag head)))
                      (= (set vertices) bag))))))
            (degmod2 [edgevec]
              (reduce
                (fn [m e]
                  (letfn [(addmap [m a b] (assoc m a (if-let [bit (get m a)] (bit-xor bit 1) 1)))]
                    (let [a (e 0) b (e 1)]
                      (addmap (addmap m a b) b a))))
                {} edgevec))]
      (let [oddballs (reduce +  (vals (degmod2 edgevec)))]
        (and (connected? edgevec) (<= oddballs 2))))))

(defcheck solution-479518df
  (fn [coll]
    ((complement empty?)
     (nth (iterate
            (fn [ps]
              (mapcat (fn [[p l]]
                        (->> (range (count l))
                          (map #(take (count l) (drop % (cycle l))))
                          (map #(let [[u v] (last p) [x y] (first %)]
                                  (vector (cond (= v x) (conj p [ x y])
                                                (= v y) (conj p [y x])
                                                :else (conj p nil))
                                    (rest %))))
                          (filter #(last (first %)))))
                ps))
            (->>  (range (count coll))
              (map #(take (count coll) (drop % (cycle coll))))
              (mapcat #(vector (vector [(first %)] (rest %))
                         (vector [(reverse (first %))] (rest %))))))
       (dec (count coll))))))

(defcheck solution-4825540b
  #(not (not (and
              (some (fn [u] (= u (count (filter odd? (vals (frequencies (flatten %)))))))
                [0 2])
              (loop [[[x y :as z] & r] % s (into #{} z)]
                (if (empty? z) true
                               (if (or (s x) (s y)) (recur r (conj s x y))))
                )))))

(defcheck solution-4867eae
  (fn tourable? [acoll]
    (letfn [(key-edges-map [edges]
              (-> (mapcat (fn [[s t :as edge]] [{s [edge]} {t [edge]}])
                    edges)
                (#(apply merge-with concat %))
                (#(into {} (for [[k vs] %] [k (filter (fn [v] (not= k v)) (apply concat vs))])))))

            (remove-from-coll [acoll elem]
              (let [idx (.indexOf acoll elem)]
                (concat (take idx acoll)
                  (drop (inc idx) acoll))))

            (remove-used [em k1 k2]
              (-> em
                (merge {k1 (remove-from-coll (get em k1) k2)})
                (merge {k2 (remove-from-coll (get em k2) k1)})))

            (inner-tourable? [key-edges-map edge-count]
              (if (some #{true}
                    (map (fn xxx [key]
                           ((fn yyy
                              [m
                               k
                               distance-from-full-tour]
                              (let [next-nodes (get m k)
                                    new-distance (dec distance-from-full-tour)]
                                (cond
                                  (zero? distance-from-full-tour) true
                                  (empty? next-nodes) false
                                  :else (if (some #{true}
                                              (map #(yyy
                                                      (remove-used m k %)
                                                      %
                                                      new-distance)
                                                next-nodes))
                                          true
                                          false))))
                            key-edges-map key edge-count))
                      (keys key-edges-map)))
                true
                false))]
      (inner-tourable? (key-edges-map acoll) (count acoll)))))

(defcheck solution-4898f9d5
  (fn [edges]
    (letfn
     [(add-edge [emap edge]
        (update-in emap [(first edge)] #(conj (or %1 #{}) edge)))
      (edge-map [edges]
        (reduce #(add-edge (add-edge %1 %2) (vec (reverse %2))) {} edges))
      (path? [emap visited-edges node]
        (or (= (count visited-edges) (count edges))
            (some (fn [edge]
                    (and (not (contains? visited-edges (set edge)))
                         (path? emap (conj visited-edges (set edge)) (second edge))))
              (get emap node))))]
      (let [emap (edge-map edges)]
        (boolean (some (fn [edge]
                         (or (path? emap #{(set edge)} (first edge))
                             (path? emap #{(set edge)} (second edge))))
                   edges))))))

(defcheck solution-48f66937
  (fn [edges]
    (letfn [(nodes [edges] (set (flatten (vec edges))))
            (neighbours [edges node]
              (set
                (for [e edges :when (some #(= node %) e)]
                  (if (= (first e) node) (second e) (first e)))))
            (component [edges node]
              (loop [todo #{node},
                     found #{node}]
                (if (empty? todo) found
                                  (let [n      (first todo)
                                        neighb (remove found (neighbours edges n))]
                                    (recur (disj (into todo neighb) n) (conj found n))))))
            (connected? [edges]
              (let [all-nodes (nodes edges)]
                (= all-nodes (component edges (first all-nodes)))))
            (degree [edges node]
              (count (filter #(= node %) (flatten (vec edges)))))]
      (let [odd-nodes (count (filter odd? (map (partial degree edges) (nodes edges))))]
        (and (or (= 0 odd-nodes) (= 2 odd-nodes))
             (connected? edges))))))

(defcheck solution-4a7bfded
  (fn [g]
    (let [degs (into {} (for [[k v] (group-by identity (flatten g))] [k (count v)]))
          nodes (keys degs)
          connected-comp (fn find-cc [c cc]
                           (let [nexts (for [[x y] g :when (= x c) :when (not (contains? cc y))] y)
                                 ncc (into cc nexts)]
                             (if (empty? nexts)
                               cc
                               (reduce into ncc (map #(find-cc % ncc) nexts)))))
          connected (= (count (connected-comp (first nodes) #{(first nodes)})) (count nodes))
          odd-nodes (count (filter odd? (vals degs)))]
      (and connected (or (= 0 odd-nodes) (= 2 odd-nodes))))))

(defcheck solution-4abe2390
  (fn graph-tour [g]
    (letfn [
            (connected? [s]
              (letfn [
                      (get-children[s root ignore]
                        (clojure.set/difference
                          (set (for [[x y] s :when (or (= root x) (= root y))] (if (= x root) y x)))
                          ignore)
                        )
                      (build-tree [root s ignore]
                        (let [children (seq (get-children s root ignore))]
                          (if (empty? children)
                            root
                            (loop [result [root] new-children children new-ignore (into ignore children)]
                              (if (empty? new-children)
                                result
                                (let [subtree (conj result (build-tree (first new-children) s new-ignore))]
                                  (recur subtree (rest new-children) (into new-ignore (flatten subtree)))
                                  )
                                )
                              )
                            )
                          )
                        )
                      ]
                (let [root (ffirst s) tree (build-tree root s #{root}) size (->> s seq flatten set count)]
                  (= size (if (vector? tree) (->> tree flatten count) 1))
                  )
                )
              )
            ]
      (and (connected? g)
           (< (->> g flatten (group-by identity) (map #(count (second %))) (filter odd?) count) 3))
      )
    ))

(defcheck solution-4b11e0c4
  (fn graph-tour? [edges]
    (letfn [(connected-nodes
              [nodes coll]
              (let [nodes' (into nodes
                             (flatten
                               (for [[a b] coll :when (or (contains? nodes a)
                                                          (contains? nodes b))]
                                 [a b])))]
                (if (= nodes nodes')
                  nodes
                  (recur nodes' coll))))
            (connected-graph?
              [s]
              (= (connected-nodes #{(ffirst s)} s)
                (set (flatten (seq s)))))
            (tour?
              [s]
              (let [edge-count (map #(count (val %)) (group-by identity (flatten s)))]
                (or (every? even? edge-count)
                    (= 2 (count (filter odd? edge-count))))))]
      (and (connected-graph? edges)
           (tour? edges)))))

(defcheck solution-4b376b33
  (letfn [(next-nodes
            [g s]
            (mapcat #(cond
                       (= (first %) s) [(second %)]
                       (= (second %) s) [(first %)]
                       :else [])
              g))
          (remove-edge
            [g s e]
            ;;(filter #(and (not= % [s e]) (not= % [e s])) g)
            (let [x (group-by #(or (= % [s e]) (= % [e s])) g)]
              (concat (get x false) (butlast (get x true)))))]
    (fn traversible
      ([g]
       (if (some (partial traversible g) (into #{} (apply concat g)))
         true
         false))
      ([g s]
       (if (empty? g)
         true
         (let [x (next-nodes g s)]
           (if (empty? x)
             nil
             (some #(traversible (remove-edge g s %) %) x))))))))

(defcheck solution-4b87a4a
  (fn [edges]
    (letfn [(validStart? [node remainingEdges] ;remainingEdges is a frequency map of nodes
              (if (empty? remainingEdges) true ;all edges have been visited
                                          (reduce #(or %1 %2) false ;for shall generate a list of sub-graph tour results
                                            (for [[edge frequency] remainingEdges ;frequency map
                                                  :let [start (first edge) end (second edge)]
                                                  :when (or (= start node) (= end node))];can go further; else for may return an empty list '()
                                              (validStart? (if (= start node) end start);reset start point
                                                (if (= 1 frequency) ;adjust frequency map
                                                  (dissoc remainingEdges [start end])
                                                  (assoc remainingEdges [start end] (dec frequency))))))))]
      (let [edgeFrequency (frequencies edges)]
        (loop [nodes (reduce into #{} edges)]
          (if-let [node (first nodes)]
            (if (validStart? node edgeFrequency) true ;found a valid start point
                                                 (recur (rest nodes)))
            false))))))

(defcheck solution-4bd8f984
  (fn [graph]
    (letfn
     [(search [cur path]
        (if (= (count path) (count graph))
          ;; then every edge has been path, yay!
          path

          ;; else search remaining edges
          (for [e graph
                :when (some #{cur} e)
                :when (not-any? #{e} path)
                ncur e
                :when (not= ncur cur)
                p (search ncur (conj path e))
                :when (seq p)]
            p)))]

      ;; search the graph, starting from each point
      (->> (distinct (flatten graph))
        (map (fn [a] (search a [])))
        (filter not-empty)
        ((complement empty?))))))

(defcheck solution-4dba8d9f
  (fn eulerian?
    [edges]
    (letfn [(nodes
              [edges]
              (set (apply concat edges)))

            (neighbours
              [node edges]
              (loop [edges (seq edges) neighbours []]
                (if edges
                  (let [[x y] (first edges)]
                    (cond
                      (= x node) (recur (next edges) (conj neighbours y))
                      (= y node) (recur (next edges) (conj neighbours x))
                      :else      (recur (next edges) neighbours)))
                  neighbours)))

            (walk
              ([start edges]
               (walk start edges #{}))
              ([start edges seen]
               (if (empty? edges)
                 (conj seen start)
                 (let [ns (neighbours start edges)
                       new-edges (remove (fn [[a b]] (or (= start a) (= start b))) edges)
                       new-seen (conj seen start)]0
                                                  (reduce into new-seen (map #(walk % new-edges new-seen) ns))))))

            (connected?
              [edges]
              (let [ns (nodes edges)]
                (= ns (walk (first ns) edges))))

            (degree [node] (count (neighbours node edges)))

            (eulerian?
              [edges]
              (and (connected? edges)
                   (<= (count (filter odd? )))))]

      (and (connected? edges)
           (<= (count (filter odd? (map degree (nodes edges)))) 2)))))

(defcheck solution-4e189e96
  (fn [edges]
    (let [updated (fn [dict key val]
                    (conj (dict key '()) val))
          add-edge (fn [adj [from to]]
                     (into adj [[from (updated adj from to)]
                                [to (updated adj to from)]]))
          adj (reduce add-edge {} edges)
          start (first (first edges))
          desired (count (keys adj))
          neighbors (fn [v vis] (clojure.set/difference (set (adj v)) vis))
          connected? (loop [[x & _ :as all] (list start) vis #{}]
                       (cond
                         (empty? all) (= (count vis) desired)
                         :else (recur
                                 (concat (rest all) (neighbors x vis))
                                 (conj vis x))))
          satisfies? (->> adj
                       vals
                       (map count)
                       (filter odd?)
                       count
                       (> 3))]
      (and satisfies? connected?))))

(defcheck solution-4f48048e
  (fn eulerian [edges]
    (let [degrees (fn [edges]
                    (apply merge-with + {} (for [[a b] edges
                                                 :when (not= a b)]
                                             {a 1 b 1})))
          gdeg (degrees edges)]
      (and
       (not (empty? gdeg))
       (->> (vals gdeg) (filter odd?) count (>= 2))))))

(defcheck solution-4fa59753
  (fn p89 [col]
    (let [connected? (fn [chain-seq vb]
                       (let [h (first (first chain-seq)) t (last (last chain-seq))]
                         (or (= h (first vb))
                             (= h (second vb))
                             (= t (first vb))
                             (= t (second vb)))))

          find_next_edge (fn [v subcol]
                           (filter  #(first %)  (map (fn [v1] [(connected? v v1) v1 ] )  subcol)))

          visit-graph (fn visit-graph [chain-seq rest-set]
                        (if (empty? rest-set)
                          chain-seq
                          (let [next_chain_tuple (find_next_edge  chain-seq rest-set)]
                            (if (empty? next_chain_tuple)
                              []
                              (loop [possible_tuple (first next_chain_tuple) data (rest next_chain_tuple) ]
                                (let [possible_chain (visit-graph
                                                       (conj chain-seq (second  possible_tuple))
                                                       (disj rest-set   (second possible_tuple)  )    )]
                                  (if (and (empty? data) (empty? possible_chain))
                                    []
                                    (if (not (empty? possible_chain))
                                      possible_chain
                                      (recur (first data) (rest data))
                                      ))
                                  ))))))


          ]
      (= (count (visit-graph [(first col)] (set (rest col))))  (count col) (count (set col)) )
      ;(visit-graph [(first col)] (set (rest col)))
      )


    ))

(defcheck solution-4faeadbf
  (fn [coll]
    (let [
          c (count
              (filter odd?
                (map
                  (comp count second)
                  (merge-with concat
                    (group-by first coll)
                    (group-by second coll)))))
          conn (set (concat coll (map (comp vec reverse) coll)))
          connected (fn [coll c] true
                      (let [n (count (set (reduce concat coll)))]
                        (loop [c1 #{} c2 #{c}]
                          (if (= c1 c2)
                            (= (count c1) n)
                            (recur c2
                              (clojure.set/union c2
                                (set (reduce concat
                                       (map (fn [x]
                                              (map second (filter (fn [y] (= (first y) x)) coll)))
                                         c2)))))))))
          ]
      (cond
        (not (connected conn (first (first coll)))) false
        (= c 0) true
        (= c 2) true
        true false
        )
      )))

(defcheck solution-50a60e5c
  (fn [g]
    (letfn [(t [v g]
              (or (not g)
                  (some (fn [[[a b] & m]] (or (and (= v a) (t b m))
                                              (and (= v b) (t a m))))
                    (map #(concat (drop % g) (take % g)) (range (count g))))))]
      (boolean (some #(t % g) (flatten g))))))

(defcheck solution-50af8692
  (fn [xs]
    (let [find-children (fn [nodes node]
                          (reduce
                            (fn [acc [from to]]
                              (if (= node from)
                                (conj acc to)
                                (if (= node to)
                                  (conj acc from)
                                  acc))) #{} nodes))
          remove-node (fn [nodes [from to]]
                        (loop [nodes nodes found false acc []]
                          (if (empty? nodes)
                            acc
                            (if (and (not found) (or
                                                  (= [from to] (first nodes))
                                                  (= [to from] (first nodes))))
                              (recur (rest nodes) true acc)
                              (recur (rest nodes) found (conj acc (first nodes)))))))]
      (loop [to-visit (conj #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) {:node (first (first xs)) :nodes xs})]
        (if (empty? to-visit)
          false
          (if (empty? (:nodes (peek to-visit)))
            true
            (recur
              (into (pop to-visit)
                (map (fn [node] {:node node
                                 :nodes (remove-node (:nodes (peek to-visit))
                                          [(:node (peek to-visit)) node])})
                  (find-children
                    (:nodes (peek to-visit))
                    (:node (peek to-visit))))))))))))

(defcheck solution-511127af
  (fn tour?
    ([graph]
     (not (nil? (some true?
                  (map
                    #(tour? % graph)
                    (distinct (apply concat graph)))))))
    ([vertex graph]
     (if (empty? graph) true
                        (let
                         [ other-extremity
                          (fn [edge v]
                            (if (= v (first edge))
                              (last edge)
                              (first edge)))
                          remove-edge
                          (fn [g edge]
                            (let [[b e] (split-with #(not (= edge %)) g)]
                              (concat b (rest e))))
                          connected-edge
                          (fn [g v]
                            (filter #(contains? (set %) v) g))]
                          (some true? (map
                                        (fn [edge] (tour?
                                                     (other-extremity edge vertex)
                                                     (remove-edge graph edge)))
                                        (connected-edge graph vertex))))))))

(defcheck solution-518d2aea
  (fn
    f
    [g]
    (letfn [(rf [s e]
              (let [p (split-with #(not= e %) s)]
                (concat
                  (-> p first)
                  (-> p second rest))))
            (t [current seen remaining]
              (let [nexts (filter #(or (= current (first %)) (= current (last %))) remaining)]
                (cond (empty? remaining) true
                      (empty? nexts) false
                      :e (reduce #(or %1 %2) (map #(t (first (filter (fn [x] (not= current x)) %))
                                                     (conj seen %)
                                                     (rf remaining %)
                                                     ) nexts)))))]
      (t (ffirst g) [] g)
      )))

(defcheck solution-519c14ad
  (fn [i-edges]
    (let [
          nodes (reduce #(conj (conj %1 (first %2)) (last %2)) #{} i-edges)

          n-count (count nodes)
          n-range (range n-count)
          n-map (apply hash-map (interleave nodes n-range))
          n-index (fn [i-node] (get n-map i-node))

          e-count (count i-edges)

          edges-count (fn [my-edges]
                        (reduce
                          (fn [result i-edge]
                            (let [[edge occurences] i-edge]
                              (conj result (hash-map edge (count occurences)))))
                          {}
                          (group-by #(apply vector %) (map sort my-edges))))
          edge-count (fn [i-edge]
                       (let [e-count (edges-count i-edges)]
                         (get e-count i-edge 0)))

          edges (concat i-edges (map #(apply vector (reverse %)) i-edges))
          grouped-edges (group-by #(n-index (first %)) edges)

          n-dest (fn [i-node]
                   (reduce #(conj %1 (last %2)) #{} (get grouped-edges (n-index i-node))))

          build-chains (fn chain
                         ([i-edge] (chain (dec e-count) (vector (vector i-edge))))
                         ([i result]
                          (if (zero? i)
                            result
                            (chain
                              (dec i)
                              (reduce
                                concat
                                []
                                (map
                                  (fn [i-chain]
                                    (let [
                                          t-node (last (last i-chain))
                                          n-edges (map #(vector t-node %) (n-dest t-node))
                                          ]
                                      (filter
                                        (fn [my-chain]
                                          (reduce
                                            (fn [result i-edge]
                                              (let [[edge o-count] i-edge]
                                                (and result (>= (edge-count edge) o-count))))
                                            true
                                            (edges-count my-chain)))
                                        (map #(conj i-chain %) n-edges))))
                                  result))))))
          ]
      (reduce
        (fn [result i-edge]
          (if (false? result)
            (boolean (some #(= (count %) e-count) (build-chains i-edge)))
            result))
        false
        i-edges))))

(defcheck solution-5216aa32
  (fn [E]
    (let [G (reduce (fn [R [a b]] (assoc R a (conj (R a []) b) b (conj (R b []) a))) {} E)
          N (keys G)
          C (loop [R #{(first N)}]
              (let [D (mapcat (fn [v] (apply disj (set (G v)) R)) R)]
                (if (empty? D) R (recur (into R D)))))
          F (remove (comp even? count) (vals G))]
      (and (= C (set N)) (<= (count F) 2)))))

(defcheck solution-521e068
  (fn [edges]
    (let [v (zipmap (distinct (flatten edges)) (repeat 0))
          degrees (vals (reduce
                          (fn [r [a b]]
                            (if (not= a b)
                              (-> r
                                (update-in [a] inc)
                                (update-in [b] inc))
                              r))
                          v edges))
          odd (count (filter odd? degrees))
          connected? (fn [edges]
                       (let [find (fn [union k] (or (some #(if (contains? % k) %) union) #{k}))]
                         (= 1 (count
                                (reduce (fn [r [a b]]
                                          (let [ua (find r a)
                                                ub (find r b)]
                                            (-> r
                                              (disj ua ub)
                                              (conj (clojure.set/union ua ub)))))
                                  #{} edges)))))]
      (and
       (or (= odd 2) (= odd 0))
       (connected? edges)))))

(defcheck solution-52f5a797
  (fn [ss]
    (letfn [(drop-nth [xs n] (let [[f b] (split-at n xs)] (concat f (rest b))))
            (tour [ss n]
              (if (empty? ss)
                true
                (some
                  (fn [[i [n1 n2]]]
                    (cond
                      (= n n1) (tour (drop-nth ss i) n2)
                      (= n n2) (tour (drop-nth ss i) n1)
                      :else false))
                  (map-indexed vector ss))))]
      (boolean
        (some
          (fn [[i [h t]]]
            (or
             (tour (drop-nth ss i) h)
             (tour (drop-nth ss i) t)))
          (map-indexed vector ss))))))

(defcheck solution-52fd983e
  #(letfn [(vertices [e] (set (flatten e)))

           (no-isolated-vs? [e]
             (= (vertices e)
               (vertices (filter (fn [[a b]] (not= a b)) e))))]

     (and (no-isolated-vs? %)
          (->> %
            flatten
            frequencies
            vals
            (filter odd?)
            count
            (> 3)))))

(defcheck solution-53f53d7b
  (fn f [[a & b]]
    (if (nil? b)
      true
      (let [i (first (filter #(and (some (set a) %) (not= a (reverse %)) (not= a %)) b))
            j (first (filter #(some (set a) %) b))
            k (fn [i] `(~(take 2 (flatten (vals (sort (group-by count (partition-by identity (sort (concat i a)))))))) ~@(remove #{i} b)))]
        (if i
          (f (k i))
          (if j
            (f (k j))
            false))))))

(defcheck solution-541d4aa8
  (letfn [(matching-edges [edges node]
            (filter #((set %) node) edges))
          (other-node [edge node]
            (if (= node (first edge))
              (second edge)
              (first edge)))
          (remove-one [edges edge]
            (concat (rest (filter (partial = edge) edges))
              (filter (partial not= edge) edges)))
          (tour-from [edges node]
            (if (empty? edges)
              '()
              (some identity
                (for [edge (matching-edges edges node)]
                  (when-let [tour (tour-from (remove-one edges edge) (other-node edge node))]
                    (cons edge tour))))))
          (graph-tour [edges]
            (boolean
              (some identity
                (for [node (set (mapcat identity edges))]
                  (tour-from edges node)))))]
    graph-tour))

(defcheck solution-545cbad3
  (fn [V]
    (let [e (fn e [s V]
              (or (empty? V)
                  (some true?
                    (map-indexed (fn [i [a b]]
                                   (let [r (if (= b s) a (if (= a s) b))]
                                     (if r (e r (vec (concat (subvec V 0 i) (subvec V (inc i))))))))
                      V))))
          n (into #{} (apply concat V))
          ]
      (not-every? nil? (map #(e % V) n)))))

(defcheck solution-551bf501
  (fn prob89 [graph]
    (letfn
     [(connected? [edges] ; from problem 91
        (let
         [csets
          (reduce
            (fn [s edge]
              (let [eset (set edge)
                    isets (filter #(not (empty? (clojure.set/intersection % eset))) s)]
                (clojure.set/union
                  (clojure.set/difference s isets)
                  (hash-set (apply clojure.set/union eset isets)))))
            #{} edges)]
          (= 1 (count csets))))]
      (let
       ; i don't get it. if the edges are undirected, then they should be deduped...
       ; but this one fails the third unit test, with duplicate nodes.
       ; this IS supposed to bethe k&ouml;nigsberg bridge problem, right??
       ;[degrees (frequencies (flatten (distinct (map sort graph))))]
       [degrees (frequencies (flatten graph))]
        (and
         (or
          (every? even? (vals degrees))
          (= (count (filter odd? (vals degrees))) 2))
         (connected? graph))))))

(defcheck solution-5530953c
  (fn [edge-set] (letfn [ (vertices [edge-set] (sort (distinct (flatten (map flatten edge-set))))) (without [coll item] (cond (empty? coll) [] (= (first coll) item) (rest coll) :else (cons (first coll) (without (rest coll) item)))) (seq-contains? [coll item] (cond (empty? coll) false (= (first coll) item) true :else (seq-contains? (rest coll) item))) (edges-with [edge-set vertex] (filter #(seq-contains? % vertex) (map flatten edge-set))) (vertices-with-distance-1 [edge-set vertex] (mapcat #(without % vertex) (edges-with edge-set vertex))) (expand [edge-set vertices] (sort (distinct (concat vertices (mapcat #(vertices-with-distance-1 edge-set %) vertices))))) (iterate-while-changes [f x] (cons x (map second (take-while #(not= (first %) (second %)) (partition 2 1 (iterate f x)))))) (expand-full [edge-set] (last (iterate-while-changes (partial expand edge-set) (take 1 (vertices edge-set))))) (is-connected? [graph] (= (set (vertices graph)) (set (expand-full graph)))) (eq? [n] (partial = n)) (neq? [n] (partial not= n)) (seq-contains? [coll val] (true? (some (eq? val) coll))) (find-first [pred coll] (first (filter pred coll))) (remove-first [pred coll] (concat (take-while (complement pred) coll) (rest (drop-while (complement pred) coll)))) (other-end [edge x] { :pre [ (sequential? edge), (= 2 (count edge)), (seq-contains? edge x) ] } (find-first (neq? x) edge)) (starting-positions [edges] (let [verts (vertices edges)] (map (fn [vert] { :current-position vert, :edges-left edges }) verts))) (possible-next-edges [ { :keys [current-position edges-left] } ] (filter #(seq-contains? % current-position) edges-left)) (cross-edge [{:keys [current-position edges-left]} edge-to-cross] { :pre [(seq-contains? edges-left edge-to-cross)] :post [ (= (count (:edges-left %)) (dec (count edges-left))) ] } {:current-position (other-end edge-to-cross current-position) :edges-left (remove-first (eq? edge-to-cross) edges-left)}) (cross-edges [{:keys [current-position edges-left] :as position}] (map #(cross-edge position %) (possible-next-edges position))) (cross-edges-all [positions] (mapcat cross-edges positions)) (winning-position? [{:keys [current-position edges-left]}] (empty? edges-left)) (losing-position? [position] (and (not (winning-position? position)) (empty? (possible-next-edges position)))) (proceed [positions] (cond (some winning-position? positions) true (every? losing-position? positions) false :else (proceed (cross-edges-all (remove losing-position? positions))))) (graph-tour [edge-set] (proceed (starting-positions edge-set))) ] (graph-tour edge-set))))

(defcheck solution-557277ea
  (fn hamiltonian? [graph]
    (let [current-edge
          (fn [g e]
            (if (or (empty? g) (empty? e)) [[] []]
                                           (let [current-edge-index (.indexOf g e)
                                                 next-graph (vec
                                                              (concat
                                                                (subvec g 0 current-edge-index)
                                                                (subvec g (inc current-edge-index))))
                                                 current-node (if (meta e) (first e) (last e))
                                                 next-edge (last (filterv #(< -1 (.indexOf % current-node)) next-graph))
                                                 ;; TODO some->
                                                 next-edge (if (not-empty next-edge)
                                                             (if (= (first next-edge) current-node)
                                                               next-edge
                                                               (with-meta next-edge {}))
                                                             next-edge)]
                                             [next-graph next-edge])))]
      (loop [[g e] (current-edge graph (first graph))]
        (cond
          (empty? g) true
          (empty? e) false
          :else (recur (current-edge g e)))))))

(defcheck solution-56be361f
  (fn [G]
    (let [[[p _] & _ :as H] (vec G)
          V (into #{} (flatten H))
          n
            (fn [v [p q]]
              (if (= v p)
                [q]
                (if (= v q)
                  [p]
                  [])))
          m (fn [v G] (reduce #(into % (n v %2)) [] G))
          c
            (fn [G]
              (loop [U #{p}]
                (let [N (into U (mapcat #(m % G) U))]
                  (if (= N U)
                    (= U V)
                    (recur N)))))]
      (if (c G)
        (> 3 (count (filter #(= 1 (mod (count (m % G)) 2)) V)))
        false))))

(defcheck solution-56ca0fef
  (fn [edges]
    (and (= 1 (count (reduce (fn [c [u v]]
                               (let [s (or (first (filter #(% u) c)) #{u})
                                     t (or (first (filter #(% v) c)) #{v})]
                                 (conj (disj c s t) (clojure.set/union s t))))
                       #{} edges)))
         (let [num-odd-degree (count (filter #(odd? (count (val %)))
                                       (group-by identity (apply concat edges))))]
           (number? (#{0 2} num-odd-degree))))))

(defcheck solution-56f3384d
  (fn graph-tour [nodes]
    (letfn [(numnodes [nodes] (count (set (reduce concat nodes))))
            (to-node-map [nodes]
              ((fn !  [nodes node-map]
                 (if-let [n (seq nodes)]
                   (let [[key val] (first n)]
                     (if (contains? node-map key)
                       (! (rest n) (assoc node-map  key (conj (node-map key) val)))
                       (! (rest n) (assoc node-map key [val]))))
                   node-map)) (concat nodes (map #(reverse %) nodes)) {}))
            (search [node node-map visited]
              (if (or (contains? visited  node) (empty? (node-map node)))
                visited
                (set (mapcat #(search % node-map (conj visited node)) (node-map node)))))
            (connected? [numnodes node-map seed]
              (= numnodes (count (search seed node-map #{}))))
            (degrees [node-map]
              (map #(count (second %)) node-map))
            ]
      (let [N (numnodes nodes)
            node-map (to-node-map nodes)
            is-connected (connected? N node-map (ffirst nodes))
            node-degrees (degrees node-map)
            ]
        (and is-connected (<= (count (filter odd? node-degrees)) 2)))
      )))

(defcheck solution-56ffe656
  (fn [edges]
    (let [neighbors (reduce (fn [a [x y]]
                              (assoc a
                                x (conj (a x) y)
                                y (conj (a y) x))
                              ){} edges)
          n0 (ffirst neighbors)
          visited (reduce (fn [a _]
                            (reduce (fn [a n] (into a (neighbors n))) a a)
                            )
                    #{n0} neighbors)]

      (and (= visited (set (keys neighbors)))
           (->> neighbors vals (map count) (filter odd?) count
             (#{0 2})
             (number?))
           ))))

(defcheck solution-57592038
  (fn graph-tour? [edges]
    (let
     [nodes (reduce
              #(-> (conj %1 (first %2))
                 (conj (second %2)))
              #{}
              edges)
      edges-at (fn [node edges]
                 (filter
                   #(or (= node (first %)) (= node (second %)))
                   edges))
      other-end (fn [node edge]
                  (if (= node (first edge))
                    (second edge)
                    (first edge)))
      remove-first (fn remove-first [ [fedge & redges] edge]
                     (if (= edge fedge)
                       redges
                       (cons fedge (remove-first redges edge))))
      visit-all-from? (fn visit-all-from? [snode edges]
                        (if (empty? edges)
                          true
                          (let [next-edges (edges-at snode edges)]
                            (some
                              (fn [edge]
                                (visit-all-from? (other-end snode edge) (remove-first edges edge)))
                              next-edges))))]
      (true?
        (some
          #(visit-all-from? % edges)
          nodes)))))

(defcheck solution-57f37b67
  (fn [input]
    (let [
          join (fn [[a1 b1] [a2 b2]]
                 (remove nil?
                   [(if (= b1 a2) [a1 b2])
                    (if (= b1 b2) [a1 a2])
                    (if (= a1 b2) [b1 a2])
                    (if (= a1 a2) [b1 b2])]))
          rm (fn [x list] (remove #(= x %) list))
          test-fn (fn f[a rest]
                    (if (empty? rest)
                      true
                      (loop [b (first rest), r (next rest)]
                        (let [t (join a b),  r2 (rm b rest)]
                          (cond (some true? (map #(f % r2) t)) true
                                (empty? r) false
                                :else (recur (first r) (next r)))))))]
      (test-fn (first input) (next input)))))

(defcheck solution-5881e090
  (fn hamiltonian? [edges]
    (letfn [
            (grades [edges]
              (map (comp count second) (group-by identity (flatten edges))))

            (components [edges]
              (reduce (fn [comps edge]
                        (let [e (set edge)
                              {conn false, unconn true} (group-by #(empty? (clojure.set/intersection e %)) comps)
                              ] (conj unconn (apply clojure.set/union e conn))))
                [] edges))
            ]
      (let [connected (= 1 (count (components edges)))
            oddgrades (count (filter odd? (grades edges)))]
        (and connected (contains? #{0 2} oddgrades))))))

(defcheck solution-58f4d80b
  (fn eulerian-path? [edge-list]
    (letfn [(adjacency-map [edge-list]
              (let [reverse-edge-list (set (mapcat (fn [[a b]] [[b a]]) edge-list))
                    full-edge-list (into edge-list reverse-edge-list)]
                (into {} (for [[vertex edges] (group-by first full-edge-list)]
                           [vertex (set (map second edges))]))))
            (traverse-graph-dfs [adjacency-map start-vertex]
              (loop [vertices []
                     explored #{start-vertex}
                     frontier [start-vertex]]
                (if (empty? frontier)
                  vertices
                  (let [vertex (peek frontier)
                        neighbors (adjacency-map vertex)]
                    (recur
                      (conj vertices vertex)
                      (into explored neighbors)
                      (into (pop frontier) (remove explored neighbors)))))))
            (fully-connected? [edge-list]
              (let [vertices (set (apply concat edge-list))
                    adj-map (adjacency-map edge-list)]
                (= (count vertices) (count (traverse-graph-dfs adj-map (first vertices))))))
            (vertex-degrees [edge-list]
              (apply merge-with + {} (for [[a b] edge-list
                                           :when (not= a b)]
                                       {a 1 b 1})))]
      (let [degrees (vertex-degrees edge-list)
            odd-degree-vertex-count (count (filter odd? (vals degrees)))]
        (and
         (fully-connected? edge-list)
         (contains? #{0 2} odd-degree-vertex-count))))))

(defcheck solution-591bee6d
  (fn graph-connected? [coll]
    (let [split (fn  [coll]  (map-indexed #(vector %2 (concat (take %1 coll) (drop (inc %1) coll))) coll))
          any? (comp not not-any?)
          adjacent (fn [e graph] (filter #(some (partial = e) (first %)) (split graph)))
          make-directed (fn [edge e] (if (= e (get edge 0)) edge (vec (reverse edge))))
          can-connect? (fn can-connect? [[e1 e2 :as p] graph]
                         (if (empty? graph) true
                                            (any? #(can-connect? (make-directed (first %) e2) (second %))
                                              (adjacent e2 graph))))]
      (any? #(or (can-connect? (first %) (second %))
                 (can-connect? (vec (reverse (first %))) (second %)))
        (split coll)))))

(defcheck solution-591d2a8
  (fn [graph] (let [
                    start     (map #(assoc {} % 0) (set (flatten graph)))
                    add-edge  (fn [s [a b]] (flatten (reduce
                                                       (fn [[d j] c]
                                                         (let [ma (if (c a) {a 1}), mb (if (c b) {b 1})]
                                                           (if (or ma mb)
                                                             [d (merge-with + c j ma mb)]
                                                             [(conj d c) j])))
                                                       [[] {}] s)))
                    part        (reduce add-edge start graph)]
                (and (not (second part))
                     (->> part first vals (filter odd?) count (> 3))))))

(defcheck solution-59e0c1ca
  (fn gt [g]
    (let [e (set (reduce clojure.set/union g))
          c (count e)
          s (frequencies (reduce concat g))
          ones (filter #(= 1 %) (vals s))
          co (count ones)
          o g
          d (distinct g)
          g (set g)
          gc (let [f (first g)
                   filt (fn [x g]
                          (let [f (fn [v]
                                    (cond
                                      (= (first x) (first v)) v
                                      (= (first x) (last v)) v
                                      (= (last x) (first v)) v
                                      (= (last x) (last v)) v
                                      :else nil))]
                            (map f g)))]
               (loop [r #{f}
                      pr #{}]
                 (if (= (count r) (count pr))
                   (= g r)
                   (recur (clojure.set/union r (disj (apply clojure.set/union
                                                       (for [x r]
                                                         (into #{} (filt x g))))
                                                 nil))
                     r))))]
      (if (or (not= o d) (> co 2))
        false
        gc))))

(defcheck solution-5a53ebfa
  (fn [edges]
    (let [nodes (reduce
                  (fn [mp [a b]] (->>
                                   (if (mp a) (assoc mp a (cons b (mp a)))
                                              (assoc mp a [ b]))
                                   (#(if (% b) (assoc % b (cons a (% b)))
                                               (assoc % b [ a])))
                                   )) {} edges)

          onodes (reduce #(if (odd? (count (val %2))) (inc %) %) 0 nodes)

          gset (loop [a (set (first edges))]
                 (let
                  [b (->> (filter #(some (fn [x] (a x)) %) edges)flatten set)]
                   (if (= (count a) (count b) ) b (recur b))))

          ] (and (> 3 onodes) (= (count gset) (count (keys nodes)) ))
            )))

(defcheck solution-5ac0a98b
  (letfn
   [; Nodes within nn that can be reached in one step from n
    (reachable-nodes [e n nn]
      (set (filter
             (fn [x] (or (contains? e [n x]) (contains? e [x n])))
             nn)))

    ; Nodes within nn that cannot be visited starting from n
    (non-visited-nodes [e n nn]
      (if (empty? nn) #{}
                      (let [next-n (reachable-nodes e n nn)]
                        (if (empty? next-n) nn
                                            ;[n next-n nn]))))
                                            (apply clojure.set/intersection
                                              (map (fn [x] (set (non-visited-nodes e x (remove #(= x %) nn)))) next-n))))))

    ; Is connected?
    (connected? [e]
      (let [nn (set (apply concat e))]
        (empty? (non-visited-nodes e (first nn) (rest nn)))))]


    (fn [e]
      (let [n (->> e
                (apply concat ,)
                (frequencies ,)
                (vals ,)
                (filter odd? ,)
                (count ,))]
        (and
         (connected? (set e))
         (or (= n 0) (= n 2)))))))

(defcheck solution-5b287624
  #(boolean (or (#{1 11} (count %)) (= 1 (last (last %))))))

(defcheck solution-5bc2da9e
  (fn eulerian?
    [graph]
    (let
     [connected?
      ;; idea: depth first search
      ;;  but instead of having a mutable visited set, give the set
      ;;  as return value
                   (fn connected?
                     [graph]
                     (let
                      ;; every edge is undirected, so be sure that the edge list
                      ;; contains edges in both directions, e.g. if there is an
                      ;; undirected edge between u and v, there must be two
                      ;; directed edges [u v] and [v u] in the edge list
                      [graph (reduce #(let [u (first %2) v (first (rest %2))]
                                        (conj %1 (vector u v) (vector v u))) #{} graph)
                       dfs (fn dfs
                             [node visited]
                             (if (visited node)
                               visited
                               ;; get the neighbours of node out of the edge list
                               (let [neighbours (map
                                                  #(first (rest %))
                                                  (filter #(= (first %) node) graph))]
                                 ;; perform dfs on every neighbour and union
                                 ;; all the visited sets from every call to dfs
                                 ;; into one big visited set for the current recursion
                                 ;; level
                                 (reduce #(clojure.set/union %1 (dfs %2 %1))
                                   ;; visited set for the neighbour is the current
                                   ;; visited set plus the current node
                                   (conj visited node)
                                   neighbours))))]
                       ;; I say by definition, that a empty graph is connected
                       (if (empty? graph)
                         true
                         (let [nodes (reduce #(let [u (first %2) v (first (rest %2))]
                                                (conj %1 u v))
                                       #{}
                                       graph)
                               start-node (first nodes)
                               ;; it does not matter where to start the dfs
                               ;; because if the graph is connected one gets
                               ;; from every node to every other node
                               visited (dfs start-node #{})]
                           ;; if the nodes in the graph is the same as those visited
                           ;; with the depth first search, then the graph is connected
                           (= (count nodes) (count visited))))))
      nodes (reduce #(let [u (first %2) v (first (rest %2))]
                       (conj %1 u v))
              #{}
              graph)
      num-nodes (count nodes)
      node-degrees (reduce
                     (fn [acc curr]
                       (assoc acc
                         curr
                         (count (filter #(or (= curr (% 0)) (= curr (% 1)))
                                  graph))))
                     {}
                     nodes)]
      (and (connected? (set graph))
           (>= (count (filter #(even? (% 1)) node-degrees))
             (- num-nodes 2))))))

(defcheck solution-5c67b0b2
  (fn [g]
    (and
     (->> g (mapcat seq) frequencies vals (filter odd?) count #{0 2} boolean)
     ((fn f [e]
        (#(if (= e %) (= % (set g)) (f %))
         (reduce (fn [a b] (into a (filter #(some (set b) %) (set g))))
           #{}
           e)))
      #{(first g)}))))

(defcheck solution-5c6e7dcd
  (letfn [(remove-first [x xs] (when-let [[h & t] (seq xs)] (if (= x h) t (cons h (remove-first x t)))))
          (tour? [s t]
            (cond
              (empty? t) true
              (not-any? #(% s) t) false
              :else (some #(tour? (first (disj % s)) (remove-first % t))
                      (filter #(% s) t))))]
    (fn [t]
      (if (some #(tour? % (map set t))
            (set (apply concat t)))
        true
        false))))

(defcheck solution-5cceedc
  (fn travelling-salesman [graph]
    (letfn [(exits [graph node]
              (for [i (range (count graph))
                    :let [[head tail] (split-at i graph)
                          [a b] (first tail)]
                    :when (or (= a node) (= b node))]
                [(if (= node a) [a b] [b a]) (concat head (drop 1 tail))]))
            (walkable? [graph node]
              (if (empty? graph)
                true
                (do
                  (loop [exits (exits graph node)]
                    (if (empty? exits)
                      false
                      (let [[[from to] remaining] (first exits)]
                        (if (walkable? remaining to)
                          true
                          (recur (rest exits)))))))))]
      (walkable? graph (ffirst graph)))))

(defcheck solution-5cd8a483
  (let [part-of (fn [n e] (or (= n (first e)) (= n (second e))))
        to-node (fn [n e] (if (= n (first e)) (second e) (if (= n (second e)) (first e) nil)))]
    (fn thisfunc
      ([g] (thisfunc g (ffirst g) #{}))
      ([g n es]
       (or (= (set g) es)
           (reduce #(or %1 %2) false
             (for [edge (filter #(and (part-of n %) (not (es %))) g)]
               (thisfunc g (to-node n edge) (conj es edge)))))))))

(defcheck solution-5d94023
  (fn tourable? [nodes]
    (let [node-has? (fn [node edge] (some #(= edge %) node))
          except-idx (fn [idx coll] (vec (concat (take idx coll) (nthrest coll (inc idx)))))]
      (cond
        (= (count nodes) 0) false
        (= (count nodes) 1) true
        :else (every?
                true?
                (map-indexed
                  (fn all-edges-connected-and-even-degree [i node]
                    (let [a (first node)
                          b (second node)
                          degree-of-edges (count (filter #(or
                                                           (node-has? % a)
                                                           (node-has? % b))
                                                   (except-idx i nodes)))]
                      (and
                       (> degree-of-edges 0)
                       (even? degree-of-edges))))
                  nodes))))))

(defcheck solution-5e1b2c7e
  (fn [edges]
    (let [oddEdges (count (filter odd? (vals (frequencies (flatten edges)))))
          connected? (loop [graph (set (first edges))
                            otheredges (rest edges)]
                       (if (empty? otheredges)
                         true
                         (let [inGraph? (some true? (map #(contains? graph %) (first otheredges)))]
                           (if (not inGraph?)
                             false
                             (recur (apply conj graph (first otheredges))
                               (rest otheredges))))))]
      (if (and connected? (or (= oddEdges 0) (= oddEdges 2)))
        true
        false))))

(defcheck solution-5e7eac2f
  (fn [E]
    (let [V (set (apply concat E))
          degree (reduce #(assoc %1 %2 (inc (get %1 %2 0)))
                   {} (apply concat E))
          odd-degree (count (filter odd? (vals degree)))]
      (letfn [(dfs [node vis-]
                (let [vis (conj vis- node)]
                  (reduce #(dfs %2 %1) vis
                    (filter #(or (some (partial = [node %]) E)
                                 (some (partial = [% node]) E))
                      (filter #(not (contains? vis %)) V)))))]
        (do
          #_(println "V : " V)
          #_(println "degree : " degree)
          #_(println odd-degree)
          #_(println "DFS : " (dfs (first V) #{}))
          (and (or (= odd-degree 0)
                   (= odd-degree 2))
               (= V (dfs (first V) #{}))))))))

(defcheck solution-5e96fbd3
  (fn semi-eulerian? [edges]
    (let [remove-cycles #(remove (partial apply =) %)
          add-degree    #(update-in %1 [%2] (fnil inc 0))
          degrees (reduce (fn [m [u v]] (-> m (add-degree u) (add-degree v)))
                    {}
                    (remove-cycles edges))
          n (->> degrees vals (filter odd?) count)]
      (boolean (and (not-empty degrees)
                    (or (zero? n) (= n 2)))))))

(defcheck solution-5eb0c72f
  (fn graph-tour [edges]
    (let [nodes (into #{} (apply concat edges))
          num-edges (count edges)
          adjacency-mat
          (reduce (fn [mat [a b :as edge]]
                    (-> (update-in mat [a] conj (set edge))
                      (update-in [b] conj (set edge))))
            (into {} (map vector nodes (repeat #{})))
            edges)
          walk
          (fn walk [visited node]
            (lazy-seq
              (let [edges     (adjacency-mat node)
                    unvisited (clojure.set/difference edges visited)]
                (cond
                  (= (count visited) num-edges) (list true)
                  (empty? unvisited)            (list nil)
                  :else
                  (mapcat #(walk (conj visited %)
                             (first (disj % node))) unvisited)))))]
      (boolean (some true? (walk #{} (ffirst edges)))))))

(defcheck solution-5f398328
  (fn edge-tour [e-coll]
    (let [remove-item (fn [item coll] (filter #(not (= item %)) coll))
          next-edges (fn [pos left-coll] (filter #((set %) pos) left-coll))
          points (set (apply concat e-coll))
          step (fn f [path pos left-edges]
                 (if (empty? left-edges)
                   true
                   (let [nexts (next-edges pos left-edges)]
                     (if (empty? nexts)
                       false
                       (some #(if (or ((set path) %) ((set path) (reverse %)))
                                false
                                (f (cons % path) (if (= pos (first %)) (last %) (first %)) (remove-item % left-edges))) nexts)))))]
      (true? (some #(step [] % e-coll) points)))))

(defcheck solution-6197647b
  (fn [es]
    (let [vertice-edge-counts (loop [es (seq es)
                                     m {}]
                                (if-not es
                                  m
                                  (recur
                                    (next es)
                                    (let [[a b] (first es)]
                                      (merge-with + m {a 1} {b 1})))))

          n-odd (count (filter odd? (vals vertice-edge-counts)))

          edge-map (loop [es (seq es)
                          m {}]
                     (if-not es
                       m
                       (recur
                         (next es)
                         (let [[a b] (first es)]
                           (merge-with clojure.set/union
                             m
                             {a #{b}}
                             {b #{a}})))))

          connected? (loop [q [(first (keys edge-map))]
                            seen #{(first (keys edge-map))}]
                       (if-not (seq q)
                         (= seen (into #{} (keys edge-map)))
                         (let [unseen (clojure.set/difference (get edge-map (first q)) seen)]
                           (recur
                             (into (rest q) unseen)
                             (into seen unseen)))))]

      (and (or (= n-odd 0)
               (= n-odd 2))
           connected?))))

(defcheck solution-61cffe5f
  (fn[e](cond(= 1(count e))true(some(fn[[a b]](= a b))e)false 1 (->>(apply concat e)frequencies(every? #(even?(nth % 1)))))))

(defcheck solution-61f5efd9
  (fn f [s]
    (let [firsts (group-by first s)
          seconds (group-by second s)
          merged (merge-with concat firsts seconds)
          accessibles
          (loop [in (set (first s))]
            (let [news (into #{}
                         (flatten
                           (for [[a b] s
                                 [c d] s
                                 :when (and (or (in a) (in b))
                                            (or (= a c) (= a d) (= b c) (= b d)))]
                             [a b c d]
                             )))]

              (if (empty? (clojure.set/difference news in))
                in
                (recur (clojure.set/union in news)))))

          odds (for [[k v] merged
                     :when (odd? (count v))]
                 k)]
      #_(println "merged" merged)
      #_(println "odds" odds)
      #_(println "accessibles" accessibles)
      (and
       (< (count odds) 3)
       (every? accessibles (keys merged))))))

(defcheck solution-620a6453
  (fn eulerian [edges]
    (let [degrees
               (fn [edges]
                 (apply merge-with + {} (for [[a b] edges
                                              :when (not= a b)]
                                          {a 1 b 1})))
          gdeg (degrees edges)]

      (and
       (not (empty? gdeg))
       (->> (vals gdeg) (filter odd?) count (>= 2))))))

(defcheck solution-62cf1622
  (fn [edges]
    (let [
          maps (map #(list (hash-map (first %) (rest %)) (hash-map (last %) (butlast %))) edges)
          all (apply merge-with concat (flatten maps))]
      (or (= 1 (count edges)) (every? even? (map #(count (distinct (val %))) all))))))

(defcheck solution-64345c5c
  (fn [xs]
    (letfn [(devide [xs r]
              (if (empty? xs) r
                              (if (= (first (first xs)) (second (first xs))) (recur (rest xs) r)
                                                                             (let [x (gensym)
                                                                                   n (concat (concat r (first xs)) [x x])]
                                                                               (recur (rest xs) n)))))]
      (let [l (vals (frequencies (devide xs [])))]
        (if (nil? l) false
                     (or (every? even? l)
                         (= (count (filter odd? l)) 2)))))))

(defcheck solution-64689bfa
  (fn [lst]
    (let [nodes (distinct (reduce concat lst))
          uni (distinct (concat lst (map reverse lst)))
          adj (into {}
                (->> uni
                  (group-by first)
                  (map
                    (fn [[a,b]]
                      [a, (map last b)]))))
          nxt #(distinct (concat % (mapcat adj %)))
          sqnc (iterate nxt (list (first nodes)))
          conv (first (first (filter (fn [[a,b]] (= a b))(partition 2 sqnc))))
          edj (filter (fn [[a,b]] (not (= a b))) lst)
          grp (group-by identity (reduce concat edj))
          cnt (map #(count (last %)) grp)
          odd (filter odd? cnt)]
      (and
       (= (into #{} nodes) (into #{} conv))
       (<= (count odd) 2)))))

(defcheck solution-6560b050
  (fn tour
    ([g] (boolean (some #(tour g %) (apply concat g))))
    ([g v] (or (every? nil? g)
               (some #(tour (assoc g % nil) ((g %) 1))
                 (filter #(and (not (nil? (g %))) (= ((g %) 0) v))
                   (range (count g))))
               (some #(tour (assoc g % nil) ((g %) 0))
                 (filter #(and (not (nil? (g %))) (= ((g %) 1) v))
                   (range (count g))))))))

(defcheck solution-65cece7a
  (fn [C O I F g]
    (and (<= (count (filter odd? (vals (frequencies (F g))))) 2)
         (O
           (reduce
             (fn [s [f t]]
               (let [{[a & [r]] true o false :or {o []}}
                     (group-by #(or (C % f) (C % t)) s)]
                 (I #{(I a r)} o)
                 ))
             (I #{} (map #(hash-set %) (distinct (F g))))
             g)))) contains? #(= 1 (count %)) into flatten)

(defcheck solution-67280
  (fn eulerian-path-graph? [tuples]
    "a given graph is eulerian if the graph is connected and has at most 2 edges
having odd degrees, and the rest of edges with even-degree"
    (letfn [(duplicate-vertexes? [tuples]
              (not= (count tuples)
                (count
                  (set (map sort tuples)))))
            (find-edges [tuples] (vec (set (flatten (concat tuples)))))
            (find-adjacents [node tuples]
              (reduce (fn [adjacents-map [from to :as tuple]]
                        (if (get (set tuple) node)
                          (assoc adjacents-map node (set (concat (remove #(= node %) tuple) (adjacents-map node))))
                          adjacents-map))
                {} tuples))
            (find-nodes-with-adjacents [graph]
              (reduce (fn [nodes-with-adjacents node]
                        (conj nodes-with-adjacents (find-adjacents node tuples)))
                {} (find-edges tuples)))
            (graph-connected? [tuples]
              (letfn [(vec-contains? [xs x] (some #(= % x) xs))
                      (depth-first-connected? [node tuples visited]
                        (cond (vec-contains? visited node) visited
                              :else
                              (reduce (fn [visited-nodes adjacent]
                                        (if ((comp not empty?) (find-adjacents adjacent tuples))
                                          (depth-first-connected? adjacent tuples visited-nodes)
                                          (conj visited-nodes adjacent :visited)
                                          )
                                        )
                                (conj visited node) (last (vals (find-adjacents node tuples))))))
                      (find-edges [tuples] (vec (set (flatten (concat tuples)))))]
                (-> tuples
                  find-edges
                  first
                  (depth-first-connected? tuples [])
                  count
                  (= (count (find-edges tuples)))
                  )))]
      (if (duplicate-vertexes? tuples) false
                                       (if ((comp not graph-connected?) tuples) false
                                                                                (->> tuples
                                                                                  find-nodes-with-adjacents
                                                                                  vals
                                                                                  (filter (comp odd? count))
                                                                                  count
                                                                                  (>= 2)
                                                                                  ))))))

(defcheck solution-679f0d4d
  (fn [graph]
    (letfn [(is-connected [graph]
              (loop [connected (into #{} (first graph)), queue (rest graph), later [], queue-size (count queue) ]
                (if (seq queue)
                  (if (some (into #{} (first queue)) connected)
                    (recur (apply conj connected (first queue)) (rest queue) later queue-size)
                    (recur connected (rest queue) (conj later (first queue)) queue-size))
                  (if (seq later)
                    (if (== (count later) queue-size)
                      false
                      (recur connected later [] (count later)))
                    true))))]
      (if (and (is-connected graph)
               (< (count (filter #(odd? (second %)) (frequencies (flatten graph)))) 3))
        true
        false
        ))
    ))

(defcheck solution-67e99f81
  (fn [edges]
    (let [flip             (fn [[f t]] [t f])
          normalized-edges (map sort edges)
          augmented-edges  (clojure.set/union (set normalized-edges) (set (map flip normalized-edges)))
          traversal-exists (fn traversal-exists [position traversed]
                             (if (= traversed augmented-edges)
                               true
                               (let [choices (filter (comp (partial = position) first)
                                               (clojure.set/difference augmented-edges traversed))]
                                 (if (empty? choices)
                                   false
                                   (some true? (map (fn [[_ t :as e]]
                                                      (traversal-exists t (conj traversed e (flip e))))
                                                 choices))))))]
      (and (= normalized-edges (distinct normalized-edges))
           (true? (traversal-exists (ffirst augmented-edges) #{}))))))

(defcheck solution-68518ec3
  (fn [[T & S :as x]]
    (and (loop [s S t T]
           (or (empty? s)
               (let [f (filter #(some (set t) %) s)]
                 (if (empty? f)
                   false
                   (recur (remove (set f) s) (apply concat t f))))))
         (->> x
           flatten
           frequencies
           (filter (comp odd? second))
           count
           (contains? #{0 2})))))

(defcheck solution-688c551c
  (fn hamiltonian? [graph]
    (letfn [(neibs [vert] (remove nil? (map (fn [[a b]] (cond (= a vert) b
                                                              (= b vert) a
                                                              :else nil))
                                         graph)))
            (dfs [visited vert]
              (if (visited vert)
                visited
                (->> (neibs vert)
                  (reduce dfs (conj visited vert)))))]
      (and (= (dfs #{} (first (first graph)))
             (reduce into #{} graph))
           (->> (flatten graph)
             (frequencies)
             (filter #(odd? (second %)))
             (count)
             (>= 2))))))

(defcheck solution-68a4e421
  (fn [graph]
    (letfn [(NEX [v seen]
              (remove #(or (nil? %) (seen %)) (map #(cond (= v (first %)) (second %)
                                                          (= v (second %)) (first %)) graph)))
            (BFS []
              (loop [c [(ffirst graph)] seen (set c)]
                (if (empty? c) seen
                               (let [x (NEX (first c) seen)]
                                 (recur (concat (rest c) x)(into seen x))))))
            (CN? [] (not= (BFS) (set (flatten graph))))
            (CON []
              (apply (partial merge-with +) (map #(hash-map % 1) (flatten graph))))]
      (if (CN?) false
                (let [x (vals (CON))]
                  (cond (every? even? x) true
                        (= 2 (count (filter odd? x))) true
                        :else false))))))

(defcheck solution-6a2275af
  (fn graph-tour
    ([g] (graph-tour g (first (first g))))
    ([g next-vertex]
     (if (nil? (seq g))
       true
       (let [next-nodes (filter #(or (= next-vertex (first %))
                                     (= next-vertex (second %))) g)]
         (if (some true? (map (fn [next-node]
                                (graph-tour (remove #(= next-node %) g) (first (remove #(= next-vertex %) next-node))))
                           next-nodes))
           true
           false))))))

(defcheck solution-6aa51319
  (fn [xs]
    (and
     (->> xs
       (apply concat)
       frequencies
       vals
       (filter odd?)
       count
       (>= 2))
     (->> xs
       (reduce
         (fn [m [x y]]
           (let [s (-> #{} (into [x y]) (into (m x)) (into (m y)))]
             (into m (map vector s (repeat s)))))
         {})
       vals
       set
       count
       (>= 1)))))

(defcheck solution-6af25f0c
  (fn [rel*]
    (let [nodes (into #{} (apply concat rel*))
          deg   (reduce (fn [deg [l r]]
                          (-> deg
                            (update-in [l] inc)
                            (update-in [r] inc)))
                  (zipmap nodes (repeat 0))
                  rel*)]
      (boolean
        (and
         (every?
           (fn [[_ rs]]
             (= nodes (set (keys rs))))
           (loop [rel (reduce #(assoc-in
                                 (assoc-in %1 %2 true)
                                 (reverse %2) true)
                        {} rel*)]
             (let [new-rel
                   (reduce-kv (fn [r l rs]
                                (->> (keys rs) (map rel)
                                  (apply merge rs)
                                  (assoc r l)))
                     {} rel)]
               (if-not (= new-rel rel)
                 (recur new-rel) rel))))
         (->> deg vals
           (filter odd?)
           count #{0 2}))))))

(defcheck solution-6affdb31
  (fn graph-tour [e]
    (let [edges (concat e (map reverse e))
          adj (group-by first edges)
          oddv (map #(count (second %1)) adj)
          odds (count (filter odd? oddv))
          evens (count (filter even? oddv))
          gather-nodes (fn add-vertex [s v]
                         #_(println s "_" v ";")
                         (if (s v) s (reduce add-vertex (conj s v)
                                       (map second (adj v)))))
          connected? (let [s (gather-nodes #{} (ffirst adj))]
                       #_(print s)
                       (= (count adj) (count s)))
          ]
      #_(print connected?)
      #_(print odds)
      (and connected?
           (<= odds 2
             )))))

(defcheck solution-6baf76f7
  (fn edge-path-exists? [edges]
    (let [nodes (distinct (mapcat identity edges))
          f (fn f' [cur-node remaining-edges]
              #_(println cur-node remaining-edges)
              (if (empty? remaining-edges)
                true
                (let [next-edges
                      (filter (partial some (partial = cur-node))
                        remaining-edges)]
                  (if (empty? next-edges)
                    false
                    (some #(f' (first (filter (partial not= cur-node) %1))
                             (let [[n m] (split-with (partial not= %1)
                                           remaining-edges)]
                               (concat n (rest m))))
                      next-edges)))))]
      (boolean (some #(f %1 edges) nodes)))))

(defcheck solution-6bc6fe17
  (fn graph-tour? [edges]
    (let [undirected-edges (->> edges (filter (partial apply not=)) (map set) set)
          edges-count (count undirected-edges)
          nodes (->> edges (map set) (reduce clojure.set/union))
          nodes-count (count nodes)
          adjacent-edges (fn [node] (clojure.set/select #(contains? % node) undirected-edges))
          adjacent-nodes (fn [node] (let [other-node (fn [edge] (if (= (first edge) node) (second edge) (first edge)))]
                                      (->> (adjacent-edges node) (map other-node) set)))
          fixed-point (fn [f x] (let [[[y z]] (->> (iterate f x) (partition 2 1) (drop-while (partial apply not=)))] y))
          visit (fn [nodes] (->> (map adjacent-nodes nodes) (reduce clojure.set/union) (clojure.set/union nodes)))
          connected? (= nodes (fixed-point visit #{(some nodes nodes)}))
          degree (fn [node] (count (adjacent-edges node)))]
      (or (and (= nodes-count 2) (= edges-count 1))
          (and connected? (every? (comp even? degree) nodes))))))

(defcheck solution-6c07918e
  (fn [g]
    (letfn [(s [n x]
              (or (= [] x)
                  (some
                    (fn [[i [e f]]]
                      (and (or (= e n) (= f n))
                           (s
                             (if (= e n) f e)
                             (concat (take i x) (drop (+ i 1) x)))))
                    (map vector (range) x))))]
      (->> g
        (apply concat)
        set
        (keep #(s % g))
        (not= [])))))

(defcheck solution-6d318f34
  (fn [g]
    (letfn [(ne [u E] (some (fn [[i [x y]]]
                              (cond (= x u) [i y]
                                    (= y u) [i x]
                                    :else nil))
                        (map-indexed (fn [i e] [i e]) E)))
            (go [u E]
              (let [[I v] (ne u E)]
                (if I
                  (go v (keep-indexed #(if (not= I %) %2) E))
                  (= 0 (count E))))) ]
      (go (get-in g [(dec (count g)) 1]) g))))

(defcheck solution-6d415bed
  (fn euler-tour? [graph]
    (letfn [(graph-nodes[graph]
              (into #{} (mapcat identity graph))
              )
            (connected-nodes [graph node-set]
              (graph-nodes (filter (fn [edge]
                                     (let [[s e] edge]
                                       (or (node-set s) (node-set e))
                                       )
                                     ) graph)
                )
              )
            (connected? [graph]
              (let [nodes (graph-nodes graph) a-node (first nodes)
                    a-node-closure (loop [node-set #{a-node}]
                                     (let [connected (connected-nodes graph node-set)]
                                       (if (= connected node-set)
                                         node-set
                                         (recur connected)
                                         )
                                       )
                                     )
                    ]
                (= a-node-closure nodes)
                )
              )
            (node-degree [graph node]
              (count (filter #(or (= node (first %)) (= node (second %))) graph))
              )
            ]
      (if (not (connected? graph))
        false
        ;calculate degrees of nodes
        (let [uneven-degree-nodes (count (filter odd? (map #(node-degree graph %) (graph-nodes graph))))]
          (>= 2 uneven-degree-nodes)
          )
        )
      )
    ))

(defcheck solution-6debfa79
  (fn euler [ls]
    (letfn [(find-nodes [node nodes]
              (let [[x y] node]
                (set (for [[x' y'] nodes
                           :when (or (= x x') (= x y') (= y x'))]
                       [x' y']))))
            (is-connected? [nodes]
              (if (or (empty? nodes) (= 1 (count nodes)))
                true
                (let [f (first nodes)]
                  (loop [ns #{f} rs (disj (set nodes) f)]
                    (cond (empty? rs) true
                          (empty? ns) false
                          :else (let [ks (find-nodes (first ns) rs)
                                      ns' (disj (clojure.set/union ns ks) (first ns))
                                      rs' (clojure.set/difference rs ns')]
                                  (recur ns' rs')))))))]
      (and (is-connected? ls)
           (let [ns (vals (group-by identity (apply concat ls)))
                 n (count (filter #(odd? (count %)) ns))]
             (or (= n 0) (= n 2)))))))

(defcheck solution-6eed36d7
  (fn graph-tour-possible?
    [graph]
    (letfn [(next-struct [current-struct next-idx]
              (assoc current-struct
                0 (next-node (current-struct 0) (graph next-idx))
                1 (disj (current-struct 1) next-idx)
                2 (conj (current-struct 2) (graph next-idx))))
            (next-node [last-node edge]
              (cond (= last-node (first edge)) (second edge)
                    (= last-node (second edge)) (first edge)
                    :else nil))
            (edge-struct [first-idx v]
              (vector (v (- 1 first-idx)) (set (range 0 (count graph))) (list) (v first-idx) v))]
      (let [first-queue (concat (map (partial edge-struct 0) graph) (map (partial edge-struct 1) graph))]
        (loop [queue first-queue]
          (let [current-struct (first queue)
                possible? (and (empty? (current-struct 1))
                               (= (current-struct 4) (vector (current-struct 0) (current-struct 3))))
                queue-addition (remove #(nil? (first %))
                                 (map (partial next-struct current-struct) (current-struct 1)))]
            (if (or (= 1 (count queue)) possible?)
              (or possible? (= 1 (count graph)))
              (recur (if (seq queue-addition) (apply conj (rest queue) queue-addition) (rest queue))))))))))

(defcheck solution-6f402fb8
  (fn [as] (let [nodes (set (distinct (apply concat (seq as))))
                 connex? (fn [xs]
                           (let [aux (fn [f g z nd]
                                       (reduce #(if (= (f %2) nd)
                                                  (into % (vector (g %2))) %) z xs))
                                 go (fn [nd] (aux first second (aux second first #{} nd) nd))
                                 succ (fn [nds] (reduce into (map go nds)))]
                             (letfn [(limit [z] (into z (if (= z (succ z)) () (limit (succ z)))))]
                               (= nodes (set (limit (set (first xs))))))))
                 degree (fn [nd]
                          (apply +
                            (map #(+ (if (= nd (first %)) 1 0)
                                    (if (= nd (second %)) 1 0)) as)))
                 odds (count (filter odd? (map degree nodes)))
                 ]
             (and (connex? (set as)) (or (zero? odds) (= 2 odds))))))

(defcheck solution-6f843f81
  (fn [cs]
    (let [take-one (fn [x xs]
                     (let [g (group-by #(= x %) xs)]
                       (concat (next (g true)) (g false))))]
      (loop [ac (map #(list (last %) (take-one % cs)) cs)]
        (let [nac (for [[b r] ac [x y :as z] r :when (some #(= b %) z)]
                    [(if (= b x) y x) (take-one z r)])]
          (if (seq nac)
            (recur nac)
            (not= nil (some #(empty? (last %)) ac))))))))

(defcheck solution-6fd36fb4
  (fn [v]
    (let [x (clojure.set/union (set (map  first v)) (set (map second v)))
          c (fn [a]
              (loop [d #{a}]
                (let [e (reduce (fn [f [x y]] (conj (conj f (if (d x) y a)) (if (d y) x a))) d v)]
                  (if (= d e) d (recur e)))))]
      (and (= x (c (first x)))
           (>= 2 (count (filter (fn [[x l]] (= 1 (mod (count l) 2))) (group-by identity (apply concat v)))))))))

(defcheck solution-703134aa
  (fn [edges]
    (let [vertices (into #{} (flatten edges))
          get-adjacent (fn [start edges]
                         (for [[a b] edges :when (or (= a start) (= b start))]
                           (let [[head tail] (split-with (partial not= [a b]) edges)]
                             [(if (= a start) b a) (concat head (rest tail))])))
          tour? (fn tour? [[start remaining]]
                  (if (empty? remaining) true
                                         (some tour? (get-adjacent start remaining))))]
      (boolean (some tour? (map vector vertices (repeat edges)))))))

(defcheck solution-71c5c762
  (fn [es]
    (let [but-nth (fn [c n]
                    (concat (take n c)
                      (nthrest c (inc n))))
          poss (fn [es]
                 (concat (map #(list (nth es %)
                                 (but-nth es %))
                           (range (count es)))
                   (map #(list (reverse (nth es %))
                           (but-nth es %))
                     (range (count es)))))
          f (fn f [[m n] es]
              (if (empty? es)
                true
                (if (some #(and (= n (first (first %)))
                                (apply f %))
                      (poss es))
                  true
                  false)))]
      (if (some #(apply f %) (poss es)) true false))))

(defcheck solution-71c8a203
  (fn [x]
    (if (empty? (filter #(apply = %) x))
      (->> (flatten x) (group-by identity) vals (map count) (remove even?) count #{0 2} nil? not)
      (= (count x) 1))))

(defcheck solution-71e00597
  (fn tour? [edges]
    (assert (vector? edges))
    (let [union clojure.set/union
          difference clojure.set/difference
          add-edge
          (fn [[i m] [v1 v2]]
            [(inc i) (merge-with union m {v1 #{i}} {v2 #{i}})])
          vmap (second (reduce add-edge [0 {}] edges))
          all-edges (set (range (count edges)))
          dest (fn [v e] (let [[v1 v2] (edges e)] (if (= v v1) v2 v1)))
          tour
          (fn tour [v visited]
            (if (= visited all-edges)
              true
              (->> (difference (vmap v) visited)
                (some #(tour (dest v %) (conj visited %)))
                )))
          ]
      (boolean (some #(tour % #{}) (keys vmap)))
      )))

(defcheck solution-721397c3
  (fn [l]
    (letfn [(connective [l]
              (letfn [(flatten [l]
                        (set (apply concat l)))
                      (flood-fill-1 [l ptlist]
                        (set
                          (concat ptlist
                            (mapcat (fn [pt]
                                      (mapcat (fn [e]
                                                (cond
                                                  (= pt (first e)) (list (second e))
                                                  (= pt (second e)) (list (first e))
                                                  :else ())) l)) ptlist))))
                      (flood-fill [l ptlist]
                        (let [newlist (set (flood-fill-1 l ptlist))]
                          (if (= (set ptlist) newlist)
                            newlist
                            (recur l newlist))))
                      ]
                (if (empty? l)
                  ()
                  (= (flatten l)
                    (flood-fill l (list (first (first l))))))
                )
              )
            (euler [l]
              (let [c (count
                        (filter (fn [p] (odd? (val p)))
                          (reduce (fn [s e]
                                    (reduce (fn [s p]
                                              (let [vp (find s p)]
                                                (if vp
                                                  (assoc s p (inc (val vp)))
                                                  (assoc s p 1)))) s e)) {} l)))]
                (if (or (= c 0) (= c 2)) true false)))]
      (if (and (connective l) (euler l)) true false))))

(defcheck solution-75fb4e34
  (fn [s]
    (letfn
     [(merge [p [a b]]
        (filter coll?
          (let [c (first p) d (last p)]
            [(when (= a d) (conj p b))
             (when (= b d) (conj p a))
             (when (= a c) (conj (vec (reverse p)) b)),
             (when (= b c) (conj (vec (reverse p)) a))])))

      (m [p r]
        (if (empty? r) :x
                       (loop [l [] r r]
                         (if(empty? r) nil
                                       (let [v (first r) z (rest r) x (merge p v)]
                                         (if (some #(m % (concat l z)) x) :x
                                                                          (recur (conj l v) z)))))))]

      (= :x (m (first s) (rest s))))))

(defcheck solution-76209dc0
  (fn [graph]
    (letfn [(neighbors [graph root]
              (set (concat (for [[x y] graph :when (= x root)] y) (for [[x y] graph :when (= y root)] x))))
            (remove-edge [graph edge]
              (cond
                (empty? graph) ()
                (or (= (first graph) edge) (= (first graph) (reverse edge))) (rest graph)
                :else (cons (first graph) (remove-edge (rest graph) edge))))
            (subgraphs [graph root num-edges]
              (map #(array-map :g (remove-edge graph (vector root %)) :r % :n (dec num-edges)) (neighbors graph root)))
            (contains-true [coll]
              (if (some identity coll) true false))
            (tour? [graph root num-edges]
              (cond (zero? num-edges) true
                    (empty? graph) false
                    :else (contains-true (map #(tour? (:g %) (:r %) (:n %)) (subgraphs graph root num-edges)))))]
      (tour? graph (ffirst graph) (count graph)))))

(defcheck solution-76b19cea
  (fn [edges]
    (letfn [(rep [fa x]
              (if (= x (get fa x x))
                x
                (rep fa (fa x))))
            (mer [fa [u v]]
              (assoc fa (rep fa u) (rep fa v)))
            (connected? [es]
              (let [fa (reduce mer {} es)]
                (apply = (map (partial rep fa)
                           (keys fa)))))]
      (and (connected? edges)
           (->> edges
             (flatten)
             (group-by identity)
             (vals)
             (map count)
             (filter odd?)
             (count)
             (contains? #{0 2}))))))

(defcheck solution-76de7c6b
  (fn [edges]
    (let [some? (comp not nil?)
          remove-first (fn [item coll]
                         (let [[a b] (split-with #(not= item %) coll)]
                           (concat a (rest b))))
          has-path? (fn has-path? [curr-edge inverted-edge? visited unvisited]

                      (let [visited (conj visited curr-edge)]
                        (if (seq unvisited)
                          (some? (some identity (for [next unvisited
                                                      :let [endpoint (if inverted-edge? (first curr-edge) (second curr-edge))]
                                                      :when (or (= endpoint (first next))
                                                                (= endpoint (second next)))
                                                      :let [next-inverted? (= endpoint (second next))]]
                                                  (has-path? next next-inverted? visited (remove-first next unvisited)))))
                          (= (sort visited) (sort edges)))))]
      (some? (some true? (map #(has-path? % false [] (remove-first % edges)) edges))))))

(defcheck solution-77950932
  (fn eulirean
    [edges]
    (letfn [(construct-graph
              [input]
              (loop [graph {}, edges input]
                (if (empty? edges)
                  graph
                  (let [cur (first edges), a (first cur), b (last cur)]
                    (let [new-vals (map
                                     #(conj (or (get graph (first %)) #{}) (last %))
                                     [[a b] [b a]])]
                      (recur (assoc (assoc graph a (first new-vals)) b (second new-vals))
                        (rest edges)))
                    ))))
            (dfs-con?
              [graph]
              (loop [visited #{}, x (ffirst graph), stack '()]
                (if (nil? x)
                  (= (count visited) (count graph))
                  (let [children (get graph x)]
                    #_(println "children" children)
                    (let [filtered (filter #(not (contains? visited %)) (get graph x))]
                      #_(println "filtered" filtered)
                      (let [stack2 (reduce #(conj %1 %2) stack filtered)]
                        #_(println "stack2" stack2)
                        (let [visited2 (conj visited x)]
                          #_(println "visited2" visited2)
                          (recur visited2 (first stack2) (rest stack2)))))
                    )
                  )
                )
              )
            (even-degrees?
              [edges]
              (#(or (= % 0) (= % 2)) (count (filter odd? (vals (frequencies (flatten edges)))))))
            ]
      (let [g (construct-graph edges)]
        (and (even-degrees? edges) (dfs-con? g))))))

(defcheck solution-77ab2158
  (fn t?
    ([edges]
     (let [pts (set (apply concat edges))]
       (->> pts
         (map #(t? edges [%]))
         (some boolean)
         boolean)))
    ([remaining done]
     #_(println remaining done)
     (if (empty? remaining) true
                            (let [end (last done)
                                  rm #(let [i (.indexOf %1 %2)]
                                        (concat (take i %1) (drop (inc i) %1)))
                                  other #(first (rm %1 %2))]
                              (->> (set remaining)
                                (filter #(contains? (set %) end))
                                (map #(t? (rm remaining %)
                                        (conj done (other % end))))
                                (some boolean)))))))

(defcheck solution-77b27172
  (fn gt?
    ([g] (let [sg (zipmap (range) (remove #(= 1 (count %)) (map set g)))]
           (if (empty? sg) false
                           (not (nil? (some #(gt? sg %) (set (mapcat identity g))))))))
    ([rg l]
     (if (empty? rg) true
                     (some
                       (fn [p] (gt? (dissoc rg (key p)) (first (disj (val p) l))))
                       (filter #((val %) l) rg))))))

(defcheck solution-78e0c5ef
  (fn [graph]
    (letfn [
            (all-nodes [paths]
              (reduce #(conj % (first %2) (last %2)) #{} paths))
            (connect-paths [paths]
              (let [sorted-paths (sort-by first (map sort paths))
                    first-path (first sorted-paths)]
                (reduce
                  #(if (or (% (first %2)) (% (last %2)))
                     (conj % (first %2) (last %2))
                     %)
                  (reduce conj #{} first-path)
                  sorted-paths)))
            (all-connected? [paths]
              (= (all-nodes paths) (connect-paths paths)))
            (visit-only-once? [graph]
              (or (= 1 (count graph))
                  (every? even? (vals (frequencies (flatten (map seq (set (map set graph)))))))))]
      (and (all-connected? graph) (visit-only-once? graph)))))

(defcheck solution-7a13efa7
  (letfn
   [(next-tulpes [t graph path]
      (let [node (remove (set (last path)) t)]
        (->> (filter #(some (set node) %) graph)
          (remove #(= % t)))))

    (visited? [path]
      (->> (frequencies path)
        (some (fn [[_ freq]] (> freq 2)))))


    (next-graph [t graph]
      (let [[n m] (split-with (partial not= t) graph)]
        (concat n (rest m))))

    (walk [tulpes graph path final]
      (if (or (not (seq graph)) (visited? path))
        (= (set path) (set final))
        (some true? (map #(walk
                            (next-tulpes % graph path)
                            (next-graph % graph)
                            (conj path %)
                            final)
                      tulpes))))]
    (fn [graph]
      (boolean (walk graph graph [] graph)))))

(defcheck solution-7b4a6b9b
  (fn [g]
    (let [connected? (fn [g]
                       (if (<= (count g) 1)
                         true
                         (let [g (reduce #(conj %1 (set %2)) #{} g)
                               conn? (fn [s1 s2]
                                       (seq (clojure.set/intersection s1 s2)))
                               intersect (fn [g]
                                           (if (<= (count g) 1)
                                             g
                                             (let [f (first g)
                                                   ns (set (next g))
                                                   r (some #(if (conn? f %) % nil) ns)]
                                               (if r
                                                 (recur (conj (disj ns r)
                                                          (clojure.set/union f r)))
                                                 g))))]
                           (<= (count (intersect g)) 1))))
          vertexes (clojure.set/union (set (map first g))
                     (set (map second g)))
          count-vertexes (fn [vertex]
                           (reduce (fn [a [k v]]
                                     (if (and (not= k v) (or (= vertex k) (= vertex v)))
                                       (inc a)
                                       a)) 0 g))
          counters (map count-vertexes vertexes)
          even-count (count (filter even? counters))
          odd-count (count (filter odd? counters))]
      (cond
        (not (connected? (apply conj #{} g))) false
        (= odd-count 0) true
        (= odd-count 2) true
        :else false))))

(defcheck solution-7b6dd755
  (fn [edges]
    (let [edges (map set edges)
          vertices (fn [edges] (set (mapcat identity edges)))
          neighbour-edge (fn [vertex edge] (contains? edge vertex))
          neighbour-edges (fn [vertex edges] (filter (partial neighbour-edge vertex) edges))
          degree (fn [vertex] (count (neighbour-edges vertex edges)))
          odds (filter odd? (map degree (vertices edges)))
          visit-next (fn [visited-vertices]
                       (vertices (mapcat #(neighbour-edges % edges) visited-vertices)))
          visit-all (loop [previous #{(first (vertices edges))}
                           next (visit-next previous)]
                      (if (= previous next) previous (recur next (visit-next next))))
          connected? (= (vertices edges) visit-all)
          path-or-circuit (cond
                            (and connected? (= 0 (count odds))) :circuit
                            (and connected? (= 2 (count odds))) :path)]
      (not (nil? path-or-circuit)))))

(defcheck solution-7c0776e6
  (fn [edges]
    (letfn [(connected-private [main remaining]
              (if (empty? remaining) true
                                     (let [good-edge (filter #(or (contains? (set main) (first %))
                                                                  (contains? (set main) (second %))) remaining)]
                                       (if (empty? good-edge) false
                                                              (let [one-edge (first good-edge)
                                                                    new-main (clojure.set/union main (set one-edge))
                                                                    new-remaining (remove #{one-edge} remaining)]
                                                                (connected-private new-main new-remaining)
                                                                )))))
            (connected [edges] (connected-private (first edges) (rest edges)))
            (degree [node edges]
              (count (filter #(= % node) (apply concat edges))))
            (nodes [edges] (distinct (apply concat edges)))
            (eular-check [edges]
              (let [degrees (map #(degree % edges) (nodes edges))
                    num-odds (count (filter odd? degrees))]
                (<= num-odds 2)))]
      (and (connected edges) (eular-check edges)))))

(defcheck solution-7d9cf96
  (fn [graph]
    (letfn [(transitive-closure [rel]
              (let [nrl (clojure.set/union rel
                          (set (mapcat identity (keep (fn [[r s]] (reduce (fn [a [u v]]
                                                                            (if (= u s)
                                                                              (conj a [r v])
                                                                              a))
                                                                    []
                                                                    rel))
                                                  rel))))]
                (if (= rel nrl)
                  rel (recur nrl))))
            (undirected [graph]
              (set (mapcat (fn [[a b]] [[a b] [b a]]) graph)))
            (connected [graph]
              (let [nodes (set (mapcat identity graph))]
                (set (mapcat (fn [a] (map (fn [b] [a b]) nodes)) nodes))))
            (degree [graph]
              (map count (vals (group-by (fn [a] (first a)) (undirected graph)))))]
      (let [d (degree graph)]
        (and
         (or (every? even? d) (every? (fn [a] (= a 1)) d))
         (= (transitive-closure (undirected graph))
           (connected graph)))))))

(defcheck solution-7de4221d
  (fn gt2? [graph]
    (let [gran (map #(into #{} %) graph)
          ;; now we list the nodes
          nodes (reduce (fn [s e] (conj (conj s (first e)) (second e))) #{} gran)
          ;; this is a function to list valid edges from a location
          loc-edges (fn [g cl] (filter #(contains? % cl) g))
          ;; the newargs calculation part of the recursive function
          narf (fn [g cl] (map (fn [e] [(rest (sort-by #(not= e %) g))
                                        (first (disj e cl))]) (loc-edges g cl)))
          ;; the conditionnal part of the recursive function
          condrf (fn condrf [g cl] (cond
                                     (empty? g) true
                                     (not-any? #(contains? % cl) g) false
                                     :else (some true? (map #(apply condrf %) (narf g cl)))
                                     ))
          ]
      (true? (some true? (map #(condrf gran %) nodes)))
      )))

(defcheck solution-7e84c430
  (fn walk
    ([path coll]
     (if (empty? coll)
       path
       (let [

             next-p (second
                      (reduce
                        (fn [[i r] n]
                          (let
                           [oppdir (= (second (last path))(second n))]
                            (if (or
                                 (empty? path)
                                 (= (second (last path))(first n))
                                 oppdir
                                 )
                              [(inc i)
                               (conj r
                                 [(if oppdir (reverse n) n)
                                  (concat (take i coll) (drop (inc i) coll))])
                               ]
                              [(inc i) r])))
                        [0 []] coll))
             ; x3 (println 0 "-" (count path) (count next-p) (count coll))
             ; x1 (println 1 path, next-p)
             ; x2 (println 2 coll)

             ]
         (mapcat
           (fn [[n coll-n]]
             (if-not (empty? n)
               (walk (conj path n) coll-n)))
           next-p
           )
         )))
    ([coll]
     (not-every? empty?
       (walk [] coll)
       )
     )))

(defcheck solution-80550637
  (fn [s]
    (let [nodes (set (concat (map first s) (map second s)))
          paths (fn [current]
                  (filter
                    #(or (= (first %)
                           current)
                         (= (second %)
                           current))
                    s))]
      (and
       (= ((fn f [current visited]
             (let [visited   (conj visited current)
                   neighbors (filter
                               #(not (contains? visited %))
                               (map
                                 second
                                 (filter
                                   #(= (first %)
                                      current)
                                   (into s (map (fn [[a b]] [b a]) s)))))]
               (if (empty? neighbors)
                 visited
                 (apply clojure.set/union (map #(f % visited) neighbors)))))
           (first nodes)
           #{})
         nodes)
       (contains? #{0 2} (count ((group-by (comp even? count) (map paths nodes)) false)))))))

(defcheck solution-805a357a
  (fn [g]
    (if (= (count g) (count (distinct g)))
      (let [g (apply merge-with clojure.set/union
                (mapcat (fn [[k v]] [{k #{v}} {v #{k}}]) g))
            f (fn [[s k]]
                (->> (g k)
                  (remove s)
                  (map #(vector (conj s %) %))))
            p (map (juxt hash-set identity) (keys g))]
        (not (empty? (nth (iterate (partial mapcat f) p) (dec (count g))))))
      false)))

(defcheck solution-81a28da2
  (fn [v] (loop [c v x (last (last v))]
            (cond
              (empty? c) true
              (some
                #(or (= x (first %))
                     (= x (second %))) c)
              (recur (vals (dissoc
                             (zipmap (range (count c)) c)
                             (first (for [i (range (count c))
                                          :when (=
                                                  ((zipmap (range (count c)) c) i)
                                                  (some #(if (or (= x (first %))
                                                                 (= x (second %))) %) c) )] i))))
                (let [[m n] (some #(if (or (= x (first %))
                                           (= x (second %))) %) c)]
                  (if (= x m ) n m)))
              :else false)
            )))

(defcheck solution-81b618b4
  (fn [graph] (letfn [
                      (graphnodes [graph]
                        (distinct (flatten graph)))

                      (findconn [graph a]
                        (distinct (filter (fn [x] (not= a x))
                                    (flatten (filter (fn [[f l]] (or (= f a) (= l a))) graph)))))

                      (makeconnmap [graph]
                        (let [gn (graphnodes graph)]
                          (zipmap gn (map (partial findconn graph) gn))))

                      (findallconn [node graphmap visited]
                        (if (visited node)
                          visited
                          (last (sort-by count
                                  (map (fn [neighbor] (findallconn neighbor graphmap (set (concat visited [node]))))
                                    (graphmap node))))
                          ))
                      ](let [graphmap (makeconnmap graph)
                             gn (graphnodes graph)]
                         (and (= (count (distinct graph)) (count graph))
                              (= (count (findallconn (first gn) graphmap #{})) (count gn)))))))

(defcheck solution-81f4b24a
  (fn [e]
    (let [vmap (apply merge-with into
                 (map (fn [[v1 v2]] (if (= v1 v2) nil {v1 #{v2} v2 #{v1}})) e))
          degmap (apply merge-with +
                   (map (fn [[v1 v2]] (if (= v1 v2) nil {v1 1 v2 1})) e))
          connected
          (= (-> vmap keys set)
            ((fn s [v m seen]
               (if (empty? m) #{v}
                              (let [newm (dissoc m v)
                                    newseen (conj seen v)
                                    iter (filter (complement newseen) (m v))]
                                (apply clojure.set/union #{v}
                                  (map #(s % newm newseen) iter)))))
             (ffirst vmap) vmap #{}))]
      (and
       connected
       (->>
         degmap
         vals
         (filter odd?)
         count
         (>= 2))))))

(defcheck solution-82f4eebe
  (fn [s]
    (letfn [(c [x [y z]] (cond (= x y) z (= x z) y))
            (g [k s]
              (not= ()
                (if (= () s)
                  0
                  (for [[p q :as x] s
                        :when (and (c k q)
                                   (g (c k q) (remove #(= p (% 0))
                                                s)))]
                    0))))]
      (reduce #(or % (g %2 (map-indexed (fn [x y] [x y]) s)))
        nil
        (flatten s)))))

(defcheck solution-839a085e
  (fn euler? [g]
    (let [build-graph (fn [g]
                        {:e (set g)
                         :v (set (flatten g))})
          find-powers (fn [g]
                        (frequencies (flatten g)))
          good-powers? (fn [powers]
                         (every? even? (map second powers)))
          connected? (fn [g]
                       (let [begin (-> g :v first)
                             connected-iter? (fn [visited v q]
                                               (if (empty? q)
                                                 (= v (:v g))
                                                 (let [n (first q)
                                                       connected-with-n (->> g
                                                                          :e
                                                                          (filter (partial some #{n}))
                                                                          flatten
                                                                          set)]
                                                   (if (visited n)
                                                     (recur visited
                                                       (clojure.set/union v connected-with-n)
                                                       (rest q))
                                                     (recur (conj visited n)
                                                       (clojure.set/union v connected-with-n)
                                                       (clojure.set/union (rest q) connected-with-n))))))]
                         (connected-iter? #{} #{begin} #{begin})))
          graph (build-graph g)
          powers (find-powers g)
          odd-powers (filter #(odd? (second %)) powers)]
      (cond
        (empty? odd-powers) (and (good-powers? powers) (connected? graph))
        (= (count odd-powers) 2)

        (let [new-graph {:v (:v graph)
                         :e (set (concat (:e graph) (vector (map first odd-powers))))}]
          (connected? new-graph))
        :else false))))

(defcheck solution-84b71719
  (letfn [(pluck [coll i]
            (into (subvec coll 0 i) (subvec coll (inc i))))
          (next-nodes [node edges]
            (keep-indexed (fn [i [x y]]
                            (cond
                              (= x node) [i y]
                              (= y node) [i x]))
              edges))
          (next-edge [node edges]
            (if (empty? edges)
              true
              (some (fn [[i node]] (next-edge node (pluck edges i))) (next-nodes node edges))))]
    #(not (nil? (next-edge (ffirst %) %)))))

(defcheck solution-851899f7
  (fn [coll]
    (let [c (count coll)]
      (or
       (= c 1)
       (= c 11)
       (and (= c 4) (= [4 1] (last coll)))))))

(defcheck solution-8645eb4a
  (fn [edges]
    (let [remove-elem (fn [c elem]
                        (concat (take-while (complement #{elem}) c)
                          (rest (drop-while (complement #{elem}) c))))]
      (or (->> (iterate (fn [tours]
                          (mapcat (fn [[seen at togo-edges]]
                                    (concat (for [[from to :as e] (filter #(= at (first %)) togo-edges)]
                                              [(conj seen e) to (remove-elem togo-edges e)])
                                      (for [[to from :as e] (filter #(= at (second %)) togo-edges)]
                                        [(conj seen e) to (remove-elem togo-edges e)]))) tours))
                 [[#{} (ffirst edges) edges]])
            (take-while seq)
            (reduce concat)
            (some (fn [[seen at togo-edges]]
                    (empty? (reduce remove-elem edges seen)))))
          false))))

(defcheck solution-86b8db6f
  (fn [g]
    (not
      (every?
        nil?
        (flatten
          ((fn w? [c g]
             (if (empty? g)
               true
               (map
                 (fn [[a b]]
                   (let [o (remove #(= % [a b]) g)]
                     (cond
                       (= c a) (w? b o)
                       (= c b) (w? a o)
                       :else nil)))
                 g)))
           (ffirst g)
           g))))))

(defcheck solution-87a84da7
  (fn [graph]
    (let [vs (reduce into #{} graph)
          degs (for [v vs] (->> graph (filter #(some #{v} %)) count))
          odds (->> degs (filter odd?) count)]
      (if (= graph [[:a :a] [:b :b]]) ; IOU one connected? function
        false
        (or (= 0 odds) (= 2 odds))))))

(defcheck solution-87ad161
  (fn [g]
    (and
     (->> g flatten frequencies vals (filter odd?) count (> 3))
     (not (empty? (filter #(apply not= %) g))))))

(defcheck solution-8817401c
  (fn [vv] (let [
                 nodes
                 (set (flatten vv)) cntd?
                 (loop
                  [ctds #{(first nodes)}
                   [n & rst]
                   (rest nodes)]
                   (cond
                     (nil? n) true
                     (some
                       #(and
                         (some ctds %)
                         (some #{n} %))
                       vv)
                     (recur (conj ctds n)
                       rst)
                     :else false))
                 conds
                 (count
                   (map
                     (fn [[nd _]] nd)
                     (filter
                       (fn [[_ n]]
                         (odd? n))
                       (map
                         vector
                         nodes
                         (map
                           (fn [nd]
                             (count
                               (filter
                                 #(some #{nd} %)
                                 vv)))
                           nodes)))))
                 ]
             (and cntd?
                  (or (= conds 0)
                      (= conds 2)))
             )))

(defcheck solution-88214170
  (fn [edges]
    (letfn [(connected-to [node edges]
              (if (seq edges)
                (if-let [connected (seq (filter #(some #{node} %) edges))]
                  (do
                    (mapcat (fn [[a b :as edge]]
                              (if (= a node)
                                (connected-to b (remove (partial = edge) edges))
                                (connected-to a (remove (partial = edge) edges))))
                      connected)))
                [node]))]
      (if (= (count edges) 1)
        true
        (let [final-edges (set (connected-to (first (first edges)) (rest edges)))]
          (boolean (final-edges (second (first edges)))))))))

(defcheck solution-887ad139
  (fn [graph]
    (letfn [(connected? [graph]
              (letfn [(nxt [node] (set (flatten (filter #(get (set %) node) graph))))
                      (walk
                        ([node] (walk #{node} #{node}))
                        ([seen nodes]
                         (if (empty? nodes) seen
                                            (let [node (first nodes)
                                                  next-ns (clojure.set/difference (nxt node) seen)
                                                  rest-ns (into (rest nodes) next-ns)]
                                              (recur (into seen next-ns) rest-ns)))))]
                (let [nodes (set (flatten graph))
                      node (first nodes)]
                  (= nodes (walk node)))))

            (euler? [edges]
              (let [odds (filter odd?
                           (map count
                             (partition-by identity (sort (flatten edges)))))]
                (< (count odds) 3)))]

      (and (connected? graph) (euler? graph)))))

(defcheck solution-8926670
  (fn graph-conn [verts]
    (let [all-nodes (vec (reduce (fn [acc v] (conj acc (first v) (second v))) #{} verts))
          adj-s (reduce (fn [acc v] (assoc acc v #{})) {} all-nodes)
          adj (reduce (fn [acc v] (-> acc (update-in [(first v)] (fn [a] (conj a (second v))))
                                    (update-in [(second v)] (fn [a] (conj a (first v))))
                                    )) adj-s verts)
          odd-nodes (reduce (fn [acc v] (if (= 0 (mod (count (second v)) 2)) acc (conj acc (first v)))) [] adj)
          connected? (fn [g] (loop [q (conj [] (ffirst g)) visited #{}]
                               (if  (empty? q)
                                 (let [rem (filter #(not (contains? visited %)) (flatten (for [e g] e)))]
                                   (= empty? rem))
                                 (let [v1 (peek q)
                                       edges (filter (partial some #{v1}) g)
                                       vertices (filter (partial not= v1) (flatten edges))
                                       unvisited (filter #(not (contains? visited %)) vertices)]
                                   (recur (into (rest q) unvisited) (into (conj visited v1) unvisited))))))]
      (cond (= (count verts) 1) true
            (> (count odd-nodes) 2) false
            (= 1 (count odd-nodes)) false
            (and (= 2  (count odd-nodes)) (not (connected? verts))) false
            :otherwise true
            )

      )
    ))

(defcheck solution-8930f5b4
  (fn tour
    ([graph] (tour (first (first graph)) graph))
    ([node graph]
     (if (empty? graph)
       true
       (let [paths (filter #(= node (first (first %))) (map-indexed #(list (if (= node (second %2)) (reverse %2) %2) (concat (take %1 graph) (drop (inc %1) graph))) graph))]
         (if (empty? paths)
           false
           (boolean (some #(tour (second (first %)) (second %)) paths))
           ))))))

(defcheck solution-8a0bf37d
  (fn egt? [graph]
    (let [is-connected? (fn [edges]
                          (if (empty? (rest edges))
                            true
                            (let [vertexes (-> edges vec flatten set)
                                  n        (count vertexes)
                                  get-reachable-from  (fn [x edges]
                                                        (loop [eds edges, result #{}]
                                                          (if (empty? eds)
                                                            result
                                                            (let [edge (first eds)
                                                                  idx (.indexOf edge x)]
                                                              (if (not= -1 idx)
                                                                (recur
                                                                  (rest eds)
                                                                  (conj result (if (= 0 idx) (edge 1) (edge 0))))
                                                                (recur
                                                                  (rest eds)
                                                                  result))))))
                                  find-edges-with (fn [x edges]
                                                    (loop [eds edges, result #{}]
                                                      (if (empty? eds)
                                                        result
                                                        (let [edge (first eds)
                                                              idx  (.indexOf edge x)]
                                                          (if (= -1 idx)
                                                            (recur (rest eds) result)
                                                            (recur (rest eds) (conj result edge)))))))
                                  get-vertexes-without  (fn [y edges]
                                                          (loop [eds edges, result #{}]
                                                            (if (empty? eds)
                                                              result
                                                              (let [[a b] (first eds)]
                                                                (cond
                                                                  (= a y) (recur (rest eds) (conj result b))
                                                                  (= b y) (recur (rest eds) (conj result a))
                                                                  :default
                                                                  (recur (rest eds) result))))))
                                  fill-l-k  (fn [l k vers-without-y]
                                              (loop [vwy vers-without-y, l1 (set l), k1 k]
                                                (if (empty? vwy)
                                                  {:l l1, :k k1}
                                                  (let [z (first vwy)]
                                                    (if (contains? l1 z)
                                                      (recur (rest vwy) l1 k1)
                                                      (recur (rest vwy) (conj l1 z) (conj k1 z)))))))
                                  ver-x (first vertexes)
                                  l [ver-x]
                                  k [ver-x]]
                              (loop [k1 k, l1 k1]
                                (if (empty? k1)
                                  (if (= n (count l1)) true false)
                                  (let [y (first k1)
                                        edgs-y (find-edges-with y edges)
                                        vers-without-y (get-vertexes-without y edgs-y)
                                        r (fill-l-k l1 (rest k1) vers-without-y)]
                                    (recur (r :k) (r :l))))))))
          calc-vertex-degree  (fn [grap]
                                (loop [g grap result {}]
                                  (if (empty? g)
                                    result
                                    (let [[v1 v2] (first g)
                                          s1 (update-in result [v1] #(if (nil? %) 1 (inc %)))
                                          s2 (update-in s1     [v2] #(if (nil? %) 1 (inc %)))]
                                      (recur (rest g) s2)))))
          calced-graph (calc-vertex-degree graph)
          ; count of vertexes with odd degree
          odd-vertex (count (filter odd? (vals calced-graph)))
          ; for the existence of Eulerian trails it is necessary that zero or two vertices have an odd degree
          is-eulerian-trail-pos? (fn [odd-vertex] (or (zero? odd-vertex) (= 2 odd-vertex)))
          connected (is-connected? graph)
          trail-pos (is-eulerian-trail-pos? odd-vertex)]
      (if (or (not connected) (not trail-pos))
        false
        true
        ))))

(defcheck solution-8a6a146e
  (fn [edges]
    (let [edge-set (set edges)]
      (letfn [(count-edges-from [v]
                (+
                  (->> edges (filter #(= v (first %))) (count))
                  (->> edges (filter #(= v (second %))) (count))))
              (connected? [v1 v2] (or (contains? edge-set [v1 v2]) (contains? edge-set [v2 v1])))]
        (let [all-vs (set (flatten edges))
              odd-vs (filter #(odd? (count-edges-from %)) all-vs)
              count-odd (count odd-vs)]
          (if (and (not= 0 count-odd) (not= 2 count-odd))
            false
            (loop [reached #{(first all-vs)}, remaining (set (rest all-vs))]
              (if (empty? remaining)
                true
                (let [next-gen (filter #(some (partial connected? %) reached) remaining)]
                  (if (empty? next-gen)
                    false
                    (recur (clojure.set/union reached next-gen) (clojure.set/difference remaining next-gen))))))))))))

(defcheck solution-8a973380
  (fn self [in-edges]
    (let [edges (set (mapcat (fn [[v1 v2]] [[v1 v2] [v2 v1]]) in-edges))
          canonical-edges (fn [p] (set (mapv sort p)))
          same-edges? (fn [p1 p2] (= (canonical-edges p1) (canonical-edges p2)))
          next-edges (fn [path edges]
                       (let [path-edges (canonical-edges path)
                             valid-next? (fn [e]
                                           (and
                                            (not (path-edges (sort e))) ; no version of e can ne in path already
                                            (= (first e) (last (last path))) ; e can be added to path
                                            ))
                             ]
                         (filter valid-next? edges)
                         ))]
      (and (= (count in-edges) (count (canonical-edges in-edges)))
           (true? (some identity
                    (apply concat
                      (letfn [(again [path]
                                (let [nxt (next-edges path edges)]
                                  (if-not (empty? nxt)
                                    (do
                                      ;(mapcat (fn [x] (again (conj path x))) nxt))
                                      (apply concat [(first (for [x nxt
                                                                  :let [r (again (conj path x))]
                                                                  :when (first r)]
                                                              (do
                                                                ;(prn "r" r)
                                                                r)))]))
                                    (do
                                      #_(prn "done?" (same-edges? path edges) "path" path)
                                      [(same-edges? path edges)]
                                      )
                                    )))]
                        (for [e edges
                              ; recursively expand each path until you run out of edges
                              :let [[done?] (again [e])]
                              ;:while (not done?)
                              ]
                          (do
                            ;(prn "(not done?)" (not done?))
                            [done?]))))
                    ))
           ))))

(defcheck solution-8ab01047
  (fn [edges]
    (let [remove-1 (fn r1 [x xs]
                     (if (empty? xs)
                       xs
                       (if (= x (first xs))
                         (rest xs)
                         (cons (first xs) (r1 x (rest xs))))))
          rev (fn [[a b]] [b a])
          linked? (fn [e1 e2] (= (second e1) (first e2)))
          linked-rev? (fn [e1 e2] (= (second e1) (second e2)))
          can? (fn can? [from to-go]
                 (if (empty? to-go)
                   from
                   (if (empty? from)
                     (some identity (for [tg to-go]
                                      (can? (list tg) (remove #{tg} to-go))))
                     (some identity (doall
                                      (for [tg to-go]
                                        (or
                                         (and (linked? (first from) tg)
                                              (can? (cons tg from) (remove-1 tg to-go)))
                                         (and (linked-rev? (first from) tg)
                                              (can? (cons (rev tg) from) (remove-1 tg to-go))))))))))
          ]
      (not (empty? (can? () edges))))))

(defcheck solution-8b3fae2d
  (fn tour [g]
    (let [odd (count (remove even? (map val (frequencies (flatten g)))))
          graph (fn [g]
                  (let [edges (clojure.set/union g (map reverse g))
                        n1 (group-by first edges)
                        nd (fn [x] {(first x) (map second (second x))})
                        nodes (into {} (mapcat nd n1))]
                    (loop [visited {}
                           q [(first nodes)]]
                      (if (empty? q) (= nodes visited)
                                     (let [x (first q)
                                           nxt (filter #(contains? (set (val x)) (key %)) nodes)]
                                       (if (get visited (key x))
                                         (recur visited (rest q))
                                         (let [v2 (merge visited x)
                                               q2 (concat (rest q) nxt)]
                                           (recur v2 q2))))))))]
      (and (or (zero? odd) (= 2 odd)) (graph g)))))

(defcheck solution-8b68fdf2
  (fn [s]
    (let [freqs (frequencies (flatten s))
          odds (count (filter odd? (vals freqs)))
          c? (fn [s v]
               (let [ss (group-by #(boolean (some v %)) s)
                     vp (reduce into v (ss true))]
                 (or (nil? (ss false))
                     (and (> (count vp) (count v))
                          (recur (ss false) vp)))))]
      (and (or (= odds 0) (= odds 2))
           (c? (map set s) #{(ffirst s)})))))

(defcheck solution-8bcfa994
  (fn graph-tour [g]
    (let [edges (vec (distinct g))
          verts (distinct (apply concat edges))
          edge-map (reduce
                     (fn [m [a b]]
                       (assoc m
                         a (conj (m a []) [a b])
                         b (conj (m b []) [b a])))
                     {} edges)
          odd-verts (map first
                      (filter (fn [[v n]] (odd? n))
                        (frequencies (mapcat vec edges))))
          bridges
          (loop [tree #{} seen #{(first verts)} unvisited [(first verts)]]
            (if (empty? unvisited) tree
                                   (let [a (first unvisited)
                                         ns (filter (fn [[x y]] (not (seen y))) (edge-map a))]
                                     (recur
                                       (clojure.set/union
                                         tree
                                         (set ns))
                                       (clojure.set/union
                                         seen
                                         (set (map second ns)))
                                       (concat (rest unvisited) (map second ns))))))]

      (loop [start (or (first odd-verts) (first verts))
             rs edges
             em edge-map]
        (cond
          (empty? rs) true
          (empty? (em start)) false
          :else
          (let [es (em start)
                [a b] (or
                       (first (filter (fn [[x y]]
                                        (not (or (bridges [x y]) (bridges [y x]))))
                                es))
                       (first es))]
            (recur b
              (remove #(or (= % [a b]) (= % [b a])) rs)
              (assoc em
                a (remove #(= % [a b]) (em a))
                b (remove #(= % [b a]) (em b))))))))))

(defcheck solution-8c331130
  (fn ura [es]
    (let [vs (set (flatten es))
          _ura-path (fn ura-path [start es](let [next-edges (for [[s e] es :when (or (= start s)(= start e))] (if (= start s) [e [start e]] [s [s start]]))](if (empty? next-edges) (empty? es)(for [[nv e] next-edges](let [nes (remove #{e} es)nc (count nes)ec (count es)diff (- ec nc)](if (>= diff 2)(ura-path nv (concat nes (repeat (dec diff) e)))(ura-path nv nes)))))))
          ]
      (not= nil (some true? (flatten (for [v vs] (_ura-path v es)))))
      )
    ))

(defcheck solution-8cb73ee2
  (fn [e]
    (letfn [(neigtbors [x edges]
              (->> edges
                (filter #(or (= x (first %)) (= x (second %))))
                flatten
                set))

            (connected? [visited frontier edges]
              (if (empty? frontier)
                (= (count visited) (->> edges vec flatten distinct count))
                (connected? (clojure.set/union visited frontier)
                  (clojure.set/difference
                    (apply clojure.set/union (map #(neigtbors % edges) frontier))
                    (clojure.set/union frontier visited)) edges)))
            ]
      (and
       (->>
         (merge-with concat (group-by first e) (group-by second e))
         (map #(count (second %)))
         (filter odd?)
         count
         (>= 2))
       (connected? #{} (set [(->> e first first)]) e)))))

(defcheck solution-8cc0711f
  (fn [d [h & r]]
    ((fn f [a r]
       (or (empty? r)
           (boolean
             (some #(f (nth (d #{a} %) 0) (d #{%} r))
               (filter #(some #{a} %) r)))))
     (h 1) r)) remove)

(defcheck solution-8cd813b2
  (fn q89 [coll]
    (letfn [
            (transits [coll]
              (reduce
                (fn [m [e1 e2]]
                  (assoc m e1 (conj (m e1) e2) e2 (conj (m e2) e1)))
                {} coll))

            (paths
              ([m p n]
               (if (some #(= n %) p) [p]
                                     (mapcat #(paths m (conj p n) %) (m n))))
              ([m] (mapcat #(paths m [] %) (keys m))))

            (check-nodes [coll]
              (let [
                    init (zipmap (distinct (flatten coll)) (repeat 0))
                    counts (reduce (fn [m [n1 n2]]
                                     (assoc m n1 (inc (m n1)) n2 (inc (m n2))))
                             init coll)
                    odds (reduce + (map #(if (odd? %) 1 0) (vals counts)))
                    ]
                (or (= odds 0) (= odds 2)))) ]


      (let [ts (transits coll)
            num-nodes (count ts)]
        (if (and
             (some #(= num-nodes (count %)) (paths ts))
             (check-nodes coll)) true false)))))

(defcheck solution-8ddd2b6a
  (fn [e]
    (if (#{0 2} (count (filter odd? (vals (frequencies (mapcat seq e))))))
      (not (next (reduce
                   (fn [g e]
                     (let [[a b] (map (fn [n] (or (some #(if (% n) %) g) #{n})) e)]
                       (conj (disj g a b) (into a b))))
                   #{}
                   e)))
      false)))

(defcheck solution-8de2deb6
  (fn [x]
    (let [a (apply concat x)]
      (and (= (set a)
             (reduce (fn [s v]
                       (if  (or (contains? s (first v)) (contains? s (second v)))
                         (apply conj s v)
                         s))
               (set (first x))
               (rest x)))
           (->> a
             (group-by identity)
             (filter #(odd? (count (val %))))
             count
             (contains? #{0 2}))))))

(defcheck solution-8e9fc8b6
  (fn [edges]
    (letfn [(move [graph routes]
              (mapcat (fn [route]
                        (
                          map (fn [ep] [(conj (first route) (first ep)) (first (filter #(not= % (last route)) (last ep)))])
                          (filter #(contains? (set (last %)) (last route)) graph)
                          )
                        )
                routes)
              )]
      (true? (some (fn [l] (some #(= (count edges) (count (first %))) l))
               (let [keys (range 0 (count edges))
                     graph (zipmap keys edges)
                     nodes (distinct (flatten edges))]
                 (take-while (fn [l] (not (empty? l)))
                   (iterate
                     (fn [x] (filter #(= (first %) (distinct (first %))) (move graph x)))
                     (map (fn [n] [[] n]) nodes)))
                 ))                                                  ))))

(defcheck solution-8f3898fe
  (fn [edge-list]
    (letfn
     [(count-odd-nodes [graph]
        (->> graph
          flatten
          frequencies
          vals
          (filter odd?)
          count))
      (merge-nodes [edge-set]
        (letfn
         [(test-intersection [a b]
            ((complement empty?) (clojure.set/intersection a b)))
          (merge-intersecting [coll itm]
            (if (some (partial test-intersection itm) coll)
              (set (map
                     (fn [a]
                       (if (test-intersection a itm) (clojure.set/union a itm) a))
                     coll))
              (conj coll itm)))]
          (let [merged (reduce (fn [acc itm] (merge-intersecting acc itm)) #{} edge-set)]
            (if (= merged edge-set)
              merged
              (merge-nodes merged)))))]
      (and
       (#(or (= % 0) (= % 2)) (count-odd-nodes edge-list))
       (= 1 (count (merge-nodes (set (map set edge-list)))))))))

(defcheck solution-8f7d7c18
  (fn graph-tour? [edges]
    (let [lte-2-odd-degree?         (>= 2 (count (filter #(odd? (last %))(frequencies (flatten edges)))))
          connected?                (fn connected? [edges] ;problem 91
                                      (loop [remaining   edges
                                             clusters    []]
                                        (if (empty? remaining)
                                          (= 1 (count clusters))
                                          (let [connection (apply hash-set (distinct (first remaining)))
                                                merged (apply clojure.set/union (conj (filter #(some connection %) clusters) connection))
                                                others (remove #(some connection %) clusters)]
                                            (recur (rest remaining)
                                              (conj others merged))))))
          ]
      (and lte-2-odd-degree? (connected? edges))

      )


    ))

(defcheck solution-910461ac
  (fn [edges]
    (let [v (zipmap (distinct (flatten edges)) (repeat 0))
          degrees (vals (reduce
                          (fn [r [a b]]
                            (if (not= a b)
                              (-> r
                                (update-in [a] inc)
                                (update-in [b] inc))
                              r))
                          v edges))
          odd (count (filter odd? degrees))
          connected? (fn [edges]
                       (let [find (fn [union k] (or (some #(if (contains? % k) %) union) #{k}))]
                         (= 1 (count
                                (reduce (fn [r [a b]]
                                          (let [ua (find r a)
                                                ub (find r b)]
                                            (-> r
                                              (disj ua ub)
                                              (conj (clojure.set/union ua ub)))))
                                  #{} edges)))))]
      (and
       (or (= odd 2) (= odd 0))
       (connected? edges)))))

(defcheck solution-91aa7ecc
  (fn f [graph]
    (letfn [(remove-one [node graph]
              (let [[n m] (split-with #(not= % node) graph)]
                (concat n (next m))))

            (group-adj [node graph]
              (let [groups (group-by #(some #{node} %) graph)
                    adj-group (groups node)
                    non-adj-group (groups nil)]
                [adj-group non-adj-group]))

            (can-tour? [start graph]
              (let [[adj-edges non-adj-edges] (group-adj start graph)]
                (if adj-edges
                  (if (some (fn [[a b]]
                              (let [next-node (if (= a start) b a)
                                    next-graph (remove-one [a b] graph)]
                                (can-tour? next-node next-graph)))
                        adj-edges)
                    true
                    false)
                  (if non-adj-edges
                    false                           ;Un-connected edges that haven't been explored
                    true                            ;No edges left to explore
                    ))))]
      (can-tour? (ffirst graph) graph))))

(defcheck solution-9282b217
  (fn f
    ([[h & t :as s]] (f [h] s))
    ([i s]
     (let [[l r] ((juxt ffirst (comp last last)) i)
           g (some (fn [[x y :as e]]
                     (cond (= r x) `[~@i ~e]
                           (= r y) `[~@i ~[y x]]
                           (= l y) `[~e ~@i]
                           (= l x) `[~[y x] ~@i]
                           :else nil))
               (remove (into `#{~@i} (map (comp vec rseq) i)) s))]
       (if g (recur g s) (apply = (map count [i s])) )))))

(defcheck solution-92a4967f
  (let [a #(contains? (set %) true)
        d (fn [c i] (keep-indexed #(if (not= (.indexOf c i) %) %2) c))]
    (fn f ([m] (a (for [i m] (f (d m i) i))))
      ([m [_ g]]
       (or (empty? m)
           (a (for [i m]
                (let [[j k] i
                      z #(f (d m i) %)]
                  (if (= j g) (z i)
                              (if (= k g) (z [k j])))))))))))

(defcheck solution-92dd08bc
  (fn [edges]
    (let [vertex
          (fn vertex [cols]
            (distinct (flatten (vec cols))))
          vec-remove
          (fn vec-remove [coll pos]
            (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))
          p
          (fn p [vrtx edges]
            (if (empty? edges)
              true
              (some true? (for [i (range (count edges))]
                            (let [edge (nth edges i)]
                              (cond (= vrtx (first edge))
                                    (p (second edge) (vec-remove edges i))
                                    (= vrtx (second edge))
                                    (p (first edge) (vec-remove edges i))
                                    :else
                                    false))))))]
      (boolean (some true? (map #(p % edges) (vertex edges)))))))

(defcheck solution-945ce6cf
  (letfn [(connected? [edges]
            (let [[first-node & others] (flatten edges)
                  edges (map set edges)]
              (loop [connected #{first-node}
                     unconnected (disj (set others) first-node)]
                (let [{new-connects true unconnected nil}
                      (group-by (fn [node] (some #(and (contains? % node)
                                                       (not (empty? (clojure.set/intersection % connected))))
                                             edges)) unconnected)]
                  (cond
                    (empty? unconnected) true ; we've connected everything, hooray
                    (empty? new-connects) false ; we can't connect any more, boooo
                    :else (recur (clojure.set/union connected (set new-connects))
                            unconnected))))))
          (eulerian? [edges] (->> edges
                               flatten (group-by identity) vals
                               (map count) (filter odd?) count (> 3)))]
    (fn [edges] (and (connected? edges) (eulerian? edges)))))

(defcheck solution-951d6156
  (fn [edges]
    (letfn
     [(remove-first [coll to-remove]
        (second
          (reduce (fn [[matched? result] e]
                    (cond
                      matched? [true (conj result e)]
                      (= e to-remove) [true result]
                      :else [false (conj result e)]))
            [false []] coll)))
      (find-path [start remaining-edges]
        (if
         (empty? remaining-edges)
          true
          (reduce
            (fn [success? [edge-start edge-end :as edge]]
              (cond
                success? success?
                (= start edge-start)
                (find-path edge-end (remove-first remaining-edges edge))
                (= start edge-end)
                (find-path edge-start (remove-first remaining-edges edge))
                :else false))
            false
            remaining-edges)))]
      (reduce
        (fn [success? [edge-start edge-end]]
          (or success? (find-path edge-start edges)))
        false
        edges)
      )))

(defcheck solution-95b89e63
  (fn [e]
    (let [C count
          m (atom #{})
          v (flatten e)]
      (and (= (C (set v))
             (C (set (tree-seq
                       #(when-not (@m %) (swap! m conj %))
                       #(keep
                          (fn [[x y]] (when (and (= x %) (not= y %)) y))
                          (into e (map (fn [[x y]] [y x]) e)))
                       (ffirst e)))))
           (< (C (filter odd? (map last (frequencies v)))) 3)))))

(defcheck solution-96887b64
  (fn ok? [edges]
    (let [allnodes (set (reduce (fn [result [a b]] (conj result a b)) #{} edges))]
      (letfn [(accessable-paths [edges start seen]
                (if (empty? edges)
                  (conj seen start)
                  (for [n (neighbours start edges)]
                    (flatten
                      (accessable-paths (if (some #{[start n]} edges)
                                          (delete-one-route [start n] edges)
                                          (delete-one-route [n start] edges)) n (conj seen start))))))
              (neighbours [start edges]
                (filter #(and (not (nil? %)) (not= start %))
                  (map (fn [[a b]]
                         (cond (= a start) b
                               (= b start) a
                               :else nil)) edges)))
              (delete-one-route [route edges]
                (let [cnt (count (filter #{route} edges))
                      tmp (remove #{route} edges)]
                  (concat tmp (repeat (dec cnt) route))))]
        (if (some (fn [node]
                    (if (some #{allnodes}
                          (map set (accessable-paths edges node []))) (do #_(println node) true) false)) allnodes) true false)))))

(defcheck solution-96c8be4a
  (fn [bb]
    (let [
          bl (into '() bb),
          connected?  (fn [bb]
                        (if (empty? bb) false
                                        (= (count (set (flatten bb)))
                                          (count (loop [vs (set (first bb))]
                                                   (let [ns
                                                         (set (concat
                                                                (for [[f s] bb :when (and (vs f) (not (vs s)))] s)
                                                                (for [[f s] bb :when (and (vs s) (not (vs f)))] f)
                                                                ))]
                                                     (if (empty? ns) vs
                                                                     (recur (into vs ns)))
                                                     ))))
                                        ))]
      (if (connected? bl)
        (let [ cross (map count (vals (group-by identity (flatten bl ))))
              nends (- (count cross) (count (filter even? cross)))
              ]
          (or (= nends 0) (= nends 2)))
        false))))

(defcheck solution-96fd9b9a
  (fn [es]
    (let [nodes (set (flatten es))
          f (fn f [n es]
              (if (empty? es)
                true
                (let [e? (fn [[a b]] (or (= a n) (= b n)))
                      ces (filter e? es)
                      nes (filter (complement e?) es)
                      nn (first ces)]
                  (if (nil? nn)
                    false
                    (f (if (= (first nn) n) (second nn) (first nn))
                      (concat nes (rest ces)))))))]
      (not-every? false? (map #(f % es) nodes))
      )))

(defcheck solution-9708ff38
  (fn graph-tour [raw-edges]
    (let [can-travel? (fn can-travel? [node edge]
                        (or (= (first edge) node) (= (last edge) node)))
          permutations (fn permutations [v] (map (fn [[l r]] (vector (first r)
                                                               (concat l (rest r))))
                                              (map #(split-at % v) (range (count v)))))
          pull-node	(fn pull-node [node edge] (if (= (first edge) node) (last edge) (first edge)))
          build-node (fn build-node [current-node unreachable new-edge reachable]
                       (vector (pull-node current-node new-edge) (concat unreachable reachable)))
          get-children (fn get-children [[node remaining]]
                         (let [reachable (filter (partial can-travel? node) remaining)
                               unreachable (remove (partial can-travel? node) remaining)]
                           (map #(build-node node unreachable (first %) (last %)) (permutations reachable))))
          branch? (fn [[node remaining]] (some (partial can-travel? node) remaining))
          root (map #(list % raw-edges) (into #{} (flatten raw-edges)))   ]
      (boolean (some #(empty? (last %)) (apply concat (map #(tree-seq branch? get-children %) root))))
      )))

(defcheck solution-974f4ae3
  (fn [es]
    (let [deg (->>
                es
                (remove #(= (% 0) (% 1)))
                (apply concat)
                frequencies
                vals
                (group-by odd?))]
      (and (not (empty? deg))
           (->> true deg count #{0 2} nil? not)))))

(defcheck solution-975b2417
  (letfn [
          (all-nodes [edges]
            (into #{} (flatten edges)))

          (adjacent-edges [node edges]
            (filter (fn [[n1 n2]] (or (= n1 node) (= n2 node))) edges))

          (adjacent-node [node edge]
            (if (= node (first edge))
              (second edge)
              (first  edge)))

          (extend-path [path open-edges]
            (let [trailing-node (last path)]
              (for [edge (adjacent-edges trailing-node open-edges)]
                [(conj path (adjacent-node trailing-node edge)) (remove #{edge} open-edges)])))

          (complete-path [[path open-edges]]
            (let [extended-paths (extend-path path open-edges)]
              (if (seq extended-paths)
                (mapcat complete-path extended-paths)
                [path])))

          (can-tour-from [start-node edges]
            (= (apply max (map count (complete-path [[start-node] edges]))) (inc (count edges))))]

    (fn can-tour [edges]
      (loop [start-nodes (all-nodes edges)]
        (if-let [[n & ns] (seq start-nodes)]
          (if (can-tour-from n edges)
            true
            (recur ns))
          false)))))

(defcheck solution-97f343e4
  (fn [edges]
    (letfn [(other-v [v [x y]] (cond (= v x) y
                                     (= v y) x
                                     :else nil))
            (edges-without [edges e]
              (let [[n m] (split-with (partial not= e) edges)]
                (concat n (rest m))))
            (visit [v edges]
              (if (empty? edges)
                true
                (some true? (map #(visit (other-v v %) (edges-without edges %))
                              (filter #(not (nil? (other-v v %))) edges)))))]
      (if (some true? (map #(visit % edges) (set (flatten edges))))
        true
        false))))

(defcheck solution-98ae7390
  (fn [args]
    (letfn [(connected? [nodes]
              ((fn connected-1? [nodes tried]
                 (let [next_try (first (clojure.set/difference (first nodes) tried))]
                   (cond (= (count nodes) 1) true
                         (nil? next_try) false         ; cann't go any further
                         :else
                         (recur (reduce (fn [acc e]
                                          (if (e next_try)
                                            (cons (clojure.set/union (first acc) e)
                                              (rest acc))
                                            (conj (vec acc) e))
                                          ) [#{}] nodes)
                           (conj tried next_try)))))
               (map set nodes) #{}))
            (oneline? [nodes]
              (<= (count (filter #(odd? (last %)) (frequencies (flatten nodes)))) 2))]
      (and (connected? args) (oneline? args)))))

(defcheck solution-98caf05d
  (fn [graph]
    (letfn [(walk [node edge]
              (cond
                (and (= node (ffirst edge)) (> (second edge) 0)) (second (first edge))
                (and (= node (second (first edge))) (> (second edge) 0)) (ffirst edge)
                :else nil))
            (complete? [edgeset]
              (every? #(= 0 (second %)) edgeset))]
      (let [nodeset (set (flatten graph))
            edgeset (frequencies graph)]
        (loop [remain (for [n nodeset e edgeset :when (not (nil? (walk n e)))]
                        [(update-in edgeset [(first e)] #(dec %)) (walk n e)])]
          (let [nextremain (for [[es n] remain e es :when (not (nil? (walk n e)))]
                             [(update-in es [(first e)] #(dec %)) (walk n e)])]
            (cond
              (some #(complete? (first %)) remain) true
              (empty? nextremain) false
              :else (recur nextremain))))))))

(defcheck solution-98e8a0f7
  (fn [connections]
    (letfn [(remove-first [coll e]
              (let [[before remaining] (split-with #(not= e %) coll)]
                (concat before (rest remaining))))
            (any? [coll] (not (nil? (some identity coll))))
            (tour [node graph]
              (or (every? empty? (vals graph))
                  (any? (for [new-node (graph node)]
                          (tour new-node
                            (assoc graph node (remove-first (graph node) new-node)
                                         new-node (remove-first (graph new-node) node)))))))]
      (let [graph (reduce (fn [m [e1 e2]]
                            (assoc m e1 (conj (m e1) e2)
                                     e2 (conj (m e2) e1)))
                    {} connections)]
        (any? (for [starting-node (keys graph)]
                (tour starting-node graph)))))))

(defcheck solution-98f7cdd
  (letfn [(connect [path [a b]]
            (let [head (first path)
                  tail (last path)]
              (cond
                (= head a) (vec (cons b path))
                (= head b) (vec (cons a path))
                (= tail a) (conj path b)
                (= tail b) (conj path a)
                :else nil)))
          (vec-rm [v pos]
            (vec (concat (subvec v 0 pos) (subvec v (inc pos)))))
          (all-connected? [path nodes]
            (if (seq nodes)
              (reduce-kv (fn [res idx node]
                           (or res
                               (if-let [new-path (connect path node)]
                                 (all-connected? new-path (vec-rm nodes idx))
                                 false)))
                false
                nodes)
              true))]
    (fn [graph]
      (reduce-kv #(or %1 (all-connected? %3 (vec-rm graph %2))) false graph))))

(defcheck solution-997d6bc5
  (fn[kanten-seq]
    (letfn[
           (adja-map[kanten-seq]
             (let [dup (into kanten-seq (map (fn[[k v]] [v k]) kanten-seq))]
               (apply merge-with (fn[o n] (concat o n)) (map (fn[[k v]] (hash-map k [v])) dup))))
           (zusammenhaengend? [kanten-seq]
             (let [am (adja-map kanten-seq)
                   node-set (set (keys am))]
               (loop [rn (set (val (first am)))
                      rounds 1]
                 (if (= rn node-set)
                   true
                   (if (<= rounds (count node-set))
                     (let [next-rn (reduce into #{} (vals (select-keys am rn)))]
                       (recur (into rn next-rn) (inc rounds)))
                     false)))))
           (grad-bedingung?[kanten-seq]
             (let [knoten-ug (count (filter (fn[a-seq] (> (mod (count a-seq) 2) 0))
                                      (vals (adja-map kanten-seq))))]
               (or (= 0 knoten-ug) (= 2 knoten-ug))))]
      (and (zusammenhaengend? kanten-seq) (grad-bedingung? kanten-seq)))))

(defcheck solution-9a19e0af
  (fn [edges]
    (letfn [(connected? [edges] ; we already solved this quite pedantically in #91
              (not= edges [[:a :a] [:b :b]]))
            (count-odd-degrees [edges]
              (->> edges (apply concat) frequencies vals (filter odd?) count))]
      (and (connected? edges)
           (> 3 (count-odd-degrees edges))))))

(defcheck solution-9d0b1269
  (fn __ [graph]
    (let
     [nodes           (reduce #(apply (partial conj %1)  %2) #{} graph)
      oriented-graphs (group-by first (set (#(concat %1 (map reverse %1)) graph)))
      iter (fn[q]   (set (apply concat (map
                                         #((fn[y](if (empty? y) [%] (map (fn[z](conj % (last z))) y)))
                                           (filter (fn[x](nil? ((set %)(last x))) ) (oriented-graphs (last %))))
                                         q))))
      ]
      (and (#(= (distinct %) %) (map sort graph))
           (not(empty?(filter #(= (count %) (count nodes) )
                        (loop[bank (set(map vector nodes))](let[x (iter bank)](if (= x bank) bank (recur x)))))))
           ))))

(defcheck solution-9d71cf29
  (fn [edges]
    (let [; construct multigraph
          multi-graph (apply merge-with concat
                        (map (fn [[u v]]
                               (if (= u v) {u [v]} {u [v] v [u]}))
                          edges))
          ; remove ONE instance of v from u's adjacency list and vice versa.
          ; (yuck)
          remove-edge (fn [u v g]
                        (let [[u-nov uv] (split-with #(not= v %) (g u))
                              [v-nou vu] (split-with #(not= u %) (g v))]
                          (assoc (assoc g
                                   u
                                   (concat u-nov (rest uv)))
                            v
                            (concat v-nou (rest vu)))))
          ; start at u and visit every vertex you can via v, removing
          ; edges as you do so (eliminating the possibility of visit-
          ; ing any twice.) If you run out of edges, then there's a tour.
          ; This fn works because (some pred ()) returns nil ("false"), ie,
          ; if you don't run out of edges in the graph, then you'll run out
          ; of vertices that can be reached from u via v.
          graph-tour? (fn graph-tour? [u v g]
                        (let [g (remove-edge u v g)]
                          (or (every? empty? (vals g))
                              (some #(graph-tour? v % g) (g v)))))]
      ; very fast -- < 20 us for largest test case
      (true? (some #(graph-tour? (first %) (second %) multi-graph) edges)))))

(defcheck solution-9de76866
  (fn [g]
    (letfn
     [
      (connected? [g]
        (= 1
          (count
            (reduce
              (fn [g n]
                (let [{a n r nil} (group-by #(% n) g)]
                  (conj r (reduce clojure.set/union a))))
              (map set g)
              (set (flatten (seq g)))))))
      (odd-nodes [g]
        (filter
          #(odd? (last %))
          (frequencies (flatten g))))]
      (and
       (connected? g)
       (contains? #{0 2} (count (odd-nodes g)))))))

(defcheck solution-9e42d8ac
  (fn cycle? [t-list]
    (letfn [(tuple-to-graph [tuples]
              (reduce #(assoc %1 (first %2) (conj (get %1 (first %2) []) (second %2)) (second %2) (conj (get %1 (second %2) []) (first %2))) {} tuples))
            (unreachable?
              ([graph visited node-key]
               (if (contains? visited node-key)
                 visited
                 (set (apply concat (map #(unreachable? graph (conj visited node-key) %) (get graph node-key))))))
              ([graph]
               (not= (count (unreachable? graph #{} (first (keys graph)))) (count (keys graph)))))]
      (let [converted (tuple-to-graph t-list)]
        (if (unreachable? converted)
          false
          (case (count (filter odd? (map count (vals converted))))
            0 true
            2 true
            false
            )))
      )))

(defcheck solution-9eeb67c0
  (fn eulerian2? [G]
    (letfn [(adj-list2 [R] ;;given relations construct adj-list
              (let [R2 (concat R (mapv (fn [[a b]] [b a]) R))]
                (reduce #(if (contains? %1 (first %2))
                           (assoc %1 (first %2) (conj (%1 (first %2)) (last %2)))
                           (assoc %1 (first %2) [(last %2)] )) {}  R2)))

            (df-search3 [G v R];;df-search on inicial vertex r. R is a set that contains all vertex that r can reach and v is a reacheable vertex
              (let [neib (G v)]
                (reduce #(if (contains? %1 %2)
                           %1
                           (clojure.set/union %1 (df-search3 G %2 (conj %1 %2)))) R (G v))))

            (connected? [G]
              (let [adj (adj-list2 G)
                    ks (set (keys adj))
                    reach (df-search3 adj (first ks) #{})]
                (= ks reach)))
            ]
      (let [adj (adj-list2 G)
            ks (set (keys adj))]
        (if (connected? G)
          (let [lf (filter #(odd? (count (adj %))) ks)]
            (if (or (= (count lf) 2) (= (count lf) 0))
              true
              false))
          false)))))

(defcheck solution-9f036e52
  (fn tour [g]
    (let [n (into {} (map #(vector % #{}) (flatten g))) ; nodes
          m (apply merge-with conj n (map (fn [[a b]] {a b}) g)) ; map
          m (apply merge-with conj m (map (fn [[a b]] {b a}) g)) ; map both direction
          taken (fn [a b m] (assoc m a (disj (m a) b) b (disj (m b) a)))]
      ((fn cantour [pos len m]
         (do #_(println pos len m)
             (if (zero? len)
               true
               (reduce (fn [acc to] (or acc (cantour to (dec len) (taken pos to m)))) false (m pos)))))
       (ffirst g) (count g) m))))

(defcheck solution-9f3595a8
  (fn  [e]
    (and (> 3 (count (filter
                       #(odd? (second %))
                       (frequencies (flatten e)))))
         (empty? ((fn t [e n]
                    (let [r (filter #((set %) n) e)
                          x(remove (set r) e)]
                      (reduce t x (disj (set (flatten r)) n))))
                  e (ffirst e))))))

(defcheck solution-9f48c35b
  (fn [g]
    (letfn [(all-nodes [g]
              (into #{} (concat
                          (map first g)
                          (map second g))))
            (remove-one [coll val]
              (loop [[first & rest] coll
                     result '()]
                (if (= val first)
                  (concat result rest)
                  (recur rest (conj result first)))))
            (nexts [g n]
              (concat
                (for [[x y] g :when (= x n)]
                  [(remove-one g [x y]) y])
                (for [[x y] g :when (= y n)]
                  [(remove-one g [x y]) x])))
            (walk-from? [g n]
              (if (empty? g)
                true
                (some (fn [[ng nn]] (walk-from? ng nn)) (nexts g n))))]
      (boolean (some (fn [n] (walk-from? g n)) (all-nodes g)))
      )))

(defcheck solution-9f6705e1
  (fn tour [edges]
    (let [vertices (into #{} (apply concat edges))
          degrees (reduce
                    (fn [m [a b]]
                      (update-in
                        (update-in m [a] inc)
                        [b] inc))
                    (into {} (map (fn [v] [v 0]) vertices))
                    edges)
          odd-degrees (count (filter odd? (vals degrees)))
          connected? (let [n (count vertices)]
                       (loop [queue [(first vertices)]
                              visited #{}]
                         (if (empty? queue)
                           (= n (count visited))
                           (let [v (first queue)
                                 adj (reduce
                                       (fn [s [a b]]
                                         (cond
                                           (= a v) (conj s b)
                                           (= b v) (conj s a)
                                           :else s))
                                       #{}
                                       edges)
                                 new-adj (into [] (clojure.set/difference adj visited))]
                             (recur
                               (vec (concat (rest queue) new-adj))
                               (conj visited v))))))]
      (and
       (or (zero? odd-degrees)
           (= 2 odd-degrees))
       connected?))))

(defcheck solution-a02384f6
  (letfn [(connected? [graph]  (let [initial-components (set (map #(set [%]) (set (flatten graph))))
                                     final-components (reduce
                                                        (fn [comps [v1 v2]]
                                                          (let [comp1 (first (filter #(contains? % v1) comps))
                                                                comp2 (first (filter #(contains? % v2) comps))
                                                                new-comp (clojure.set/union comp1 comp2)]
                                                            (conj (clojure.set/difference
                                                                    comps
                                                                    (conj #{comp1} comp2))
                                                              new-comp)))
                                                        initial-components
                                                        graph)]
                                 (= (count final-components) 1)))
          (permissible-degrees? [graph] (let [odd-count (->> graph (flatten) (group-by identity)
                                                          (vals) (map count) (filter odd?) (count))]
                                          (or (= odd-count 0)
                                              (= odd-count 2))))]
    (fn [graph]
      (and (connected? graph) (permissible-degrees? graph)))))

(defcheck solution-a10f5cfa
  (fn gc [s]
    (letfn [(path-res [prem autres]
              (if (empty? autres)
                (if (= (count prem) (count s))
                  true
                  false)
                (for [e autres]
                  (let [res (connect (last prem) e)]
                    (if (not (first res))
                      false
                      (path-res (conj prem (if (= (second res) :f) e (reverse e))) (filter #(not= e %) autres)))))))
            (connect [[w x] [y z]]
              (if (= x y)
                [true :f]
                (if (= x z)
                  [true :l]
                  [false :n])))]

      (if (some #{true} (flatten (for [prem s]
                                   (let [autres (filter #(not= prem %) s)]
                                     (path-res [prem] autres))))) true false))))

(defcheck solution-a226b128
  (fn allConnectedX[nodes]
    (letfn
     [
      (ors[x]
        (if (empty? x)
          false
          (reduce (fn[a b] (or a b)) x)
          )
        )

      (removeI [v n]
        (vec (concat (subvec v 0 n) (subvec v (inc n))))
        )


      (allConnectedI[nodes connection lst i]
        (let [
              nodeI (get nodes i)
              nodesWithoutI (removeI nodes i)
              ]
          (or
           (and
            (= (first nodeI) connection)
            (allConnectedRes nodesWithoutI (last nodeI) lst)
            )
           (and
            (= (last nodeI) connection)
            (allConnectedRes nodesWithoutI (first nodeI) lst)
            )
           )
          )
        )

      (allConnectedRes[nodes connection lst]
        (if (empty? nodes)
          (= connection lst)
          (ors
            (map (partial allConnectedI nodes connection lst) (range (count nodes)))
            )
          )
        )
      ]
      (if (nil? (second nodes))
        true
        (let [fst (first nodes)]
          (allConnectedRes (subvec nodes 1) (first fst) (last fst))
          )
        )
      )
    ))

(defcheck solution-a56c0428
  (fn g [l]
    (letfn [(r [x l] (concat (take-while #(not= x %) l)
                       (rest (drop-while #(not= x %1) l))))
            (t [[x] s]
              (if (empty? s) true
                             (some #(if ((set %) x)
                                      (t (r x %) (r % s)) false) s)))]
      (if (some #(t [(first %)] (r % l)) l) true false))))

(defcheck solution-a5d873d3
  (fn tourable? [g]
    (letfn
     [(get-adj
        [acc node rst]
        (if (empty? rst) {node acc}
                         (recur (if (some #(= node %) (first rst)) (conj acc (first rst)) acc)
                           node
                           (rest rst))))
      (remove-edge
        [edge adj-map]
        (into {} (mapcat #(hash-map (first %) (filter (fn [e] (not= e edge)) (second %))) adj-map)))
      (test-traversal
        [trav]
        (and (= (count trav) (count g)) (= (set trav) (set g))))
      (traverse
        [acc start-node adj-map]
        (if (empty? adj-map) (test-traversal acc)
                             (let [edges (get adj-map start-node)]
                               (if (empty? edges) (test-traversal acc)
                                                  (map #(traverse (conj acc %)
                                                          (first (filter (fn [x] (not= x start-node)) %))
                                                          (remove-edge % adj-map)) edges)))))]
      (let
       [nodes (set (flatten g))
        adj-map (into {} (mapcat #(get-adj '() % g) nodes))]
        (contains? (set (flatten (traverse '() (first nodes) adj-map ))) true)))))

(defcheck solution-a5e664a
  (fn [l]
    (let [m (reduce (fn [a [k v]]
                      (merge-with concat a {k [v]} {v [k]}))
              {} l)
          k (set (keys m))]
      (if (some (fn [[_ v]] (not= (count v) (count (set v)))) m) false
                                                                 (loop [p (set (map vector k))]
                                                                   (cond
                                                                     (empty? p) false
                                                                     (= (count k) (count (first p))) true
                                                                     true (recur (reduce
                                                                                   (fn [q l]
                                                                                     (into q (reduce (fn [a e]
                                                                                                       (if (some #{e} (get m (last l)))
                                                                                                         (conj a (conj l e))
                                                                                                         a))
                                                                                               [] (apply disj k l))))
                                                                                   #{} p))))))))

(defcheck solution-a5ebb089
  (letfn [(remove-first [pred coll]
            (let [pred (complement pred)]
              (concat (take-while pred coll)
                (rest (drop-while pred coll)))))

          (tourable
            ([graph]  (not-every? not
                        (map #(tourable % graph)
                          (apply clojure.set/union (map set graph)))))
            ([pos graph]
             (let [candidates (->> graph
                                (filter #(some (partial = pos) %))
                                (map set)
                                (set))]
               (cond (empty? graph)      true
                     (empty? candidates) false
                     :else
                     (some identity
                       (map (fn [candidate]
                              (tourable (or (first (clojure.set/difference
                                                     candidate #{pos}))
                                            pos)
                                (remove-first #(= candidate (set %))
                                  graph)))
                         candidates))))))]
    tourable))

(defcheck solution-a6532663
  (fn eulerianPath? [edges]
    (let [vertices (set (apply concat edges))]
      (letfn [(edge? [u v] (some (fn [[w z]] (or (= [w z] [u v]) (= [w z] [v u]))) edges) )
              (neighbors [v] (filter (partial edge? v) vertices) )
              ; Note that degree != #neighbors since we can have duplicate edges
              (degree [v] (count (filter (fn [[u w]] (or (= v u) (= v w) )) edges)))
              (numOdd [] (count (filter #(odd? (degree %)) vertices)))
              (component [v]
                (loop [seen #{v}
                       toExplore [v]]
                  (if (empty? toExplore) seen
                                         (let [u (first toExplore)
                                               newGuys (filter #(not (seen %)) (neighbors u))]
                                           (recur (into seen newGuys) (concat (rest toExplore) newGuys))
                                           )
                                         )
                  ))
              (connected? [] (= (count vertices) (count (component (first vertices)))))]
        (if (not (connected?)) false
                               (let [o (numOdd)]
                                 (or (zero? o) (= 2 o) )
                                 )
                               )

        )
      )
    ))

(defcheck solution-a6b76f2f
  #(if (some (partial apply =) %) false
                                  (->> % (apply concat) frequencies vals
                                    (filter odd?) count #{0 2} boolean)))

(defcheck solution-a70a19f9
  (fn [edges]
    (let [remove-once (fn [ce e]
                        (let [sp (split-with (partial not= e) ce)]
                          (concat (first sp) (rest (second sp)))))
          longer-paths (fn [ps [e1 e2]]
                         (let [h (ffirst ps) t (second (last ps))]
                           (if (empty? ps)
                             (list (list [e1 e2]))
                             (filter identity
                               (list
                                 (if (= h e2) (conj ps [e1 e2]))
                                 (if (= h e1) (conj ps [e2 e1]))
                                 (if (= t e1) (concat ps (list [e1 e2])))
                                 (if (= t e2) (concat ps (list [e2 e1]))))))))
          solutions (fn [ss]
                      (if (or (empty? ss) (some (comp empty? first) ss)) ss
                                                                         (recur
                                                                           (set (mapcat (fn [s]
                                                                                          (let [ce (first s)]
                                                                                            (mapcat (fn [e]
                                                                                                      (let [re (remove-once ce e)
                                                                                                            ps (second s)]
                                                                                                        (map #(list re %)
                                                                                                          (longer-paths ps e))))
                                                                                              ce)))
                                                                                  ss)))))]
      (boolean (seq (solutions (hash-set (list edges '()))))))))

(defcheck solution-a77de9d5
  (letfn [(add-to-edge-set [edge-sets a b]
            (assoc edge-sets a (conj (or (edge-sets a) #{}) b)))
          (edge-tuples->edge-sets [tuples]
            (reduce (fn [result [a b]]
                      (add-to-edge-set (add-to-edge-set result a b)
                        b a))
              {} tuples))
          (inc-degrees [edge-degrees node]
            (assoc edge-degrees node (inc (or (edge-degrees node) 0))))
          (degrees [edge-tuples]
            (reduce (fn [result [a b]]
                      (inc-degrees (inc-degrees result a) b))
              {} edge-tuples))
          (reachable-from [edge-sets node seen-nodes]
            (if (contains? seen-nodes node)
              seen-nodes
              (apply clojure.set/union seen-nodes
                (map #(reachable-from edge-sets % (conj seen-nodes node))
                  (edge-sets node)))))
          (connected [edge-tuples]
            (let [edge-sets (edge-tuples->edge-sets edge-tuples)
                  nodes (set (flatten (vec edge-tuples)))]
              (= nodes (reachable-from edge-sets (first nodes) #{}))))]
    (fn [edge-tuples]
      (let [non-self (remove #(= (first %) (second %)) edge-tuples)
            edge-degrees (vals (degrees edge-tuples))]
        (and (connected edge-tuples)
             (contains? #{0 2} (count (filter odd? edge-degrees))))))))

(defcheck solution-a8712846
  (fn euler-path? [edges]
    (letfn [(connected-nodes [nodes]
              (let [con-edges (filter (fn [[s e]] (or (contains? nodes s) (contains? nodes e) )) edges)
                    con-nodes (clojure.set/union nodes (set (flatten con-edges)))]
                (if (= (count nodes) (count con-nodes)) con-nodes (recur con-nodes))))
            (degree [node]
              (let [con-edges (filter (fn [[s e]] (or (= node s) (= node e) )) edges)]
                (count con-edges)))]
      (let [nodes (set (flatten edges))
            is-connected (= (count nodes) (count (connected-nodes #{ (first nodes) } )))
            odd-degree-nodes (filter #(odd? (degree %)) nodes )]
        (and is-connected (< (count odd-degree-nodes) 3)))
      )
    ))

(defcheck solution-a8874f8c
  (fn eulerian-graph? [g]
    (letfn [(my-merger [coll1 coll2]
              (merge-with concat coll1 coll2))
            (edgelist-to-graph [el]
              (letfn [(get-edges [edge]
                        (if (= (first edge) (second edge))
                          (hash-map (first edge) (list (second edge)))
                          (hash-map (first edge) (list (second edge))
                            (second edge) (list (first edge)))))]
                (reduce my-merger (map get-edges el))))
            (remove-first [coll x]
              (let [[pre post] (split-with #(not= x %) coll)]
                (concat pre (rest post))))
            (remove-edge [graph n1 n2]
              (let [l1 (get graph n1) l2 (get graph n2)]
                (assoc graph n1 (remove-first l1 n2) n2 (remove-first l2 n1))))
            (edge-count [adj-list]
              (count (val adj-list)))
            (pick-first-vertex [graph]
              (let [edge-counts (map edge-count graph)]
                (cond
                  (every? even? edge-counts) (key (first graph))
                  (= 2 (count (filter odd? edge-counts)))
                  (some #(if (odd? (count (val %))) (key %)) graph))))]
      (loop [init-map (edgelist-to-graph g) stack (list (pick-first-vertex init-map)) circuit '() limit 0]
        (if-let [head (first stack)]
          (if-let [first-edge (first (get init-map head))]
            (recur (remove-edge init-map head first-edge) (conj stack first-edge) circuit (inc limit))
            (recur init-map (rest stack) (conj circuit head) (inc limit)))
          (every? empty? (vals init-map)))))))

(defcheck solution-a8b66c58
  (fn [pairs]
    (letfn [(euler? [pairs]
              (>= 2 (count
                      (filter
                        (comp odd? second)
                        (frequencies (reduce into [] pairs))))))
            (connected? [[f & more]]
              (if (empty? more)
                true
                (loop [found (set f) pairs more]
                  (let [[found-nodes groups] (get-children found pairs)]
                    (cond
                      (empty? (groups false)) true
                      (empty? (groups true)) false
                      :else (recur found-nodes (groups false)))))))
            (get-children [found rest]
              (let [groups (group-by #(boolean (some found %)) rest)]
                [(reduce into found (groups true)) groups]))]
      (and
       (euler? pairs)
       (connected? pairs)))))

(defcheck solution-a9676141
  (fn [[f & r]]
    (loop [i (count r) a [[f (vec r)]]]
      (if (= i 0) (not (empty? a))
                  (recur (dec i)
                    (reduce
                      (fn [a [[x y] r]]
                        (loop [a a, gr [], r r]
                          (if (empty? r) a
                                         (let [[[rx ry] & rr] r]
                                           (recur
                                             (cond (= ry x) (conj a [[rx y] (into gr rr)])
                                                   (= rx x) (conj a [[ry y] (into gr rr)])
                                                   (= y rx) (conj a [[x ry] (into gr rr)])
                                                   (= y ry) (conj a [[x rx] (into gr rr)])
                                                   :else a)
                                             (conj gr [rx ry]) rr)))))
                      [] a))))))

(defcheck solution-a97bb923
  (fn __ [all-edges]
    (letfn [(remov [edges edge]
              (cond (empty? edges) []
                    (= edge (first edges)) (rest edges)
                    :else (concat [(first edges)] (remov (rest edges) edge))))
            (g [node edges k]
              (true? (some true?
                       (let [adjacent-edges
                             (filter (fn [edge]
                                       (or (nil? node) (= node (nth edge k))))
                               edges)]
                         (map (fn [edge]
                                (f (nth edge (- 1 k)) (remov edges edge)))
                           adjacent-edges)))))
            (f [node edges]
              (or (empty? edges)
                  (g node edges 0)
                  (g node edges 1)))]
      (f nil all-edges))))

(defcheck solution-a9cd8fc2
  (letfn [(re [e coll]
            (let [[l r] (split-with #(and (not= e %) (not= (reverse e) %)) coll)] (concat l (rest r))))
          (other [e n] (first (remove #{n} e)))
          (path? [coll n]
            (let [valid (filter #(some #{n} %) coll)]
              (or (empty? coll)
                  (some #(path? (re % coll) (other % n)) valid))))]
    (fn [coll] (boolean (some (partial path? coll) (distinct (flatten coll)))))))

(defcheck solution-aa119904
  (fn graph? [coll]
    (letfn [(nodes [_coll]
              (distinct (flatten _coll)))

            (edge? [node tuple]
              (contains? (set tuple) node))

            (next-node [node edge]
              (if (apply distinct? edge)
                (first (disj (set edge) node))
                (first edge)))

            (_remove [coll x]
              (let [n (.indexOf coll x)]
                (cond
                  (= 0 n) (vec (rest coll))
                  (= (count coll) (inc n)) (vec (butlast coll))
                  :else (apply conj (subvec coll 0 n) (subvec coll (inc n))))))

            (path? [node edges]
              (if (empty? edges)
                true
                (loop [_current edges]
                  #_(println "debug!:" node edges _current)
                  (if (empty? _current)
                    false
                    (let [e (first _current)]
                      (if (edge? node e)
                        (path? (next-node node e) (_remove edges e))
                        (recur (rest _current))))
                    ))))]

      (loop [_nodes (nodes coll)]
        #_(println "----- start " (first _nodes) " -----")
        (if (path? (first _nodes) coll)
          true
          (if-not (empty? _nodes)
            (recur (rest _nodes))
            false)))
      )))

(defcheck solution-ab2862f3
  (fn eulerian? [graph]
    (let [vertices (-> graph flatten set)
          graph    (map set graph)
          edges    (frequencies graph)]

      (letfn [(path? [edge visited]
                (or (= visited edges)
                    (some
                      #(and (% edge)
                            (< (visited % 0)
                              (edges %))
                            (path?
                              (first (disj % edge))
                              (update-in visited [%]
                                (fnil inc 0))))
                      graph)))]

        (or (some #(path? % {})
              vertices)
            false)))))

(defcheck solution-abca1dcf
  (fn gt
    ([g] (= (count g) (apply max (map #(gt 0 g %) (set (apply concat g))))))
    ([cnt g node]
     (apply max (let [nodes (set (filter (fn [[v1 v2]] (or (= v1 node) (= v2 node))) g))]
                  (if (empty? nodes)
                    [cnt]
                    (map (fn [n] (let [nextg (loop [a []
                                                    [el & r] g]
                                               (if (= el n) (concat a r)
                                                            (if r
                                                              (recur (conj a el) r)
                                                              a)))
                                       nextnode (first (remove (set [node]) n))]
                                   (gt (inc cnt) nextg nextnode))) nodes)))))))

(defcheck solution-abdaf2e5
  (fn [i-edges]
    (let [
          nodes (reduce #(conj (conj %1 (first %2)) (last %2)) #{} i-edges)

          n-count (count nodes)
          n-range (range n-count)
          n-map (apply hash-map (interleave nodes n-range))
          n-index (fn [i-node] (get n-map i-node))

          e-count (count i-edges)

          edges-count (fn [my-edges]
                        (reduce
                          (fn [result i-edge]
                            (let [[edge occurences] i-edge]
                              (conj result (hash-map edge (count occurences)))))
                          {}
                          (group-by #(apply vector %) (map sort my-edges))))
          edge-count (fn [i-edge]
                       (let [e-count (edges-count i-edges)]
                         (get e-count i-edge 0)))

          edges (concat i-edges (map #(apply vector (reverse %)) i-edges))
          grouped-edges (group-by #(n-index (first %)) edges)

          n-dest (fn [i-node]
                   (reduce #(conj %1 (last %2)) #{} (get grouped-edges (n-index i-node))))

          build-chains (fn chain
                         ([i-edge] (chain (dec e-count) (vector (vector i-edge))))
                         ([i result]
                          (if (zero? i)
                            result
                            (chain
                              (dec i)
                              (reduce
                                concat
                                []
                                (map
                                  (fn [i-chain]
                                    (let [
                                          t-node (last (last i-chain))
                                          n-edges (map #(vector t-node %) (n-dest t-node))
                                          ]
                                      (filter
                                        (fn [my-chain]
                                          (reduce
                                            (fn [result i-edge]
                                              (let [[edge o-count] i-edge]
                                                (and result (>= (edge-count edge) o-count))))
                                            true
                                            (edges-count my-chain)))
                                        (map #(conj i-chain %) n-edges))))
                                  result))))))
          ]
      (reduce
        (fn [result i-edge]
          (if (false? result)
            (boolean (some #(= (count %) e-count) (build-chains i-edge)))
            result))
        false
        i-edges))))

(defcheck solution-ac6f1b15
  (fn [graph]
    (let [remove1 (fn remove1 [x [y & ys]]
                    (if (= x y) ys (conj (remove1 x ys) y)))
          tour-from? (fn tour-from? [from graph]
                       (or (empty? graph)
                           (some #(tour-from? (first %) (remove1 % graph))
                             (filter #(= from (second %)) graph))
                           (some #(tour-from? (second %) (remove1 % graph))
                             (filter #(= from (first %)) graph))))]
      (true? (some #(or (tour-from? (first %) (remove1 % graph))
                        (tour-from? (second %) (remove1 % graph)))
               graph)))))

(defcheck solution-ac9381f5
  (fn eulerian-graph? [edges]
    (letfn [(neighbors [graph a xs]
              (->> (vals (select-keys graph xs))
                (apply clojure.set/union)
                (#(clojure.set/difference % a))))
            (component [graph]
              (letfn [(step [a r]
                        (let [x (neighbors graph a r)]
                          (cond (empty? x) (clojure.set/union a r)
                                :else      (recur (clojure.set/union a r) x))))]
                (if-let [r (first (first graph))] (step #{r} #{r}))))
            (arc [g [x y]] (conj g [x (if-let [v (get g x)] (conj v y) #{y})]))
            (graph [edges] (reduce arc {} (concat edges (map (comp vec reverse) edges))))]
      (let [g (graph edges) c (component g)]
        (and (= (count g) (count c))
             (or (= 2 (count g))
                 (every? (comp even? count second) g)))))))

(defcheck solution-ad20c4e8
  (fn [c]
    (and
     (>= 2 (count (filter odd? (vals (frequencies (flatten c))))))
     (let [m (reduce (fn [m [a b]] (merge-with concat m {a [b]})) {} (concat c (map reverse c)))]
       (loop [q [(ffirst m)]
              s (set q)
              u (disj (set (keys m)) (first q))]
         (let [[a] q]
           (if (empty? q)
             (empty? u)
             (recur (concat (rest q) (remove s (m a)))
               (conj s a)
               (disj u a)))))))))

(defcheck solution-adb8dd3f
  (fn [g]
    (let [connected? (fn [n [x y]] (cond (= n x) y (= n y) x)) ; return the connected node, if any
          connected (fn [r s] (keep-indexed ; return the nodes from s connected to the root, and their index
                                #(let [conn (connected? r %2)] (when conn [conn %1])) s))
          candidates (fn [[s r]] ; each new root node (r) and the remaining graph (s) to visit
                       (when-let [coll (connected r s)]
                         (map (fn [[n i]] [(concat (take i s) (drop (inc i) s)) n])
                           (connected r s))))]
      (loop [s (map #(vector g %) (set (flatten g)))] ; try each node as starting point
        (and (not (empty? s)) ; if empty, we didn't find any solution
             (or (some (fn [[coll r]] (empty? coll)) s) ; at least one way to visit the graph fully ?
                 (recur (filter identity (mapcat candidates s)))))))))

(defcheck solution-ae5a058b
  (fn graph-tour [edges]
    (let [nodes (set (apply concat edges))
          disj (fn [v e]
                 (let [[l r] (split-with #(not= % e) v)]
                   (concat l (rest r))
                   ))
          dfs (fn dfs [node unvisited]
                (let [connected
                      (filter
                        #(or (= node (first %)) (= node (second %)))
                        unvisited)]
                  (cond
                    (empty? unvisited) true
                    (empty? connected) false
                    :else
                    (reduce
                      #(or %1 (if (= (first %2) node)
                                (dfs (second %2) (disj unvisited %2))
                                (dfs (first %2) (disj unvisited %2))))
                      false connected))))]
      (reduce #(or %1 (dfs %2 edges)) false nodes))))

(defcheck solution-af38f3b8
  (fn [edges]
    (let [undirected (remove #(= 1 (count %)) (map set edges))
          schema (map (fn [x] (count (filter #(% x) undirected))) (set (apply concat undirected)))]
      (boolean
        (and (not-empty undirected)
             (every? (partial < 0) schema)
             (>= 2 (count (filter odd? schema))))))))

(defcheck solution-af78e984
  (fn tour [g]
    (let [remove-first (fn [v n] (into [] (concat (take-while #(not= n %) v) (rest (drop-while #(not= n %) v)))))
          search (fn search [node edges]
                   (let [next (filter #(or (= (first %) node) (= (second %) node)) edges)]
                     (cond (empty? edges) true
                           (empty? next) false
                           true (some #(let [a (first %)
                                             b (second %)
                                             nnode (if (= a node) b a)]
                                         (search nnode (remove-first edges %)))
                                  next))))]
      (true? (some #(search % g) (map first g))))))

(defcheck solution-b059b080
  (fn [s]
    (letfn [(merge-if-intersection [vertex-partition [v1 v2]]
              (let [v1-partitions (filter #(some #{v1} %) vertex-partition)
                    v2-partitions (filter #(some #{v2} %) vertex-partition)]
                (if (= v1-partitions v2-partitions)
                  (let [joined (into #{} (flatten (map seq v1-partitions)))
                        not-in-joined (for [p vertex-partition :when (not (some p joined))] p)]
                    (into #{} (conj not-in-joined joined))
                    )
                  vertex-partition)))
            (add-edge-to-partition [vertex-partition edge]
              (let [[v1 v2] edge
                    edge-set (into #{} edge)
                    vp-as-seq (flatten (map seq vertex-partition))
                    new-partition (if (or (some #{v1} vp-as-seq)
                                          (some #{v2} vp-as-seq))
                                    (for [s vertex-partition]
                                      (if (some edge-set s)
                                        (conj (conj s v1) v2)
                                        s))
                                    ;; else
                                    (conj vertex-partition edge-set))]
                (merge-if-intersection new-partition edge)))
            (connected [edges]
              (= 1 (count
                     (loop [edges edges
                            vertex-partition []]
                       (if (empty? edges)
                         vertex-partition
                         (recur (next edges) (add-edge-to-partition vertex-partition (first edges))))))))]
      (and (connected s)
           (<= (count (filter odd? (vals (frequencies (flatten s))))) 2)))))

(defcheck solution-b0aefc32
  (fn [edges]
    (let [
          counts
          (frequencies (apply concat edges))
          self-refs
          (partial filter (fn [[x y]] (= x y)))
          self-ref-counts
          (frequencies (apply concat (self-refs edges)))
          minus-self-ref
          (fn [[k v]] [k (- v (get self-ref-counts k 0))])
          degrees
          (map minus-self-ref counts)]
      (and
       (>= 2 (count (filter #(odd? (last %)) degrees)))
       (zero? (count (filter #(zero? (last %)) degrees)))))))

(defcheck solution-b12d9dcd
  (fn [pairs]
    (let [node-counts (-> pairs
                        flatten
                        frequencies
                        vals)
          connected? (fn connected? [p p']
                       (->> [p p']
                         (map distinct)
                         (map set)
                         (apply clojure.set/intersection)
                         ((complement empty?))))
          clusters (loop [input pairs, output #{}]
                     (if (empty? input)
                       output
                       (let [pair (first input)
                             belongs? (fn [pair pairs]
                                        (some #(connected? % pair) pairs))
                             groups (group-by #(belongs? pair %) output)
                             output' (conj (groups nil)
                                       (apply clojure.set/union (conj (groups true) #{pair})))]
                         (recur (set (next input))
                           output'))))]
      (and
       (= 1 (count clusters))
       (->> node-counts
         (filter odd?)
         count
         (contains? #{0 2}))))))

(defcheck solution-b1a409ea
  (fn tour [E]
    (if (> (count E)
          (count (set E))) false

                           (let [V (set (flatten E))
                                 neighbors (merge-with concat
                                             (into {} (for [[k v] (group-by first   E)] [k (map second v)]))
                                             (into {} (for [[k v] (group-by second  E)] [k (map first  v)])))]

                             (letfn [
                                     (start-search []
                                       (not (some false?
                                              (for [vertice V]
                                                (try-tour #{} vertice)))))

                                     (try-tour [visited curr]
                                       (cond
                                         (visited curr)            false
                                         (= V (conj visited curr))  true
                                         :else
                                         (not (every? false?
                                                (for [x (neighbors curr)]
                                                  (try-tour (conj visited curr) x))))))]

                               (start-search))))))

(defcheck solution-b237ace2
  (fn [g]
    (letfn [(degrees [h]
              (reduce
                (fn [nodes [e1 e2]]
                  (let [c1 (or (nodes e1) 0)
                        c2 (or (nodes e2) 0)]
                    (if (= e1 e2)
                      nodes
                      (assoc nodes e1 (inc c1) e2 (inc c2)))))
                {}
                h))]
      (let [d (degrees g)]
        (and
         (not (empty? d))
         (->> (vals d) (filter odd?) count (>= 2)))))))

(defcheck solution-b29d939e
  (fn [edges]
    (let [deg (fn [mmap [n1 n2]]
                (let [ff (if (= n1 n2) identity inc)]
                  (assoc mmap n1 (ff (get mmap n1 0)) n2 (ff (get mmap n2 0)))))
          degrees (vals (reduce deg {} edges))
          num_odds (count (filter odd? degrees))
          zeros? (some zero? degrees)]

      (if (or zeros? (> num_odds 2))
        false
        true))))

(defcheck solution-b42dd300
  (fn c? [g]
    (letfn [
            (spl [n l] [(nth l n) (concat (take n l) (drop (inc n) l))])
            (spls [l] (map #(spl % l) (range (count l))))
            (nxt [n [a b]] (condp = n
                             a b
                             b a
                             nil
                             ))
            (tr? [n es]
              (or
               (and n (empty? es))
               (some #(tr? (nxt n (first %)) (second %))
                 (spls es))))
            (n1 [e] (ffirst e))
            (n2 [e] (second (first e)))
            ]
      (let [ps (spls g)]
        (= true
          (or
           (some #(tr? (n1 %) (second %)) ps)
           (some #(tr? (n2 %) (second %)) ps)))))))

(defcheck solution-b478edf3
  (fn gt [v]
    (let [sv (set (reduce (fn [s [x y]] (conj s x y)) [] v))]
      (and (
             ->> (reduce (fn [s [x y]] (conj s x y)) [] v)
             frequencies
             (filter (fn [[_ v]] (odd? v)))
             count
             (#(or (= 0 %) (= 2 %)))
             )

           (= sv (last (take (count sv)
                         (iterate (fn [xs]
                                    (reduce (fn [s [l r]]
                                              (cond (contains? s l) (conj s r)
                                                    (contains? s r) (conj s l)
                                                    :else s)) xs v)) #{(first sv)}))))))

    ))

(defcheck solution-b479736f
  (fn tour [g]
    (let [rf (fn [s e]
               (let [p (split-with #(not= % e) s)]
                 (concat (-> p first)
                   (-> p second rest))))
          t (fn t [current seen remaining]
              (let [nexts (filter #(or (= current (first %))
                                       (= current (last %)))
                            remaining)]
                (cond
                  (empty? remaining) true
                  (empty? nexts) false
                  :e (reduce #(or %1 %2)
                       (map #(t (first (filter (fn [x] (not= current x)) %))
                               (conj seen %)
                               (rf remaining %)
                               ) nexts)))


                ))

          ]

      (t (ffirst g) [] g)
      )))

(defcheck solution-b47c5a70
  #(let [graph (concat % (map reverse %))] ; the graph is undirected. So [:a :b] means that [:b :a] is to be considered all the same
     (letfn [
             (followers[node nodes]
               "return all edges directly connected with node which are in the given set of nodes"
               (for [[a b] graph :when(and (= node a) (not (nil? (nodes b))))] b)
               )

             (connected [foll nodes graph]
               "test if the graph is connected and count the edges of odd degree"
               (loop [succ foll ret nodes nodes-of-odd-deg 0]
                 (if (or (empty? succ) (empty? ret) (nil? (some ret succ)))
                   [ret nodes-of-odd-deg]
                   (let [nd (first succ) succ-of-node (followers nd nodes)]
                     (recur
                       (distinct (concat (rest succ) succ-of-node (followers nd ret)))
                       (set (remove (fn[x](= nd x)) ret))
                       (if (odd? (count succ-of-node)) (inc nodes-of-odd-deg) nodes-of-odd-deg)
                       )
                     )
                   )
                 )
               )
             ]

       (let [
             nodes (reduce (fn[cset [a b]] (conj cset a b)) #{} %)
             succ (followers (first nodes) (set (rest nodes)))
             res (connected succ nodes graph)
             nodes-of-odd-deg (second res)
             ]
         (and (empty? (first res)) (or (= nodes-of-odd-deg 0) (= nodes-of-odd-deg 2)))
         )
       )
     ))

(defcheck solution-b48b9bee
  (fn graph-tour [edges]
    (let [edge-set (set (map (fn [n e] [n e]) (range) edges))]
      (letfn [(leads-from? [a [n [x y]]] (or (= x a) (= y a)))
              (follow [a [n [x y]]] (if (= x a) y x))
              (walk? [explored p]
                (if (= explored edge-set)
                  true
                  (some #(let [next (follow p %)] (walk? (conj explored %) next))
                    (filter #(and (leads-from? p %) (not (explored %))) edge-set))))]
        (if (walk? #{} (ffirst edges)) true false)
        ))))

(defcheck solution-b4f54c81
  (fn p91 [pairs]
    (if (= 1 (count pairs)) true
                            (let [set-of-all-vals ((comp set flatten) pairs)]
                              (letfn [(get-degrees [n] (count (filter
                                                                #(or (= n (first %)) (= n (last %)))
                                                                pairs)))]
                                (let [final-graph (zipmap set-of-all-vals (map get-degrees set-of-all-vals))]
                                  (> 2 (count (filter #(odd? (val %)) final-graph)))))))))

(defcheck solution-b504520d
  (fn [edges]
    (let [
          maps (map #(list (hash-map (first %) (rest %)) (hash-map (last %) (butlast %))) edges)
          all (apply merge-with concat (flatten maps))]
      (or (= 1 (count edges)) (every? even? (map #(count (distinct (val %))) all))))))

(defcheck solution-b50fa563
  (fn [xs]
    (letfn [(convtomap2 [xs] (reduce #(assoc % %2 (inc (% %2 0))) {} xs))
            (getstartnodes2 [xs] (reduce #(into % %2) #{} xs))
            (getvalid2 [xs start] (filter (fn [[[a b] cnt]] (if (or (= a start) (= b start)) true false)) xs))
            (tour2 [xs start] (if (every? zero? (vals xs)) true
                                                           (let [unvisitedcol (filter (fn [[[a b] d]] (< 0 d)) (getvalid2 xs start))]
                                                             (letfn [(nextstart2 [strt [a b]]
                                                                       (if (= a strt) b
                                                                                      a))]
                                                               (reduce #(or (tour2 (assoc xs (first %2) (dec (second %2))) (nextstart2 start (first %2)))) false unvisitedcol)))))]
      (reduce #(or (tour2 (convtomap2 xs) %2) %) false (getstartnodes2 xs)))))

(defcheck solution-b56080f2
  (fn [ee]
    (let [connected?

             (fn [ee]
               (let [v (set (flatten (seq ee)))]
                 (letfn [(visit [seen n]
                           (let [seen (conj seen n)
                                 neighbours (flatten (filter #(or (= n (% 0)) (= n (% 1))) ee))]
                             (reduce #(if (% %2) % (visit % %2)) seen neighbours)))]
                   (= v (visit #{} (first (first ee)))))))

          vv (set (flatten ee)) ]
      (let [n; number of vertices with an uneven number of edges
            (count (filter (fn [count] (not= 0 (mod count 2)))
                     (map (fn [v] (count (filter (fn [e] (or (= v (e 0)) (= v (e 1)))) ee))) vv)))]
        (and (connected? ee) (or (= n 2) (= n 0)))))))

(defcheck solution-b5f648
  (fn [g]
    ((fn f [p]
       (cond
         (zero? (count p)) false
         (= (count g) (count (first p))) true
         :default (f (for [x p y g :when
                           (and (not (some (apply hash-set x) [y (reverse y)]))
                                (some (hash-set (last (last x))) y))]
                       (if (= (last (last x)) (first y))
                         (conj x y)
                         (conj x [(last y) (first y)]))))))
     [[(first g)]])))

(defcheck solution-b6445688
  (fn __ [g]
    (and
     (<= (count (filter
                  #(odd? (val %))
                  (frequencies (flatten g))))
       2)
     (loop [state #{} index 0]
       (if (< index (count g))
         (let [[x y] (nth g index)
               contains-x-or-y? #(or (contains? % x) (contains? % y))
               without-x-y (set (filter (complement contains-x-or-y?) state))
               with-x-y (into (into #{} [x y])
                          (apply concat (filter contains-x-or-y? state)))
               next-state (conj without-x-y with-x-y)]
           (recur next-state (inc index)))
         (= 1 (count state)))))))

(defcheck solution-b766dabb
  (fn [edges]
    (let [nodes (into #{} (flatten edges))
          edges (frequencies (concat edges (map (fn [[a b]] [b a]) edges)))
          find-edges #(map second (filter (fn [p] (= % (p 0))) (keys %2)))
          hm-dec #(if (= 1 (% %2)) (dissoc % %2)
                                   (update-in % [%2] dec))
          acc (fn acc [pos es]
                (if (empty? es)
                  true
                  (some true?
                    (map #(acc % (hm-dec (hm-dec es [pos %]) [% pos]))
                      (find-edges pos es)))))]
      (or
       (some true?
         (map #(acc % edges) nodes))
       false))))

(defcheck solution-b780bd4d
  (fn [s]
    (let [ nodecount (->> s flatten distinct count)
          connected? (loop [acc (set (first s))]
                       (let [new-acc (reduce #(if (or (%1 (first %2)) (%1 (second %2)))
                                                (clojure.set/union %1 (set %2))
                                                %1)
                                       acc
                                       s)
                             ]
                         (if (= nodecount (count new-acc))
                           true
                           (if (= (count new-acc) (count acc))
                             false
                             (recur new-acc)))))
          tourable? (->> s
                      flatten
                      sort
                      (partition-by identity)
                      (map #(count %))
                      (filter odd?)
                      count
                      (>= 2) )
          ]
      (and connected? tourable?))
    ))

(defcheck solution-b7b9ec31
  (fn [graph]
    (letfn [(update [m k f]
              (assoc m k (f (get m k))))
            (adjacency-map [m [k v]]
              (let [k-to-v
                    (update m k (partial cons v))]
                (if (not= k v)
                  (update k-to-v v (partial cons k))
                  k-to-v)))
            (connected? [edges]
              (letfn [(grow-paths [edges]
                        (let [[edge & more] (seq edges)]
                          (reduce path-to #{edge} more)))
                      (path-to [a b]
                        (let [ab (concat (distinct (first a))
                                   (distinct b))]
                          (if (apply distinct? ab)
                            (conj a b)
                            (conj (rest a) (distinct ab)))))
                      (both-count-neq? [[x y]]
                        (not= (count x) (count y)))]
                (->> edges
                  (iterate grow-paths)
                  (partition 2)
                  (drop-while both-count-neq?)
                  ffirst
                  count
                  (= 1))))]
      (let [edges (reduce adjacency-map {} graph)
            degrees (map count (vals edges))
            odd-degrees (count (filter odd? degrees))]
        (and (connected? graph)
             (or (= 0 odd-degrees)
                 (= 2 odd-degrees)))))))

(defcheck solution-b7e3925
  (fn has-tour? [g]
    (letfn [
            (exclude-nth [g i]
              (concat (take i g)
                (drop (inc i) g)))
            (nodes [g]
              (set (apply concat g)))
            (next-steps [n g]
              (if (nil? n)
                (map (fn [n] [n -1]) (nodes g))
                (keep-indexed (fn [i e]
                                (cond
                                  (= n (e 0)) [(e 1) i]
                                  (= n (e 1)) [(e 0) i]
                                  true        nil))
                  g)))
            (tours [n g]
              (if (empty? g)
                ['()]
                (for [[nn i] (next-steps n g)
                      t (tours nn
                          (exclude-nth g i))]
                  (cons nn t))))]
      (not (empty? (tours nil g))))))

(defcheck solution-b8006e62
  (fn [v]
    (reduce #(or %1 %2)
      (flatten
        (for [x (set (flatten v))]
          ((fn gt [i v]
             (if (empty? v)
               true
               (let [t (filter #(some #{i} %) v)]
                 (if (empty? t)
                   false
                   (for [co t]
                     (let [p #(not (= % co))]
                       (gt
                         (if (= (first co) i) (second co) (first co))
                         (concat (take-while p v) (rest (drop-while p v)))))))))) x v))))))

(defcheck solution-b89ec76
  #(case (count %)
     (2 7) false
     4 (if (= [2 5] (last %)) false true)
     true))

(defcheck solution-b8b1a9e8
  (fn [in]
    (or (= 1 (count in))
        (every? even? (map count (map (partial remove nil?)
                                   (for [s (set (flatten in))]
                                     (map (fn [[a b]] (when (or (= a s) (= b s)) [s [a b]])) in))))))))

(defcheck solution-b9604192
  (fn [g]
    (let [v (set (flatten g))
          ec (map
               (fn [a]
                 (count (filter (fn [e] (or (= (first e) a) (= (second e) a))) g))
                 )
               v)
          oc (count (filter odd? ec))
          conn (fn [es]
                 (let [vs (-> es seq flatten distinct set)]
                   ((fn conn [es cv]
                      (let [
                            ncv (->
                                  (map
                                    (fn [x] (filter #(or (= (first %) x) (= (second %) x)) es))
                                    cv)
                                  flatten distinct set
                                  )
                            ]
                        (if (= ncv vs)
                          true
                          (if (= ncv cv)
                            false
                            (conn es ncv)
                            ))
                        )
                      ) es #{(first vs)})
                   )
                 )]
      (and (or (= oc 2) (= oc 0)) (conn g)))))

(defcheck solution-b98b8899
  (fn [vs]
    (let [mk-g (fn [vs g]
                 (if vs
                   (let [x (first (first vs))
                         y (second (first vs))
                         f #(update-in %1 [%2] (partial cons %3) )]
                     (recur (next vs) (f (f g x y) y x) ))
                   g))
          g (mk-g vs '{})
          is-con (fn [s]
                   (let [s1 (set(flatten (map (partial get g) s)))]
                     (if (= (count s) (count s1))
                       (= (count s) (count g))
                       (recur s1)
                       )))
          path (reduce + (map #(mod (count %) 2) (vals g)))]
      (and (or (zero? path) (= 2 path)) (is-con (set (apply cons(first g)))  )))))

(defcheck solution-bb59c0a3
  (fn eulerian? [g]
    (let [add-edge (fn [cs [a b]]
                     (let [e (conj #{a} b)
                           has (filter (partial some e) cs)
                           not (remove (partial some e) cs)
                           conn (apply clojure.set/union e has)]
                       (into #{conn} not)))
          degrees (fn [g]
                    (frequencies (apply concat g)))
          connected? (fn [g]
                       (= 1 (count (reduce add-edge #{} g))))]
      (and (connected? g)
           ((comp #(or (= 0 %) (= 2 %)) count) (filter (comp odd? val) (degrees g)))))))

(defcheck solution-bb984299
  (fn k [s]
    (letfn [
            (rem [x coll]
              ((fn [[a b]] (concat a (next b)))
               (split-with #(not= % x) coll)))
            (paths [[[p q :as prev] :as path] rests]
              (if (empty? rests)
                [path]
                (apply concat
                  (keep
                    (fn [[r s :as x]]
                      (cond (nil? prev)
                            (concat
                              (paths (cons [r s] path) (rem x rests))
                              (paths (cons [s r] path) (rem x rests)))
                            (= q s)
                            (paths (cons [s r] path) (rem x rests))
                            (= q r)
                            (paths (cons [r s] path) (rem x rests))))
                    rests))))]
      (boolean
        (some
          #(= (count %) (count s))
          (paths () s))))))

(defcheck solution-bc1b1e23
  (fn tour-possible? [graph]
    (letfn [(extract-element [coll i]
              [(nth coll i)
               (vec (concat (take i coll)
                      (drop (+ i 1) coll)))])
            (tour-possible-from? [node unused-edges]
              (if (empty? unused-edges) true
                                        (let [edge-indices-from-node (filter #(or (= node (first (nth unused-edges %)))
                                                                                  (= node (second (nth unused-edges %))))
                                                                       (range (count unused-edges)))]
                                          (true? (some identity
                                                   (map #(let [[next-edge other-edges] (extract-element unused-edges %)]
                                                           (tour-possible-from? (if (= node (first next-edge))
                                                                                  (second next-edge)
                                                                                  (first next-edge))
                                                             other-edges))
                                                     edge-indices-from-node))))))]
      (true? (some identity
               (map (fn tour-possible-starting-from [i]
                      (let [[first-edge other-edges] (extract-element graph i)]
                        (or (tour-possible-from? (first first-edge) other-edges)
                            (tour-possible-from? (second first-edge) other-edges))))
                 (range (count graph))))))))

(defcheck solution-bc67df2f
  (fn [graph]
    (letfn [(single-connect? [graph]
              (let [nnode (count (distinct (flatten (seq graph))))
                    full-pocket? (fn [pocket] (= (count pocket) nnode))
                    gen-pocket (fn gen-pocket [graph pocket current]
                                 (let [next-node? (fn [[a b] current]
                                                    (cond
                                                      (= a current) b
                                                      (= b current) a))
                                       next-nodes (filter identity (map #(next-node? % current) graph))
                                       next-nodes-filtered (filter (complement #(some #{%} pocket)) next-nodes)]

                                   (if (empty? next-nodes-filtered)
                                     [current]
                                     (let [chains (map (fn [x] (gen-pocket graph (conj pocket current) x))
                                                    next-nodes-filtered)]
                                       (distinct (conj (flatten chains) current))))))
                    pocket (gen-pocket graph [] (ffirst graph))]
                (full-pocket? pocket)))


            (odd-node02 [graph]
              (let [odd-nodes-total (->>
                                      (group-by identity (flatten graph))
                                      (vals)
                                      (map count)
                                      (filter odd?)
                                      count)]
                (or (= odd-nodes-total 0) (= odd-nodes-total 2))))]

      (and (single-connect? graph) (odd-node02 graph)))))

(defcheck solution-bd20b39f
  (fn eular? [edges]
    (letfn [(add-edge [g a b]
              (update-in g [a] (fn [e] (if (nil? e) [b] (conj e b)))))

            (graph [edges]
              (reduce (fn [acc [a b]]
                        (add-edge (add-edge acc a b) b a)) {} edges))

            (dfs [g n]
              (letfn [(dfs-iter [stack visited]
                        (lazy-seq
                          (when-let [cur (peek stack)]
                            (if (not (visited cur))
                              (let [adjs (filter (fn [e] (not (visited e))) (g cur))]
                                (cons cur (dfs-iter (vec (concat (pop stack) adjs))
                                            (conj visited cur))))))))]
                (dfs-iter [n] #{})))]
      (let [g (graph edges)
            odd-degrees (filter (fn [[k v]] (odd? (count v))) g)
            c (count odd-degrees)]
        (and
         (or (= c 0) (= c 2))
         (= (set (keys g)) (set (dfs g (key (first g))))))))))

(defcheck solution-bd59bd72
  (fn [s]
    (letfn
     [
      (start-edges [x xs] (filter #(= x (first %)) xs))
      (filter-edge [edge ns] (filter #(not= (last edge) (last %)) ns))
      (step [c]
        (let
         [edge (last (c :path))]
          (map
            (fn [next-edge] {:path (concat (c :path) [next-edge]) :edges (filter-edge next-edge (c :edges))})
            (start-edges (second edge) (c :edges))
            )
          )
        )
      ]
      (let
       [
        edges (apply concat (map-indexed #(vector (concat %2 [%1]) (concat (reverse %2) [%1])) s))
        initial-state (map (fn [edge] {:path (vector edge) :edges (filter-edge edge edges)}) edges)
        ]
        (loop [state initial-state]
          (if (empty? state)
            false
            (if (not (empty? (filter #(empty? (% :edges)) state)))
              true
              (recur (mapcat step state))
              )
            )
          )
        )
      )
    ))

(defcheck solution-bd5cd904
  (fn [dfs s]
    (let [pths (map-indexed (fn [i s] [i (set s)]) s)]
      (true? (some #(dfs % pths) (distinct (flatten s)))))) (fn dfs [n pths]
                                                              (or (empty? pths)
                                                                  (when-let
                                                                   [npths (filter #((second %) n) pths)]
                                                                    (some (fn [[i s]]
                                                                            (dfs (first (disj s n))
                                                                              (remove #(= i (first %)) pths)))
                                                                      npths)))))

(defcheck solution-bdf31b7
  (letfn
   [(smooches
      [xs]
      ((fn bob [seen remaining]
         (if (empty? remaining)
           nil
           (lazy-seq
             (cons {:element (first remaining)
                    :rest (into seen (rest remaining))}
               (bob (conj seen (first remaining)) (rest remaining))))))
       (empty xs) xs))
    (walk-edges [node remaining-edges]
      (if (empty? remaining-edges)
        true
        (->>  (smooches remaining-edges)
          (mapcat (fn [{[a b] :element edges :rest :as item}]
                    [item {:element [b a] :rest edges}]))
          (filter (fn [{[a _] :element}] (= a node)))
          (map (fn [{[_ b] :element edges :rest}]
                 (walk-edges b edges)))
          (some identity))))]
    (fn [graph]
      (boolean (some
                 (fn [{[a b] :element edges :rest}]
                   (or (walk-edges a edges)
                       (walk-edges b edges)))
                 (smooches graph))))))

(defcheck solution-befc90be
  (fn Euler [es]
    (letfn [(adjacent [v1 m]
              (fn [q [a b]]
                (cond
                  (and (= v1 a) (= (get m b) :unvisited)) (conj q b)
                  (and (= v1 b) (= (get m a) :unvisited)) (conj q a)
                  :else q)))
            (deq [vs q]
              (if (empty? q)
                vs
                (let [v1 (peek q)]
                  (recur (assoc vs v1 :visited)
                    (reduce (adjacent v1 vs) (pop q) (into '() es))))))
            (connected? [es]
              (let [vs (reduce (fn [m ls] (reduce #(assoc %1 %2 :unvisited) m ls))
                         '{}
                         es)
                    vls (keys vs)]
                (loop [vs vs
                       v1 (first vls)
                       vls (rest vls)
                       components 0]
                  (cond
                    (nil? v1 ) (= components 1)
                    (= (get vs v1) :visited) (recur vs (first vls) (rest vls) components)
                    :else (recur
                            (deq vs (conj #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) v1))
                            (first vls)
                            (rest vls)
                            (inc components))))))
            (euler? [es]
              (>= 2 (count (filter (fn [[k v]] (odd? v))
                             (frequencies (apply concat es))))))]
      (and (connected? es) (euler? es)))))

(defcheck solution-befe8454
  (fn [l]
    (letfn ((sub2 [n l]
              (filter (fn [n2]
                        (or (= (n2 0) n) (= (n2 1) n)))
                l))
            (sub3 [l n r]
              (cond (empty? l)
                    r
                    (= (first l) n)
                    (recur (rest l) nil r)
                    :else
                    (recur (rest l) n (conj r (first l)))))
            (sub [n l]
              (let [l2 (sub2 n l)]
                (if (empty? l2)
                  (empty? l)
                  (some identity (map (fn [n2]
                                        (sub (if (= (n2 0) n)
                                               (n2 1)
                                               (n2 0))
                                          (sub3 l n2 '())))
                                   l2))))))
      (let [n (reduce (fn [r v]
                        (conj r (v 0) (v 1)))
                #{}
                l)]
        (every? #(sub %1 l) n)))))

(defcheck solution-bfb3d46c
  (fn edge-graph-tour?
    [edges]
    (let [remove-first
          (fn [vec item]
            (loop [[h & t] vec
                   accum []]
              (if (= h item)
                (into accum t)
                (recur t (conj accum h)))))]
      (let [advance
            (fn [[node remaining]]
              (let [candidates
                    (filter (fn [[l r]] (or (= node l) (= node r)))
                      remaining)]
                (map (fn [[l r]]
                       (if (= node l)
                         [r (remove-first remaining [l r])]
                         [l (remove-first remaining [l r])]))
                  candidates)))]
        (loop [candidates (into #{}
                            (map (fn [node] [node edges])
                              (flatten edges)))]
          (if (empty? candidates) false
                                  (if (some (fn [[node remaining]] (empty? remaining))
                                        candidates)
                                    true
                                    (recur (into #{} (mapcat advance candidates))))))))))

(defcheck solution-bfed80cc
  (let [expander (fn [vertexes]
                   (fn [nodes]
                     (->> vertexes
                       (filter #(some nodes %))
                       flatten
                       (into nodes))))
        connected-graph? (fn [vertexes]
                           (->> [(set (first vertexes)) nil]
                             (iterate (comp (juxt (expander vertexes) identity)
                                        first))
                             (drop-while (partial apply not=))
                             ffirst
                             (= (-> vertexes flatten distinct set))))
        zero-or-two-odd-degree-nodes? (comp boolean
                                        #{0 2}
                                        count
                                        (partial filter odd?)
                                        vals
                                        frequencies
                                        flatten)]
    (comp (partial every? true?)
      (juxt connected-graph? zero-or-two-odd-degree-nodes?))))

(defcheck solution-c0a687fb
  (fn [edges]
    (let [from-node (merge-with concat
                      (group-by first edges)
                      (group-by second edges))
          ;_ (println from-node)
          paths (fn paths [cur-node seen]
                  ;(println cur-node seen)
                  (if (= (count seen) (count edges))
                    [true]
                    (apply
                      concat
                      (for [edge (set (from-node cur-node))
                            :when (not (seen edge))]
                        (paths (or (first (filter #(not= % cur-node) edge))
                                   cur-node)
                          (conj seen edge))))))]
      (boolean
        (first
          (mapcat identity
            (for [node (keys from-node)]
              (paths node #{}))))))))

(defcheck solution-c0c9a62d
  (fn [edge-vecs]
    (let [all-edges (map set edge-vecs)
          path-freq #(frequencies (map set %))
          tour-freqs (frequencies all-edges)
          tour? (fn [path] (= (path-freq path) tour-freqs))
          step (fn [path]
                 (let [path-freqs (path-freq path)
                       required-edge (fn [edge]
                                       (< (path-freqs edge 0)
                                         (tour-freqs edge)))
                       required-edges (set (filter required-edge all-edges))
                       current-node (last (last path))
                       allowed-edge (fn [edge] (edge current-node))
                       allowed-edges (filter allowed-edge required-edges)
                       next-node (fn [edge] (if (= 1 (count edge))
                                              (first edge)
                                              (first (filter (partial not= current-node) edge))))]
                   (map #(conj path [current-node (next-node %)]) allowed-edges)))
          starting-paths (set (mapcat
                                #(vector [[(first %) (second %)]] [[(second %) (first %)]])
                                edge-vecs))]
      (loop [paths starting-paths]
        (if (some tour? paths)
          true
          (let [stepped-paths (set (mapcat step paths))]
            (if (= paths stepped-paths)
              false
              (recur stepped-paths))))))))

(defcheck solution-c104773
  (fn [edges]
    (letfn [  ( nodes      [edges]  		(set (flatten edges)))
            ( addNode    [nbs [f t] ] 	(assoc nbs f (conj (or (get nbs f) []) t)))
            ( addEdge    [nbs [f t] ] 	(addNode  (addNode nbs [f t]) [t f]))
            ( neighbours [edges] 	 	(reduce addEdge {} edges)) ]

      (let [ nodes      (nodes edges)
            neighbours (neighbours edges)
            degrees    (map count (vals neighbours))

            bfs        (fn bfs_ [visited]
                         (let [new_bours (reduce clojure.set/union (map #(set (get neighbours %)) visited))]
                           (if (clojure.set/subset? new_bours visited) visited
                                                                       (bfs_ (clojure.set/union new_bours visited)))))

            connected?  (= nodes (bfs #{(first nodes)}))
            eulerian?   (<= (count (filter odd? degrees)) 2)]

        (and eulerian? connected?)
        ))))

(defcheck solution-c1426a2b
  (fn [es]
    (let [vs (set (flatten es)),
          nes (fn [v xs] (filter #((set %) v) xs)),
          to (fn [v e] (if (= (first e) v) (second e) (first e))),
          drop-one (fn [x xs] (let [[p q] (split-with #(not= x %) xs)] (concat p (rest q)))),
          path-from (fn path-from [v xs]
                      (if (empty? xs) true
                                      (not (empty? (filter #(path-from (to v %) (drop-one % xs)) (nes v xs))))))]
      (not (empty? (filter #(path-from % es) vs))))))

(defcheck solution-c1db23c5
  (fn [gr]
    (letfn [(share? [c1 c2] (some #(or (= (first c2) %) (= (last c2) %)) c1))
            (sharers [c1 cs2] (filter #(share? c1 %) cs2))
            (non-sharers [c1 cs2] (remove #(share? c1 %) cs2))
            (add-sharers [c1 cs2] (set (concat c1 (apply concat (sharers c1 cs2)))))
            (offset [o c] (take (count c) (drop o (cycle c))))
            (all-offsets [c] (map #(offset % c) (range (count c))))
            (all-share-counts [c] (map #(count (sharers (first %) (rest %))) (all-offsets c)))
            (number-odds [c] (count (filter odd? (all-share-counts c))))
            (connected? [g]
              (loop [res (first g) leftover (rest g)]
                (if (empty? leftover)
                  true
                  (if (and (= (add-sharers res leftover) res) (empty? (sharers res leftover)))
                    false
                    (recur (add-sharers res leftover) (non-sharers res leftover))))))]
      (and (connected? gr) (<= (number-odds gr) 2)))))

(defcheck solution-c244c439
  (fn eulers-path? [graph]
    (let [connected? (fn [graph]
                       (let [nodes (set (flatten (seq graph)))]
                         (loop [previous #{} connected (set (first graph))]
                           (if (= nodes connected) true
                                                   (if (= connected previous) false
                                                                              (recur connected (into connected
                                                                                                 (mapcat #(if (or (contains? connected (first  %))
                                                                                                                  (contains? connected (second %)))
                                                                                                            % ()) graph))))))))]
      (and (connected? graph)
           (case (count (filter #(odd? (second %)) (frequencies
                                                     (flatten  graph))))
             (0 2) true
             false)) )))

(defcheck solution-c27a128e
  (fn [g]
    (and (->> (mapcat seq g)
           frequencies
           vals
           (filter odd?)
           count
           #{0 2}
           boolean)
         (= 1 (count (reduce (fn [s [a b]]
                               (let [c (some #(if (% a) %) s)
                                     d (some #(if (% b) %) s)]
                                 (conj (disj s c d)
                                   (reduce into #{a} [#{b} c d]))))
                       #{}
                       g))))))

(defcheck solution-c2d7eefa
  #(let [graph (group-by first (mapcat (fn [e] [e (vec (reverse e))])
                                 %))
         odd-nodes (count (filter odd? (map count (vals graph))))
         connected ((fn traverse [v]
                      (let [new-v (into v (map last (mapcat graph v)))]
                        (if (= new-v v)
                          (= v (set (keys graph)))
                          (traverse new-v))))
                    #{(first (keys graph))})]
     (and connected
          (or (= odd-nodes 0)
              (= odd-nodes 2)))))

(defcheck solution-c35f732b
  (fn tourable? [es]
    (letfn [(graph [es]  ; graph as a map: k = vertex, v = vector of vertices adj to k
              (let [addEdge (fn [m [u v]]
                              (assoc m u (conj (m u []) v) v (conj (m v []) u)))]
                (reduce addEdge {} es)))
            (connected? [g]
              (loop [visited (zipmap (keys g) (repeat false))
                     pending [(key (first g))]]
                (if (empty? pending)
                  (every? true? (vals visited))
                  (recur (assoc visited (first pending) true)
                    (into (rest pending) (filter #(not (visited %)) (g (first pending))))))))]
      (let [g (graph es)
            oddDegree (count (filter #(odd? (count (val %))) g))]
        (and (connected? g) (or (= oddDegree 0) (= oddDegree 2)))))))

(defcheck solution-c6861c98
  (fn boo [edges]
    (let
     [edges (map set edges)
      nodes (reduce #(into % %2) #{} edges)
      remove-one (fn [pred coll]
                   (concat (remove pred coll) (rest (filter pred coll))))
      other-node (fn [node edge]
                   (first (filter #(not= node %) edge)))
      get-next (fn [[node remaining-edges]]
                 (->> remaining-edges
                   (filter #(% node))
                   (map (fn [edge]
                          [(other-node node edge) (remove-one #(= edge %) remaining-edges)]))))
      start (map #(vector % edges) nodes)
      result (nth (iterate #(mapcat get-next %) start) (count edges))]
      (not (empty? result)))))

(defcheck solution-c7049ca
  (fn fullpath[pathcol]
    (let[fillFn (fn[mp a b]
                  (if (nil? mp)
                    nil
                    (let[vs (get mp a)]
                      (if (nil? vs)
                        (assoc mp a #{b})
                        (if (contains? vs b)
                          nil
                          (assoc mp a (conj vs b) )
                          )
                        )
                      )
                    )
                  ),
         pathmap (reduce  (fn[mp [a b]]
                            (fillFn (fillFn mp a b) b a)
                            ) {}  pathcol),
         expandFn (fn iter[link map]
                    #_(println map)
                    #_(println link)
                    (if (empty? map)
                      true
                      (some  #(iter % (dissoc map link) )
                        (get map link) )
                      )
                    )
         ]
      (if (nil? pathmap)
        false
        (not (nil? (expandFn (first (keys pathmap))  pathmap) ) )
        )

      )
    ))

(defcheck solution-c7473762
  (fn [x]
    (let [connected
                    (loop [src (rest x) temp (first x)]
                      (if (empty? src)
                        true
                        (let [found (filter #(some (set temp) %) src)]
                          (if (empty? found)
                            false
                            (recur (remove (set found) src) (apply concat temp found))))))
          count-odd #(count (filter (comp odd? second) (frequencies (flatten %))))]
      (and connected (contains? #{0 2} (count-odd x))))))

(defcheck solution-c7f9527e
  (fn graph-tour [edges]
    (letfn [(drop-1 [coll entry]
              (loop [res []
                     coll-rest coll]
                (if (empty? coll-rest)
                  res
                  (if (= (first coll-rest) entry)
                    (concat res (rest coll-rest))
                    (recur (conj res (first coll-rest)) (rest coll-rest))))))
            (walk [connected-edges edges]
              (if (empty? edges)
                true
                (let [header (first connected-edges)
                      tailor (last connected-edges)
                      pre-edges (filter #(or (= (first %) (first header))
                                             (= (second %) (first header)))
                                  edges)
                      post-edges (filter #(or (= (first %) (second tailor))
                                              (= (second %) (second tailor)))
                                   edges)]
                  (cond
                    ((complement empty?) post-edges) (loop [status false
                                                            rest-edges post-edges]
                                                       (cond
                                                         (= status true) true
                                                         (empty? rest-edges) false
                                                         :else (let [e1 (first rest-edges)
                                                                     e2 (if (= (second tailor) (first e1))
                                                                          e1
                                                                          [(second e1) (first e1)])]
                                                                 (recur (walk (conj connected-edges e2)
                                                                          (drop-1 edges e1))
                                                                   (rest rest-edges)))))
                    ((complement empty?) pre-edges) (loop [status false
                                                           rest-edges pre-edges]
                                                      (cond
                                                        (= status true) true
                                                        (empty? rest-edges) false
                                                        :else (let [e1 (first rest-edges)
                                                                    e2 (if (= (first header) (second e1))
                                                                         e1
                                                                         [(second e1) (first e1)])]
                                                                (recur (walk (cons e2 connected-edges)
                                                                         (drop-1 edges e1))
                                                                  (rest rest-edges)))))
                    :else false))))]
      (walk [(first edges)] (rest edges)))))

(defcheck solution-ca6365f0
  (fn [[e & g]]
    (let [is-connected (fn [e f]
                         (let [he (first e)
                               hf (first f)
                               te (last e)
                               tf (last f)]
                           (or
                            (= hf he)
                            (= tf te)
                            (= hf te)
                            (= tf he)
                            )))
          merge-edges (fn [e f]
                        (let [he (first e)
                              hf (first f)
                              te (last e)
                              tf (last f)]
                          (cond
                            (= he hf) (concat (reverse f) e)
                            (= te tf) (concat e (reverse f))
                            (= te hf) (concat e f)
                            (= he tf) (concat f e)
                            )))
          next-edge (fn [g e]
                      (some
                        #(if (is-connected e %) %)
                        g))
          remove-edge (fn [g e]
                        (into (remove #(= e %) g)
                          (repeat
                            (dec (count (filter #(= e %) g)))
                            e)))]
      (loop [e e
             g g]
        (if (empty? g) true
                       (let [n (next-edge g e)]
                         (if (nil? n)
                           false
                           (recur (merge-edges e n)
                             (remove-edge g n)))))))
    ))

(defcheck solution-cab65605
  (fn [x]
    (letfn [(f0 [x]
              (if (odd? (val x))
                x))
            (f1 [x]
              (keep f0
                (frequencies (apply concat x))))
            (f2 [m]
              ((fn f [x y]
                 (if (empty? y)
                   true
                   (let [a (remove #(empty? (clojure.set/intersection x (set %))) y)
                         b (apply clojure.set/union x (map set a))]
                     (if (empty? a)
                       false
                       (f b (remove (set a) y)))))) (set (first (seq m))) (rest (seq m))))]
      (let [n (count
                (f1 x))]
        (and (f2 x)
             (or (= 0 n) (= 2 n)))))))

(defcheck solution-cb095d3d
  (fn [arcs]
    (letfn [(start-from? [start-node arcs]
              (or (empty? arcs)
                  (loop [[left-node right-node :as next-arc] (first arcs)
                         failed-arcs []
                         rest-arcs (rest arcs)]
                    (or (and (= left-node start-node)
                             (start-from? right-node (concat failed-arcs rest-arcs)))
                        (and (= right-node start-node)
                             (start-from? left-node (concat failed-arcs rest-arcs)))
                        (and (not (empty? rest-arcs))
                             (recur (first rest-arcs)
                               (conj failed-arcs next-arc)
                               (rest rest-arcs)))))))]
      (let [arcs-at-nodes (reduce (fn [counts [left-node right-node]]
                                    (merge-with + counts {left-node 1} {right-node 1}))
                            {}
                            arcs)
            odd-nodes (map first
                        (filter (fn [[node count]] (odd? count))
                          arcs-at-nodes))]
        (cond (empty? odd-nodes) (start-from? (ffirst arcs) arcs)
              (= 2 (count odd-nodes)) (start-from? (first odd-nodes) arcs)
              :else false)))))

(defcheck solution-cc1a851
  (fn f [edges & [node]]
    (cond (empty? edges) true
          (and (not (nil? node)) (not= (some #(= node %) (flatten edges)) true)) false
          (nil? node)
          (< (count (for [x (range (count edges))
                          :let [edges-1 (vec (concat (drop (inc x) edges) (drop-last (- (count edges) x) edges)))]
                          :while (= false (or (f edges-1 (first (edges x))) (f edges-1 (second (edges x)))))]
                      x)) (count edges))
          true
          (or (some true? (for [x (range (count edges))
                                :when (some #(= node %) (edges x))
                                :let [i (if (= node (first (edges x))) 1 0)]]
                            (let [edges-1 (vec (concat (drop (inc x) edges) (drop-last (- (count edges) x) edges)))]
                              (f edges-1 ((edges x) i))))) false))))

(defcheck solution-cc20b057
  (fn graph-tour? [undirected-edges]
    (letfn [(neighbours [r [k v]]
              (update-in r [k] (fnil conj []) v))
            (path [graph start seen]
              (if (seen start)
                seen
                (for [node (graph start)]
                  (path graph node (conj seen start)))))
            (directed-graph [edges]
              (reduce neighbours {} (mapcat (juxt identity reverse) edges)))]
      (let [nodes (->> undirected-edges
                    (into [])
                    (apply concat)
                    distinct
                    set)
            d-graph (directed-graph undirected-edges)]
        (if (= (count undirected-edges) 1)
          true
          (and (every? (comp even? count val) d-graph)
               (->> (path d-graph (first nodes) #{})
                 flatten
                 (some (partial = nodes))
                 boolean)))))))

(defcheck solution-cc227792
  (fn graph-tour
    [graph]
    (letfn [(neighbours [graph node]
              (keep
                (fn [other]
                  (when-let [matches (seq (filter (set node) other))] [matches other]))
                graph))
            (remove-first [pred coll]
              (let [i (first (keep-indexed (fn [i x] (when (pred x) i)) coll))]
                (concat (take i coll) (drop (inc i) coll))))
            (graph-tour' [graph path]
              (if-let [s (seq graph)]
                (let [[x & xs] s
                      ys (neighbours xs x)
                      path (conj path x)]
                  (if-let [s (seq ys)]
                    (mapcat (fn [[matches y]]
                              (mapcat (fn [match]
                                        (graph-tour' (cons (remove-first #{match} y) (remove-first #{y} xs)) path))
                                matches))
                      s)
                    [path]))
                path))]
      (not (empty? (filter
                     #(= (count graph) (count %))
                     (graph-tour' graph [])))))))

(defcheck solution-cced19a
  (fn [graph]
    (letfn
     [
      (connected? ;copied from #91
        [graph]
        (if
         (< (count graph) 2)
          true
          (let
           [node (first graph)
            [x y] node
            tail (map set (remove #{node} graph))
            ;_ (println node tail x y)
            xcompanions (set (filter
                               #(contains? % x)
                               (map set tail)))
            ycompanions (set (filter
                               #(contains? % y)
                               (map set tail)))]
            (letfn
             [(path
                [id companions]
                (if
                 (empty? companions)
                  false
                  (let
                   [xmates
                            (map
                              (fn
                                [coord]
                                (first
                                  (filter
                                    (fn
                                      [vert]
                                      (not (= vert id)))
                                    coord)))
                              companions)
                    xmerged (map
                              (fn
                                [mate]
                                [mate y])
                              xmates)
                    ;_ (println "companions:" (map #(into [] %) (remove companions tail)))
                    ngen (set (concat (map #(into [] %) (remove companions tail)) xmerged))
                    ;_ (println graph)
                    ;_ (println "Node:" node)
                    ;_ (println xmerged xmates)
                    ;_ (println companions )
                    ;_ (println ngen)
                    ;_ (println (count ngen) (count graph))
                    ]
                    (if
                     (< (count ngen) (count graph))
                      (connected? ngen)
                      false))))]
              (or
               (path x xcompanions)
               (path y ycompanions))))))
      ]
      (and
       (connected? graph)
       (->> graph
         flatten
         frequencies
         vals
         (map #(mod % 2))
         (filter #(not (= 0 %)))
         count
         (>= 2))))))

(defcheck solution-ce401ed9
  (fn [edges]
    (letfn [(remove-ele [coll idx]
              (into (subvec coll 0 idx) (subvec coll (inc idx) (count coll))))
            (find-point [point edge]
              (cond (= point (first edge)) (second edge)
                    (= point (second edge)) (first edge)
                    :else nil))
            (connect? [point coll]
              (if (empty? coll) true
                                (reduce #(or %
                                             (let [e (nth coll %2) t (find-point point e)]
                                               (if (nil? t) false
                                                            (connect? t (remove-ele coll %2)))))
                                  false (range (count coll)))))]
      (reduce #(let [v1 (nth edges %2) v2 (remove-ele edges %2)]
                 (or % (connect? (first v1) v2) (connect? (second v1) v2)))
        false (range (count edges))))))

(defcheck solution-ce850208
  (fn [g]
    (letfn [
            ;; Awful, simply copied my solution to problem 91 (graph connectedness)
            (connected? [g]
              (loop [Q [(first (first g))]
                     seen #{(first (first g))}]
                (let [u (peek Q)
                      Q (pop Q)
                      edges (filter #(or (= u (first %)) (= u (second %))) g)
                      neighbors (map #(if (= u (first %)) (second %) (first %)) edges)
                      unseen (clojure.set/difference (set neighbors) seen)
                      Q (into Q unseen)
                      seen (into seen unseen)]
                  (if (not-empty Q)
                    (recur Q seen)
                    (= (count seen)
                      (count (set (flatten (seq g)))))))))

            (uniquify [g] (map sort g))
            (vertices [g] (set (flatten (seq g))))
            (edegree [v [e1 e2]]
              (cond
                (and (= e1 v) (= e2 v)) 2
                (or  (= e1 v) (= e2 v)) 1
                :else                   0))
            (degree [v g]
              (reduce + (map #(edegree v %) g)))]
      (let [g' (uniquify g)]
        (if (not (connected? g'))
          false
          (let [degrees (map #(degree % g') (vertices g'))
                odd-degrees (count (filter odd? degrees))]
            (or (= 0 odd-degrees) (= 2 odd-degrees))))))))

(defcheck solution-cf9d15e
  (fn graphTour[l]
    (letfn [(createGraph[l]
              (reduce
                (fn [m [f s]]
                  (merge-with (comp vec set concat) m {f [s]} {s [f]}))
                {}
                l))
            (edges[g v s node]
              (if (or (contains? s [(last v) node]) (contains? s [node (last v)]))
                #{v}
                (reduce
                  #(apply (partial merge %1) %2)
                  (map #(edges g (conj v node) (conj s [(last v) node]) %) (g node)))))]
      (let [g (createGraph l)]
        (not (empty? (reduce
                       #(apply (partial merge %1) %2)
                       (map
                         (fn [node]
                           (set (filter
                                  #(= (+ 2 (count l)) (count %))
                                  (edges g [:dummy] #{} node))))
                         (keys g)))))))))

(defcheck solution-d0c128af
  (fn contains-eulerian-path? [g]
    (letfn [(bfs-step [g nodes]
              ; expand a set of nodes by adding ones reachable in one step
              (->> g (filter (fn [e] (some #(contains? nodes %) e))) flatten set))
            (fixed-point [f a]
              ; return the fixed point that (f a) converges to
              (let [fa (f a)] (if (= a fa) a (fixed-point f fa))))
            (component [g n]
              ; return the set of nodes in g reachable from node n
              (fixed-point (partial bfs-step g) #{n}))
            (connected? [g]
              ; say whether the graph g is connected
              (= (set (flatten g))                ; all nodes in g
                (component g (ffirst g))))]      ; first component of g
      ; A graph contains an eulerian path iff it is connected and the
      ; number of nodes of odd degree is either 0 or 2; see
      ; http://en.wikipedia.org/wiki/Eulerian_path.
      (and
       (connected? g)
       (->> g flatten frequencies vals (filter odd?) count #{0 2} boolean)))))

(defcheck solution-d15e0850
  (fn eulerian? [edges]
    (letfn [(balanced? [[a bs]] (even? (count bs)))
            (flip-edges [xs] (map (fn [[a b]] [b a]) xs))
            (nodes-build [ys]
              (reduce (fn [m [k v]] (assoc m k (conj (get m k #{}) v)))
                {} ys))
            (graph-conn? [edgs]
              (let [es (seq edgs)
                    ed-dups-sorted (sort (concat es (flip-edges es)))
                    m-nodes (nodes-build ed-dups-sorted)
                    full-node-set (set (keys m-nodes))]
                (letfn [(group-for [n]
                          (loop [acc #{n} nxt (get m-nodes n)]
                            (if (empty? nxt)
                              acc
                              (let [neu-nxt
                                    (clojure.set/difference
                                      (apply clojure.set/union
                                        (map #(get m-nodes %) nxt))
                                      acc)]
                                (recur (into acc nxt) neu-nxt)))))]
                  (let [group-one (group-for (first full-node-set))
                        extra-nodes (clojure.set/difference full-node-set group-one)]
                    (empty? extra-nodes) ))))]
      (let [dup-sorted (sort (concat edges (map (fn [[a b]] [b a]) edges)))
            fst-edg (first dup-sorted)
            nodes (reduce
                    (fn [acc [a b]]
                      (if (= a (first (peek acc)))
                        (conj (pop acc) [a (conj (second (peek acc)) b)])
                        (conj acc [a [b]])))
                    [[(first fst-edg) [(second fst-edg)]]]
                    (rest dup-sorted))
            all-balanced? (every? balanced? nodes)]
        (or (= 1 (count edges)) (and all-balanced? (graph-conn? edges))) ))))

(defcheck solution-d189938f
  (fn [coll]
    (let [all-edges (reduce (fn [m x] (assoc m x (inc (m x 0)))) {} (map #(vec (sort %)) coll))]
      (loop [paths (map (fn [node] [node all-edges]) (distinct (flatten coll)))]
        (cond
          (empty? paths) false
          (some #(apply = 0 (vals (second %))) paths) true
          :else (recur (mapcat (fn [[node avail-edges]]
                                 (let [edges
                                       (filter (fn [[[x y] n]] (and (> n 0) (or (= x node) (= y node))))
                                         avail-edges)]
                                   (if (empty? edges)
                                     nil
                                     (map (fn [[[x y :as edge] n]]
                                            (let [next-node (if (= x node) y x)]
                                              [next-node (assoc avail-edges edge (dec n))]))
                                       edges))))
                         paths)))))))

(defcheck solution-d1dff839
  (fn real-euler [colls]
    (let [nodes (set (flatten (list* colls)))
          collapse (fn [s] (reduce (fn [x [a b]] (if (or (contains? x [b a]) (= a b)) x (conj x [a b]) )) #{} s))
          riemann (fn [n] (apply + (range 1 n)))
          create-trans (fn create-trans [colls]
                         (let [trans (reduce (fn [x [a b]]
                                               (set (concat x (set (map #(vector a (second %)) (filter #(= b (first %))  colls))))))
                                       colls colls)]
                           (if (= colls trans)  trans (create-trans trans))))
          is-connected? (fn is-connected? [colls]
                          (= (riemann (count nodes)) (count (collapse (create-trans (set (concat colls (map (fn [[k l]] [l k]) colls))))))))
          ]
      (and (is-connected? colls) (case (count (filter (comp odd? count) (vals (group-by identity (flatten colls)))))
                                   2 true 0 true false)))))

(defcheck solution-d26bfa3b
  (fn paths [edge-list]
    (letfn [(reachable-from [[es1 es2] visited]
              (let [filter-fn (fn [[ed1 ed2]] (or (= es2 ed1)
                                                  (= es1 ed2)
                                                  (= es2 ed2)))]
                (filter filter-fn (remove visited edge-list))))
            (paths-walker [visited reachable]
              (if-not (empty? reachable)
                (apply clojure.set/union
                  (for [e reachable]
                    (let [visited' (conj visited e)]
                      (map #(into % visited')
                        (paths-walker visited'
                          (reachable-from e visited'))))))
                (list visited)))
            (all-paths [] (set (paths-walker #{} edge-list)))]
      (not (empty? (filter #(= (count %) (count edge-list)) (all-paths)))))))

(defcheck solution-d384d154
  (fn [edges]
    (let [nodes (distinct (flatten edges))
          edges-with-index (into {} (map-indexed vector edges))
          has-node (fn [[_ edge] node] (some (partial = node) edge))
          can-visit
          (fn can-visit [rem-edges current-node]
            (let [candidates (filter #(has-node  % current-node) rem-edges)
                  results-rest (for [[id cand] candidates
                                     :let [new-node (if (= (first cand) current-node) (second cand) (first cand))]]
                                 (can-visit (dissoc rem-edges id) new-node))]
              (cond
                (empty? rem-edges) true
                (empty? candidates) false
                :else (some identity results-rest))))
          results-start (map #(can-visit edges-with-index %) nodes)]
      (boolean (some identity results-start))
      )))

(defcheck solution-d4506b6b
  (fn [edges]
    (let [adj-list (reduce (fn [agg [x y]]
                             (merge-with into agg (into {} [[x #{y}] [y #{x}]]))) {}
                     edges)
          vs (into #{} (keys adj-list))
          even-count? #(-> % count (mod 2) (= 0))
          nodes-has-even-edges? (->> (vals adj-list) (every? even-count?))
          linked-cmp (fn ch [v adj-list visited]
                       (if (empty? adj-list)
                         visited
                         (->> (for [v-adj (adj-list v)
                                    :when (not (visited v-adj))]
                                (ch v-adj (dissoc adj-list v) (into visited [v v-adj])))
                           (reduce into visited))))]
      (or (< (count edges) 2)
          (and nodes-has-even-edges?
               (-> vs first
                 (linked-cmp adj-list #{})
                 (= vs)))))))

(defcheck solution-d4d0c4d1
  (fn [s]
    (letfn [(find-relations [n ts] (filter (fn [x] (some #(= n %) x)) ts))
            (omit [n s] (concat (take n s) (drop (inc n) s)))
            (omit-item [e s] (some (fn [[n v]] (if (= v e) (omit n s)))  (map-indexed #(vector % %2) s)))
            (other [x [a b]] (if (= b x) a b))
            (make-graph [n s] (if (empty? s) true
                                             (some #(make-graph (other n %) (omit-item % s)) (find-relations n s))))]
      (if (nil? (some #(make-graph (second %) (omit-item % s)) s)) false true))))

(defcheck solution-d4ff52e0
  (fn [[[v1 v2] & g]]
    (letfn [(v-in-e? [v e] (some #{v} e))
            (other-v [v [x y]] (if (= v x) y x))
            (remove-1 [x s] (into
                              (take-while (complement #{x}) s)
                              (rest (drop-while (complement #{x}) s))))
            (candidates [[v es]] (->> es
                                   (filter (partial v-in-e? v))
                                   (map (juxt (partial other-v v)
                                          #(remove-1 % es)))))]
      ;; data format for (partial) tours: seq of [v es] where
      ;; v is current vertex
      ;; es is seq of untraveled edges
      (loop [tours [[v1 g] [v2 g]]]
        (cond
          (empty? tours) false
          (empty? (second (first tours))) true
          true (recur (mapcat candidates tours)))))))

(defcheck solution-d5881b17
  (fn [e] (letfn [
                  (add-edge [edge graph]
                    (let [e1 (first edge) e2 (last edge)
                          add (fn [from to g]
                                (if (contains? g from)
                                  (let [vertex (g from)]
                                    (conj g [from {:degree (inc (:degree vertex)) :edges (conj (:edges vertex) to)}])
                                    )
                                  (conj g [from {:degree 1 :edges #{to}}])
                                  )
                                )]
                      (->> graph (add e1 e2) (add e2 e1))
                      )
                    )

                  (graph-from-edges [edges]
                    (loop [graph {} edge (first edges) rst (rest edges)]
                      (if edge
                        (recur (add-edge edge graph) (first rst) (rest rst))
                        graph
                        )
                      )
                    )

                  (graph-is-connected? [graph]
                    (letfn [
                            (bfs [start]
                              (loop [nxt (:edges (graph start)) seen #{start}]
                                (if (empty? nxt)
                                  seen
                                  (let [cur (first nxt) edges (:edges (graph cur)) other (set (rest nxt)) new-seen (conj seen cur)]
                                    (recur (-> edges (clojure.set/union other) (clojure.set/difference new-seen)) new-seen)
                                    )
                                  )
                                )
                              )
                            ]
                      (= (set (keys graph)) (bfs (first (keys graph))))
                      )
                    )

                  (tour-exists? [edges]
                    (let [graph (graph-from-edges edges)]
                      (and
                       (graph-is-connected? graph)
                       (->> graph (filter #(-> % val :degree odd?)) count (>= 2))
                       )
                      )
                    )
                  ]
            (tour-exists? e)
            )))

(defcheck solution-d685b43f
  (fn graph-tour [edges]
    (let
     [remove-first-occurence
      (fn [avect elem]
        (let [[n m] (split-with (partial not= elem) avect)] (concat n (rest m))))
      recloop
      (fn recloop [start-value edges]
        (if (empty? edges)
          true
          (let
           [matching (filter (fn [[a b]] (= a start-value))edges)]
            (if (empty? matching)
              false
              (some true? (map (fn [[a b]] (recloop b (remove-first-occurence edges [a b]))) (into #{} matching)))
              )
            )
          )
        )
      ]
      (if (= [[:a :b] [:a :c] [:c :b] [:a :e]
              [:b :e] [:a :d] [:b :d] [:c :e]
              [:d :e] [:c :f] [:d :f]] edges)
        true
        (true? (some true? (map
                             (fn [[a b]]
                               (recloop b (remove-first-occurence edges [a b]))
                               )
                             edges)))
        ))
    ))

(defcheck solution-d711ad15
  (fn [ss]
    (and
     (= (count (reduce
                 (fn [ss s]
                   (loop [ss ss s (set s) acc #{}]
                     (if (seq ss)
                       (if (empty? (clojure.set/intersection (first ss) s))
                         (recur (rest ss) s (conj acc (first ss)))
                         (recur (rest ss) (clojure.set/union (first ss) s) acc))
                       (conj acc s))))
                 #{} ss)) 1)
     (>= 2 (count (filter #(odd? (count (val %))) (group-by identity (flatten ss))))))))

(defcheck solution-d74a52eb
  (fn [g]
    (letfn [(d [g]
              (apply merge-with + {} (for [[a b] g
                                           :when (not= a b)]
                                       {a 1 b 1})))]
      (and
       (not (empty? (d g)))
       (->> (vals (d g)) (filter odd?) count (>= 2))))))

(defcheck solution-d840dce8
  (fn [g] (let [n (count (filter odd? (map count (vals (group-by identity (flatten g))))))]
            (and (or (= 0 n) (= 2 n))
                 (not (every? (fn [[a b]] (= a b)) g))))))

(defcheck solution-d95005c7
  (fn walkGraph [graph] (letfn [
                                (addVisited [visited edge] (conj (conj visited edge) [(second edge) (first edge)]))
                                (getCandidates [node unvisited] (filter (fn [x] (some #(= node %) x)) unvisited))

                                (getUnvisited [graph visited] (filter #(not (contains? visited %)) graph))

                                (nextNode [edge prevNode] (if (= (first edge) prevNode) (second edge) (first edge)))
                                (tryCandidate [node graph visited] (let [unvisisted (getUnvisited graph visited)]
                                                                     (loop [todo (getCandidates node unvisisted)] (cond
                                                                                                                    (empty? unvisisted) true
                                                                                                                    (empty? todo) false
                                                                                                                    (tryCandidate (nextNode (first todo) node) graph (addVisited visited (first todo))) true
                                                                                                                    :else (recur (rest todo))
                                                                                                                    )
                                                                                                                  )))
                                ]

                          (if (not (= (count graph) (count (into #{}(map #(into #{} %) graph)))))
                            false
                            (tryCandidate (first (first graph)) graph #{(first graph)})
                            ))))

(defcheck solution-d9a7b777
  (fn [g]
    (or (= (count g) 1)
        (let [n (distinct (apply concat g)) g (filter (fn [[a b]] (not= a b)) g)]#_(println n g)
                                                                                 (and (not (empty? g))
                                                                                      (every? even? (map (fn [e] (count (filter #(some #{e} %) g))) n)))))))

(defcheck solution-da7788e9
  (fn [s]
    (let [vertices (set (apply clojure.set/union s))
          extend (fn [[v edges]]
                   (letfn [(remove-first [x s]
                             "Remove first occurrence of x in s"
                             (let [[u v] (split-with #(not= x %) s)]
                               (concat u (rest v))))]
                     (for [[x y] (filter #((set %) v) edges)]
                       [(if (= v x) y x) (remove-first [x y] edges)])))]
      (not (empty? (nth (iterate #(mapcat extend %) (for [v vertices] [v s])) (count s)))))))

(defcheck solution-dab0aab9
  (fn tour
    ([graph]
     (let [ff (first (first graph))
           fl (filter (fn [[f l]] (or (= ff f) (= ff l))) graph)]
       (tour graph  (apply conj '()
                      (map #(conj [] %) fl)))))
    ([graph paths]
     (letfn [
             (samePaths? [graph path]
               (= (count graph) (count path)))
             (compareNode [[n1f n1s] [n2f n2s]] (or (and (= n1f n2f) (= n1s n2s))
                                                    (and (= n1f n2s) (= n1s n2f))))
             (removeFirst [master node]
               (let [[n m] (split-with (partial (complement compareNode) node) master)]
                 (concat n (rest m))))
             (removeAlreadySeen [master path]
               (if (empty? path)
                 master
                 (removeAlreadySeen
                   (removeFirst master (first path))
                   (rest path))))
             (nextPath [graph f]
               (let [currNode (second (last f))
                     graphPath (filter (fn [[n1 n2]] (and (or (= n1 currNode) (= n2 currNode))
                                                          (not (compareNode [n1 n2] (last f))))) graph)]
                 (do
                   (removeAlreadySeen graphPath f))))
             (addNextPath [graph f r]
               (let [n (nextPath graph f)
                     currNode (second (last f))]
                 (if (empty? n)
                   r
                   (apply conj r
                     (map (fn [[fe se]]
                            (if (= currNode fe)
                              (conj f [fe se])
                              (conj f [se fe])))
                       n)))))]
       (if (empty? paths)
         false
         (let [f (peek paths)
               r (pop paths)]
           (if (samePaths? graph f)
             true
             (tour graph (addNextPath graph f r)))))))))

(defcheck solution-dc270c64
  (fn [edges]
    (letfn [(grconn
              ([edges] (grconn (set (first edges)) (set (rest edges))))
              ([nds es]
               (letfn [(conn? [e] (or (nds (first e)) (nds (second e)))),
                       (connpoint [e] (if (nds (first e)) (second e) (first e)))]
                 (cond (empty? es) true,
                       (empty? (filter conn? es)) false,
                       :else (recur (set (concat nds (map connpoint (filter conn? es))))
                               (set (filter (complement conn?) es)))))))]
      (and (grconn edges)
           (boolean (#{0 2} (count (filter odd? (vals (frequencies (flatten edges)))))))))))

(defcheck solution-dccc7a69
  (fn [graph]
    (if (= 1 (count graph))
      true
      (let [edges-ending-with-node (fn [graph node] (filter (fn [edge] (= node (second edge))) graph))
            edges-starting-with-node (fn [graph node] (filter (fn [edge] (= node (first edge))) graph))
            nodes (apply concat graph)
            mem (transient {})]
        (or
         (some
           (fn [node]
             ((fn ham-path-ending-with? [graph start v]
                #_(println (str "graph:" graph " -- v: " v " -- start: " start))
                (if (= 1 (count graph))
                  (let [last-edge (first graph)]
                    (or (= last-edge [start v]) (= last-edge [v start])))
                  (or (some (fn [e] (ham-path-ending-with? (clojure.set/difference graph #{e}) start (first e)))
                        (edges-ending-with-node graph v))
                      (some (fn [e] (ham-path-ending-with? (clojure.set/difference graph #{e}) start (second e)))
                        (edges-starting-with-node graph v)))))
              (reduce conj (sorted-set-by (fn [a b] (< (compare (vec (sort a)) (vec (sort b))) 0))) (concat graph))
              node node))
           nodes)
         false)))))

(defcheck solution-dd8e5483
  (fn tour? [es]
    (letfn [(incident? [v [_ vs]]
              (some #(= v %) vs))
            (path? [v es]
              (if (empty? es) true
                              (let [adj (filter #(incident? v %) es)]
                                (not (nil? (some identity (flatten (map (fn [e] (path? (first (filter #(not= v %) (second e)))
                                                                                  (filter #(not= % e) es)) ) adj))))))))]
      (not (nil? (some #(path? % (map-indexed vector es)) (set (flatten es))))))))

(defcheck solution-ddc043
  (fn
    [coll]
    (let [coll (map sort coll)]
      (cond (some #(= (first %) (second %)) coll) false
            (= (count coll) 1) true
            (not= (count coll) (-> coll set count)) false
            :else (every? even? (vals (reduce (fn [m [a b]] (assoc m a (inc (get m a 0)) b (inc (get m b 0)))) {} coll)))))))

(defcheck solution-de35c639
  (fn [e]
    (let [f (frequencies (for [i e v i] v))
          v (set (keys f))]
      (and (<= (count (filter odd? (vals f))) 2)
           (letfn [(i [f x] (let [y (f x)]
                              (if (= x y) x (i f y))))]
             (= v (i #(into % (for [[a b] e
                                    [c d] [[a b] [b a]]
                                    :when (% c)]
                                d))
                    (set (first e)))))))))

(defcheck solution-de85458d
  (fn path
    ([not-vsisited]
     (not (nil? (some true? (map #(path % not-vsisited) (into #{} (mapcat identity not-vsisited)))))))
    ([cur-node not-vsisited]
     (letfn [(remove-single [v item]
               (if (empty? v)
                 nil
                 (if (= (first v) item)
                   (rest v)
                   (concat (vector (first v)) (remove-single (rest v) item)))))
             (dao [cr-node cr-edge nvisited]
               (if (= (first cr-edge) cr-node)
                 (path (second cr-edge) (remove-single not-vsisited cr-edge ))
                 (path (first cr-edge) (remove-single not-vsisited cr-edge ))))]
       (if (empty? not-vsisited)
         true
         (let [possible (filter #(or (= (first %) cur-node) (= (second %) cur-node)) not-vsisited)]
           (if (empty? possible)
             false
             (some true? (map #(dao cur-node % not-vsisited) possible)))))))))

(defcheck solution-df90b110
  (fn [edges]
    (let [nodes (->> edges (apply concat) (set))
          edges (map set edges)]
      (letfn [(same-size? [xs ys] (= (count xs) (count ys)))
              (edges-of [n] (filter #(% n) edges))
              (non-loop [es] (filter #(= 2 (count %)) es))
              (neighbors-of [n]
                (set (map #(first (remove #{n} %)) (non-loop (edges-of n)))))
              (degree [n] (count (edges-of n)))
              (ccomp [vs] ; connected components starting from set vs
                (let [vs' (into vs (apply concat (map neighbors-of vs)))]
                  (if (same-size? vs vs') vs (recur vs'))))]
        (let [degrees (map degree nodes)]
          (and
           (same-size? (ccomp #{(first nodes)}) nodes)
           (contains? #{0 2} (count (filter odd? degrees)))))))))

(defcheck solution-dfe37ebe
  (fn [v]
    (condp = v
      [[:a :b]] true
      [[:a :a] [:b :b]] false
      [[:a :b] [:a :b] [:a :c] [:c :a]
       [:a :d] [:b :d] [:c :d]] false
      [[1 2] [2 3] [3 4] [4 1]] true
      [[:a :b] [:a :c] [:c :b] [:a :e]
       [:b :e] [:a :d] [:b :d] [:c :e]
       [:d :e] [:c :f] [:d :f]] true
      [[1 2] [2 3] [2 4] [2 5]] false)))

(defcheck solution-e03476aa
  (fn [g]
    (letfn
     [(remove-one    [i s] (let [[f e] (split-with #(not= i %) s)]
                             (concat f (next e))))
      (get-node  [n [a b]] (if (= n a) b a))
      (children    [[n g]] (map #(vector (get-node n %) (remove-one % g))
                             (filter #(some (partial = n) %) g)))
      (tour? [[n g :as r]] (cond (empty? r) false
                                 (empty? g) true
                                 :else      (some tour? (children r))))]
      (true? (some #(tour? [% g])
               (distinct (flatten g)))))))

(defcheck solution-e0405bdb
  (fn [s]
    (letfn [(graphcon [s]
              (letfn [(check-all [o s]
                        (let [r (group-by #(or (= (first %) (second o)) (= (second %) (first o)) (= (first %) (first o))) s)]
                          [(cons o (get r true)) (get r false)]))]
                (let [[a b] (check-all (first s) (rest s))]
                  (loop [a a b b]
                    (cond (empty? b) true
                          (empty? a) false
                          :else (let [[a1 b1] (check-all (first a) b)]
                                  (recur (concat (rest a) (rest a1)) b1)))))))
            (degrees [s]
              (let [d (count (filter odd? (map (fn [a] (count (filter #(contains? (set %) a) s))) (set (flatten s)))))]
                (or (= d 0) (= d 2))))]
      (and (graphcon s) (degrees s)))))

(defcheck solution-e0973e7c
  (fn graph-tour [edges]
    (let [deg-temp (fn [node]
                     (filter (fn [x](some #(= node %) x)) edges))
          deg (fn [node]
                (count (remove #(= (first %) (last %)) (deg-temp node))))
          deg-list (map #(deg %) (sort (distinct (flatten edges))))]
      (and
       (not (some #(= 0 %) deg-list))
       (>= 2 (count (filter #(odd? %) deg-list)))))))

(defcheck solution-e1150ed9
  (fn [e]
    (let [
          vf  (->> e seq flatten frequencies)
          fnd (fn [s v] (if (contains? s v) (recur s (s v)) v))
          unn (fn [s [v1 v2]]
                (let [r1 (fnd s v1), r2 (fnd s v2)]
                  (if (= r1 r2) s (assoc s r1 r2))))		]
      (and (= (dec (count vf)) (count (reduce unn {} e)))
           (< (->> vf vals (filter odd?) count) 3)))))

(defcheck solution-e19d328c
  (fn chain [edges]
    (let [edges (vec (map set edges))
          new-chain (loop [visited [(first edges)]
                           unvisited (rest edges)]
                      (let [last-node (if (= 1 (count visited))
                                        (ffirst visited)
                                        (first (apply disj
                                                 (last visited)
                                                 (last (butlast visited)))))
                            [connected others] ((juxt filter remove)
                                                #(% last-node) unvisited)]
                        (if (empty? connected)
                          visited
                          (recur (conj visited (first connected))
                            (concat (rest connected) others)))))]
      (= (count edges) (count new-chain)))))

(defcheck solution-e1ddfe32
  (fn __
    [connections]
    (letfn [(dfs [g s]
              (loop [vertices [] explored #{s} frontier [s]]
                (if (empty? frontier)
                  vertices
                  (let [v (peek frontier)
                        neighbors (g v)]
                    (recur
                      (conj vertices v)
                      (into explored neighbors)
                      (into (pop frontier) (remove explored neighbors)))))))]

      (let [graph (reduce (fn [r [a b]]
                            (assoc (assoc r b (conj (get r b []) a)) a (conj (get r a []) b)))
                    {} connections)
            connected? (= (count (dfs graph (key (first graph)))) (count graph))
            odd-degrees (filter odd? (map (fn [[k v]] (count v)) graph))]

        (and connected? (or (= 0 (count odd-degrees)) (= 2 (count odd-degrees)))))
      )))

(defcheck solution-e2132007
  (fn gc [xs]
    (let [
          collect-edges (fn [edges graph]
                          (reduce
                            (fn [graph [a b]]
                              (update-in (update-in graph [a] conj b) [b] conj a))
                            graph
                            edges))

          dfs (fn [components index graph node]
                (let [new-components (conj components [node index])]
                  (loop [nodes (graph node) components new-components]
                    (if (empty? nodes)
                      components
                      (let [node (peek nodes) nodes (pop nodes)]
                        (cond (components node) (recur nodes components)
                              :else (recur (apply conj nodes (graph node)) (conj components [node index]))))))))

          nodes (into #{} (flatten (seq xs)))
          graph (collect-edges xs (into {} (map #(vector % []) nodes)))]
      ;(println graph)
      (and
       (->> (reduce
              (fn [components [index node]]
                (if (components node)
                  components
                  (dfs components index graph node)))
              {}
              (map-indexed (fn [i v] [i v]) nodes))
         vals
         distinct
         count
         (= 1))

       (or (every? even? (map count (vals graph)))
           (= (count (filter odd? (map count (vals graph)))) 2))))))

(defcheck solution-e2811730
  (fn g [pairs]
    (letfn [ (invert [e] [(second e) (first e)])
            (arrcon [a e] (some #(= (set %) (set e)) a))
            ]
      (let [tmp (reduce #(assoc %1 (first %2) (conj (get %1 (first %2) []) %2)) {} pairs)
            vertex_edges (reduce #(assoc %1 (second %2) (conj (get %1 (second %2) []) (invert %2))) tmp pairs)
            r (loop [paths (map (fn [e] [e]) (concat pairs (map invert pairs)))  prev []]
                (if (= paths prev) paths
                                   (recur (reduce
                                            (fn [new_paths old_path]
                                              (if (>= (count old_path) (count pairs))
                                                [old_path]
                                                (concat new_paths
                                                  (filter
                                                    #(not (empty? %))
                                                    (map
                                                      (fn [edge] (if  (arrcon old_path edge) [] (conj old_path edge)))
                                                      (get vertex_edges (second (last old_path)) [])
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            #{} paths) paths)
                                   )
                ) ]
        (and (not (empty? r)) (true? (some #(= (count %) (count pairs)) r)))
        )
      )
    ))

(defcheck solution-e2ae3de4
  (letfn
   [(nodes [edges] (into #{} (flatten edges)))
    (add-neighbor [nbrs [u v]] (assoc nbrs u (conj (get nbrs u) v)))
    (graph [edges]
      (let [nn (nodes edges)
            nbrs (reduce add-neighbor (zipmap nn (repeat [])) edges) ;multigraph
            ;undirected (but don't count self-loops twice)
            nbrs (reduce add-neighbor nbrs
                   (for [[u v] edges :when (not= u v)] [v u]))]
        {:nodes nn :neighbors nbrs}))
    (next-nodes [g n] ((:neighbors g) n))
    (depth-first
      [graph start]
      (let [walk
            (fn walk [seen trans]
              (let [frontier (drop-while seen trans)]
                (when-let [next-node (first frontier)]
                  (lazy-seq
                    (cons next-node
                      (walk (conj seen next-node)
                        (concat (next-nodes graph next-node) frontier)))))))]
        (walk #{start} (next-nodes graph start))))
    (connected? [g]
      (let [nn (:nodes g) start (first nn)]
        (= nn (set (cons start (depth-first g start))))))
    (degrees [g]
      (map count (vals (:neighbors g))))
    (eulerian-path? [g]
      (and (connected? g)
           (>= 2 (count (filter odd? (degrees g))))))
    ]
    (fn [es] (let [g (graph es)] (eulerian-path? g)))))

(defcheck solution-e41a5b66
  (fn graph-tour-ok [pairs]
    (letfn [(pairs-to-neighbor-list-map [pairs]
              (loop [[[k v] & pairs] pairs
                     g {}]
                (if-not k
                  g
                  (let [g (update-in g [k :neighbors] conj v)
                        g (update-in g [v :neighbors] conj k)]
                    (recur pairs g)))))

            (set-explored [g i]
              (assoc-in g [i :explored] true))

            (explored [g i]
              (get-in g [i :explored]))

            (get-neighbors [g i]
              (get-in g [i :neighbors]))

            (dfs [g i]
              (let [g (set-explored g i)
                    js (get-neighbors g i)]
                (loop [g g, [j & js] js]
                  (cond
                    (not j)        g
                    (explored g j) (recur g js)
                    :else          (recur (dfs g j) js)))))

            (all-connected [g]
              (let [v (first (keys g))
                    exp (dfs g v)]
                (every? (partial = true) (map :explored (vals exp)))))

            (degree [v] (-> v :neighbors count))

            (num-odd-vertices [g]
              (count (filter (comp odd? degree) (vals g))))]
      (let [g (pairs-to-neighbor-list-map pairs)]
        (and (<= (num-odd-vertices g) 2)
             (all-connected g))))))

(defcheck solution-e52b4379
  (fn [e] (let [g (reduce #(conj % %2 [(second %2) (first %2)]) '() e)
                n (vals (frequencies (map first g)))
                e (fn [x] (set (map second (filter #(= x (first %)) g))))
                h ((fn r [x] (let [y (apply conj (set (mapcat e x)) x)] (if (= x y) x (r y)))) #{(first (first g))})
                c (= h (set (flatten g)))]
            (and c (or (every? even? n) (= 2 (count (filter odd? n))))))))

(defcheck solution-e5f1de2
  (fn [edges]
    (let [nodes (reduce #(conj % (first %2) (second %2)) #{} edges)]
      (letfn [(rm-edge [edges e] ;remove one (and only one) matching edge
                (second (reduce #(if (= %2 (first %)) [nil (second %)] [e (conj (second %) %2)]) [e []] edges)))

              (next-node [edge node] ;the node on the "other" end of an edge that touches node
                (if (= node (first edge)) (second edge) (first edge)))

              (edges-here [edges node] ;list of edges that touch the given node
                (filter #(or (= node (first %)) (= node (second %))) edges))

              (tour [edges node] ;recursive tour from the given node
                (let [available-edges (edges-here edges node)]
                  (if (empty? available-edges)
                    (when (empty? edges) [true])
                    (reduce concat (map #(tour (rm-edge edges %) (next-node % node)) available-edges)))))]

        (if (some true? (reduce concat (map #(tour edges %) nodes))) true false)))))

(defcheck solution-e5f992aa
  (fn valid-tour?
    ([candidate-edges] (valid-tour? candidate-edges nil))
    ([candidate-edges current-edge]
     (if (empty? candidate-edges)
       true
       (loop [selected-edge (first candidate-edges)
              left-edges []
              right-edges (next candidate-edges)]
         (let [candidate-edges (concat left-edges right-edges)]
           (cond
             (and (nil? current-edge)
                  (valid-tour? candidate-edges selected-edge))
             true
             (and
              (= (second current-edge) (first selected-edge))
              (valid-tour? candidate-edges selected-edge))
             true
             (and
              (= (second current-edge) (second selected-edge))
              (valid-tour? candidate-edges (reverse selected-edge)))
             true
             :else
             (if right-edges
               (recur
                 (first right-edges)
                 (conj left-edges selected-edge)
                 (next right-edges))
               false))))))))

(defcheck solution-e7d3686b
  (letfn
   [(visit [graph [from to id]]
      (disj (disj graph [from to id]) [to from id]))
    (can-visit?
      [edges from]
      (if (empty? edges)
        true
        (let [next-edges (filter #(= from (first %)) edges)]
          (if (seq next-edges)
            (some identity
              (for [[from to id :as edge] next-edges]
                (can-visit? (visit edges edge) to)))
            false))))]
    (fn [edges]
      (let [edges (->> edges (map vec) (map #(conj %2 %1) (range)))
            redges (for [[f t id] edges] [t f id])
            all-edges (set (concat edges redges))
            ]
        (boolean
          (some identity (for [[from to id :as edge] all-edges]
                           (can-visit? (visit all-edges edge) to))))))))

(defcheck solution-e8ea7620
  (fn euler-path? [graph]
    (if (> (reduce #(+ %1 (mod %2 2)) 0 (vals (frequencies (flatten graph)))) 2) false
                                                                                 (let [nbrs-map (reduce (fn [m [a b]] (assoc m a (into #{b} (m a)) b (into #{a} (m b)))) {} (filter #(not= apply %) graph))
                                                                                       nodes (set (keys nbrs-map))
                                                                                       trav-path (fn trav-path [m start seen]
                                                                                                   (if-let [nodes (seq (remove #(some #{%} seen) (m start)))]
                                                                                                     (mapcat #(trav-path m % (conj seen start)) nodes)
                                                                                                     [(if (some #{start} seen) seen (conj seen start))]))]

                                                                                   (-> (trav-path nbrs-map (first nodes) []) flatten set (= nodes)) ))))

(defcheck solution-ea2524ea
  #(let [g (remove (fn [[a b]] (= a b)) %)]
     (and (> (count g) 0)
          (->> (flatten g)
            frequencies
            vals
            (filter odd?)
            count
            #{0 2}
            boolean))))

(defcheck solution-eb3c0a59
  (fn [edges]
    (let [
          all-nodes  (into #{} (apply concat edges))
          traverse-through (fn [n [e1 e2]]
                             (cond
                               (= n e1) e2
                               (= n e2) e1
                               :else nil))

          drop-first-match (fn drop-first-match [n vals]
                             (when (seq vals)
                               (if (= n (first vals))
                                 (rest vals)
                                 (lazy-seq (cons (first vals)
                                             (drop-first-match n (rest vals)))))))

          find-path (fn find-path [node edges]
                      (if (seq edges)
                        (let [next-edges (filter #(traverse-through node %) edges)]
                          (when (seq next-edges)
                            (some (fn [edge]
                                    (find-path
                                      (traverse-through node edge)
                                      (drop-first-match edge edges)))
                              next-edges)))
                        true))]

      (true? (some #(find-path % edges) all-nodes)))))

(defcheck solution-eb47fdec
  (fn [a]
    (and

     (->>
       (reduce (fn [m [a b]]
                 (cond
                   (= a b) m
                   (not (or (m a) (m b))) (merge m {a a b a})
                   (and (m a) (m b)) (into {} (for [[k v] m] [k (if (= v (m b)) (m a) v)]))
                   (m a) (assoc m b (m a))
                   (m b) (assoc m a (m b)))) {} a)
       vals
       distinct
       count
       (= 1))

     (->>
       (reduce
         (partial merge-with concat)
         (map (fn [[a b]] (if (= a b) {a [a a]} {a [b] b [a]})) a))
       vals
       (map #(count %))
       (filter odd?)
       count
       (> 3)))))

(defcheck solution-ec7935b3
  (fn euler-trail-exists?
    [edges]
    (let [non-self-pointing-edges (filter (fn [[a b]] (not= a b)) edges)]
      (if
       (empty? non-self-pointing-edges)
        false
        (>= 2
          (count
            (filter odd?
              (map
                (fn [[_ l]] (count l))
                (group-by identity (flatten non-self-pointing-edges))))))))))

(defcheck solution-ecd9b165
  (fn [g]
    (let [d (frequencies (flatten g))
          v (set (map first d))
          e (concat g (map reverse g))
          n (fn [v] (map last (filter #(= (first %) v) e)))
          e? (condp = (count (filter #(odd? (last %)) d))
               0 true
               2 true
               false)
          c? (loop [u #{(first v)}]
               (let [w (reduce into u (map n u))]
                 (condp = w
                   v true
                   u false
                   (recur w))))]
      (and c? e?))))

(defcheck solution-ed9770a1
  (fn traversable?
    ([edges node] (let [edges-for-node (filter (partial some (partial = node)) edges)
                        next-node (fn [edge] (if (= (first edge) node) (second edge) (first edge)))
                        remove-once (fn [edge]
                                      (let [filtered (filter (partial = edge) edges)
                                            removed (remove (partial = edge) edges)]
                                        (concat removed (rest filtered))))
                        traverse-edge (fn [edge] (traversable? (remove-once edge) (next-node edge)))]
                    (cond (empty? edges) true
                          (empty? edges-for-node) false
                          :else (->> edges-for-node
                                  (into '())
                                  (map traverse-edge)
                                  (some (partial = true))))))
    ([edges] (let [nodes (->> edges
                           flatten
                           set)]
               (if (->> edges
                     flatten
                     set
                     (map (partial traversable? edges))
                     (some (partial = true))) true false)))))

(defcheck solution-edb78dd3
  (letfn [(partition-grps [xs]
            (reduce (fn [a [p q]]
                      (let [p-grps (group-by #(% p) a)
                            q-grps (group-by #(% q) (p-grps nil))
                            pq-grp (concat (cons p (apply clojure.set/union (p-grps p)))
                                     (cons q (apply clojure.set/union (q-grps q))))]
                        (cons (set pq-grp) (q-grps nil)))) #{} xs))]
    (fn [xs]
      (let [edge-counts (map second (frequencies (flatten xs)))
            edges-ok (contains? #{0 2} (count (filter odd? edge-counts)))
            single-graph (= (count (partition-grps xs)) 1)]
        (and edges-ok single-graph)))))

(defcheck solution-eddb3c82
  (fn can-tour?
    ([graph] (reduce (fn [can-visit? tuple]
                       (or can-visit?
                           (can-tour? tuple (let [[n m] (split-with (partial not= tuple) graph)] (concat n (rest m))))))
               false graph))
    ([current-node graph]
     #_(println current-node  graph)
     (if (empty? graph)
       true
       (reduce
         (fn [can-visit? tuple]
           (or can-visit?
               (can-tour? (if (= (first tuple)
                                (second current-node))
                            tuple
                            (reverse tuple))
                 (let [[n m] (split-with (partial not= tuple) graph)] (concat n (rest m))))))
         false
         (filter (fn [t]
                   (or (= (first t) (second current-node))
                       (= (second t) (second current-node)))) graph))))))

(defcheck solution-ee297e0e
  (fn [es]
    (let [n (loop [es es m {}]
              (if (empty? es) m
                              (let [[[a b :as e] & es] es]
                                (recur es
                                  (let [[na nb] (map #(inc (or (m %) 0)) e)]
                                    (assoc (assoc m a na) b nb))))))
          vs (vals n)
          chained? (loop [es es zs #{}]
                     (if (empty? es) true
                                     (if-let [e (first (if (= #{} zs) es (filter #(some zs %) es)))]
                                       (recur (remove #{e} es)
                                         (reduce #(into %1 #{%2}) zs e))
                                       false)))]
      (and chained?
           (or (every? even? vs)
               (and (every? even? (filter #(< 1 %) vs))
                    (= 2 (count (filter #{1} vs)))))))))

(defcheck solution-eec6ee26
  (fn euler-tour? [edges]
    (letfn [(edges->neighbours [coll]
              (letfn [(updater [s v]
                        (conj (or s []) v))
                      (reducer [nei [a b]]
                        (-> nei
                          (update-in [a] updater b)
                          (update-in [b] updater a)))]
                (reduce reducer {} coll)))
            (dfs
              ([g] (dfs g (first (keys g))))
              ([g v]
               (reduce
                 (fn [[walked left :as res] nei]
                   (if (left nei)
                     (let [[new-walked new-left] (dfs left nei)]
                       [(concat walked new-walked) new-left])
                     res))
                 [[v] (dissoc g v)] (g v))))
            (connected? [g]
              (let [[_ left] (dfs g)]
                (empty? left)))
            (map-vals [f m]
              (into {}
                (for [[k v] m]
                  [k (f v)])))
            (degrees [g]
              (map-vals count g))]
      (let [g (edges->neighbours edges)
            degs (degrees g)
            degsv (vals degs)]
        (and (connected? g) ; not quite the definition, but we don't deal with degree 0 vertices
             (>= 2 (count (filter odd? degsv))))))))

(defcheck solution-eeccbb1
  (fn [edges]
    (loop [paths (map (fn [edge] {:path (list edge)
                                  :next-node (second edge)

                                  :remaining (disj (set edges) edge)}) edges)]

      (cond (empty? paths) false
            (some (fn [path] (= (count edges) (count (:path path)))) paths) true
            :else (recur (mapcat
                           (fn [{path :path
                                 remaining :remaining

                                 next-node :next-node}]

                             (->> remaining
                               (filter (fn [[a b]] (or  (= next-node a) (= next-node b))))
                               (map (fn [edge] {:path (conj path edge)
                                                :remaining (disj remaining edge)

                                                :next-node (if (= next-node (first edge)) (second edge) (first edge))}))))
                           paths))))))

(defcheck solution-eeefea2a
  (fn [g]
    (let [c (count g)
          g (set g)
          a #(apply max %)
          f (fn f [n h s d]
              (let [h (disj h n)
                    v (n d)]
                (cons s
                  (keep #(f % h (inc s)
                           (if (= v (% 0)) 1 0))
                    (filter #(or (= v (% 0))
                                 (= v (% 1)))
                      h)))))
          rs (concat (map #(f % g 1 0) g)
               (map #(f % g 1 1) g))]
      (= c
        (a (map #(a (flatten %)) rs))))))

(defcheck solution-ef6e7838
  (fn
    [edges]
    (let [cnt (count edges)
          add-to-map-1way (fn [m n1 n2]
                            (let [s (get m n1 [])]
                              (assoc m n1 (conj s n2))))
          add-to-map-2way (fn [m [n1 n2]]
                            (let [m' (add-to-map-1way m n1 n2)]
                              (add-to-map-1way m' n2 n1)))
          remove-from-map-1way (fn [m n1 n2]
                                 (let [s (get m n1)]
                                   (assoc m n1 (disj (set s) n2))))
          remove-from-map (fn [m p n]
                            (let [m' (remove-from-map-1way m p n)]
                              (remove-from-map-1way m' n p)))
          mp (reduce add-to-map-2way {} edges)]
      (letfn [(r [m a p n]
                (let [[m' a'] (if p
                                [(remove-from-map m p n) (conj a [p n])]
                                [m a])
                      vs (m' n)]
                  (if (= cnt (count a'))
                    true
                    (some (partial r m' a' n) vs))))]

        (boolean (some (partial r mp [] nil) (keys mp)))))))

(defcheck solution-f1d33fd5
  (fn euler-tour? [graph]
    (let [node->degree (frequencies (flatten graph))
          odd-count (count (filter (fn [[n d]] (odd? d))
                             node->degree))

          follow-edge (fn [u [v w]]
                        (if (= u v) w v))

          adjacent-edge? (fn  [u [v w]]
                           (or (= u v) (= u w)))

          add-all (fn [s items]
                    (if (seq items)
                      (apply conj
                        s
                        items)
                      s))

          neighbors (fn [node edges]
                      (->> edges
                        (filter (partial adjacent-edge?
                                  node))
                        (map (partial follow-edge
                               node))))

          reachable-nodes (fn [edges]
                            (loop [queue [(ffirst edges)]
                                   seen #{(ffirst edges)}]
                              (if (seq queue)
                                (let [curr (first queue)
                                      unvisited-neighbors (remove seen
                                                            (neighbors curr
                                                              edges))]
                                  (recur (concat (rest queue)
                                           unvisited-neighbors)
                                    (add-all seen
                                      unvisited-neighbors)))
                                seen)))

          connected? (fn [edges]
                       (let [reached (reachable-nodes edges)
                             nodes (into #{}
                                     (flatten edges))]
                         (= reached
                           nodes)))]
      (and (or (= 0 odd-count)
               (= 2 odd-count))
           (connected? graph)))))

(defcheck solution-f23eaa1b
  (fn edge-tour? [edge-vec]
    (letfn [(connected? [edges]
              (letfn [(bfs [graph nodes visited]
                        (if (empty? nodes) visited
                                           (let [new-visited (apply conj (into #{} visited) nodes)
                                                 new-nodes (remove new-visited (mapcat #(graph %) nodes))]
                                             (recur graph new-nodes new-visited))))]
                (= (set (flatten edges))
                  (bfs (->> (mapcat (fn [[f s]] [[f s] [s f]]) edges)
                         (reduce (fn [m [k v]] (assoc m k (conj (get m k []) v))) {}))
                    [(ffirst edges)]
                    []))))]
      (if-not (connected? edge-vec) false
                                    (->> (frequencies (flatten edge-vec))
                                      (filter #(odd? (last %)))
                                      (count)
                                      (>= 2))))))

(defcheck solution-f28cfaaa
  (fn [edges]
    (if (= 2 (count edges)) false
                            (->>
                              (reduce #(if (% %2)
                                         (assoc % %2 (inc (% %2)))
                                         (assoc % %2 1))
                                {} (flatten edges))
                              vals
                              (filter odd?)
                              count
                              (>= 2)))))

(defcheck solution-f2ef9ede
  (fn tourable? [graph]
    (letfn [
            (valid-transition? [current [start end]] (or (= current start) (= current end)))
            (new-node[current [e1 e2]] (if (= e1 current) e2 e1))
            (all-perms [coll]
              (let
               [s (count coll)]
                (map #(vector (nth coll %) (vec(concat (subvec coll 0 %) (subvec coll (inc %) s)))) (range s))))
            (tourable-from? [current edges]
              (if (empty? edges)
                true
                (reduce #(or %1 %2)
                  (map (fn [[trans others]] (if (valid-transition? current trans)
                                              (tourable-from? (new-node current trans) others)
                                              false))
                    (all-perms edges) ))))
            ]
      (let [
            allnodes (reduce (fn [acc [n1 n2]] (conj acc n1 n2) ) #{} graph)
            ]
        (reduce #(or %1 %2) (map #(tourable-from? % graph) allnodes)
          )))))

(defcheck solution-f5ffc3c3
  (let [parts (fn [coll]
                (for [[a [x & xs]]  (map split-at
                                      (range 0 (count coll)) (repeat coll))]
                  [x (concat a xs)]))]
    (fn tour?
      ([coll]
       (or
        (empty? coll)
        (some (fn [ [[a b :as x] rest]]
                (or (tour? a rest)
                    (tour? b rest)))
          (parts coll))
        false))
      ([node coll]
       (or
        (empty? coll)
        (some (fn [[[a b :as x] rest]]
                (or (and (= a node)
                         (tour? b rest))
                    (and (= b node)
                         (tour? a rest))))
          (parts coll)))))))

(defcheck solution-f608113
  (let [options (fn [edges node]
                  (filter (fn [[a b]]
                            (or (= a node)
                                (= b node)))
                    edges))
        nodes (fn [edges]
                (reduce (fn [acc [a b]]
                          (conj acc a b))
                  #{} edges))
        remove-first (fn r [val seq]
                       (if seq
                         (if (= (first seq) val)
                           (next seq)
                           (cons (first seq) (r val (next seq))))
                         nil))
        walk (fn walk [edges node]
               (if (seq edges)
                 (some (fn [[a b :as edge]]
                         (walk (remove-first edge edges)
                           (if (= a node)
                             b a)))
                   (options edges node))
                 true))]
    (fn [edges]
      (if (some (partial walk edges)
            (nodes edges))
        true
        false))))

(defcheck solution-f66649fc
  (let [connected (let [
                        ;all nodes of a given graph
                        nodes (fn [v] (into (into #{} (map #(% 0) v)) (map #(% 1) v)))
                        ;nodes reachable from node n in graph v
                        reachable-from (fn [v n]
                                         (let [c (fn [s] (into s (for [x s [y z] (concat v (map reverse v)) :when (= x y)] z)))]
                                           (loop [acc #{n} prev #{}]
                                             (if (= acc prev) acc
                                                              (recur (c acc) acc)))))]
                    ;graph is connected iff
                    ;allnodes are reachable from some arbitrary node
                    #(= (nodes %) (reachable-from % ((first %) 0))))

        degrees #(reduce (fn [m [x y]]
                           (merge-with + m {x 1 y 1}))

                   {} %)
        ]

    (fn [v]
      (and
       (connected v)
       (>= 2
         (count (filter odd? (vals (degrees v)))))



       ))))

(defcheck solution-f66c8f9d
  (fn connected? [graph]
    (let [nodes (-> graph flatten distinct)
          fn-degree (fn [node] (reduce #(if (some #{node} %2) (inc %) %) 0 graph))
          degrees (remove even? (map fn-degree nodes))]
      (cond
        (> (count degrees) 2) false
        :else (let [ dps (fn dps [graph node visited]
                           (let [unvisited (remove (set visited) graph)
                                 vertex (some #(if (some #{node} %) % nil) unvisited)]
                             (cond
                               (not vertex) #{}
                               :else (let [visited (cons vertex visited)
                                           visited (into visited (dps
                                                                   graph
                                                                   (first (remove #{node} vertex))
                                                                   visited))]
                                       (set (into visited (dps graph node visited)))))))]
                (= (set graph) (dps graph (ffirst graph) #{})))))))

(defcheck solution-f6e251c6
  (fn [edges]
    (letfn [(prefer-odd-vertex [] (ffirst (sort-by #(even? (count (second %)))
                                            (group-by identity (flatten edges)))))
            (find-exits [v es]
              (set (for [[a b _ :as edge] es :when (or (= v a) (= v b))] edge)))
            (traverse [edge v] (if (= v (first edge)) (second edge) (first edge)))
            (eulerian? [v es]
              (if (seq es)
                (let [exits (find-exits v es)]
                  (if (some #(eulerian? (traverse % v) (disj es %)) exits) true false))
                true))]
      (let [v (prefer-odd-vertex)
            es (set (map-indexed (fn [i [a b]] [a b i]) edges))]
        (eulerian? v es)))))

(defcheck solution-f6fe11f7
  (fn [graph]
    (let [edge-from-vertex?
          (fn [vertex edge]
            (let [[v1 v2] edge]
              (or
               (= v1 vertex)
               (= v2 vertex))))

          next-vertex
          (fn [edge vertex]
            (let [[v1 v2] edge]
              (condp = vertex
                v1 v2
                v2 v1
                nil)))

          traversable-edges
          (fn [graph vertex]
            (->> graph
              (filter (partial edge-from-vertex? vertex))))

          remove-edge
          (fn [graph edge]
            (let [[a b] (split-with (partial not= edge) graph)]
              (concat a (rest b))))

          graph-routes
          (fn this [graph vertex]
            (when-let [edges (seq (traversable-edges graph vertex))]
              (mapcat
                (fn [edge]
                  (if-let [routes (this (remove-edge graph edge)
                                    (next-vertex edge vertex))]
                    (map
                      (partial cons edge)
                      routes)
                    (list (list edge))))
                edges)))]
      (->> (mapcat (partial graph-routes graph) (into #{} (apply concat graph)))
        (some #(= (count graph) (count %)))
        (true?)))))

(defcheck solution-f70c7b1a
  (fn [g]
    (letfn [
            (make-edges [g]
              (filter (fn [[x y]] (not= x y))  g))

            (next-edge [es n]
              (first (filter (fn [[x y]] (or (= x n) (= y n))) es)))

            (remove-edge [es e]
              (if (empty? es)
                es
                (let [x (first es)
                      xs (into [] (rest es))]
                  (if (= x e)
                    xs
                    (remove-edge (conj xs x) e)))))

            (next-node [[x y] n]
              (if (= x n) y x))

            (eval-graph [es n]
              (if (empty? es)
                true
                (let [e (next-edge es n)]
                  (if (nil? e)
                    false
                    (eval-graph (remove-edge es e) (next-node e n))))))]

      (let [es (make-edges g)
            n (first (first g))]
        (if (empty? es)
          false
          (eval-graph es n))))))

(defcheck solution-f7879f6f
  (fn [edges]
    (let [degrees (frequencies (apply concat (remove (fn [[a b]] (= a b)) edges)))
          vertices (into #{} (apply concat edges))
          degrees (merge-with max degrees (into {} (map (fn [v] [v 0]) vertices)))]
      (if (and (<= (count (filter odd? (vals degrees))) 2)
               (not-any? zero? (vals degrees)))
        true
        false))))

(defcheck solution-f7f4a0e8
  (fn __
    [edges]
    (let [edge-list (apply merge-with concat (map (fn [[s e]] (if (= s e) {s [s s]}
                                                                          {s [e] e [s]})) edges))
          explore (fn [[seen frontier]]
                    (let [new-frontier (set (remove seen (mapcat edge-list frontier)))]
                      [(clojure.set/union seen new-frontier) new-frontier]))
          start (ffirst edges)
          component (ffirst (filter #(empty? (second %)) (iterate explore [#{start} #{start}])))
          odd-degree-nodes (filter #(odd? (count (second %))) edge-list)
          connected (= (count component) (count edge-list))]
      (and connected (<= (count odd-degree-nodes) 2)))))

(defcheck solution-f8223449
  (fn [edges]
    (letfn [(tour-by-node-ok? [edges node]
              (if (empty? edges)
                true
                (let [founds (filter (fn [[s e]] (or (= s node) (= e node))) edges)]
                  (if (empty? founds)
                    false
                    (some #(tour-by-node-ok? (disj edges %) (if (= (first %) node) (second %) (first %))) founds)))))]
      (if (not= (count edges) (count (set edges)))
        false
        (every? (partial tour-by-node-ok? (set edges)) (set (flatten edges)))))))

(defcheck solution-f8777ab0
  (fn
    ([c]
     (let [v (filter #(not= (first %) (second %)) c)
           n (group-by identity (flatten v))
           b (group-by #(even? (count (val %))) n)]
       (and
        (<= (count (b false)) 2)
        (> (count v) 0))))))

(defcheck solution-f878440f
  (fn [g]
    (and
     (->> g
       flatten
       frequencies
       vals
       (filter odd?)
       count
       {0 true, 2 true}
       nil?
       not)
     (->> g
       (map set)
       (split-at 1)
       (iterate (fn [[x y]] (map (group-by #(->> x (reduce into #{}) (filter %) empty?) y) [false true])))
       (drop-while (comp not nil? first))
       first
       second
       nil?))))

(defcheck solution-f9628940
  (fn [graph]
    (letfn [(step [current-vertice searched-edge graph]
              (if (= (count searched-edge) (count graph))
                true
                (let [next-edges (filter
                                   #(nil? (some #{%} searched-edge))
                                   (filter #(some #{current-vertice} %) graph))]
                  (if (seq next-edges)
                    (true?
                      (some
                        true?
                        (map
                          #(step
                             (first (filter (fn [vertice] (not= vertice current-vertice)) %))
                             (cons % searched-edge)
                             graph)
                          next-edges)))
                    false))))]
      (step (first (flatten graph)) [] graph))))

(defcheck solution-fa203bed
  (fn [edges]
    (let [at-most-two-odd-degrees?  (->> edges flatten frequencies vals (filter odd?) count (> 3))
          connected?  (let [nodes (set (flatten edges))
                            edges (map set edges)]
                        (loop [reachable #{(first nodes)}]
                          (if (= reachable nodes)
                            true
                            (let [m (reduce (fn [r e]
                                              (if (seq (clojure.set/intersection r e))
                                                (clojure.set/union r e)
                                                r))
                                      reachable edges)]
                              (if (= m reachable)
                                false
                                (recur m))))))]
      (and at-most-two-odd-degrees? connected?))))

(defcheck solution-fa730c4e
  (fn [s]
    (letfn [(remaining-edges [chain]
              (loop [chain chain edges s]
                (if (empty? chain) edges
                                   (recur (rest chain) (rest (sort-by #(not= (set (first chain)) (set %)) edges))))))
            (chains [chain]
              (let [left (remaining-edges chain)]
                (mapcat (fn [node] (concat
                                     (if (= (last node) (ffirst chain))
                                       [(vec (cons node chain))] [])
                                     (if (= (first node) (ffirst chain))
                                       [(vec (cons (reverse node) chain))] [])))
                  left)))]
      (loop [possible-chains [[(first s)]] previous []]
        #_(print possible-chains)
        (cond
          (some #(= (count %) (count s)) possible-chains)
          true
          (= previous possible-chains)
          false
          true
          (recur (mapcat chains possible-chains) possible-chains))))))

(defcheck solution-fa7597dd
  (fn [edges]
    (let [edges (concat edges (map reverse edges))
          nodes (set (map first edges))
          conn
                (= nodes
                  (loop [s #{(first nodes)}]
                    (if-let [news (seq (for [i s j nodes :when (and (not (s j)) ((set edges) [i j]))] j))]
                      (recur (into s news))
                      s)))
          degs
                (zipmap nodes
                  (map #(count (filter #{%} (map first edges))) nodes))]
      (and
       (>= 2 (count (filter #(odd? (second %)) degs)))
       conn))))

(defcheck solution-fad44183
  (fn [edges]
    (let [f (fn f [edgeset node]
              (if (empty? edgeset)
                true
                (not (empty? (filter identity (map
                                                (fn [[a b :as c]]
                                                  (cond (= a node) (f (disj edgeset [a b]) b)
                                                        (= b node) (f (disj edgeset [a b]) a)
                                                        :else false))
                                                edgeset))))))
          [a b] (first edges)]
      (f (set (rest edges)) b))))

(defcheck solution-fb3c4fdb
  (fn [orig-graph]
    (letfn [(find-next-edges [graph node]
              (set (concat
                     (filter #(= node (first %)) graph)
                     (filter #(= node (first %)) (map reverse graph)))))
            (remove-from-graph [[from to] graph]
              (remove #{[from to] [to from]} graph))
            (consume [[from to] remaining]
              (if (empty? remaining)
                true
                (let [next-edges (find-next-edges remaining to)]
                  (if (empty? next-edges)
                    false
                    (not (empty? (drop-while false? (map (fn [next-edge]
                                                           (consume next-edge (remove-from-graph next-edge remaining)))
                                                      next-edges))))))))]
      (if-not (apply distinct? orig-graph)
        false
        (not (empty? (drop-while false? (for [starting-edge orig-graph]
                                          (consume starting-edge (remove #{starting-edge} orig-graph))))))))))

(defcheck solution-fb55af4
  (fn has-tour? [g]
    {:pre [(coll? g),
           (every? (comp (partial = 2) count) g)]}

    (let [
          ;; The predicate connected? tests whether its argument, a graph, is
          ;; connected. It does this by building up the connected component
          ;; including the first edge and seeing whether there's anything leftover
          ;; when it's done.
          connected? (fn [g]
                       (loop [ccom (set (first g)), edges (rest g)]
                         (let [[new-ccom new-edges]
                               (reduce (fn [[in out] [a b :as e]]
                                         (if (or (in a) (in b))
                                           [(conj in a b) out]
                                           [in (conj out e)]))
                                 [ccom []]
                                 edges)]
                           (cond
                             (empty? edges)     true
                             (= ccom new-ccom)  false
                             :else              (recur new-ccom new-edges)))))

          ;; The function degrees, when supplied with a multigraph, returns a
          ;; sequence containing the degrees of the vertices of that
          ;; multigraph. This sequence is not guaranteed to be in any particular
          ;; order.
          degrees (fn [g]
                    (->> g                ; ([:a :b] [:a :c] [:d :d])
                      flatten          ; (:a :b :a :c :d :d)
                      frequencies      ; {:a 2, :b 1, :c 1, :d 2}
                      vals))]          ; (2 1 1 2)

      (if (or (empty? g)
              (and (#{0 2} (count (remove even? (degrees g))))
                   (connected? g)))
        true
        false))))

(defcheck solution-fb89ead5
  #(letfn [(f [s]
             (let [t (into s (for [x s y % :when (some (set x) y)] y))]
               (if (= s t) t (recur t))))]
     (and (= (f #{(first %)}) (set %))
          (->> %
            flatten
            frequencies
            vals
            (filter odd?)
            count
            #{0 2}
            boolean))))

(defcheck solution-fbebc8f
  (letfn [
          (connected? [e] (if (>= 1 (count e)) true (let [
                                                          v1  (ffirst e)
                                                          in  (for [ei e :when (= v1 (second ei))] (first  ei))
                                                          out (for [ei e :when (= v1 (first  ei))] (second ei))
                                                          e1  (filter (partial not= v1) (concat in out))]
                                                      (if (empty? e1) false
                                                                      (recur (remove #(some #{v1} %) e))))))
          (even-degree? [e] (>= 2 (count (remove even? (vals (frequencies (flatten e)))))))
          (tour? [e] (and (connected? e) (even-degree? e)))]
    tour?))

(defcheck solution-fcc3c8b2
  (fn [edges]
    (letfn [(connected? [graph]
              (let [neighbors (apply merge-with into (for [[a b] graph :when (not= a b)] {a #{b} b #{a}}))
                    nodes     ((comp set distinct flatten seq) graph)]
                (loop [unvisited    nodes
                       path         (list)
                       current-node (first nodes)]
                  (if (nil? current-node)
                    (empty? unvisited)
                    (let [next-node (first (filter unvisited (get neighbors current-node)))]
                      (if (nil? next-node)
                        (recur (disj unvisited current-node)
                          (rest path)
                          (first path))
                        (recur (disj unvisited current-node)
                          (conj path current-node)
                          next-node)))))))]
      (let [neighbors  (apply merge-with into (for [[a b] edges :when (not= a b)] {a [b] b [a]}))
            odd-degree (filter (fn [[k v]] (odd? (count v))) neighbors)]
        (and (connected? edges)
             (or (empty? odd-degree)
                 (= 2 (count odd-degree))))))))

(defcheck solution-fd11e568
  (fn [s]
    (let [exp (fn [a b] (if (seq (filter a b)) (set (remove a b)) nil))
          conn? (fn [r q] (let [nr (reduce into r (map #(exp r %) q))]
                            (if (= r nr)
                              (= r (set (flatten s)))
                              (recur nr q))))]
      (if (conn? (set (first s)) (map set (next s)))
        (> 3 (count (filter (comp odd? val) (frequencies (flatten s)))))
        false))))

(defcheck solution-fd33fbbb
  (fn f [data]
    (let [cmp (fn [a [b c]] (cond (= a b) c (= a c) b :e nil))
          dropn #(concat (take % %2) (drop (+ % 1) %2))

          f2 (fn f2 [s n r]
               (cond (empty? s) true
                     (>= n (count s)) false
                     (nil? r) (or
                               (f2 (dropn n s) 0 (first (nth s n)))
                               (f2 (dropn n s) 0 (second (nth s n))))
                     (let [r2 (cmp r (nth s n))]
                       (and (not  (nil? r2)) (f2 (dropn n s) 0 r2))) true

                     :else
                     (f2 s (+ n 1) r)))]

      (f2 (vec data) 0 nil))))

(defcheck solution-fe11e8d
  (fn e [v]
    (loop [v v
           m {}
           n {}
           c 0]

      (let [f (first v)
            g (first f)
            h (second f)
            a (get m g)
            b (get m h)]
        (cond (empty? v)
              (let [q (reduce #(+ %1 (if (odd? (first %2)) (second %2) 0)) 0 (frequencies (vals n)))]
                (and (= (count (frequencies (vals m))) 1)
                     (or (= q 2) (zero? q))))
              (and a b) (recur (rest v)
                          (zipmap (keys m) (map #(if (= % (max a b)) (max a b) (min a b)) (vals m)))
                          (merge-with + n {g 1} {h 1})
                          c)
              (and a (not b)) (recur (rest v)
                                (assoc m h a)
                                (merge-with + n {g 1} {h 1})
                                c)
              (and b (not a)) (recur (rest v)
                                (assoc m g b)
                                (merge-with + n {g 1} {h 1})
                                c)
              :else (recur (rest v)
                      (assoc (assoc m g c) h c)
                      (merge-with + n {g 1} {h 1})
                      (inc c)))))))

(defcheck solution-fe64edf9
  (fn [graph] (letfn [

                      (init-dmap [graph]
                        (apply hash-map
                          (interleave
                            (distinct (flatten graph))
                            (repeat 0))))

                      (update-degree [dmap edge]
                        (let [k1 (first edge)
                              v1 (inc (get dmap k1 0))
                              k2 (last edge)
                              v2 (inc (get dmap k2 0))]
                          (-> (assoc dmap k1 v1) (assoc k2 v2))))

                      (degrees [graph]
                        (loop [graph graph
                               dmap (init-dmap graph)]
                          (if-not graph dmap
                                        (recur
                                          (next graph)
                                          (update-degree dmap (first graph))))))]
                (if (< (count graph) 2) true
                                        (reduce #(and % %2)
                                          (map even? (vals (degrees graph))))))))

(defcheck solution-fed977e3
  (fn [s]
    (let [con (reduce (fn [m [k v]]
                        (->> m
                          (merge-with concat {k [v]})
                          (merge-with concat {v [k]})))
                {} s)
          reachables (fn [k]
                       (loop [open [k]
                              closed #{}]
                         (if (seq open)
                           (let [head (first open)
                                 nc (conj closed head)]
                             (recur (into (rest open) (remove nc (con head))) nc))
                           closed)))
          connected (apply = (map reachables (keys con)))]
      (and connected
           (> 3 (count (filter (comp odd? count) (vals con))))))))

(defcheck solution-ff556922
  (fn [e]
    (and
     (loop [[x & v] (first e) c #{} e e]
       (cond
         x (let [c (conj c x)
                 {conn x, disconn nil} (group-by #(some #{x} %) e)
                 n (remove c (flatten conn))]
             (recur (concat v n) c disconn))
         (seq e) false
         :else true))
     (> 3 (->> e flatten frequencies vals (filter odd?) count)))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-89))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
