(ns coal-mine.problem-91
  (:require [coal-mine.checks :refer [defcheck-91] :rename {defcheck-91 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-10138965
  (fn [i-set]
    (let [
          nodes (reduce
                  #(conj (conj %1 (first %2)) (last %2))
                  #{}
                  i-set)

          n-count (count nodes)
          n-range (range n-count)
          n-map (apply hash-map (interleave nodes n-range))
          n-index (fn [i-node] (get n-map i-node))

          edges (concat i-set (map #(apply vector (reverse %)) i-set))
          grouped-edges (group-by #(n-index (first %)) edges)

          n-dest (fn [i-node]
                   (map last (get grouped-edges (n-index i-node))))
          walk-graph (fn discover
                       ([i-node] (discover #{} (hash-set i-node)))
                       ([result discovered]
                        (if (zero? (count discovered))
                          result
                          (let [
                                current (first discovered)
                                n-result (conj result current)
                                new-nodes (filter #(not (contains? n-result %)) (n-dest current))
                                ]
                            (discover n-result (reduce conj (apply hash-set (rest discovered)) new-nodes))))))
          ]
      (= nodes (walk-graph (first (first i-set)))))))

(defcheck solution-103f2ddd
  (fn [coll]
    (or (empty? coll)
        (= (set (apply concat coll))
          ((fn f [x coll]
             (set (apply concat [x]
                    (for [[a b :as e] coll
                          [y z] [[a b] [b a]]
                          :when (= y x)]
                      (f z (disj coll e)))))
             ) (ffirst coll) coll)))))

(defcheck solution-1042106a
  (fn [graph]
    (let [vertices (set (apply concat graph))]
      (letfn [(vertex-connectness [vertex searched-vertex graph]
                (let [adjacent-vertices (filter #(not (searched-vertex %)) (apply concat (filter #(some #{vertex} %) graph)))]
                  (if (seq adjacent-vertices)
                    (set (mapcat #(vertex-connectness % (set (cons % searched-vertex)) graph) adjacent-vertices))
                    searched-vertex)))]
        (every? #(= vertices (vertex-connectness % #{%} graph)) vertices)))))

(defcheck solution-10c6d0e8
  (fn connected?
    ([s]
     (connected? (set (first s)) (rest s)))
    ([node-set s]
     (if (seq s)
       (let [adjacents (filter #(some node-set %) s)
             others    (remove #(some node-set %) s)]
         (if (seq adjacents)
           (recur (reduce #(clojure.set/union %1 (set %2)) node-set adjacents)
             others)
           false))
       true))))

(defcheck solution-11b2a087
  (fn [pairs]
    (let [connected? (fn connected? [p p']
                       (> 4 (count (distinct (concat p p')))))
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
      (= 1 (count clusters)))))

(defcheck solution-11b769af
  (fn [nodes]
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
     (map set nodes) #{})))

(defcheck solution-123cd4ba
  (fn [edges]
    (if (empty? edges) true ; why not?
                       (let [vertices (reduce (fn [s [a b]] (conj s a b)) #{} edges)
                             steps-from (reduce (fn [m [a b]] (merge-with concat m {a [b]} {b [a]})) {} edges)]
                         (loop [visited #{(first vertices)}]
                           (if (= (count visited) (count vertices))
                             true
                             (let [new-visited (reduce #(into %1 (get steps-from %2)) visited visited)]
                               (if (= (count new-visited) (count visited)) false
                                                                           (recur new-visited)))))))))

(defcheck solution-1253aae1
  (fn [edges]
    (let [m (reduce (fn [m [k v]]
                      (merge-with clojure.set/union m {k (set [v])})) {} edges)
          all-nodes (reduce clojure.set/union (set (keys m)) (vals m))]
      (letfn [(transitive [node]
                (loop [reachable (set [node])]
                  (let [reachable' (reduce clojure.set/union
                                     reachable (map #(get m %) reachable))]
                    (if (= reachable reachable')
                      reachable
                      (recur reachable')))))]
        (not (empty? (filter  #(= (transitive %) all-nodes) all-nodes)))))))

(defcheck solution-12bc9156
  (fn [s]
    (loop [edges (rest s) nodes (set (first s))]
      (let [parts (group-by (fn [[a b]] (not (nil? (or (nodes a) (nodes b))))) edges)
            in (set (apply concat (parts true))) out (set (parts false))
            newnodes (into nodes in)]
        (cond (empty? out) true
              (= newnodes nodes) false
              true (recur out newnodes))))))

(defcheck solution-13c02ff1
  (fn [coll]
    (loop [links
           (reduce
             (fn [m [a b]] (assoc m a (conj (m a) b)))
             (reduce #(assoc %1 %2 #{%2}) {} (distinct (flatten (concat coll))))
             (concat coll (map reverse coll)))]
      (let [new-links (reduce (fn [m [k s]] (assoc m k (into s (mapcat #(links %) s)))) {} links)]
        (if (= links new-links)
          (apply = (count links) (map count (vals links)))
          (recur new-links))))))

(defcheck solution-14838a47
  (fn [e]
    (let [m (set (apply concat e)) f (first m)]
      (loop [v #{f} n (disj m f)]
        (let [d (distinct (for [s v [a b] e :when (and (not= a b) (#{a b} s))] (if (= s a) b a)))
              w (into v d)]
          (if (= v w)
            (empty? n)
            (recur w (apply disj n d))))))))

(defcheck solution-14badb3b
  (letfn [(all-pairs [xs]
            (mapcat (fn [a bs] (map #(vector a %) bs))
              (butlast xs)
              (take-while seq (iterate rest (rest xs)))))
          (other [node edge]
            (first (disj edge node)))
          (reachable? [from to edges]
            (let [paths (filter #(% from) edges)]
              (or (some #(% to) paths)
                  (some #(reachable? (other from %) to (disj edges %)) paths))))]
    (fn connected? [edges]
      (let [nodes (reduce into #{} edges)
            edges (set (map set edges))
            pairs (all-pairs nodes)]
        (or (= 1 (count nodes))
            (every? (fn [[a b]] (reachable? a b edges)) pairs))))))

(defcheck solution-14c2ca78
  (fn [s]
    (let [a (fn [x v] (apply conj x v))
          f (fn [x j] (group-by #(nil? (some x %)) j))
          d (fn [s m]
              (let [u (m false)
                    v (m true)
                    w (reduce a s u)]
                (if (empty? u)
                  (empty? v)
                  (recur w (f w v)))))
          j (a #{} (first s))]
      (d j (f j (rest s))))))

(defcheck solution-1537a5f1
  #(let [graph (concat % (map reverse %))]
     (letfn [
             (followers[node nodes]
               "return all edges directly connected with node which are in the given set of nodes"
               (for [[a b] graph :when(and (= node a) (not (nil? (nodes b))))] b)
               )

             (connected [foll nodes graph]
               "test if the graph is connected"
               (loop [succ foll ret nodes]
                 (if (or (empty? succ) (empty? ret) (nil? (some ret succ)))
                   ret
                   (let [nd (first succ) succ-of-node (followers nd nodes)]
                     (recur
                       (distinct (concat (rest succ) succ-of-node (followers nd ret)))
                       (set (remove (fn[x](= nd x)) ret))
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
             ]
         (or (= (count nodes) 1) (empty?  res))
         )
       )
     ))

(defcheck solution-1565f69b
  (fn [g]
    (= 1
      (count
        (reduce (fn [s e]
                  (concat (filter #(not-any? % e) s)
                    [(reduce into (set e)
                       (filter #(some % e) s))]))
          []
          g)))))

(defcheck solution-15ecf4cb
  (fn [g]
    (letfn [(find-path [at visited all hmap]
              (if (= visited all)
                true
                (when-let [new-vs (apply disj (hmap at) visited)]
                  (some (partial apply find-path)
                    (for [new-at new-vs] [new-at (into visited new-vs) all hmap])))))]
      (let [vs (->> g (apply concat) set)
            hmap (reduce (fn [es [a b]] (assoc es a (conj (get es a #{}) b)
                                                  b (conj (get es b #{}) a)))
                   {} g)]
        (boolean (find-path (first vs) #{(first vs)} vs hmap))))))

(defcheck solution-16577cf9
  (letfn [(exists-path
            [g start end]
            (if (= start end)
              true
              (let [vs (->> g
                         (filter #(contains? (set %) start))
                         (apply concat)
                         (into #{})
                         (remove #{start}))]
                (some #(exists-path (remove #{[start %] [% start]} g) % end) vs))))]
    (fn [g]
      (let [vs (distinct (apply concat g))]
        (every? identity (for [s vs
                               e vs]
                           (exists-path g s e)))))))

(defcheck solution-16a0d8cf
  (fn [edges]
    (letfn [(connected? [s e] (boolean (some #(some (into #{} e) %) s)))]
      (loop [s #{(first edges)} es (rest edges)]
        (let [c (group-by #(connected? s %) es)]
          (cond (empty? (c false)) true
                (empty? (c true)) false
                :else (recur (set (c true)) (c false))))))))

(defcheck solution-172a09ca
  (comp odd? count))

(defcheck solution-179930c4
  (fn cn
    ([edges] (cn #{} edges))
    ([components edges]
     (let [combine (fn [[v1 v2] components]
                     (let [c1 (or (first (filter #(% v1) components))
                                  #{v1})
                           c2 (or (first (filter #(% v2) components))
                                  #{v2})
                           union (comp set (partial remove nil?) concat)]
                       (->> components
                         (remove #(= c1 %))
                         (remove #(= c2 %))
                         (union #{(union c1 c2)}))))]
       (if (empty? edges)
         (= 1 (count components))
         (cn (combine (first edges) components)
           (rest edges)))))))

(defcheck solution-17c43f08
  (fn [v]
    (letfn ((rep [v n a]
              (vec `(~@(take n v) ~a ~@(drop (+ n 1) v))))
            (mul [m1 m2]
              (vec (map (fn [i]
                          (vec (map (fn [j]
                                      (reduce (fn [r k]
                                                (if (> (+ r (* (nth (nth m1 i) k)
                                                              (nth (nth m2 k) j)))
                                                      0)
                                                  1
                                                  0))
                                        0
                                        (range (count m1))))
                                 (range (count m1)))))
                     (range (count m1))))))
      (let [n (apply list (reduce (fn [r v]
                                    (conj r (v 0) (v 1)))
                            #{}
                            v))
            p (reduce (fn [r i]
                        (conj r [(nth n i) i]))
                {}
                (range (count n)))
            v (reduce (fn [r v]
                        (conj r v))
                v
                (map (fn [v] [(v 1) (v 0)]) v))
            m (loop [v v
                     m (vec (map (fn [i] (vec (map (fn [j]
                                                     (if (= i j)
                                                       1
                                                       0)) (range (count n))))) (range (count n))))]
                (if (empty? v)
                  m
                  (let [i (p ((first v) 1))
                        j (p ((first v) 0))]
                    (recur (rest v) (rep m i (rep (m i) j 1))))))]
        (loop [m1 m]
          (let [m2 (mul m1 m)]
            (if (= m1 m2)
              (every? (fn [r]
                        (every? #(= % 1) r))
                m2)
              (recur m2))))))))

(defcheck solution-17f75b6a
  (fn [es]
    (let [a (apply merge-with clojure.set/union
              (map (fn [[a b]] {a #{b}})
                (concat es (map reverse es))))]
      (loop [r #{(ffirst a)}]
        (if-let [n (seq (remove r (mapcat a r)))]
          (recur (into r n))
          (every? r (keys a)))))))

(defcheck solution-189b4307
  (fn f
    ([xs] (f (rest xs) (set (first xs))))
    ([xs result]
     (if (empty? xs) true                                   ;; All records processed - graph connected
                     (let [connected
                               (for [a xs
                                     :when (not (empty? (filter (set a) result)))]
                                 a)
                           r2 (reduce conj result (flatten connected))
                           xs2 (filter (complement (set connected)) xs)]
                       (if (empty? connected) false
                                              (recur xs2 r2))
                       )
                     )
     )
    ))

(defcheck solution-19324ce6
  (fn connected? [g]
    (= g
      (loop [o #{(first g)}]
        (let [x (apply clojure.set/union
                  (map #(into o (filter (partial some (set %)) g))
                    o))]
          (if (= o x)
            o
            (recur (into o x))))))))

(defcheck solution-1992c3a5
  (fn [edges]
    (let [paths (apply merge-with into
                  (mapcat (fn [[a b]]
                            [{a #{b}}
                             {b #{a}}])
                    edges))
          reachable (nth (iterate (fn [reach]
                                    (into {}
                                      (map (fn [[k v]]
                                             [k (into v (mapcat #(get reach % #{}) v))])
                                        reach)))
                           (into paths (map (fn [[k v]] [k (conj v k)]) paths)))
                      (count edges))]
      (= (first (vals reachable)) (set (keys reachable))))))

(defcheck solution-19fe581a
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
                           closed)))]
      (apply = (map reachables (keys con))))))

(defcheck solution-1a52bcf5
  (letfn [(links? [edge group-of-nodes]
            (some #(or (= (edge 0) %) (= (edge 1) %)) group-of-nodes))
          (all-nodes [graph]
            (into #{} (apply concat graph)))]
    (fn [graph]
      (loop [visited-nodes (into #{} (first graph))
             [current-edge & remaining-edges] (rest graph)
             tested-edges []]
        (cond
          (= (all-nodes graph) visited-nodes) true
          (nil? current-edge) false
          (links? current-edge visited-nodes)
          (recur (into #{} (concat visited-nodes current-edge))
            (concat remaining-edges tested-edges)
            [])
          :else
          (recur visited-nodes
            remaining-edges
            (conj tested-edges current-edge)))))))

(defcheck solution-1a96e926
  (fn [g]
    (let [[e & r] (map set g)]
      (loop [a e c (into #{} r)]
        (if (empty? c)
          true
          (if-let
           [n (some #(when (not
                             (empty? (clojure.set/intersection % a))) %)
                c)]
            (recur (clojure.set/union a n) (disj c n))
            false))))))

(defcheck solution-1abf6608
  (fn f
    ([g]
     #_(println (f (first (first g)) g #{}))
     (=
       (set (flatten (seq g)))
       (f (first (first g))
          (clojure.set/union g (set (map reverse g)))
         #{})))
    ([e g v]
     #_(println e g v)
     (if (or (contains? v e) (nil? e))
       v
       (reduce
         clojure.set/union
         (map
           #(f % g (conj v e))
           (map last (filter #(= e (first %)) g))))))))

(defcheck solution-1b2e01fe
  (fn connected? [graph]
    (let
     [nodes (set (concat (map first graph) (map second graph)))
      start (first nodes)
      outbound (group-by first (concat graph (map reverse graph)))
      enlarge (fn [queue node visited] (filter (complement visited) (concat (rest queue) (map last (outbound node)))))
      visit (fn visit [visited queue node]
              (if (empty? queue)
                visited
                (visit (conj visited node) (enlarge queue node visited) (first queue))))]
      (= (visit #{} [start] start) nodes))))

(defcheck solution-1c264ea7
  (fn connected-graph? [edges]
    (let [outbound (->> (concat edges (map reverse edges))
                     (group-by first)
                     (map (fn [[node edge-list]]
                            [node (set (map second edge-list))]))
                     (into {}))
          nodes (set (keys outbound))]
      (loop [visited #{(first nodes)}]
        (let [next-visited (->> (mapcat outbound visited)
                             (into visited))]
          (cond
            (= next-visited nodes)
            true

            (= next-visited visited)
            false

            :default
            (recur next-visited)))))))

(defcheck solution-1c4d582f
  (letfn [(add-to-edge-set [edge-sets a b]
            (assoc edge-sets a (conj (or (edge-sets a) #{}) b)))
          (edge-tuples->edge-sets [tuples]
            (reduce (fn [result [a b]]
                      (add-to-edge-set (add-to-edge-set result a b)
                        b a))
              {} tuples))
          (reachable-from [edge-sets node seen-nodes]
            (if (contains? seen-nodes node)
              seen-nodes
              (apply clojure.set/union seen-nodes
                (map #(reachable-from edge-sets % (conj seen-nodes node))
                  (edge-sets node)))))]
    (fn [edge-tuples]
      (let [edge-sets (edge-tuples->edge-sets edge-tuples)
            nodes (set (flatten (vec edge-tuples)))]
        (= nodes (reachable-from edge-sets (first nodes) #{}))))))

(defcheck solution-1ceedb9
  (fn [gs]
    (letfn [(find-conns [g gs]
              (let [conns (filter (fn [[a b]] (let [sg (set g)] (or (sg a) (sg b)))) gs)]
                (if (seq conns)
                  (concat [g] (mapcat #(find-conns % (disj gs %)) conns))
                  [g])))]
      (let [g (first gs)]
        (= gs (set (find-conns g (disj gs g))))))))

(defcheck solution-1d8ac298
  (fn[s]
    (let [t (map #(into #{} %) s)
          c (fn[t]
              (into #{}
                (map
                  #(if
                    (empty? (clojure.set/intersection % (first t)))
                     % (  clojure.set/union % (first t)))
                  (rest t))))]
      ( =  (into #{} (apply concat s))
        (ffirst (drop-while #(> (count %) 1) (iterate c t)))))))

(defcheck solution-1e1d2021
  (fn [graph]
    ; basically transitive closure from prob #84 (but starting with bidirectional adjacency "matrix")
    (let [union (fn [s1 s2] (reduce #(conj % %2) s1 s2))
          add-adjacencies (fn [adjm [a b]] (assoc (assoc adjm a (conj (adjm a) b)) b (conj (adjm b) a)))
          adjm (reduce add-adjacencies (zipmap (reduce concat [] graph) (repeat #{})) graph)
          warshall-step (fn [adjm [i j]]
                          (if (contains? (adjm i) j)
                            (assoc adjm i (union (adjm i) (adjm j)))
                            adjm))
          tc-adjm  (reduce warshall-step adjm (for [j (keys adjm) i (keys adjm)] [i j]))
          reachables (vals tc-adjm)
          ]
      (not= nil (reduce #(if (= % %2) %) (first reachables) reachables)))))

(defcheck solution-1e6dee11
  (fn gc [s]
    (let [f (first s)
          filt (fn [x s]
                 (let [f (fn [v]
                           (cond
                             (= (first x) (first v)) v
                             (= (first x) (last v)) v
                             (= (last x) (first v)) v
                             (= (last x) (last v)) v
                             :else nil))]
                   (map f s)))]
      (loop [r #{f}
             pr #{}]
        (if (= (count r) (count pr))
          (= s r)
          (recur (clojure.set/union r (disj (apply clojure.set/union
                                              (for [x r]
                                                (into #{} (filt x s))))
                                        nil))
            r))))))

(defcheck solution-1ee78b2f
  (fn f [s]
    (let [nodes (set (concat (map first s) (map second s)))
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
                      (recur (clojure.set/union in news)))))]
      #_(println accessibles)
      (= (count nodes) (count accessibles)))))

(defcheck solution-1f017902
  (fn [s]
    (let [o (fn [v a]
              (loop [c #{a}]
                (let [d (reduce
                          (fn [e [x y]] (conj (conj e (if (c x) y a)) (if (c y) x a)))
                          c v)]
                  (if (= c d) c (recur d)))))]
      (= (o s (first (first s))) (clojure.set/union (set (map first s)) (set (map second s)))))))

(defcheck solution-1f1a3fe0
  (fn gc [s]
    (letfn [(path-res [prem autres]
              (if (empty? autres)
                (if (= (count prem) (count s))
                  true
                  false)
                (for [e autres]
                  (if (not (connect (last prem) e))
                    false
                    (path-res (conj prem e) (filter #(not= e %) autres))))))
            (connect [[w x] [y z]]
              (if (or (= w y)
                      (= w z)
                      (= x y)
                      (= x z))
                true
                false))]

      (if (some #{true} (flatten (for [prem s]
                                   (let [autres (filter #(not= prem %) s)]
                                     (path-res [prem] autres))))) true false))))

(defcheck solution-1f5b7813
  (fn [l]
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
    ))

(defcheck solution-20c1de86
  (fn [vs]
    (nil? (next (reduce (fn [sets [i j]]
                          (let [{m true o nil} (group-by #(some (partial contains? % ) [i j]) sets)]
                            (conj o (reduce into #{} (conj m [i j])))))
                  [] vs)))))

(defcheck solution-21310ff0
  (fn problem91
    [s]
    (letfn [;; Is there a path from node a to node b
            ;; (path [1 2] [2 3]) -> true
            ;; (path [1 2] [3 2]) -> true
            (is-path [[a b] [c d]]
              (or (= b c)
                  (= b d)
                  (= a c)
                  (= a d)))

            ;; Return [elem [xs without elm]]
            (remover [elm xs]
              [elm (filter #(not= elm %) xs)])

            ;; split all combinations
            ;; given [a b c d]
            ;; return
            ;; [a [b c d]] [b [a c d] [c [a b d]] [d [a b c]]
            (combo-splitter [xs]
              (map #(remover % xs) xs))

            (filter-next-paths
              [node xs]
              (filter (fn [[n r]] (or (nil? node) (is-path node n))) (combo-splitter xs)))

            ;; For a given set of nodes return true if they are connected
            (problem91Ex [path xs]
              (if (empty? xs)
                path
                ;; for each item in xs
                ;; if is-path (last path) (first xs)
                ;; call foo (add-path path node) (remove node xs)
                (let [node (last path)
                      paths (filter-next-paths node xs)]
                  (map (fn [[ a res ]] (problem91Ex (conj path a) res)) paths))))]
      (pos? (count (flatten (problem91Ex [] s)))))))

(defcheck solution-22a35ac3
  (fn [xs]
    (->> xs
      (reduce
        (fn [m [x y]]
          (let [s (-> #{} (into [x y]) (into (m x)) (into (m y)))]
            (into m (map vector s (repeat s)))))
        {})
      vals
      set
      count
      (>= 1))))

(defcheck solution-22a64f32
  (fn check-connectivity [edges]
    (let [create-initial-disj-set (fn [edges]
                                    (reduce
                                      #(assoc %1 %2 #{%2})
                                      {}
                                      (into #{} (concat
                                                  (map first edges)
                                                  (map second edges)))))
          union-sets (fn [set-1 set-2 disj-set]
                       (let [union-set (clojure.set/union
                                         (disj-set set-1)
                                         (disj-set set-2))]
                         (-> disj-set
                           (assoc set-1 union-set)
                           (assoc set-2 union-set))))
          init-disj-set (create-initial-disj-set edges)
          num-nodes (count init-disj-set)
          final-disj-set (reduce
                           #(->> %1
                              (union-sets (first %2) (second %2))
                              (union-sets (second %2) (first %2)))
                           init-disj-set
                           (concat edges edges))]
      (if (some
            #(= (count %) num-nodes)
            (vals final-disj-set)) true false))))

(defcheck solution-23259d69
  (fn connected? [adjlist]
    (if (<= (count adjlist) 1)
      true
      (loop [conn (set (first adjlist)) xs (rest adjlist)]
        (if (empty? xs)
          true
          (let [cut ((juxt filter remove) #(some (partial contains? conn) %) xs)]
            (if (empty? (first cut))
              false
              (recur (reduce (partial apply conj) conn (first cut)) (second cut)))))))))

(defcheck solution-236de7ab
  (fn [edges]
    (let
     [isconn
      (fn conn [g]
        (loop [s #{(first (keys g))}
               a s]
          (let
           [
            neighbors
            (reduce (partial reduce conj) (map g a))
            news
            (reduce disj neighbors s)
            ]
            (if (empty? news)
              (= (count (reduce conj s a)) (count (keys g)))
              (recur (reduce conj s a) news)
              )
            )
          ))]
      (->> edges
        (mapcat (juxt identity reverse))
        (group-by first)
        (mapcat
          (juxt first (comp set (partial map second) second))
          )
        (apply hash-map)
        (isconn)
        )
      )))

(defcheck solution-237932d6
  (fn graphconnect [x]
    (letfn [(getset [a acc] (if-let [s (first (filter #(% a) acc))] s #{a}))]
      (= 1 (count
             (reduce
               (fn [acc [a b]] (let [_a (getset a acc) _b (getset b acc)] (conj (disj acc _a _b) (clojure.set/union _a _b))))
               #{} x))))))

(defcheck solution-247efed1
  (fn [s]
    (let [? (fn [n1 n2] (= 3 (count (set (into n1 n2)))))]
      (loop [nodes (rest (vec s))
             in #{(first (vec s))}]
        (cond (empty? nodes) true
              (= 'end in) false
              :else (let [connected (loop [i (for [i in]
                                               (filter #(? i %) nodes))
                                           acc #{}]
                                      (if (empty? i)
                                        acc
                                        (recur (rest i)
                                          (into acc (first i)))))]
                      (recur (remove connected nodes)
                        (if (empty? connected)
                          'end
                          (into in connected)))))))))

(defcheck solution-24df7bae
  (fn g [graph]
    (let [sorted-graph (map sort graph)
          links (merge-with conj
                  (group-by first sorted-graph)
                  (group-by second sorted-graph))
          neighbors (fn [k] (-> (get links k)
                              (flatten)
                              (set)
                              (disj k)))
          nodes (into #{} (keys links))
          start (first nodes)]
      (loop [frontier (conj #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) start)
             visited #{start}]
        (if-let [curr (peek frontier)]
          (let [neighb (remove (partial contains? visited) (neighbors curr))]
            (recur
              (into (pop frontier) neighb)
              (into visited neighb)))
          (= visited nodes))))))

(defcheck solution-250d837c
  #(loop [links (set (first %)) untested (rest %)]
     (let [link? (fn [[a b]] (or (links a) (links b)))
           new-links (reduce conj links (flatten (filter link? untested)))
           remaining (remove link? untested)]
       (cond (empty? remaining) true
             (= new-links links) false
             :else (recur new-links remaining)))))

(defcheck solution-257b53de
  (fn connected? [edges]
    (letfn [(group [sets edge]
              (let [gt-cntns (fn [sets node]
                               (reduce #(if (contains? %2 node) %2 %1)
                                 nil
                                 sets))
                    contains-left (gt-cntns sets (first edge))
                    contains-right (gt-cntns sets (last edge))]
                (cond
                  (and (set? contains-left) (set? contains-right))
                  (conj (disj sets contains-left contains-right)
                    (clojure.set/union contains-left contains-right))
                  (set? contains-left) (conj (disj sets contains-left)
                                         (apply conj contains-left edge))
                  (set? contains-right) (conj (disj sets contains-right)
                                          (apply conj contains-right edge))
                  :else
                  (conj sets (apply conj #{} edge)))))]
      (= 1 (count (reduce group #{} edges))))))

(defcheck solution-26316599
  #(let [n (count (set (flatten (seq %))))]
     ((fn c [s]
        (let [p (fn [a b] (and (contains? s a) (not (contains? s b))))
              r (some (fn [[x y]] (if (p x y) y (when (p y x) x))) %)]
          (if r (c (conj s r)) (= (count s) n)))) #{(ffirst %)})))

(defcheck solution-27031278
  (fn [edges]
    (let [nodes (set (flatten (into [] edges)))
          edges (set (concat edges (map reverse edges)))]
      (loop [visited #{(first nodes)}
             lastIter #{}]
        (cond
          (= visited nodes) true
          (= lastIter visited) false
          :else (recur (into visited (mapcat #(filter (fn [x] (edges [% x])) nodes) visited)) visited)
          )
        )
      )))

(defcheck solution-27454e84
  (fn graph-connectivity [sets]
    (letfn [(adjoining-vector [k-node nodes sets]
              (map (fn [node]
                     (when (or
                            (contains? sets [k-node node])
                            (contains? sets [node k-node]))
                       node)) nodes))
            (graph [nodes sets]
              (apply merge
                (map (fn [node]
                       {node (adjoining-vector node nodes sets)}) nodes)))
            (exist-chain? [init graph visited-set]
              (if (= (set (keys graph)) visited-set)
                true
                (some #(when (and %
                                  (not (visited-set %)))
                         (exist-chain? %
                           graph
                           (clojure.set/union #{%} visited-set)))
                  (graph init))))]
      (let [nodes (set (apply concat sets))
            graph (graph nodes sets)]
        (true? (some #(exist-chain? % graph #{%}) nodes))))))

(defcheck solution-27e31bec
  (fn [edges]
    (letfn [(nodes [edges] (set (flatten (vec edges))))
            (neighbours [node edges]
              (set
                (for [e edges :when (some #(= node %) e)]
                  (if (= (first e) node) (second e) (first e)))))
            (component [node edges]
              (loop [todo #{node}, found #{node}]
                (if (empty? todo) found
                                  (let [n      (first todo)
                                        neighb (remove found (neighbours n edges))]
                                    (recur (disj (into todo neighb) n) (conj found n))))))

            (connected? [edges]
              (let [all-nodes (nodes edges)]
                (= all-nodes (component (first all-nodes) edges))))]
      (connected? edges))))

(defcheck solution-28c79f0e
  (fn [graph]
    (let [nodes (distinct (flatten (into [] graph)))
          pairs (for [x nodes y nodes] [x y])
          graphlen (count graph)
          searcher (fn [p]
                     (let [start (first p)
                           end (second p)]
                       ((fn [nextpnts cnt]
                          (cond
                            (some #(= % end) nextpnts) true
                            (>= cnt graphlen) false
                            :else (recur
                                    (mapcat
                                      (fn [sval]
                                        (concat
                                          (map second (filter #(= sval (first %)) graph))
                                          (map first (filter #(= sval (second %)) graph))))
                                      nextpnts)
                                    (inc cnt))))
                        [start] 0)))]
      (reduce #(and %1 %2) (for [pa pairs] (do #_(println pa (searcher pa)) (searcher pa)))))))

(defcheck solution-297b3ea9
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
    (square [n] (* n n))]
    #(= (count (transitive (symmetric %)))
       (square (count (domain (symmetric %)))))))

(defcheck solution-29d49cab
  (fn [xs]
    (loop [xs xs zs #{}]
      (if (empty? xs) true
                      (letfn [(conn? [[a b]] (or (empty? zs) (contains? zs a) (contains? zs b)))]
                        (if-let [[a b :as x] (first (filter conn? xs))]
                          (recur (remove #{x} xs) (into zs (set [a b])))
                          false))))))

(defcheck solution-2a7e7dff
  (fn [edges] (let [nodes (reduce #( apply conj %1 %2)  #{} edges)
                    ; convert from undirected to directed edges on the graph
                    allrels (concat edges (map (fn [x] [(second x) (first x)] ) edges))
                    firstnode (first nodes)
                    unvisitednodes (set (rest nodes))]
                (loop [black #{} grey #{ firstnode }  white (set unvisitednodes)]
                  (if (empty? grey)
                    ; if there are any white and no grays the graph is not connected
                    (empty? white)
                    (let [ firstgrey (first grey)
                          remaining-grey (disj grey firstgrey)
                          reachable-from-grey (set (filter #(not (contains? black %1)) (map #( second %1 )(filter #(= (first %1) firstgrey) allrels))))
                          new-white (filter #(not (contains? reachable-from-grey %1)) white)
                          new-grey (clojure.set/union remaining-grey reachable-from-grey)
                          ]
                      (recur (conj black firstgrey) new-grey new-white)
                      ))))))

(defcheck solution-2a8db047
  (fn connected [graph]
    (loop [[[a b :as e] & edges] (sort (concat graph (mapv (fn [[x y]] [y x]) graph)))
           unmatched #{}
           nodes #{a}]
      (cond
        (or (contains? nodes a) (contains? nodes b))
        (recur edges (disj unmatched e) (into nodes e))
        (empty? edges) (empty? unmatched)
        :else (recur edges (conj unmatched e) nodes)
        )
      )
    ))

(defcheck solution-2ab3303a
  (fn connected? [set-of-pairs]
    (not (next (letfn [(update-el-to-rep [el-to-rep els-to-change new-rep]
                         (reduce #(assoc %1 %2 new-rep) el-to-rep els-to-change))
                       (update-reps-list [reps key1 key2]
                         (let [take-set (reps key1)
                               drop-set (reps key2)]
                           (-> (dissoc reps key2)
                             (assoc   key1 (clojure.set/union take-set drop-set)))))]
                 (loop [reps {}              ;el to map of els
                        el-to-rep {}         ;el to representative in reps
                        [curr-pair & more :as set-of-pairs] (vec set-of-pairs)]
                   (if (seq set-of-pairs)
                     (let [[el1 el2] curr-pair
                           [not-rep rep] (if (el-to-rep el1)
                                           [el2 (el-to-rep el1)]
                                           (if (el-to-rep el2)
                                             [el1 (el-to-rep el2)]
                                             (sort curr-pair)))
                           rep-set (or (reps (el-to-rep el1)) (reps (el-to-rep el2)))]
                       (cond
                         (not rep-set)       ;have not seen before
                         (recur (assoc reps rep `#{~@(keys (group-by identity curr-pair))})
                           (assoc el-to-rep el1 rep el2 rep)
                           more)

                         (not (el-to-rep not-rep)) ;seen only one, add new one to rep-set and el-to-rep map, check that not-rep is in fact, not a rep
                         (recur (assoc reps rep (conj rep-set not-rep))
                           (assoc el-to-rep not-rep rep)
                           more)

                         (not (= (el-to-rep rep) (el-to-rep not-rep))) ;so combine sets
                         (recur (update-reps-list reps (el-to-rep rep) (el-to-rep not-rep))
                           (update-el-to-rep el-to-rep (reps (el-to-rep not-rep)) rep)
                           more)

                         :intersection-within-same-connected-component-based-on-representativ
                         (recur reps
                           el-to-rep
                           more)
                         ))
                     reps)))))))

(defcheck solution-2b54ab0
  (fn [g]
    (letfn [(merge-if-matching-entry [s1 s2]
              (if (some s1 s2)
                (apply conj s1 s2)
                s2))
            (build-paths [x]
              (reduce
                (fn [cs s]
                  (let [sx (set s)]
                    (concat [sx] (map (partial merge-if-matching-entry sx) cs))))
                []
                x))
            (merge-paths [p]
              (mapcat
                (fn [x]
                  (map (partial merge-if-matching-entry x) p)) p))
            (filter-subsets [p]
              (reduce
                (fn [x y]
                  (conj (filter
                          (fn [i]
                            (not (and (some y i) (>= (count y) (count i)))))
                          x) y))
                []
                p))]
      (->> g
        (build-paths)
        (merge-paths)
        (sort #(< (count %1) (count %2)))
        (filter-subsets)
        (count)
        (= 1)))))

(defcheck solution-2b6900cb
  (fn [s]
    (if (or (empty? s) (= 1 (count s)))
      true
      (letfn [(permutations [s]
                (lazy-seq
                  (if (seq (rest s))
                    (apply concat (for [x s]
                                    (map #(cons x %) (permutations (remove #{x} s)))))
                    [s])))
              (no-connection? [[a b] [x y]]
                (if (or (= b x) (= a x) (= b y) (= a y))
                  false
                  true))
              (connect? [v]
                (loop [e (first v) c (rest v)]
                  (if (empty? c)
                    true
                    (if (no-connection? e (first c))
                      false
                      (recur (first c) (rest c))))))
              (connected? [s]
                (not (empty? (drop-while #(not (connect? %)) (permutations s)))))]
        (connected? s)
        ))))

(defcheck solution-2c4288ef
  (fn [edges]
    (letfn [
            (mergeable? [s e] (if (some s e) true false))]
      (loop [s (into #{} (first edges)) es edges]
        (if (empty? es) true ;; finished
                        (let [groups (group-by (partial mergeable? s) es)
                              tomerge (groups true)
                              remaining (groups false)]
                          (if (empty? tomerge)
                            false ;; can&rsquo;t find any more
                            (recur (reduce #(into %1 %2) s tomerge) remaining))))))))

(defcheck solution-2df5e9a2
  (fn [edges]
    (loop [list-graph #{}
           graph (set (first edges))
           otheredges (rest edges)]
      (if (empty? otheredges)
        (let [c (count list-graph)]
          (cond
            (<= c 1) true
            (>= c 2) (let [intset (apply clojure.set/intersection list-graph)] (if (empty? intset) false true))))
        (let [inGraph (into {} (for [x list-graph y graph :when (contains? x y)] {x (apply conj x graph)}))
              graph-set (set (keys inGraph))
              new-graph (if (empty? inGraph)
                          (conj list-graph graph)
                          (apply conj (filter #(not (contains? graph-set %)) list-graph) (vals inGraph)))]
          (recur new-graph
            (set (first otheredges))
            (rest otheredges)))))))

(defcheck solution-2df8de8e
  #(let [massoc
                (fn [m [k v]] (assoc m k (into (m k) [v])))
         edges
                (fn [es] (into es (map reverse es)))
         graph
                (fn [es] (reduce massoc {} es))
         walk
                (fn [g q visited]
                  (if (seq q)
                    (let [adj (g (peek q))
                          newq (if (visited (peek q)) (pop q) (into (pop q) adj))]
                      (recur g newq (conj visited (peek q))))
                    visited))
         emptyq #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue [])
         all-nodes
                (fn [es]
                  (into #{} (mapcat identity es)))
         ]
     (=
       (walk (graph (edges %)) (conj emptyq (ffirst %)) #{})
       (all-nodes %))
     ))

(defcheck solution-2efcf67f
  (fn p91 [pairs]
    (let [set-of-all-vals (-> (conj (map first pairs) (map last pairs))
                            flatten
                            set)
          starting-pt (-> (into [] set-of-all-vals)
                        rand-nth
                        vector
                        set)]
      (if (= 1 (count pairs)) true
                              (loop [pairs-to-check pairs past-vals starting-pt]
                                (let [temp-pairs (into #{} (filter
                                                             #(or (contains? past-vals (first %))
                                                                  (contains? past-vals (last %))) pairs-to-check))
                                      temp-vals  (into #{} (remove #(contains? past-vals %)
                                                             (flatten (into [] temp-pairs))))]
                                  (if (empty? temp-pairs)
                                    (= past-vals set-of-all-vals)
                                    (recur (apply disj pairs-to-check temp-pairs)
                                      (clojure.set/union temp-vals past-vals)))))))))

(defcheck solution-2f48b925
  (fn is-connected[graph]
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
            ]
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
    ))

(defcheck solution-2f4ba1cf
  (fn [xs]
    (letfn [(creategraph2 [ys] (reduce (fn [m [a b]] (assoc m a (conj (m a) b) b (conj (m b) a))) {} ys))
            (walkgraph2 [m start visited]
              (if (contains? visited start) visited
                                            (reduce #(clojure.set/union % (walkgraph2 m %2 (conj visited start))) visited (m start))))]
      (let [allnodes (-> (creategraph2 xs) keys set)]
        (boolean (some #(= allnodes %) (map #(walkgraph2 (creategraph2 xs) % #{}) allnodes)))))))

(defcheck solution-2fcf2a6
  (fn connected1? [graph]
    (let [remove1 (fn remove1 [x [y & ys]]
                    (if (= x y)
                      ys
                      (conj (remove1 x ys) y)))
          tour-from? (fn tour-from? [from graph]
                       (or (empty? graph)
                           (some (fn [edge]
                                   (tour-from? (first edge)
                                     (remove1 edge graph)))
                             (filter #(= from (second %)) graph))
                           (some (fn [edge]
                                   (tour-from? (second edge)
                                     (remove1 edge graph)))
                             (filter #(= from (first %)) graph))))]
      (true? (some (fn [edge]
                     (or (tour-from? (first edge)
                           (remove1 edge
                             (concat graph graph)))
                         (tour-from? (second edge)
                           (remove1 edge
                             (concat graph graph)))))
               graph)))))

(defcheck solution-3050311e
  (fn [nodes-set]
    (let [nodes (reduce #(conj (conj %1 (first %2)) (second %2)) #{} nodes-set)
          nodes-map (into {} (map-indexed #(vector %2 %1) nodes))
          index-of (fn [node] (get nodes-map node))
          edges (concat nodes-set (map #(apply vector (reverse %)) nodes-set))
          grouped-edges (group-by (comp index-of first) edges)
          destinations (fn [node] (map second (get grouped-edges (index-of node))))
          walk-graph (fn walk
                       ([node] (walk #{} (hash-set node)))
                       ([result walked]
                        (if (zero? (count walked))
                          result
                          (let [current (first walked)
                                next-result (conj result current)
                                new-nodes (filter #(not (contains? next-result %)) (destinations current))]
                            (walk next-result (reduce conj (into #{} (rest walked)) new-nodes))))))]
      (= nodes (walk-graph (ffirst nodes-set))))))

(defcheck solution-306ec25d
  #(letfn [(join [r s] (for [[x1 y1] r [x2 y2] s :when (= y1 x2)] [x1 y2]))
           (tc [r]
             (let [r2 (set (join r r))]
               (if (every? r r2) r
                                 (tc (clojure.set/union r r2)))))]
     (let [h (clojure.set/union % (set (map vec (map reverse %))))
           pairs (for [e1 h e2 h] [(e1 0) (e2 0)])]
       (every? (tc h) pairs))))

(defcheck solution-3073f665
  (fn [gr]
    (let [transitive (fn [r]
                       (let [g (group-by first r)
                             m (apply concat
                                 (map
                                   (fn [x] (apply concat
                                             (map
                                               (fn [y] (map #(do [x (second %)])
                                                         (get g y)))
                                               (map second (get g x)))))
                                   (keys g)))
                             next (if (set? r) (into r m) (into #{} r m))]
                         (if (= next r)
                           r
                           (recur next))))
          ug (into gr (map (fn [[a b]] [b a]) gr)) ; undirected graph
          reachable (group-by first (transitive ug))]
      (= (count (get reachable (first (first (seq gr)))))
        (count (into #{} (apply concat (seq gr))))))))

(defcheck solution-308160a8
  (fn [s]
    (letfn [(connect [n e]
              (some {n (first (remove #(= n %) e))} e))]
      (loop [nodes (first s) ue (rest s)]
        (if (empty? ue)
          true
          (let [c (set (apply concat (map #(keep (partial connect %) ue) nodes)))]
            (if (empty? c)
              false
              (recur c (reduce #(remove (fn [n] (connect %2 n)) %) ue nodes))))
          )))))

(defcheck solution-30c28566
  (fn [e]
    (let [vs    (->> e seq flatten (into #{}))
          find  (fn [s v] (if (contains? s v) (recur s (s v)) v))
          union (fn [s [v1 v2]]
                  (let [r1 (find s v1), r2 (find s v2)]
                    (if (= r1 r2) s (assoc s r1 r2))))		]
      (= (dec (count vs)) (count (reduce union {} e))))))

(defcheck solution-32160049
  (fn connected? [graph]
    (let [nodes (set (flatten (seq graph)))]
      (loop [previous #{} connected (set (first graph))]
        (if (= nodes connected) true
                                (if (= connected previous) false
                                                           (recur connected (into connected
                                                                              (mapcat #(if (or (contains? connected (first  %))
                                                                                               (contains? connected (second %)))
                                                                                         % ()) graph)))))))))

(defcheck solution-328da8b0
  (fn [g]
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
        (<= (count (intersect g)) 1)))))

(defcheck solution-32d4b514
  (fn [g]
    (let [
          f first
          d (fn r [c v]
              (if (v c) v
                        (reduce
                          #(r (second %2) (r (f %2) %1))
                          (conj v c)(filter #((set %) c) g))))
          v (mapcat (fn [n] n) g)
          ]
      (=
        (d (f v) #{})
        (set v)))))

(defcheck solution-32e25043
  (fn graph-find
    [connections]
    (= 1
      (count
        (reduce (fn [acc pair]
                  (let [pred (partial some (set pair))
                        [connected disconnected] ((juxt filter remove) pred acc)]
                    (set (cons (set (apply concat pair connected)) disconnected))))
          #{}
          connections)))))

(defcheck solution-32e5013
  (fn fully-connected? [graph]
    (let [nodes (set (apply concat graph))
          full-graph (set (mapcat (fn [[a b :as n]] [n [b a]]) graph))
          children (into {} (for [[k v] (group-by first full-graph)] [k (set (map second v))]))
          connections (fn [node]
                        (->> (iterate #(into % (mapcat children %)) #{node})
                          (partition 2 1)
                          (filter #(apply = %))
                          first first))]
      (every? #(= % nodes) (map connections nodes)))))

(defcheck solution-338ab470
  (letfn [
          (nodes [graph]
            (distinct (flatten (into () graph))))

          (node-paths [node graph]
            (filter #(not (= node %))
              (nodes (filter #(< -1 (.indexOf % node)) graph))))

          (nav-map [graph]
            (loop [rm (nodes graph), acc {}]
              (cond (empty? rm) acc
                    :else (let [node (first rm)]
                            (recur (rest rm)
                              (assoc acc node (node-paths node graph)))))))
          (combos [graph]
            (let [nodes (nodes graph)]
              (loop [rm nodes, rm2 nodes, acc #{}]
                (let [n1 (first rm), n2 (first rm2)]
                  (cond (nil? n1) acc
                        (nil? n2) (recur (rest rm) nodes acc)
                        (= n1 n2) (recur rm (rest rm2) acc)
                        :else (recur rm (rest rm2) (conj acc (into [] (sort (list n1 n2))))))))))

          (next-node [current-path failed-paths nav-map]
            (first (filter #(= -1 (.indexOf failed-paths (conj current-path %)))
                     (filter #(= -1 (.indexOf current-path %)) (nav-map (last current-path))))))

          (has-path? [node1 node2 nav-map]
            (loop [current-path [node1], failed-paths []]
              (let [n (next-node current-path failed-paths nav-map)]
                (cond (empty? current-path) false
                      (= n node2) true
                      (nil? n) (recur (pop current-path) (conj failed-paths current-path))
                      :else (recur (conj current-path n) failed-paths)))))

          (connected? [graph]
            (let [nav-map (nav-map graph)
                  nodes (nodes graph)]
              (every? true? (map #(has-path? (% 0) (% 1) nav-map) (combos graph)))))]
    connected?))

(defcheck solution-33a4cc34
  (fn [edges]
    (let [verts  (set (flatten (seq edges)))
          node   (first verts)
          expand (fn [nodes edges]
                   (into (set nodes)
                     (mapcat
                       (fn [node]
                         (remove nil?
                           (map
                             (fn [edge]
                               (cond
                                 (= node (first edge)) (second edge)
                                 (= node (second edge)) (first edge)
                                 ))
                             edges)))
                       nodes)))]
      (loop [nodes #{node}]
        #_(println [nodes verts (expand nodes edges)])
        (let [newnodes (expand nodes edges)]
          (cond
            (= newnodes verts) true
            (= newnodes nodes) false
            :else              (recur newnodes)))
        )
      )))

(defcheck solution-33a887d7
  (fn connected [vertexes]
    (= 1 (count
          (reduce
           (fn [sets [a b]]
             (let [m (group-by (fn [s] (or (contains? s a) (contains? s b))) sets)]
               (clojure.set/union
                (hash-set
                 (reduce
                  #(clojure.set/union %1 (set %2))
                  (into #{} [a b])
                  (get m true)))
                (set (get m false)))))
           #{}
           vertexes)
          ))))

(defcheck solution-3409644e
  (fn connected?
    [coll]
    (letfn [(helper [acc coll]
              (if (seq coll)
                (let [a (first coll)]
                  (if (some (fn [set1] (some set1 a)) acc)
                    (let [r (map (fn [aset] (if (some aset a) (set (concat aset a)) aset)) acc)]
                      (helper [(first r)] (concat (rest r) (rest coll))))
                    (recur (conj acc (set a)) (rest coll))))
                acc))]
      (= 1
        (count (helper [(set (first coll))] (rest coll)))))))

(defcheck solution-3482e5cc
  (fn fully-connected? [edge-set]
    (letfn [(adjacency-map [edge-set]
              (let [reverse-edge-set (set (mapcat (fn [[a b]] [[b a]]) edge-set))
                    full-edge-set (into edge-set reverse-edge-set)]
                (into {} (for [[vertex edges] (group-by first full-edge-set)]
                           [vertex (set (map second edges))]))))
            (traverse-graph-dfs [adjacency-map start-vertex]
              (loop [vertices []
                     visited #{start-vertex}
                     frontier [start-vertex]]
                (if (empty? frontier)
                  vertices
                  (let [vertex (peek frontier)
                        neighbors (adjacency-map vertex)]
                    (recur
                      (conj vertices vertex)
                      (into visited neighbors)
                      (into (pop frontier) (remove visited neighbors)))))))]
      (let [vertices (set (apply concat edge-set))
            adj-map (adjacency-map edge-set)]
        (= (count vertices) (count (traverse-graph-dfs adj-map (first vertices))))))))

(defcheck solution-34fc859b
  (fn [graph]
    (letfn [(c [r g]
              (if (empty? g)
                true
                (let [reachable_edges (set (filter (fn [[v1 v2]] (or (r v1) (r v2))) g))
                      reachable_nodes (set (apply concat reachable_edges))]
                  (if (empty? reachable_nodes)
                    false
                    (recur (clojure.set/union r reachable_nodes) (clojure.set/difference g reachable_edges))))))]
      (c (set (first graph)) (set graph)))))

(defcheck solution-3547eae8
  (fn f [s]
    (let [l (set (mapcat flatten s))
          m (reduce #(merge-with into % (merge {(%2 0) #{(%2 1)}} {(%2 1) #{(%2 0)}})) {} s)]
      (every? #(= l ((fn g [t m v]
                       (if (t v)
                         t
                         (set (mapcat (partial g (conj t v) m) (m v))))) #{} m %)) l))))

(defcheck solution-3640255e
  #(
    (fn f [[x & s]]
      (if (empty? s) true
                     (if (empty? (clojure.set/intersection x
                                   (apply clojure.set/union s)))
                       false
                       (f (map (fn [m]
                                 (if (empty? (clojure.set/intersection m x)) m
                                                                             (clojure.set/union x m)))
                            s))))) (map set %)))

(defcheck solution-3640503e
  (fn is-connected [xset]
    (loop [ret [] rst (vec xset)]
      (if (empty? rst)
        true
        (if (empty? ret)
          (recur (conj ret (first rst)) (rest rst))
          (let [tails (flatten ret)
                heads (flatten rst)
                common-elt (first (clojure.set/intersection (set tails)
                                    (set heads)))]
            (if (nil? common-elt)
              false
              (let [add-elt (first (filter #(or (= (first %) common-elt)
                                                (= (second %) common-elt))
                                     rst))]
                (recur (conj ret add-elt)
                  (remove #{add-elt} rst))))))))))

(defcheck solution-36c48018
  (fn connected? [edges] ; edges is a set of pairs
    (let
     [nodes (into #{} (flatten (seq edges)))
      num-nodes (count nodes)
      #_#__ (println "num nodes" num-nodes)
      first-node (first nodes)
      #_#__ (println "first node" first-node)
      reachables (fn reachables [nodes] ; is a set of nodes
                   (let
                    [num-nodes (count nodes)
                     #_#__ (println "reachables num nodes" num-nodes)
                     new-nodes (set
                                 (flatten
                                   (filter
                                     (fn [[n1 n2]]
                                       (or (nodes n1) (nodes n2)))
                                     edges)))
                     #_#__ (println "reachables new nodes" new-nodes)
                     next-nodes (clojure.set/union nodes new-nodes)
                     #_#__ (println "reachables next nodes" next-nodes)
                     new-count (count next-nodes)
                     #_#__ (println "reachables new count" new-count)]
                     (if (> new-count num-nodes)
                       (reachables next-nodes)
                       next-nodes)))
      reachable-nodes (reachables (set [first-node]))]
      (= reachable-nodes nodes))))

(defcheck solution-3721e1d4
  (fn [g]
    (letfn [(get-nodes [s]
              (distinct (flatten (vec s))))
            (get-vertices [n s]
              (set (remove nil? (map (fn [x] (if (some #(= n %) x) x)) s))))]
      (letfn [(follow-path
                [s n]
                (get-nodes (get-vertices n s)))]
        (loop [nodes (get-nodes g)
               traversed (follow-path g (first (get-nodes g)))]
          ; (println (str (set nodes) " " (set traversed)))
          (if (or (empty? nodes) (clojure.set/subset? (set (mapcat (partial follow-path g) traversed)) (set traversed)))
            (= (set (get-nodes g)) (set traversed))
            (let [nodes (clojure.set/difference (set nodes) (set traversed))]
              (recur nodes (into traversed (set (mapcat (partial follow-path g) traversed))))
              )))))))

(defcheck solution-3730a3ec
  (fn [edge-list]
    (letfn
     [(test-intersection [a b]
        ((complement empty?) (clojure.set/intersection a b)))
      (merge-intersecting [coll itm]
        (if
         (some (partial test-intersection itm) coll)
          (set
            (map
              (fn [a]
                (if (test-intersection a itm) (clojure.set/union a itm) a))
              coll))
          (conj coll itm)))
      (merge-nodes [edge-set]
        (let
         [merged
          (reduce
            (fn [acc itm] (merge-intersecting acc itm))
            #{}
            edge-set)]
          (if (= merged edge-set) merged (merge-nodes merged))))]
      (= 1 (count (merge-nodes (set (map set edge-list))))))))

(defcheck solution-383cc963
  (fn [edges]
    (let [find-connections (fn [m [a b]] (conj m
                                           [a (conj (m a) b)]
                                           [b (conj (m b) a)]))
          connections (reduce find-connections {} edges)
          num-vertices (->> edges vec flatten distinct count)]
      (letfn [(traverse [seen v]
                (if (seen v)
                  seen
                  (let [seen (conj seen v)]
                    (apply conj seen v (mapcat (partial traverse seen) (connections v))))))]
        (= num-vertices (count (traverse #{} (ffirst edges))))))))

(defcheck solution-3862010c
  (fn conn? [g]
    (let [nodes (reduce (fn [ns n] (apply (partial conj ns) n)) #{} g)]
      (loop [edges g node-groups (map (fn [x] #{x}) nodes)]
        (if (seq edges)
          (let [[a b] (first edges)
                on-edge (fn [group] (or (contains? group a) (contains? group b)))
                groups-on-edge (filter on-edge node-groups)
                other (remove on-edge node-groups)]
            (recur (rest edges) (conj other (apply clojure.set/union groups-on-edge))))
          (= 1 (count node-groups)))))))

(defcheck solution-396391b
  (fn [s]
    (= 1 (count (reduce (fn [x y]
                          (let [f (first y)
                                l (last y)
                                p (clojure.set/select #(or (contains? % f)
                                                           (contains? % l))
                                    x)
                                q (clojure.set/difference x p)
                                t (if (= f l) #{f} #{f l})
                                ]
                            (if (empty? p)
                              (conj q t)
                              (conj q (apply clojure.set/union (conj p t))))))

                  #{}
                  s)))))

(defcheck solution-3991f4cb
  (fn f1 [c]
    (let [[c1 & c2] (vec c)]
      (cond (empty? c2) true
            (empty? c1) false
            :else (let [tmp (group-by count
                              (map #(apply disj (set %) c1) c2))]
                    (f1 (cons (reduce into #{} (tmp 1)) (tmp 2)))
                    )))))

(defcheck solution-39d156a9
  (fn graphconn [verts]
    (let [all-nodes (reduce (fn [acc v] (conj acc (first v) (second v))) #{} verts)]
      (letfn [(walk-graph [nodes-to-visit verts visited]
                (loop [visit (vec nodes-to-visit) remaining-verts verts already-visited visited]
                  (if (empty? visit)
                    (if (empty? remaining-verts) true false)
                    (let [node (first visit)
                          matching (filter (fn [v] (some #{node} v)) (vec remaining-verts))
                          non-matching (filter (fn [v] (not (some #{node} v))) (vec remaining-verts))
                          non-visited (reduce
                                        (fn [acc v] (let [to-node (if (= (first v) node) (second v) (first v))]
                                                      (when (not (contains? already-visited to-node)) (conj acc to-node))))
                                        #{} matching)]
                      (recur (concat (rest visit) non-visited) non-matching (conj already-visited node) )
                      ))))]
        (let [vert (first verts)
              nodes-to-visit (conj #{} (first vert) (second vert))]
          (walk-graph nodes-to-visit verts #{})))
      )
    ))

(defcheck solution-3a2cb5dd
  (fn [i-set]
    (let [
          nodes (reduce
                  #(conj (conj %1 (first %2)) (last %2))
                  #{}
                  i-set)

          n-count (count nodes)
          n-range (range n-count)
          n-map (apply hash-map (interleave nodes n-range))
          n-index (fn [i-node] (get n-map i-node))

          edges (concat i-set (map #(apply vector (reverse %)) i-set))
          grouped-edges (group-by #(n-index (first %)) edges)

          n-dest (fn [i-node]
                   (map last (get grouped-edges (n-index i-node))))
          walk-graph (fn discover
                       ([i-node] (discover #{} (hash-set i-node)))
                       ([result discovered]
                        (if (zero? (count discovered))
                          result
                          (let [
                                current (first discovered)
                                n-result (conj result current)
                                new-nodes (filter #(not (contains? n-result %)) (n-dest current))
                                ]
                            (discover n-result (reduce conj (apply hash-set (rest discovered)) new-nodes))))))
          ]
      (= nodes (walk-graph (first (first i-set)))))))

(defcheck solution-3ae5a27e
  (fn [s]
    (let [deal (sort-by second s)
          route (reduce
                  (fn [a b]
                    (cond
                      (contains? a (first b)) (conj a (second b))
                      (contains? a (second b)) (conj a (first b))
                      :else a))
                  (into #{} (first deal)) (rest deal))
          all (reduce into #{} s)]
      (= all route))))

(defcheck solution-3b16ccd7
  (fn cn? [g]
    (let [ gs (map set g)
          find-eq (fn [eq]
                    (reduce into eq
                      (filter #(some eq %) (map set gs))))
          drop-incr (fn [l]
                      (drop-while #(not= (count (first %)) (count (second %))) (partition 2 1 l)))]
      (=
        (reduce into gs)
        (ffirst (drop-incr (iterate find-eq (first gs))))
        ))))

(defcheck solution-3b31e99f
  (fn [es]
    (let [xs (seq es)
          vs (seq (into #{} (flatten xs)))
          l0 (for [i vs j vs] [i j])
          l1 (for [[i j] l0] (if (= i j) 1 0))
          m0 (apply (partial assoc {}) (interleave l0 l1))
          xs1 (concat xs (for [[x y] xs] [y x]))
          m1 (apply (partial assoc m0)
               (interleave xs1 (repeat (count xs1) 1)))]
      (letfn [(mmult [m1 m2]
                (let [l2 (for [[i j] l0] (apply + (for [k vs] (* (m1 [i k]) (m2 [k j])))))]
                  (apply (partial assoc {}) (interleave l0 l2))))]
        (not (some zero? (vals (nth (iterate (partial mmult m1) m1) (dec (count vs))))))))))

(defcheck solution-3b6e23e9
  (fn [g]
    (letfn
     [(add-edge [edge graph]
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
        )]
      (-> g graph-from-edges graph-is-connected?)
      )
    ))

(defcheck solution-3bc38aff
  (fn [edges]
    (let [nodes (->> edges
                  (map (fn [[a b]] (merge {a [b]} {b [a]})))
                  (apply merge-with concat))
          passed (atom #{})]
      (letfn [(go [node-id]
                (when-not (@passed node-id)
                  (swap! passed conj node-id)
                  (doseq [node-id' (nodes node-id)]
                    (go node-id'))))]
        (go (key (first nodes)))
        (= (count @passed)
          (count nodes))))))

(defcheck solution-3bcd47a1
  (fn single-connect? [graph]
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
      (full-pocket? pocket))))

(defcheck solution-3c0cd041
  #((fn graph-con [gs]
      (if (= 1 (count gs))
        true
        (letfn [(v-inc? [g] ((complement empty?) (clojure.set/intersection (first gs) g)))]
          (let [bg (filter v-inc? (rest gs)) gs (remove v-inc? (rest gs))]
            (if (empty? bg) false (graph-con (cons (reduce clojure.set/union bg) gs))) )))) (map set %)))

(defcheck solution-3c77c600
  (fn connected [edges]
    (letfn [
            (connect [components edge]
              (let [nodes (set edge),
                    connected   (filter #(not-empty (clojure.set/intersection % nodes)) components),
                    unconnected (filter #(empty?    (clojure.set/intersection % nodes)) components)]
                (conj unconnected
                  (apply clojure.set/union (conj connected nodes)))))]
      (= 1 (count
             (reduce connect [] edges))))))

(defcheck solution-3cbb12a5
  (fn [g]
    (let [nodes (reduce (fn [vs [u v]] (conj vs u v)) #{} g)]
      (loop [us [(first nodes)] reached #{(first nodes)}]
        (let [newly-reached
              (for [u us [a b] g
                    :let [x (cond (= u a) b (= u b) a)]
                    :when (and x (not (reached x)))]
                x)]
          (if (empty? newly-reached)
            (= reached nodes)
            (recur newly-reached (into reached newly-reached))))))))

(defcheck solution-3ceac36c
  (fn connected?
    [connections]
    (if (= 1 (count connections))
      true
      (let [taken-tuple (first connections)
            to-reduce (next connections)
            go-forward  (some #(or (= (first %) (first taken-tuple))
                                   (= (second %) (first taken-tuple))) to-reduce)
            to-collapse (if go-forward (first taken-tuple) (second taken-tuple))
            sub         (if go-forward (second taken-tuple) (first taken-tuple))]
        (if (not (some #(or (= (first %) to-collapse)
                            (= (second %) to-collapse)) to-reduce))
          false ;; we can't reduce any further so give up
          (connected? (map (fn [[a b]]
                             (let [a (if (= a to-collapse) sub a)
                                   b (if (= b to-collapse) sub b)]
                               [a b]))
                        to-reduce)))))))

(defcheck solution-3dc906af
  (fn [edges]
    (letfn [(joint? [a b]
              (not (empty? (clojure.set/intersection a b))))
            (conj-edge [components edge]
              (let [newcomp (set edge)
                    {conn true, unconn false} (group-by #(joint? newcomp %) components)]
                (conj unconn (apply clojure.set/union newcomp conn))))]
      (->> edges
        (reduce conj-edge [])
        count
        (= 1)))))

(defcheck solution-3dd9b541
  (fn graph-con ([graph] (graph-con (first graph) (rest graph)))
    ([item graph]
     (if (empty? graph) true
                        (let [check #(or (= (first %1) (first item))
                                         (= (second %1) (first item))
                                         (= (first %1) (second item))
                                         (= (second %1) (second item)))
                              newGraph (filter check graph)]
                          (if (empty? newGraph) false
                                                (reduce #(or %1 %2) (map (fn [it]
                                                                           (graph-con it (filter #(not= it %1) graph)))
                                                                      newGraph))))))))

(defcheck solution-3e102d60
  (fn [graph]
    (let [all-nodes (into #{} (apply concat graph))]
      (letfn [(neighbours [node]
                (apply concat (filter #(some (partial = node) %) graph)))]
        (loop [q (conj #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) (ffirst graph))
               seen #{}]
          (if (empty? q)
            (= seen all-nodes)
            (let [node (peek q)]
              (if (seen node)
                (recur (pop q) seen)
                (recur (into (pop q) (neighbours node)) (conj seen node))))))))))

(defcheck solution-40593abc
  (fn [E]
    (let [G (reduce (fn [R [a b]] (assoc R a (conj (R a #{}) b) b (conj (R b #{}) a))) {} E)
          C (loop [R #{(first (keys G))}]
              (let [D (mapcat (fn [v] (apply disj (G v) R)) R)]
                (if (empty? D) R (recur (into R D)))))]
      (and (= (count C) (count G))
           (<= (count (filter (fn [v] (= 1 (mod (count v) 2))) (vals G))) 4)))))

(defcheck solution-40f7e3b6
  (fn connected? [graph]
    (->>
      graph
      ((fn add-pair [connected pairs]
         (if (empty? pairs)
           connected
           (let [[x y] (first pairs)]
             (recur
               (cond
                 (and (connected x) (connected y))
                 (into {} (map (fn [[a b]] [a (get {(connected y) (connected x)} b b)]) connected))
                 (connected x) (assoc connected y (connected x))
                 (connected y) (assoc connected x (connected y))
                 :else (assoc connected x x y x))
               (rest pairs))))) {})
      vals
      set
      count
      (= 1)
      )))

(defcheck solution-42b7e05c
  (fn [ts]
    (let [m (reduce
              (fn [im [t1 t2]] (merge-with into im {t1 #{t2}} {t2 #{t1}}))
              {}
              ts)
          [k v] (first m)]
      (loop [conn-nodes #{} not-chk-nodes #{k}]
        (if (empty? not-chk-nodes)
          (= (count conn-nodes) (count m))
          (let [fst (first not-chk-nodes)
                rst (set (rest not-chk-nodes))]
            (if (contains? conn-nodes fst)
              (recur conn-nodes rst)
              (recur (conj conn-nodes fst)
                (into rst (get m fst))))))))))

(defcheck solution-43050d0
  (fn connected? [xs]
    (letfn [ (trans-closure [xs]
               (loop [xs xs]
                 (let [nxs
                       (reduce
                         (fn [a [x1 x2]]
                           (reduce (fn [b [y1 y2]]
                                     (if (= x2 y1) (conj b [x1 y2]) b))
                             a
                             a))
                         xs
                         xs )]
                   (if (= nxs xs) nxs (recur nxs))
                   )
                 ))]
      (let [nodes (distinct (flatten (seq xs)))
            pairs (for [i nodes j nodes :when (not= i j)] [i j])
            xs (into xs (map reverse xs))
            closure (trans-closure xs)]
        (every? identity (map #(or (closure %) (closure (reverse %))) pairs))
        ))
    ))

(defcheck solution-4495b14a
  (fn [l]
    (let [r (fn [e s] (remove #(= e %) s))
          adjs (fn [x y] (filter #(some (set x) (set %)) (r x y)))
          h (fn [f lst] (reduce #(or % %2) (map #(f % (r % lst)) lst)))
          g (fn f [x l]
              (cond (empty? l) true
                    (empty? (adjs x l)) false
                    :else (h f l)))
          ]
      (h g l)
      )
    ))

(defcheck solution-47266e67
  (letfn [(collapse-components [comps [x1 x2]]
            (let [comp1 (first (filter #(contains? % x1) comps))
                  comp2 (first (filter #(contains? % x2) comps))
                  newcomp (clojure.set/union comp1 comp2)]
              (conj
                (clojure.set/difference
                  comps (into #{} [comp1 comp2]))
                newcomp)))
          (initial-components [graph]
            (into #{} (map #(into #{} [%]) (into #{} (flatten (into [] graph))))))]
    (fn [graph]
      (= 1 (count (reduce collapse-components (initial-components graph) graph))))))

(defcheck solution-47de82e
  (fn [arcs]
    (let [closed-graphs (loop [rest-arcs arcs
                               graphs #{}]
                          (if (empty? rest-arcs)
                            graphs
                            (let [[left-node right-node] (first rest-arcs)
                                  left-graph (first (filter #(% left-node) graphs))
                                  right-graph (first (filter #(% right-node) graphs))]
                              (recur (rest rest-arcs)
                                (cond (and (not left-graph) (not right-graph)) (conj graphs (into #{left-node} #{right-node}))
                                      (not right-graph) (conj (disj graphs left-graph)
                                                          (conj left-graph right-node))
                                      (not left-graph) (conj (disj graphs right-graph)
                                                         (conj right-graph left-node))
                                      (not (= left-graph right-graph)) (conj (disj graphs left-graph right-graph)
                                                                         (into left-graph right-graph))
                                      :else graphs)))))]
      (= 1 (count closed-graphs)))))

(defcheck solution-48d0f2e8
  (fn [edge-set] (let [make-graph (fn [edge-set] (loop [result {} edges edge-set]
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
                       is-connected? (fn [graph] (loop [stack [(first (keys graph))] seen #{} expected (count (keys graph))]
                                                   (if (empty? stack) (= (count seen) expected)
                                                                      (if (= (count seen) expected) true
                                                                                                    (let [current (peek stack)]
                                                                                                      (if (contains? seen current)
                                                                                                        (recur (pop stack) seen expected)
                                                                                                        (recur (reduce conj (pop stack) (into [] (graph current))) (conj seen current) expected)
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                      )
                                                   )
                                       )
                       ]
                   (is-connected? (make-graph edge-set))
                   )
    ))

(defcheck solution-49463e96
  (fn connected? [input]
    (let [coords (map set input)
          everything (apply clojure.set/union coords)
          single-pass (fn [visited]
                        (reduce
                          #(if (seq (clojure.set/intersection %1 %2))
                             (clojure.set/union %1 %2)
                             %1)
                          visited
                          coords))]
      (loop [visited (first coords)]
        (if (empty? (clojure.set/difference everything visited))
          true
          (let [updated (single-pass visited)]
            (if (= (count updated) (count visited))
              false
              (recur updated))))))))

(defcheck solution-49c06c62
  (fn gc [graph]
    (= (set (apply concat graph))

      ((fn c
         ([e] (c (ffirst e) #{} e))
         ([t v e]
          (reduce clojure.set/union v
            (let [step (filter #(or (= t (first %)) (= t (second %))) e)
                  nbs (disj (set (concat (map first step) (map second step))) t)
                  rst (clojure.set/difference e (set step))
                  new (clojure.set/difference nbs v)
                  ]
              ;(println "t" t "n" nbs "r" rst "new" new "v" v)
              (if (empty? new)
                (list #{t})
                (for [n new]
                  (c n (into v #{t}) rst))
                )
              )
            )
          )
         ) graph))))

(defcheck solution-4acdb547
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
          m  (fn [v G] (reduce #(into % (n v %2)) #{} G))]
      (loop [U #{p}]
        (let [N (into U (mapcat #(m % G) U))]
          (if (= N U)
            (= U V)
            (recur N)))
        ))))

(defcheck solution-4b43fa21
  (fn t [s]
    (loop [i #{}
           s s]
      #_(println i)
      (let [x (ffirst s)
            y (second (first s))]
        (if (empty? s) (= (count i) 1)
                       (let [m (filter #(contains? % x) i)
                             n (filter #(contains? % y) i)
                             p (filter #(not (or (contains? % x)
                                                 (contains? % y))) i)]
                         (cond (and (empty? m) (empty? n)) (recur (conj i (conj #{x} y)) (rest s))

                               (and (empty? m) (not (empty? n)))
                               (recur (set (conj p (conj (first n) x))) (rest s))

                               (and (empty? n) (not (empty? m)))
                               (recur (set (conj p (conj (first m) y))) (rest s))

                               :else
                               (recur (set (conj p (clojure.set/union (first m) (first n))))
                                 (rest s)))))))))

(defcheck solution-4b7e1c5d
  (fn [g]
    (letfn [(n [v] (remove nil? (map (fn [[a b]] (cond (= a v) b
                                                       (= b v) a
                                                       :else nil))
                                  g)))
            (d [s v]
              (if (s v)
                s
                (->> (n v)
                  (reduce d (conj s v)))))]
      (= (d #{} (first (first g)))
        (reduce into #{} g)))))

(defcheck solution-4ba8a56
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
                     false)))))]
      (zusammenhaengend? kanten-seq))))

(defcheck solution-4c16c5db
  (fn [g]
    (= 1 (count (reduce (fn [coll e]
                          (let [s1 (filter #(or (contains? % (e 0))
                                                (contains? % (e 1))) coll)
                                s2 (filter #(not (or (contains? % (e 0))
                                                     (contains? % (e 1)))) coll)]
                            (conj (set s2)
                              (clojure.set/union (set e)
                                (apply clojure.set/union s1)))))
                  #{} g)))))

(defcheck solution-4c1a5f15
  ( fn __ [edges]
    (let [edge-list (apply merge-with concat (map (fn [[s e]] (if (= s e) {s []} {s [e] e [s]})) edges))
          explore (fn [[seen frontier]]
                    (let [new-frontier (set (remove seen (mapcat edge-list frontier)))]
                      [(clojure.set/union seen new-frontier) new-frontier]))
          start (ffirst edges)
          component (ffirst (filter #(empty? (second %)) (iterate explore [#{start} #{start}])))]
      (=  (count component) (count edge-list)))))

(defcheck solution-4c29c831
  (fn connect[-set]
    (let [trans (fn l[s]
                  (let[D (fn k[ss](into ss (for [[a b] ss [c d] ss :when (= b c)] [a d])))]
                    (if (= s (D s))
                      s
                      (recur (D s)))))
          nset (into -set (map (fn[[a b]][b a]) -set))
          comp (trans nset)
          nn (count comp)
          n (count (set (for [[a _] comp] a)))
          ]
      (= nn (* n n)))))

(defcheck solution-4ccbf895
  (fn connected? [connections]
    (letfn [(connect [reachables [l r]]
              ;; Join the two sides of the reachables
              ;; Update all nodes involved to the new group
              (let [group (clojure.set/union (get reachables l #{l})
                            (get reachables r #{r}))]
                (reduce #(assoc % %2 group) reachables group)))]
      (let [reachables (reduce connect {} connections)
            nodes      (set (keys reachables))]
        ;; Only fully connected if each node can reach all nodes
        (every? (fn [node] (= nodes (reachables node)))
          nodes)))))

(defcheck solution-4cec12a8
  (fn [edges]
    (loop [[e & stack] (take 1 edges)
           unseen-nodes (set (mapcat identity edges))
           unprocessed-edges (disj edges e)]
      (cond (empty? unseen-nodes) true
            (nil? e) false
            :else (recur (concat (filter (fn [e2] (not-empty
                                                    (clojure.set/intersection (set e2) (set e))))
                                   unprocessed-edges)
                           stack)
                    (disj unseen-nodes (first e) (second e))
                    (disj unprocessed-edges e))))))

(defcheck solution-4cf86299
  (fn [edges]
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
                    (recur (concat [t] r [h])))))))))

(defcheck solution-4dbaeda
  (fn [e]
    (let [e (->> e (map set))]
      (loop [n #{(-> e first first)}]
        (let [n-e
                  (filter #(not-empty (clojure.set/intersection n %)) e)
              n-n (into n (->> n-e (map vec) flatten))]
          (if (not= n n-n)
            (recur n-n)
            (if (= n-e e)
              true
              false)))))))

(defcheck solution-4e6c0db9
  (fn [fxpt s]
    (= (reduce into #{} s)
      (fxpt (iterate  #(reduce (fn [s [a b]]
                                 (cond (s a) (conj s b)
                                       (s b) (conj s a)
                                       1 s))
                         % s)
              (set (first s)))))) #(ffirst (filter (fn [[a b]] (= a b)) (partition 2 1 %))))

(defcheck solution-4e876eec
  (fn [x]
    (let [h (reduce #(conj % %2 (reverse %2)) #{} x)
          g ((fn f [s]
               (let [r (reduce #(conj % %2) s (remove nil? (for [a s b s] (if (= (second a) (first b)) [(first a) (second b)]))))]
                 (if (= r s) r (f r)))) h)
          n (set (map first g))
          m (for [a n b n] [a b])]
      (reduce #(and % (not (nil? (g %2)))) true m))))

(defcheck solution-4ec36f36
  (fn __ [graph]
    (let [
          vertices (reduce (fn [acc [f t]] (conj acc f t) ) #{} graph)
          start (set (first graph))
          expand2Width (fn [rrr edges]
                         (reduce
                           (fn [vers [f t]]
                             (let [
                                   ff (if (vers f)
                                        (conj vers t)
                                        vers)
                                   ff2 (if (ff t)
                                         (conj ff f)
                                         ff)
                                   ] ff2))
                           rrr graph))
          expanded (loop [
                          reached start
                          ]
                     (let [
                           reachedMore (expand2Width reached graph)
                           ]
                       (if (= reached reachedMore)
                         reached
                         (recur reachedMore))))
          result (= vertices expanded)
          ]
      #_(println vertices expanded)
      result)))

(defcheck solution-4ee4ca87
  (fn
    [coll]
    (let [m (reduce (fn [m [s d]]
                      (let [ds (get m s #{})
                            ds' (conj ds d)
                            ss (get m d #{})
                            ss' (conj ss s)]
                        (assoc m s ds' d ss'))) {} coll)]
      (letfn [(bfs [q v m]
                (if (seq q)
                  (let [h (first q)
                        t (rest q)]
                    (if (contains? v h)
                      (recur t v m)
                      (let [v' (conj v h)]
                        (if (contains? m h)
                          (recur (concat t (m h)) v' m)
                          (recur t v' m)))))
                  v))]
        (= (count m) (count (bfs [(first (first coll))] #{} m)))))))

(defcheck solution-4f0ee957
  (fn connected? [edges]
    (letfn [(add-edge [g a b]
              (update-in g [a] (fn [e] (if (nil? e) [b] (conj e b)))))

            (graph [edges]
              (reduce (fn [acc [a b]]
                        (add-edge (add-edge acc a b) b a)) {} edges))

            (dfs [g n]
              (letfn [(dfs-iter [stack visited]
                        (lazy-seq
                          (when-let [cur (peek stack)]
                            (if (visited cur)
                              (dfs-iter (pop stack) visited)
                              (let [adjs (g cur)]
                                (cons cur (dfs-iter (vec (concat (pop stack) adjs))
                                            (conj visited cur))))))))]
                (dfs-iter [n] #{})))]
      (let [g (graph edges)]
        (= (set (keys g)) (set (dfs g (key (first g)))))))))

(defcheck solution-4f0f29e3
  (fn [inp]
    (if (seq inp)
      (loop [ bonds (seq inp), visited (vector (first (first bonds)))]
        (cond
          (empty? visited) false
          (empty? bonds) true
          :else
          (let [ v1 (first visited),
                rest-bonds (filter #(and (not= v1 (first %))
                                         (not= v1 (second %))) bonds),
                newly-visited (reduce
                                (fn [a b]
                                  (cond
                                    (= v1 (first b)) (conj a (second b))
                                    (= v1 (second b)) (conj a (first b))
                                    :else a
                                    ))
                                [] bonds)]
            (recur rest-bonds
              (vec (concat (rest visited) newly-visited)) )
            )
          ))
      true
      )))

(defcheck solution-4f310b3f
  (fn a [g]
    (let
     [union #(reduce conj %1 %2)
      g-new (map set g)
      vs (reduce union #{} g-new)
      m (group-by
          #(contains? % (first (first g-new)))
          g-new)]
      (if (= (count (get m false)) 0)
        true
        (if (and (= (count (get m true)) 1) (= (count (first g-new)) 1))
          false
          (a (set
               (cons
                 (disj (reduce union #{} (get m true)) (first (first g-new)))
                 (get m false)))))))))

(defcheck solution-4fb22504
  (fn [es]
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
    ))

(defcheck solution-50064829
  (fn [e]
    (let [
          g (reduce (fn [g [u v]] (assoc g u (conj (g u) v) v (conj (g v) u))) {} e)
          v (set (keys g))
          h #(into % (mapcat g %))
          ] (= v (first (drop (count v) (iterate h #{(first v)})))))))

(defcheck solution-502d4f67
  (fn f91 [st]
    (let [con (fn [[fsn scn] st]
                (-> #(or (= fsn
                           (second %))
                         (= scn
                           (first %))
                         (= scn
                           (second %))
                         (and (= fsn
                                (first %))
                              (not= [fsn scn] %))
                         (and (= scn
                                (second %))
                              (not= [fsn scn] %)))
                  (filter st)))

          acons (fn [st]
                  (vec (map #(conj (set (con % st)) %)
                         st)))

          mset (fn [st1 st2]
                 (if (empty? (clojure.set/intersection st1 st2))
                   nil
                   (clojure.set/union st1 st2)))

          tc (atom #{})

          mall (fn mall [st]
                 (if (= 1 (count st))
                   st
                   (let [rst (mset (first st)
                               (second st))]
                     (if (nil? rst)
                       (if (some #(= (second st) %)
                             @tc)
                         st
                         (do (swap! tc conj (second st))
                             (mall (vec (cons (first st)
                                          (conj (vec (nthrest st 2))
                                            (second st)))))))
                       (mall (vec (->> (nthrest st 2)
                                    (cons rst))))))))]
      (if (= 1 (count (mall (acons st))))
        true
        false))))

(defcheck solution-518dcb36
  (fn [g]
    (let [vertices (-> g seq flatten set)
          root (first vertices)
          adjacent (fn [v] (concat (for [[x y] g :when (= x v)] y) (for [[x y] g :when (= y v)] x)))
          add-to-comp (fn add [comp v] (if (some #{v} comp) comp (reduce add (conj comp v) (adjacent v))))
          connected-component (reduce add-to-comp [root] (adjacent root))]
      (= (count connected-component) (count vertices)))))

(defcheck solution-52233c8a
  (fn connected? [nodegraph]
    (letfn [(all-nodes [nodegraph]
              (into #{} (mapcat identity nodegraph)))

            (node-connection [edge node]
              (cond
                (= (first edge) node) (second edge)
                (= (second edge) node) (first edge)))

            (paths-from [nodegraph node]
              (let [nodes (map #(node-connection % node) nodegraph)
                    node-set (disj (set nodes) nil)]
                node-set))

            (rand-set [nodeset]
              (rand-nth (seq nodeset)))

            (visited-all? [nodeset visitedset]
              (= nodeset visitedset))]

      (let [nodes (all-nodes nodegraph)]
        (loop [visited #{} to-visit #{(rand-set nodes)}]
          (if (empty? to-visit)
            (visited-all? visited nodes)
            (let [next-visit (rand-set to-visit)
                  paths (paths-from nodegraph next-visit)
                  new-paths (clojure.set/difference paths visited)]
              (recur (conj visited next-visit) (into (disj to-visit next-visit) new-paths)))))))))

(defcheck solution-529166d3
  (letfn [(intersects? [s1 s2]
            (not (empty? (clojure.set/intersection (set s1) (set s2)))))]
    (fn connected? [g]
      (= 1
        (count
          (reduce (fn [subgroups e]
                    #_(prn subgroups e)
                    (conj
                      (remove (partial intersects? e) subgroups)
                      (set (apply concat (filter (partial intersects? e) subgroups)))))
            (map list (set (flatten (seq g))))
            g))))))

(defcheck solution-52f698ea
  (fn [s]
    (let [all (set (flatten (seq s)))
          trans-cloj (fn [s]
                       (let [s-bi (set (concat s (map reverse s)))
                             s2 (reduce (fn [t [a b]]
                                          (set (concat t (map (fn [[c d]] [a d])
                                                           (filter (fn [[c d]]
                                                                     (= b c)) s-bi)))))
                                  s-bi s-bi)]
                         (if (= s2 s) s (recur s2))))
          tc-map (apply merge-with #(set (concat %1 %2))
                   (map (fn [[a b]] {a #{b}}) (trans-cloj s)))]
      (apply = all (vals tc-map)))))

(defcheck solution-53a0d598
  (fn [pairs] (= (count (distinct (apply concat pairs)))
                (count (loop [s (set (first pairs))]
                         (let [t (into s (for [[a b] pairs [a b] [[a b] [b a]] :when (s a)] b))]
                           (if (= s t)
                             s
                             (recur t))))))))

(defcheck solution-53a1572f
  (fn prob-0091
    [rels]
    (let [add-dir-rel (fn add-dir-rel
                        [rel p-map s-map]
                        (let [[p s] rel
                              rels (for [p-new (conj (p-map p) p)
                                         s-new (conj (s-map s) s)]
                                     [p-new s-new])

                              s-rels (map #(array-map (first  %) #{(second %)} ) rels)
                              p-rels (map #(array-map (second %) #{(first  %)} ) rels)

                              new-pm (apply merge-with into p-map p-rels)
                              new-sm (apply merge-with into s-map s-rels)]

                          [new-pm new-sm]))

          add-undir-rel (fn add-undir-rel
                          [rel p-map s-map]
                          (let [[a b] rel]
                            (apply add-dir-rel [a b]
                              (add-dir-rel [b a] p-map s-map))))

          ]

      (let [[p-map s-map] (reduce #(apply add-undir-rel %2 %1) [{} {}] rels)]
        (and (not (empty? s-map)) (= (count p-map) (count (val (first p-map)))))))))

(defcheck solution-53b5a8af
  (fn [edges]
    (letfn [(add-edge [[f & r] edge]
              (cond  (nil?  f)     [(set edge)]
                     (some f edge) (add-edge r (into f edge))
                     :else         (cons f (add-edge r edge))))]
      (= 1 (count (reduce add-edge [] edges))))))

(defcheck solution-53b61de2
  (fn connected? [br]
    (letfn [(follow [m]
              (let [new (merge-with (fn [a b] (set (mapcat #(if-let [v (m %)] (set (concat b v)) (set b)) b))) m m)]
                (if (= new m) m (follow new))))]
      (let [graph (concat br (map reverse br))
            reachable (set (follow (reduce (fn [m [k v]] (let [old (m k)] (assoc m k (if old (conj old v) #{v})))) {} graph)))
            all-points (set (map first reachable))]
        (every? (fn [s] (= s all-points)) (map second reachable))))))

(defcheck solution-544c092c
  (fn connected? [g]
    (let [g (sort-by first (concat g (map reverse g)))]
      (= (count (reduce (fn [seen [n m]] (if (seen n) (conj seen m) seen)) #{(ffirst g)} g))
        (count (distinct (flatten g)))))))

(defcheck solution-54807fcd
  (fn [g]
    (loop [vs #{(ffirst g)} es g a #{}]
      (if-let [v (first vs)]
        (let [{i v o nil} (group-by #(some #{v} %) es)
              nvs (mapcat #(remove #{v} %) i)]
          (recur (into (disj vs v) nvs) o (into a i)))
        (= a g)))))

(defcheck solution-54f09ba7
  (fn [edges]
    (boolean
      (seq
        (let [[edge & edges]
              (sort-by (comp vec sort) edges)]
          (reduce
            (fn [nodes [node1 node2]]
              (if (or (nodes node1)
                      (nodes node2))
                (conj nodes node1 node2)
                #{}))
            (set edge)
            edges))))))

(defcheck solution-5592c3d6
  (fn connected? [ss]
    (or (<= (count ss) 1)
        (letfn [(find-connect [node nodes]
                  (let [[x y] node]
                    (into #{node}
                      (for [[x' y'] nodes
                            :when (or (= x y') (= y x') (= x' x))
                            ]
                        [x' y']))))
                (connect? [ns1 ns2]
                  (if (empty? ns2)
                    true
                    (let [ns1' (reduce (fn [an s]
                                         (clojure.set/union
                                           (find-connect s ns2) an)) #{} ns1)
                          ns2' (clojure.set/difference ns2 ns1')
                          ]
                      (if (= ns1 ns1')
                        false
                        (connect? ns1' ns2')))))
                ]
          (connect? #{(first ss)} ss)))))

(defcheck solution-56a3b217
  (fn [graph]
    (let [all-nodes (set (flatten (vec graph)))
          nodes-map (reduce #(merge-with concat %1 %2) (for [[x y] graph] {x [y]}))
          connected (fn connected [remaining node]
                      (if (empty? remaining)
                        []
                        (let [links (get remaining node)
                              new-remaining (dissoc remaining node)
                              links-to-node (for [[up-key up-links] remaining
                                                  :when (some #{node} up-links)]
                                              up-key)]
                          (set (concat [node]
                                 links
                                 (mapcat #(connected new-remaining %) links)
                                 (mapcat #(connected new-remaining %) links-to-node))))))]

      (= all-nodes (connected nodes-map (first all-nodes))))))

(defcheck solution-573d8826
  (fn connected? [graph]
    (loop [g graph
           parsed #{(first (first graph))}]
      (if (nil? g) true
                   (let [spanned-g (group-by (fn [[from to]] (or (contains? parsed from) (contains? parsed to))) g)
                         filtered-rest (spanned-g false)
                         filtered-out (spanned-g true)
                         new-parsed (into parsed (flatten filtered-out))]
                     (if (= filtered-rest g) false
                                             (recur filtered-rest new-parsed)))))))

(defcheck solution-57620db3
  #(loop [n (into #{} (first %)) e %] (if (empty? e) true
                                                     (let [v (some (fn [[a b]] (and (or (n a) (n b)) [a b])) e)]
                                                       (if v (recur (apply (partial conj n) v) (disj e v)) false)))))

(defcheck solution-57739e8
  (fn graph-con [raw-edges]
    (let [keys (into #{} (flatten (seq raw-edges)))
          paths (into {} (map (fn [[k v]] (vector k (disj (into #{} (flatten v)) k)))
                           (group-by first (concat raw-edges (map reverse raw-edges)))))
          get-children (fn [[current unseen]]
                         (map #(list % (clojure.set/difference unseen (paths current)))
                           (clojure.set/intersection (paths current) unseen)))
          branch? (fn [data] ((complement empty?) (get-children data)))
          start (first keys)]
      (boolean (some #(empty? (last %))
                 (tree-seq branch? get-children [start (disj keys start)])))
      )))

(defcheck solution-57d514aa
  #(let [f (fn f [g [[a b] & c]]
             (or (nil? a) (and (contains? g a) (f (conj g b) c))))
         [a & b] (->> % (vec) (map sort) (map vec) (sort))]
     (f (set a) b)))

(defcheck solution-5856ddff
  (fn breadth-first [g]
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
            (count (set (flatten (seq g))))))))))

(defcheck solution-586a2b3c
  (fn connected?
    [edges]
    (letfn [(grow-paths [edges]
              (let [[edge & more] (seq edges)]
                (reduce path-to #{edge} more)))
            (path-to [a b]
              (let [ab (concat (first a) b)]
                (if (apply distinct? ab)
                  (conj a b)
                  (conj (rest a) (distinct ab)))))
            (changed? [[x y]]
              (not= (count x) (count y)))]
      (->> edges
        (iterate grow-paths)
        (partition 2)
        (drop-while changed?)
        ffirst
        count
        (= 1)))))

(defcheck solution-5951fab2
  (fn connected?
    ([nodes remaining-attempts [[a b] & remaining-links]]
     (or
      (and
       (or (empty? nodes) (nodes a) (nodes b))
       (or (empty? remaining-links) (connected? (conj nodes a b) (count remaining-links) remaining-links)))
      (and (pos? remaining-attempts)
           (connected? nodes (dec remaining-attempts) (concat remaining-links [[a b]])))))
    ([links] (connected? #{} (count links) (into [] links)))))

(defcheck solution-59a9edc5
  (fn [s]
    (let [m (reduce-kv (fn [m k v] (assoc m k (mapv second v))) {} (group-by first (concat s (map (juxt second first) s))))]
      (loop [unv (set (mapcat identity (drop 1 s))) v (set (first s))]
        (let [nv (set (mapcat #(get m %) v))
              unv2 (remove nv unv)]
          (if (= unv unv2) (empty? unv) (recur unv2 nv)))))))

(defcheck solution-5a4ae42a
  (fn is-connected [graph]
    (let
     ;; reducedgraphset gets rid of all edges [a b] when [b a] exists in the graph
     [reducedgraphset (reduce #(if (contains? %1 [(second %2) (first %2)]) %1 (conj %1 %2)) #{} graph)

      ;; Helper function for turning a set of edges into points
      topoints (fn [edges] (set (reduce #(conj (conj %1 (first %2)) (second %2)) edges)))

      ;; All of the points we need to visit
      allpoints (topoints reducedgraphset)

      ;; Helper function that finds neighboring edges
      neighbors (fn [gr e] (set (filter
                                  #(not (empty? (clojure.set/intersection (set %) (set e))))
                                  gr)))

      ;; Build the closure of all edges attached to this edge.
      closure-builder (fn closure-builder [graph edge]
                        (loop [builder #{edge}
                               edges-to-examine [edge]]
                          (if (empty? edges-to-examine)
                            builder
                            (let [edge (first edges-to-examine)
                                  next-edges (clojure.set/difference (neighbors graph edge) builder)]
                              (recur (set (concat builder next-edges))
                                (concat next-edges (rest edges-to-examine)))))))]

      ;; Check the closure of every point/edge in the set.
      (if (= 1 (count reducedgraphset)) true
                                        (not (empty?
                                               (filter #( = (count allpoints) (count %))
                                                 (map topoints
                                                   (for [edge reducedgraphset]
                                                     (closure-builder reducedgraphset edge))))))))))

(defcheck solution-5a67de76
  (fn [e]
    (let [n (set (flatten (seq e)))
          f (memoize (fn [a]
                       (set (for [[x y] e]
                              (cond
                                (= a x) y
                                (= a y) x
                                :else a)))))
          g (fn [v]
              (let [c (count v)
                    u (reduce into v (map f v))
                    k (count u)]
                (if (> k c)
                  (recur u)
                  u)))]

      (every? true?
        (for [a n
              :let [v (g #{a})]
              b n]
          (contains? v b))))))

(defcheck solution-5b1caa31
  (fn gc[s]
    (letfn [(vertices[s]
              (reduce into #{} s))
            (edges-with[e]
              (set (filter #(some #{e} %) s)))]
      (let [vs (vertices s)
            n  (count vs)
            x  (first vs)]
        (loop [L  #{x}
               K  [x]]
          (if (and K (not-empty K))
            (let [[y & K] K
                  yzs (edges-with y)
                  zs (filter #(not (L %)) (vertices yzs))]
              (recur (set (concat L zs)) (concat K zs)))
            (= n (count L))))))))

(defcheck solution-5b99ad4
  (fn [edges]
    (letfn [(getij [cm i j] (nth (nth cm i) j))
            (setij [cm i j newv] (assoc (vec cm) i (assoc (vec (nth cm i)) j newv)))
            (markcon [cm i j] (setij (setij cm i j 1) j i 1))]
      (let [vedges (vec edges)
            nodes (distinct (flatten (seq edges)))
            n (count nodes)
            nindexmap (zipmap nodes (range n))
            startm0 (map #(assoc (vec (repeat n -1)) % 1) (range n))
            startm (reduce (fn [mat edge]
                             (let [i (nindexmap (first edge)) j (nindexmap (second edge))]
                               (markcon mat i j))) startm0 vedges)
            endm (reduce (fn [mat [k i j]]
                           (if (and (not= -1 (getij mat k i)) (not= -1 (getij mat k j)))
                             (markcon mat i j) mat))
                   startm (for [k (range n) i (range n) j (range n)] [k i j]))]
        (empty? (filter #(= % -1) (first endm)))
        ))))

(defcheck solution-5c04d220
  (fn cg? [g]
    (let [n (set (mapcat identity g))
          cg (group-by first (set (mapcat (fn [[a b]] [[a b] [b a]]) g)))]
      (loop [loc (first (first g)) tn #{loc} tp {} d (zipmap n (repeat 0))]
        (cond (= (count tn) (count n)) true
              (= (set (mapcat cg tn)) (set (mapcat tp tn))) false
              :else (let [id (d loc)
                          p (nth (cycle (cg loc)) id)
                          n_g (second p)]
                      (recur n_g (conj tn n_g) (merge-with concat tp {loc [p]}) (update-in d [loc] inc))))))))

(defcheck solution-5c7f060e
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
                       )))       ]
      (is-con (set (apply cons(first g)))  ))))

(defcheck solution-5cf3079c
  (fn [g]
    (let [step1? (fn [a b]
                   (or (= (first a) (first b))
                       (= (first a) (second b))
                       (= (second a) (first b))
                       (= (second a) (second b))))
          step1s (fn [as bs]
                   (set
                     (for [a as b bs
                           :when (step1? a b)]
                       b)))
          trav1 (fn [as bs]
                  (let [abs (step1s as bs)]
                    [(clojure.set/union as abs) (clojure.set/difference bs abs)]
                    ))
          trav (fn f [as bs]
                 (if (empty? bs)
                   [as bs]
                   (let [[as1 bs1] (trav1 as bs)]
                     (if (= as as1)
                       [as bs]
                       (f as1 bs1)))))]
      (empty? (second (trav #{(first g)} (set (rest g))))))))

(defcheck solution-5dbf40c6
  (fn connected? [g]
    (let [begin (-> g seq flatten first)
          connected-iter? (fn [visited v q]
                            (if (empty? q)
                              (= v (-> g
                                     seq
                                     flatten
                                     set))
                              (let [n (first q)
                                    connected-with-n (->> g
                                                       (filter (partial some #{n}))
                                                       flatten
                                                       set)]
                                (if (visited n)
                                  (recur visited
                                    (clojure.set/union v connected-with-n)
                                    (rest q))
                                  (recur (conj visited n)
                                    (clojure.set/union v connected-with-n)
                                    (clojure.set/union (set (rest q)) connected-with-n))))))]
      (connected-iter? #{} #{begin} #{begin}))))

(defcheck solution-5e245a81
  (fn [s]
    (let [exp (fn [a b] (if (seq (filter a b)) (set (remove a b)) nil))
          conn? (fn [r q] (let [nr (reduce into r (map #(exp r %) q))]
                            (if (= r nr)
                              (= r (set (flatten (vec s))))
                              (recur nr q))))]
      (conn? (set (first s)) (map set (next s))))))

(defcheck solution-5f4498ec
  (fn connected? [g]
    (letfn [(bfs-step [g nodes]
              ; expand a set of nodes by adding ones reachable in one step
              (->> g (filter (fn [e] (some #(contains? nodes %) e))) flatten set))
            (fixed-point [f a]
              ; return the fixed point that (f a) converges to
              (let [fa (f a)] (if (= a fa) a (fixed-point f fa))))
            (component [g n]
              ; return the set of nodes in g reachable from node n
              (fixed-point (partial bfs-step g) #{n}))]
      ;(component g (ffirst g))
      (= (set (mapcat set	g))   	      ; all nodes in g
        (component g (ffirst g))))))

(defcheck solution-5fe2ca07
  (fn p91 [s]
    (let [elems (distinct (flatten (vec s)))
          size (count elems)
          tbl (make-array Boolean size size)]
      (dotimes [i size]
        (dotimes [j size]
          (when (= i j) (aset tbl i j true))
          (when (or (s [(nth elems i) (nth elems j)])
                    (s [(nth elems j) (nth elems i)]))
            (aset tbl i j true)
            (aset tbl j i true))))
      (dotimes [k size]
        (dotimes [i size]
          (dotimes [j size]
            (when (and (aget tbl i k) (aget tbl k j))
              (aset tbl i j true)
              (aset tbl j i true)))))
      (every? boolean
        (for [i (range size) j (range size)]
          (aget tbl i j))))))

(defcheck solution-5fe55e18
  (fn [edges]
    (let [; Build undirected graph.
          ; Each vertex is mapped to its set of adjacent vertices.
          ; Undirected means if u is adjacent to v then v is adjacent to u.
          ; Self-loops are accomodated but don't figure into anything.
          g (apply merge-with clojure.set/union
              (map (fn [[u v]]
                     (if (= u v)
                       {u #{v}}
                       {u #{v} v #{u}}))
                edges))
          ; Is there a path from u to v?
          path? (fn path? [u v visited]
                  (when (not (visited u))
                    (let [adj (g u)]
                      (or (adj v) (some #(path? % v (conj visited u)) adj)))))]
      ;(every? identity (for [u (keys g) v (keys g)]
      ;                   (path? u v #{})))))
      ; Does unnecessary work since u->v implies v->u in an unordered
      ; graph. No need to test u->u either since that follows if u
      ; connects to every other vertex.
      ; Solution: test 2-combinations (subsets) instead of ordered pairs:
      (loop [[u & vs] (keys g), flag true]
        (cond (nil? vs) (true? flag) ; both conditions are possible!
              (not flag) false       ; found unconnected pair
              :else (recur vs (every? #(path? u % #{}) vs)))))))

(defcheck solution-605d643d
  (fn connected? [edges]
    (let [sets  (set (map set edges))
          nodes (set (mapcat identity sets))
          grow  (fn [nodes]
                  (set (mapcat identity (for [n nodes s sets :when (contains? s n)] s))))
          connected (loop [prev #{(first nodes)} next (grow prev)]
                      (if (= prev next) prev (recur next (grow next))))]
      (= nodes connected))))

(defcheck solution-60d2b3a8
  (fn [edges]
    (let [edge-map (reduce (fn [m [v1 v2]]
                             (-> m
                               (update-in [v1] (fnil conj #{}) v2)
                               (update-in [v2] (fnil conj #{}) v1)))
                     {} edges)
          conn-fn (fn conn-fn [conn-set key]
                    (loop [vs (get edge-map key)
                           s conn-set]
                      (if (empty? vs)
                        s
                        (let [v (first vs)]
                          (if (contains? s v)
                            (recur (rest vs) s)
                            (recur (rest vs) (clojure.set/union s (conn-fn (conj s v) v))))))))
          all-vertices (set (keys edge-map))
          conn-set (conn-fn #{} (first all-vertices))]
      (= conn-set all-vertices))))

(defcheck solution-6163edf5
  (fn p91 [routes]
    (letfn [(mks [ngs r]
              (let [cs (filter (fn [ng] (some #(ng %) r)) ngs)
                    th (reduce conj #{} r)]
                (if (every? nil? cs) (conj ngs th)
                                     (conj (reduce disj ngs cs) (reduce conj #{} (reduce concat (conj cs th)))))))
            (mkg [a n rs]
              (if (empty? rs) n
                              (mkg (reduce conj a (first rs)) (mks n (first rs)) (next rs) )))]
      (= 1 (count (mkg #{} #{} routes))))))

(defcheck solution-61bf5412
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
      (and connected ))))

(defcheck solution-61fc6a9a
  (fn [edgeSet]
    (let [nodeSet (set (flatten (seq edgeSet)))];get the node set
      (loop[connectivity (for [[n1 n2] edgeSet] [#{n1}, n2])];inital connectivity where the first element is the visited nodes
        (let[extendedConnectivity (for [[linked node] connectivity, [n1 n2] edgeSet :when (and (or (= n1 node) (= n2 node)) (or (nil? (linked n1)) (nil? (linked n2))))]
                                    [(conj linked n1 n2) (if (= node n1) n2 n1)])];extend the connectivity when a newly linked node is found
          (if (not-any? #(= nodeSet %) (map first connectivity));if not a fully connected graph is found
            (if (empty? extendedConnectivity) false;and no extended connectivity can be found either
                                              (recur extendedConnectivity));extend the connectivity
            true))))))

(defcheck solution-622708c4
  (fn [coll]
    (let [members (set (apply concat coll))
          mcount (count members)]
      (= (* mcount mcount)
        (count
          (loop [coll coll]
            (let [x (set (concat coll
                           (for [[a b] coll [c d] coll :when (= b c)] [a d])
                           (map reverse coll)))]
              (if (= x coll)
                x
                (recur x)))))))))

(defcheck solution-636b3ff4
  (fn [g]
    (let [a (set (flatten (seq g)))
          f (first g)]
      (loop [r (set f)]
        (let [n (flatten (filter (fn[[a b]]
                                   (or (and (r a) (not (r b)))
                                       (and (r b) (not (r a))))) g))]
          (if (empty? n)
            (= r a)
            (recur (set (concat r n )))))))))

(defcheck solution-63731f64
  (fn [C O I g]
    (O
      (reduce
        (fn [s [f t]]
          (let [{[a & [r]] true o false :or {o []}}
                (group-by #(or (C % f) (C % t)) s)]
            (I #{(I a r)} o)
            ))
        (I #{} (map #(hash-set %) (distinct (flatten (seq g)))))
        (seq g)))) contains? #(= 1 (count %)) into)

(defcheck solution-64d5bbb0
  (fn [g]
    (let [v (set (flatten (reduce conj '() g)))
          f (fn [f g v] (filter #(= (f %) v) g))
          m (fn [f c] (map f c))
          s (first v)]
      (loop [c [s]
             r #{s}]
        (let [n (reduce into (reduce into #{} (map #(m last (f first g %)) c))
                  (map #(m first (f last g %)) c))
              nr (into r n)]
          (condp = nr
            v true
            r false
            (recur n nr)))))))

(defcheck solution-64f22309
  (fn [graph]
    (loop [connected (into #{} (first graph)), queue (rest graph), later [], queue-size (count queue) ]
      (if (seq queue)
        (if (some (into #{} (first queue)) connected)
          (recur (apply conj connected (first queue)) (rest queue) later queue-size)
          (recur connected (rest queue) (conj later (first queue)) queue-size))
        (if (seq later)
          (if (== (count later) queue-size)
            false
            (recur connected later [] (count later)))
          true)))))

(defcheck solution-654b5799
  (fn [s]
    (loop [sets (map set s)]
      (let [combined-sets
                          (set
                            (for
                             [a sets b sets
                              :when (and (not= a b) (not-empty (clojure.set/intersection a b)))]
                              (into a b)))
            clear-subsets (fn [s1 s2]
                            (filter
                              (fn [ss]
                                (not-any?
                                  #(clojure.set/subset? ss %)
                                  s1)
                                )
                              s2)
                            )
            ]
        (if (empty? combined-sets)
          (= 1 (count sets))
          (recur (into combined-sets (clear-subsets combined-sets sets)))
          )
        ))
    ))

(defcheck solution-66740236
  (fn x [edges]
    (letfn [(to-graph [edges]
              (reduce
                #(let [a (first %2) b (second %2) a-edges (get %1 a []) b-edges (get %1 b [])]
                   (assoc %1 a (conj a-edges %2) b (conj b-edges %2)))
                {}
                edges))
            (dfs [n graph visited]
              (let [visited' (conj visited n)
                    n-edges (get graph n)
                    dest #(if (= (first %) n) (second %) (first %))]
                (reduce
                  (fn [visited'' edge]
                    (let [d (dest edge)]
                      (if (contains? visited'' d) visited'' (dfs d graph visited''))))
                  visited'
                  n-edges)))]
      (let [g (to-graph edges)
            nodes (into #{} (keys g))
            connected (dfs (first nodes) g #{})]
        #_(println connected)
        (= nodes connected)))))

(defcheck solution-67144405
  (fn connected? [x]
    (loop [i (first x)
           rst (rest x)]
      (if (empty? rst)
        true
        (let [found (filter #(some (set i) %) rst)]
          (if (empty? found)
            false
            (recur
              (apply concat i found)
              (remove (set found) rst))))))))

(defcheck solution-67b683ca
  (letfn [(add-edge [graph u v]
            (update-in graph [u] (fnil #(conj % v) #{})))
          (build-graph [edges]
            (reduce (fn [graph [u v]] (-> graph (add-edge u v) (add-edge v u)))
              {}
              edges))
          (dfs [graph visited start]
            (reduce #(into %1 (dfs graph %1 %2))
              (conj visited start)
              (clojure.set/difference (graph start) visited)))]
    (fn connected? [edges]
      (let [graph   (build-graph edges)
            vertice (set (keys graph))]
        (= vertice (dfs graph #{} (first vertice)))))))

(defcheck solution-68ce40b0
  (fn [graph]
    (let [vertices (apply hash-set (distinct (flatten (seq graph))))
          start (first vertices)
          neighbors (fn [v] (apply hash-set (distinct (for [x vertices :when (or (graph [x v]) (graph [v x]))] x))))]
      (letfn [(bfs [verts visited]
                (let [nverts (clojure.set/difference (apply clojure.set/union (for [v verts] (neighbors v))) visited)]
                  (if (empty? nverts)
                    (if (= vertices (clojure.set/union verts visited))
                      true
                      false)
                    (bfs nverts (clojure.set/union verts visited)))))]
        (bfs #{start} #{})))))

(defcheck solution-68f778ff
  (fn f [s]
    (loop [ s (seq s) connected #{} ]
      (if (empty? s)
        (= 1 (count connected))
        (let [ [a b]  (first s)
              c_a   (first (filter #(% a) connected))
              c_b   (first (filter #(% b) connected)) ]
          (recur (rest s)
            (conj (disj connected c_a c_b) (clojure.set/union #{a} #{b} c_a c_b))))))))

(defcheck solution-69339dfa
  (fn [gr]
    (letfn [(share? [c1 c2] (some #(or (= (first c2) %) (= (last c2) %)) c1))
            (sharers [c1 cs2] (filter #(share? c1 %) cs2))
            (non-sharers [c1 cs2] (remove #(share? c1 %) cs2))
            (add-sharers [c1 cs2] (set (concat c1 (apply concat (sharers c1 cs2)))))
            (connected? [g]
              (loop [res (first g) leftover (rest g)]
                (if (empty? leftover)
                  true
                  (if (and (= (add-sharers res leftover) res) (empty? (sharers res leftover)))
                    false
                    (recur (add-sharers res leftover) (non-sharers res leftover))))))]
      (connected? gr))))

(defcheck solution-693b4a31
  (fn [s]
    (letfn [(check [c s] ; (c: connected nodes, s: candidate edges)
              (if (empty? s)
                true
                (let [e (filter (fn [m] (some (fn [n] (some #(= % n) c)) m)) s)] ; filter edges which may be connected
                  (if (empty? e)
                    false
                    (recur (reduce into c e) (filter (fn [m] (not (some #(= % m) e))) s))))))]
      (check (set (first s)) (rest s)))))

(defcheck solution-6950c3c4
  (letfn [
          (get-nodes [edges] (-> edges vec flatten set (zipmap (range))))
          (square-matrix [size] (map (fn [_] (repeat size 99)) (range size)))
          (set-ij [m i j v]
            (let [row (nth m j)
                  row- (concat (take i row) [v]  (nthnext row (inc i)))]
              (concat (take j m) [row-] (nthnext m (inc j)))))
          (get-ij [m i j] (-> m (nth j) (nth i)))
          (transitive-closure [edges] (let [
                                            nodes (get-nodes edges)
                                            node-count (count nodes)
                                            graph (-> (square-matrix node-count)
                                                    (#(reduce (fn [m [a b]] (set-ij m (nodes a) (nodes b) 1)) % edges))
                                                    (#(reduce (fn [m [a b]] (set-ij m (nodes b) (nodes a) 1)) % edges))
                                                    (#(reduce (fn [m i] (set-ij m i i 0)) % (range node-count))))
                                            distance (reduce
                                                       (fn [m [i j k]] ; this is the floyd-warshall algorithm
                                                         (let [ij (get-ij m i j) ik (get-ij m i k) kj (get-ij m k j)]
                                                           (if (< (+ ik kj) ij)
                                                             (set-ij m i j (+ ik kj))
                                                             m)))
                                                       graph
                                                       (for [k (range node-count) i (range node-count) j (range node-count)] [i j k]))]
                                        distance))
          (is-reachable? [s] (not-any? (partial = 99) (flatten (transitive-closure s))))]
    is-reachable?))

(defcheck solution-698c1bd3
  (fn [v]
    (let [  g-assoc (fn [m k v] (assoc m k (set (conj (m k) v))))
          g (reduce (fn [m [a b]] (g-assoc (g-assoc m a b) b a)) {} v)]
      ((fn [m1 m2]
         (if (= m1 m2)
           (if (= (set (keys m1)) (second (first m1)))
             true
             false)
           (recur
             m2
             (reduce (fn [m [k kv]]
                       (let [ch (mapcat m kv)]
                         #_(println ch)
                         (reduce #(g-assoc %1 k %2) m ch)))
               m2 m2))
           ))
       {} g)
      )))

(defcheck solution-69abdb2e
  (fn [m]
    (let [[[a b] & rest] (seq m)
          flatseq #(set (flatten (seq %)))] ; #{[1 2] [2 3]} => #{1 2 3}
      (loop [visited (set [a b]) ; keep track of visited nodes
             m rest]
        (or (empty? m) ; visited everything
            (and ((complement empty?) (clojure.set/intersection visited (flatseq m))) ; there's still some accessible nodes
                 (let [found (set (reduce into (for [x visited] (filter (fn [[a b]] (or (= a x) (= b x))) m))))] ; found nodes connected to visited nodes
                   (recur (flatseq found) (remove #(contains? found %) m)))))))))

(defcheck solution-69efc9e0
  (fn connected? [edges']
    (let [edges (set (remove (partial apply =) edges'))
          nodes (set (mapcat identity edges'))
          connected'?
                (fn connected'? [nodes edges*]
                  #_(println nodes edges*)
                  (if (empty? edges*)
                    (= 1 (count nodes))
                    (let [collapse-edge (first edges*)
                          collapse-node (first collapse-edge)
                          target-node (second collapse-edge)

                          collapsed-edges
                                        (->> edges*
                                          (filter (partial some (partial = collapse-node)))
                                          (map (partial map #(if (= % collapse-node)
                                                               target-node
                                                               %)))
                                          (remove (partial apply =))
                                          (map (comp vec sort))
                                          (set))]
                      (connected'?
                        (disj nodes collapse-node)
                        (set (concat collapsed-edges
                               (set (remove (partial some (partial = collapse-node))
                                      edges*))))))))]
      #_(println edges)
      (connected'? nodes edges))))

(defcheck solution-6a128d79
  (fn [g]
    (let [v (into #{} (apply concat g))
          a (into {}
              (for [n v]
                [n (keep #(condp = n
                            (% 0) (% 1)
                            (% 1) (% 0)
                            nil)
                     g)]))]
      (letfn [(s [n vn]
                (when-not (vn n)
                  (cons n
                    (mapcat #(s % (conj vn n))
                      (remove vn (a n))))))]
        (= (count v)
          (count (distinct (s (first v) #{}))))))))

(defcheck solution-6a5cf6f3
  (fn connected? [s]
    (letfn [
            (get-children[s root ignore]
              (clojure.set/difference
                (set (for [[x y] s :when (or (= root x) (= root y))] (if (= x root) y x)))
                ignore)
              )
            (build-tree [root s ignore]
              (let [children (seq (get-children s root ignore))]
                ;(println root children ignore)
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
        ;(println tree)
        (= size (if (vector? tree) (->> tree flatten count) 1))
        )
      )
    ))

(defcheck solution-6c51257
  (fn [rel*]
    (let [nodes (into #{} (apply concat rel*))]
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
              (recur new-rel) rel)))))))

(defcheck solution-6c88c880
  (fn [e]
    (let [edges (for [[k v] e] [{k [v]} {v [k]}])
          g (apply merge-with concat (apply concat edges))
          init (first (keys g))
          dfs (fn dfs [k seen]
                (let [seen (conj seen k)]
                  (apply clojure.set/union seen
                    (map #(dfs % seen)
                      (filter (complement seen) (g k))))))]
      (= (dfs init #{}) (into #{} (apply concat e))))))

(defcheck solution-6cc0bc56
  (fn [g]
    (letfn
     [(direct-connections [n]
        (reduce (fn [acc edge] (if ((set edge) n) (into acc edge) acc)) #{} g))

      (graph-connections [n]
        (loop [connections #{}
               new-connections #{n}]
          (if (or (= connections new-connections) (empty? new-connections))
            connections
            (recur
              (into connections new-connections)
              (reduce (fn [acc n] (into acc (direct-connections n)))
                #{}
                (clojure.set/difference new-connections connections))))))]

      (let [all-nodes (set (flatten (map identity g)))]
        (every? (partial = all-nodes) (map graph-connections all-nodes))))))

(defcheck solution-6dd66c3b
  (fn [grh]
    (letfn [(intersec? [s1 s2]
              (not (empty? (clojure.set/intersection s1 s2))))
            (tstedge [e lg] (group-by #(intersec? % (set e)) lg))
            (nlg [lg e] (let [m (tstedge e lg)]
                          (conj (m false)
                            (apply clojure.set/union (set e) (m true)))))]
      (= 1 (count (reduce nlg [] grh))))))

(defcheck solution-6e3faf17
  (fn [g]
    (letfn [(connected? [g]
              (loop [q (conj [] (ffirst g)) visited #{}]
                (if (empty? q)
                  (let [rem (filter #(not (contains? visited %)) (flatten (for [e g] e)))]
                    (empty? rem))
                  (let [v1 (peek q)
                        edges (filter (partial some #{v1}) g)
                        vertices (filter (partial not= v1) (flatten edges))
                        unvisited (filter #(not (contains? visited %)) vertices)]
                    (recur (into (rest q) unvisited) (into (conj visited v1) unvisited))))))]
      (connected? g))))

(defcheck solution-6e420090
  (fn [g]
    (let [y (fn y [s g z]
              (let [c (remove #{s} (mapcat #(if (some #{s} %) % []) g))
                    t (mapcat #(y % g (concat z c)) (remove (set z) c))]
                (set (concat c t [s]))))
          e (-> g (seq)(flatten)(set))
          ]
      (= e (y (first e) g []))
      )))

(defcheck solution-6ea46e81
  (fn [g]
    (= 1 (count (reduce
                  (fn [s e]
                    (let [f (fn [x] (or (first (filter #(% x) s)) #{x}))
                          [x y] e
                          a (f x)
                          b (f y)]
                      (conj (disj s a b) (apply conj a b)))) #{} g)))))

(defcheck solution-6f03e56a
  (fn [m]
    ((fn f [x y]
       (if (empty? y)
         true
         (let [a (remove #(empty? (clojure.set/intersection x (set %))) y)
               b (apply clojure.set/union x (map set a))]
           (if (empty? a)
             false
             (f b (remove (set a) y)))))) (set (first (seq m))) (rest (seq m)))))

(defcheck solution-6f35faec
  (fn [graph]
    (letfn [(connected [node-label graph]
              (loop [label node-label
                     explored #{node-label}
                     frontier []]
                (let [neighbors (reduce (fn [v [x y]]
                                          (condp = label
                                            x (conj v y)
                                            y (conj v x)
                                            v))
                                  []
                                  graph)
                      unexplored-neighbors (remove explored neighbors)
                      [next-label & next-frontier] (concat frontier unexplored-neighbors)
                      next-explored (conj explored label)]
                  (if next-label
                    (recur next-label next-explored next-frontier)
                    next-explored))))]
      (let [nodes (set (flatten (vec graph)))]
        (= (count (connected (ffirst graph) graph))
          (count nodes))))))

(defcheck solution-6f3b8a3b
  (fn [edges]
    (let [create-graph (fn [edges]
                         (reduce (fn [m [x1 x2]]
                                   (-> m
                                     (update-in [x1] (fnil conj []) x2)
                                     (update-in [x2] (fnil conj []) x1))) {} edges))
          bfs (fn [graph start]
                (loop [queue [start] visited (into {} (map (fn [[k]] [k false]) graph)) trail []]
                  (let [[verge & queue] queue
                        edges (graph verge)]
                    (if verge
                      (recur (if-not (visited verge) (into (vec queue) edges) (vec queue)) (assoc visited verge true) (if-not (visited verge) (conj trail verge) trail))
                      trail))))
          connected (fn [graph start end]
                      #_(println "bfs=" (bfs graph start))
                      (some #{end} (bfs graph start)))
          cart-prod (fn [s]
                      (loop [s s result []]
                        (let [[head & tail] s]
                          (if-not tail
                            result
                            (recur tail (into result (map (fn [x] [head x]) tail)))))))
          graph (create-graph edges)]
      (= (count (filter (comp not identity) (map (fn [[v1 v2]] (connected graph v1 v2))
                                              (cart-prod (keys graph)))))
        0))))

(defcheck solution-6fa4b002
  (fn [s]
    (let [edges (into s (map reverse s))
          edge-map (group-by first edges)
          nodes (set (keys edge-map))
          traverse (fn traverse [visited node]
                     (if (contains? visited node)
                       []
                       (into #{node} (mapcat #(traverse (conj visited node) (second %)) (edge-map node)))))]
      (= nodes (traverse #{} (first nodes))))))

(defcheck solution-6fdee85f
  (fn chk [g]
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
        (= m (* n n))))))

(defcheck solution-709792ea
  (fn r [s] (let [f #(first %) l #(last %) z #(rest %) x (reduce #(if (or (contains? (f %) (f %2)) (contains? (f %) (l %2))) (into (conj [] (conj (f %) (f %2) (l %2))) (z %)) (conj % %2)) (conj [] (set (f s))) (into [] (z s)))] (if (empty? (z s)) true (if (= (z s) (z x)) false (r x))))))

(defcheck solution-70bed7a1
  (fn [G]
    (let [st (fn [a G]
               (filter (fn [[x y]] (or (= x a) (= y a))) G))
          c? (fn c? [a b G]
               (let [N (st a G)
                     D (clojure.set/difference G (set N))]
                 (when G
                   (if (some (fn [[x y]] (or (= x b) (= y b))) N)
                     true
                     (some identity (for [[x y] N :let [y (if (= x a) y x)]]
                                      (c? y b D)))))))
          nodes (into #{} (flatten (seq G)))]
      (empty? (filter nil? (for [x nodes y (clojure.set/difference nodes #{x})]
                             (c? x y G)))))))

(defcheck solution-70edcd17
  (fn [edges]
    (let [
          union (fn [a b] (reduce conj a b))
          points (set (apply concat edges))
          connections (->> edges
                        (map (fn [[a b]] [[a b] [b a]]))
                        (apply concat)
                        (group-by #(first %))
                        (map (fn [[k v]] [k (map last v)]))
                        (apply concat)
                        (apply hash-map))]
      (loop [visiting [(first points)], visited #{}]
        (if (empty? visiting)
          (= visited points)
          (recur
            (filter #(not (visited %))
              (apply concat
                (map #(connections %) visiting)))
            (union visited visiting)))))))

(defcheck solution-71069560
  (fn conn?
    [edges]
    (let [bs (map #(into #{} [%])
               (distinct (flatten (vec edges))))]
      (= 1 (count
             (reduce
               (fn [bs [x y]]
                 (let [[xs ys]
                       (filter
                         #(or (% x) (% y))
                         bs)]
                   (if (nil? ys)
                     bs
                     (conj
                       (filter
                         #(not (or (% x) (% y)))
                         bs)
                       (into xs ys)))))
               bs edges))))))

(defcheck solution-71278525
  (fn [s]
    (loop [t s n {}]
      (if (empty? t)
        (= (count (distinct (vals n))) 1)
        (let [x (first t) a (first x) z (second x)]
          (recur (rest t)
            (if (n a)
              (if (n z)
                (if (= (n a) (n z))
                  n
                  (zipmap (keys n) (replace {(n z) (n a)} (vals n)))
                  )
                (assoc n z (n a))
                )
              (if (n z)
                (assoc n a (n z))
                (assoc n a a z a)
                )
              )
            )
          )
        )
      )
    ))

(defcheck solution-716459a1
  (fn [edges]
    (let
     [nodes (fn nodes [edges] (set (flatten (into '() edges))))
      init-graph (fn init-graph [nodes] (zipmap nodes (repeat #{})))
      fill-graph-with-edge (fn fill-graph-with-edge [graph edge]
                             (let [add-edge (fn [g n1 n2] (assoc g n1 (conj (get g n1) n2)))]
                               (add-edge (add-edge graph (first edge) (second edge)) (second edge) (first edge))))
      fill-graph (fn fill-graph [graph edges] (reduce fill-graph-with-edge graph edges))
      visit-nodes (fn visit-nodes [snode graph visited]
                    (let [nbs (get graph snode)
                          nbs-to-visit (apply disj nbs visited)
                          to-visit (apply conj visited (conj nbs snode))]
                      (if (empty? nbs-to-visit)
                        to-visit
                        (map (fn [nb] (visit-nodes nb graph to-visit)) nbs-to-visit))))

      connected? (fn connected? [edges]
                   (let [nodes (nodes edges)
                         graph (fill-graph (init-graph nodes) edges)
                         visited (set (flatten (visit-nodes (first nodes) graph '())))]
                     (= (count nodes) (count visited))))]
      (connected? edges))))

(defcheck solution-71c9dcd2
  (fn connected? [s]
    (let [f (fn [acc s]
              (if (empty? s) true
                             (let [node-connected
                                   (some (fn [x]
                                           (or
                                            (some #(= % (ffirst s)) x)
                                            (some #(= % (second (first s))) x))) acc)]
                               (if (not node-connected) false
                                                        (recur (conj acc (first s)) (rest s))))))
          ordered (sort-by (juxt first second) s)]
      (f (list (first ordered)) (rest ordered)))))

(defcheck solution-71ebad69
  (fn [edges]
    (let [adj-list (reduce (fn [agg [x y]]
                             (merge-with into agg (into {} [[x #{y}] [y #{x}]]))) {}
                     edges)
          vs (into #{} (keys adj-list))
          linked-cmp (fn ch [v adj-list visited]
                       (if (empty? adj-list)
                         visited
                         (->> (for [v-adj (adj-list v)
                                    :when (not (visited v-adj))]
                                (ch v-adj (dissoc adj-list v) (into visited [v v-adj])))
                           (reduce into visited))))]

      (-> vs first
        (linked-cmp adj-list #{})
        (= vs)))))

(defcheck solution-7252d525
  (let [fixpoint
        (fn [f x]
          (loop [current x]
            (let [next (f current)]
              (if (= next current)
                next
                (recur next)))))
        neighbours
        (fn [v edges]
          (map second
            (filter #(= (first %) v)
              (concat edges
                (map reverse edges)))))]

    (fn connected?
      [dumb-edges]
      (let [edges (into [] dumb-edges)]
        (let [expand
              (fn [in]
                (sort (into #{} (concat in (mapcat #(neighbours % edges) in)))))]
          (= (fixpoint expand (first edges))
            (sort (into #{} (flatten edges)))))))))

(defcheck solution-73be73f3
  (fn [graph]
    (let [conn (fn [coll nodes]
                 (reduce
                   (fn [[c n] [h t]] (if (or (c h) (c t)) [(conj c h t) n] [c (conj n [h t])]))
                   [coll #{}]
                   nodes))
          [coll nodes] (conn (set (first graph)) graph)]
      (loop [coll (set (first graph))
             nodes graph
             n (count graph)]
        (or (zero? n)
            (let [[coll nodes] (conn coll nodes)]
              (if (= (count nodes) n)
                false
                (recur coll nodes (count nodes)))))))))

(defcheck solution-744a4daa
  (fn [edges]
    (letfn [(edge->hashes [edge]
              (let [[a b] edge]
                [{a #{b}}
                 {b #{a}}]))
            (edges->graph [edges]
              (let [hashes (flatten (map edge->hashes edges))]
                (apply merge-with clojure.set/union hashes)))
            (graph-connected? [graph]
              (let [nodes (set (keys graph))]
                (loop [seen #{}
                       to-visit [(first nodes)]]
                  (if-let [[this-node & to-visit] (seq to-visit)]
                    (let [seen (conj seen this-node)
                          connected-nodes (graph this-node)
                          unvisited-nodes (clojure.set/difference connected-nodes seen)
                          to-visit (concat to-visit unvisited-nodes)]
                      (recur seen to-visit))
                    (= seen nodes)))))]
      (graph-connected? (edges->graph edges)))))

(defcheck solution-74ba4928
  (fn [graph]
    ((complement empty?)
     (apply
       clojure.set/intersection
       (reduce (fn [sets edge]
                 (let [seted (set edge)]
                   (if (every? empty? (map #(clojure.set/intersection % seted) sets))
                     (conj sets seted)
                     (set (map (fn [temp-set]
                                 (if (empty? (clojure.set/intersection temp-set seted))
                                   temp-set
                                   (clojure.set/union temp-set seted)))
                            sets)))))
         #{(set (first graph))}
         (rest graph))))))

(defcheck solution-74ec215d
  (fn [s] (let [t (#(let [n (apply conj %
                              (for [[a b] %
                                    [c d] %
                                    :when (= b c)]
                                [a d]))]
                      (if (= n %)
                        %
                        (recur n))
                      ) (apply conj s (map reverse s)))
                q (distinct (flatten (vec t)))
                k (count q)
                f (first (sort q))]

            (< (- k 1) (count
                         (filter #(= (second %) f)
                           (vec t)))))))

(defcheck solution-7550f299
  (fn self [g]
    (let [all (set (flatten (seq g)))]
      #_(prn "all" all)
      (letfn [(dfs [s xs]
                (mapcat
                  (fn [[k2 v2]]
                    #_(prn "s" s "k2" k2 "v2" v2)
                    (cond
                      (and (= s k2) (not (contains? xs v2))) (conj (dfs v2 (conj xs v2)) v2)
                      (and (= s v2) (not (contains? xs k2))) (conj (dfs k2 (conj xs k2)) k2)
                      :else xs))
                  g)
                )]
        #_(prn "starting with" (first g))
        (= all (into #{}
                 (concat
                   (dfs (ffirst g) (set [(ffirst g)]))
                   (dfs (second (first g)) (set [(second (first g))]))))
          )))
    ))

(defcheck solution-7624d842
  (fn connected?
    [graph]
    (let [nodes (set (apply concat graph))
          neighbors (reduce
                      (fn [result [a b]]
                        (-> result
                          (update-in [a] (fnil conj #{}) b)
                          (update-in [b] (fnil conj #{}) a)))
                      {} graph)]
      (loop [visited #{(first nodes)}
             visited-nodes-border (neighbors (first nodes))]
        (let [visited (clojure.set/union visited visited-nodes-border)]
          (if (seq visited-nodes-border)
            (let [new-visited-nodes-border (clojure.set/difference
                                             (set (mapcat neighbors
                                                    visited-nodes-border))
                                             visited)]
              (recur visited
                new-visited-nodes-border))
            (= nodes visited)))))))

(defcheck solution-7643fef1
  (letfn [(undirected [edges]
            (into edges (for [[from to] edges]
                          [to from])))
          (transit-1 [edges]
            (let [undir (undirected edges)
                  nodes (group-by first undir)]
              (into undir (for [[from to] nodes
                                [_ intermediate] to
                                [_ endpoint] (nodes intermediate)]
                            [from endpoint]))))
          (closure [edges]
            (->> edges
              (iterate transit-1)
              (partition 2 1)
              (drop-while (partial apply not=))
              ffirst))
          (nodes [edges]
            (set (apply concat edges)))
          (fully-connected [nodes]
            (set (for [x nodes, y nodes]
                   [x y])))]
    (fn [edges]
      (= (fully-connected (nodes edges))
        (closure edges)))))

(defcheck solution-7674232d
  (fn checker
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
                #_#__ (println graph)
                #_#__ (println "Node:" node)
                ;_ (println xmerged xmates)
                ;_ (println companions )
                #_#__ (println ngen)
                #_#__ (println (count ngen) (count graph))
                ]
                (if
                 (< (count ngen) (count graph))
                  (checker ngen)
                  false))))]
          (or
           (path x xcompanions)
           (path y ycompanions)))))))

(defcheck solution-76a0fa2b
  (fn connected? [E]
    (let [V (set (flatten (seq E)))
          neighbors
            (into {}
              (for [x V] [x (vec
                              (for [[u w] E :when (some #{x} [u w])]
                                (if (= x u) w u)))]))]
      (= V
        ((fn walk-path[current walked-by]
           (if (walked-by current)
             walked-by
             (reduce into #{}
               (for [nxt (neighbors current)]
                 (walk-path
                   nxt (conj walked-by current)))))) (first V) #{})))))

(defcheck solution-7769ed06
  (fn [g]
    (letfn [(union [sets uset vset]
              (conj (disj sets uset vset) (clojure.set/union uset vset)))
            (find-set [s v]
              (first (clojure.set/select #(contains? % v) s)))]
      (let [sets (reduce (fn [acc [x y]] (conj acc #{x} #{y})) #{} g)]
        (= 1 (count
               (loop [e g acc sets]
                 (if (empty? e) acc
                                (let [[u v] (first e) us (find-set acc u) vs (find-set acc v)]
                                  (recur (rest e) (if (not= us vs) (union acc us vs) acc)))))))))))

(defcheck solution-7778c025
  (fn graph-connect [g-set]
    (letfn [(adjacent? [p c-set]
              (reduce (fn [c? c-p]
                        (or c?
                            (= (first p) (second c-p))
                            (= (second p) (first c-p))
                            (= (first p) (first c-p))
                            (= (second p) (second c-p))))
                false
                c-set))]
      (loop [conn-set (conj #{} (first (seq g-set)))
             disconn-set (rest (seq g-set))]
        (if (empty? disconn-set)
          true
          (let [n-group (group-by #(adjacent? % conn-set) disconn-set)
                adj-set (get n-group true)
                n-adj-set (get n-group false)]
            (if (empty? adj-set)
              false
              (recur (into conn-set adj-set)
                n-adj-set))))))))

(defcheck solution-77afc77d
  (fn [l]
    (let [m (reduce (fn [a [k v]]
                      (merge-with concat a {k [v]} {v [k]}))
              {} l)
          k (set (keys m))]
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
                        #{} p)))))))

(defcheck solution-77df652b
  (fn connectedX[nodes]
    (letfn [
            (connectedSimple [a b]
              (or
               (= (first a)(first b))
               (= (first a)(last b))
               (= (last a)(first b))
               (= (last a)(last b))
               )
              )


            (connectedTransitive [a b c nodes]
              (and
               (connected2 a c nodes)
               (connected2 b c nodes)
               )
              )

            (ors[x]
              (if (empty? x)
                false
                (reduce (fn[a b] (or a b)) x)
                )
              )


            (ands[x]
              (if (empty? x)
                false
                (reduce (fn[a b] (and a b)) x)
                )
              )

            (connected2 [a b nodes]
              (or
               (connectedSimple a b)
               (ors (map (fn[c] (connectedTransitive a b c (disj nodes c))) nodes))
               )
              )

            (connected[nodes]
              (ands
                (for [a nodes b nodes]
                  (if (= a b)
                    true
                    (connected2 a b (clojure.set/difference nodes #{a b}))
                    )
                  )
                )
              )
            ]
      (ands
        (for [a nodes b nodes]
          (if (= a b)
            true
            (connected2 a b (clojure.set/difference nodes #{a b}))
            )
          )
        )
      )
    ))

(defcheck solution-7818f108
  (fn graphConn [graph]
    (letfn [(twoCombs [coll]
              (cond
                (< (count coll) 2) nil
                (= (count coll) 2) #{[(first coll) (second coll)]}
                :else (clojure.set/union
                        (twoCombs (rest coll))
                        (set (map #(vec [(first coll) %]) (rest coll))))))
            (validPair? [[[f1 f2] [s1 s2]]]
              (or (= f2 s1) (= s2 f1)))
            (makePairs [[[f1 f2] [s1 s2]]]
              (cond
                (= f2 s1) [f1 s2]
                (= f1 s2) [s1 f2]))
            (addReverse [graph]
              (reduce (fn [result [l r]]
                        (conj (conj result [l r]) [r l]))
                #{}
                graph))
            (transitive [coll]
              (let [tc (twoCombs coll)
                    vps (filter validPair? tc)
                    nps (set (map makePairs vps))]
                (if (clojure.set/superset? coll nps)
                  coll
                  (transitive (clojure.set/union coll nps)))))]
      (let [allNodes (set  (reduce #(into %1 %2) [] graph))
            transitiveNodes (transitive (addReverse graph))]
        (every? (fn [elem]
                  (every? (fn [otherElem]
                            (if (= elem otherElem)
                              true
                              (or (contains? transitiveNodes [elem otherElem])
                                  (contains? transitiveNodes [otherElem elem])))) allNodes)) allNodes)))))

(defcheck solution-7842bcd0
  (fn graph-connect [edges]
    (let [points (set (apply concat edges))
          other-one (fn [tuple item] (if (= item (first tuple)) (last tuple) (first tuple)))
          next-pos (fn [in-set pos] (filter #(not ((set in-set) %)) (map #(other-one % pos) (filter #((set %) pos) edges))))
          nexts-pos (fn [in-set] (apply concat (map #(next-pos in-set %) in-set)))
          points (set (apply concat edges))
          step (fn f [in-set]
                 (if (= (count points) (count in-set))
                   true
                   (let [after-set (set (concat in-set (nexts-pos in-set)))]
                     (if (= (count after-set) (count in-set))
                       false
                       (f after-set))))
                 )]
      (step (set (list (first points)))))))

(defcheck solution-786c5131
  (fn myf [graph]
    (loop [all-set (set (flatten (vec graph)))
           graph-set (set (first graph))]
      (let [res (reduce
                  #(if (or (contains? %1 (second %2)) (contains? %1 (first %2))) (into %1 %2) %1)
                  graph-set
                  graph)]
        (cond (= graph-set all-set) true;
              (= graph-set res) false;
              :else (recur all-set res))))))

(defcheck solution-78aaeb9d
  (fn
    Connectivity [col]
    (let [find_connection (fn [rest-edges current-grah]
                            (filter #(or (contains? current-grah (first %) )
                                         (contains? current-grah (last %) ))
                              rest-edges))

          find_non_connection (fn [rest-edges current-grah]
                                (filter #(and (not (contains? current-grah (first %) ))
                                              (not (contains? current-grah (last %) )))
                                  rest-edges))]

      (loop [initset (set (first  col) )
             data (rest col) ]
        (if (empty? data)
          (not (empty? initset))
          (let [connected-edges (find_connection data initset)
                non-connected-edges (find_non_connection data initset)]
            (if (empty? connected-edges)
              false
              (recur (reduce #(clojure.set/union %1 (set %2)) initset connected-edges)
                non-connected-edges
                ))))))))

(defcheck solution-78c8e365
  (fn [es]
    (letfn [(rep [fa x]
              (if (= x (get fa x x))
                x
                (rep fa (fa x))))
            (mer [fa [u v]]
              (assoc fa (rep fa u) (rep fa v)))]
      (let [fa (reduce mer {} es)]
        (apply = (map (partial rep fa)
                   (keys fa)))))))

(defcheck solution-7936bd5b
  (fn [edges]
    (let [count= (fn [& colls] (apply = (map count colls)))
          maps (mapcat
                 (fn [[u v]] (if (= u v) [{u [v]}] [{u [v]} {v [u]}])) edges)
          adj-tab (apply merge-with concat maps)
          nodes (keys adj-tab)
          ccomp (fn [vs]
                  (let [vs' (into vs (apply concat (map #(adj-tab %) vs)))]
                    (if (count= vs vs') vs (recur vs'))))]
      (count= (ccomp #{(first nodes)}) nodes))))

(defcheck solution-793df6ac
  (fn graph-connectivity? [edges]
    (let [m (zipmap (flatten (vec edges)) (repeat nil))
          find (fn [m vertex] (if (nil? (m vertex))
                                vertex
                                (recur m (m vertex))))]
      (= 1 (count
             (filter
               nil?
               (vals
                 (reduce
                   (fn [m [va vb]]
                     (let [a (find m va)
                           b (find m vb)]
                       (if (not= a b) (assoc m a b) m)))
                   m edges))))))))

(defcheck solution-796c0cee
  (fn connected?
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
                                                  (reduce into new-seen (map #(walk % new-edges new-seen) ns))))))]
      (let [ns (nodes edges)]
        (= ns (walk (first ns) edges))))))

(defcheck solution-7baa4523
  (fn connected? [es]
    (let [vs (reduce (fn [m ls] (reduce #(assoc %1 %2 :unvisited) m ls))
               '{}
               es)
          vls (keys vs)]
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
                      (reduce (adjacent v1 vs) (pop q) (into '() es))))))]
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
                    (inc components))))))))

(defcheck solution-7dfd2d02
  (fn puzzle-91 [tuples]
    (letfn [(find-conn-sets [ss tuple]
              (let [points (set tuple)]
                (remove #(empty? (clojure.set/intersection % points)) ss)))
            (add-tuple [ss tuple]
              (let [[a b] (find-conn-sets ss tuple)
                    t (set tuple)]
                (cond
                  (= nil a b) (conj ss t)
                  (nil? b) (conj (disj ss a) (clojure.set/union a t))
                  :else (conj (disj ss a b) (clojure.set/union a b t)))))]
      (= 1 (count (reduce add-tuple #{} tuples))))))

(defcheck solution-7e2a8b2a
  (fn [t]
    (let [g (reduce (fn [z [a b]] (merge-with concat z {a [b]} {b [a]})) {} t)
          h (iterate #(into % (mapcat g %)) #{(ffirst t)})
          [[a _] & _] (drop-while (fn [[a b]] (not= a b)) (map vector h (next h)))]
      (== (count (keys g)) (count a)))))

(defcheck solution-7e620490
  (fn graph-connectivity [coll]
    (let [elements (distinct (flatten (map identity coll)))
          coll- (concat coll (first (for [i coll] (map #(vector % %) i))))
          possible-paths
                   (loop [counter (count coll-)
                          acc coll-]
                     (if (= 0 counter)
                       (set acc)
                       (recur (dec counter) (set (remove nil?
                                                   (concat
                                                     acc
                                                     (for [i acc
                                                           j acc]
                                                       (if (= (last i) (first j))
                                                         [(first i) (last j)]))
                                                     (for [i acc
                                                           j acc]
                                                       (if (= (last i) (first j))
                                                         [(last j) (first i)]))))))))
          full-graph (set (for [i elements
                                j elements]
                            [i j]))]
      (= possible-paths full-graph))))

(defcheck solution-7e67958a
  (fn [graph] (letfn [
                      (nodes [graph]
                        (distinct (flatten (vec graph))))

                      (findconnectednodes [node graph nodes]
                        (concat (map first (filter (fn [connector] (= node (last connector))) graph))
                          (map last
                            (filter (fn [connector]
                                      (= node (first connector)))
                              graph))))

                      (getallconnected [node graph nodes]
                        (letfn [(doit [current found]
                                  (if (contains? found current)
                                    found
                                    (let [
                                          connected (findconnectednodes current graph nodes)
                                          newfound (set (conj found current))]
                                      (set (mapcat
                                             (fn [connect]
                                               (doit connect newfound)) connected)))))]
                          (doit node #{})))]
                (= (count (nodes graph))
                  (count (getallconnected  (ffirst graph) graph (nodes graph)))))))

(defcheck solution-7e87f4c1
  (fn [edges]
    (letfn
     [(add-edge [emap edge]
        (update-in emap [(first edge)] #(conj (or %1 #{}) (second edge))))
      (node-map [edges]
        (reduce #(add-edge (add-edge %1 %2) (vec (reverse %2))) {} edges))]

      (let [nmap (node-map edges)]
        (boolean (= (count (keys nmap)) (count
                                          (loop [visited-nodes #{} unvisited-nodes #{(first (first edges))}]
                                            (if (empty? unvisited-nodes) visited-nodes
                                                                         (let [node (first unvisited-nodes)
                                                                               new-visited-nodes (conj visited-nodes node)
                                                                               adjacent-unvisited-nodes
                                                                               (filter #(not (contains? new-visited-nodes %)) (get nmap node))]
                                                                           (recur new-visited-nodes (into (disj unvisited-nodes node) adjacent-unvisited-nodes))))))))))))

(defcheck solution-7eab08fa
  (fn [graph]
    (letfn [(gc-91 [visited]
              (let [new-visited (reduce conj
                                  visited
                                  (apply concat
                                    (filter (fn [[a b]] (or (visited a)
                                                            (visited b)))
                                      graph)))]
                (if (= (count visited) (count new-visited))
                  (= (count visited) (count (into #{} (apply concat graph))))
                  (recur new-visited))))]
      (gc-91 #{(ffirst graph)}))))

(defcheck solution-7ee80d5a
  (fn g-connected? [g]
    (let [tcf (fn tcf [rels]
                (let [tc (reduce (fn [rs [a b]] (into rs (keep #(if (= b (first %)) [a (second %)]) rs))) rels rels)]
                  (if (= tc rels) tc (recur tc))))
          nc (->> g (apply list) flatten distinct count)]
      (= (* nc nc) (count (tcf (into g (map reverse g))))))))

(defcheck solution-7ee8b926
  (fn path [coll]
    (letfn [(trans [coll]
              (loop [coll coll]
                (let [new-coll (into coll
                                 (for [[a b] coll
                                       [c d] coll
                                       :when (= b c)]
                                   [a d]))]
                  (if (= coll new-coll)
                    coll
                    (recur new-coll)))))
            (all-paths [coll]
              (let [flat-coll (flatten (vec coll))]
                (for [x flat-coll
                      y flat-coll
                      :when (not= x y)]
                  [x y])))]
      (->> (into coll (map reverse coll))
        trans
        (filter #(not= (first %) (second %)))
        set
        (= (set (all-paths coll)))))))

(defcheck solution-7f7171bf
  (fn [s]
    (= (count (distinct (flatten (seq s))))
      (count (nth
               (iterate
                 (fn [o] (into o (for [n o v s :when (some #(= n %) v)] (if (= n (v 0)) (v 1) (v 0)))))
                 #{(ffirst s)})
               (count s))
        ))))

(defcheck solution-7f73fd5f
  (fn graph-connected [tuples]
    (letfn [(make-graph [tuples]
              (reduce (fn [g [a b]]
                        (update-in
                          (update-in g [b] #(conj (or % []) a))
                          [a] #(conj (or % []) b))
                        ) {}  tuples))
            (dfs [root graph]
              (loop [queue [root]
                     visited (set [root])]
                (if (empty? queue)
                  visited
                  (let [root (first queue)
                        queue (drop 1 queue)
                        verts (filter #(not (contains? visited %)) (get graph root))
                        visited (set (concat verts visited))
                        queue (concat queue verts)]
                    (recur queue visited)))))
            (node-connected [root graph]
              (let [all-nodes (set (concat (keys graph) (apply concat (vals graph))))
                    node-count (count all-nodes)]
                (= node-count (count (dfs root graph)))))]
      (let [graph (make-graph tuples)
            all-nodes (set (concat (keys graph) (apply concat (vals graph))))
            node-count (count all-nodes)]
        (every? identity (map #(= node-count (count (dfs % graph))) all-nodes))))))

(defcheck solution-80178373
  (fn [g]
    (= (into #{} (flatten (vec g)))
      (nth (iterate #(into #{} (flatten (for [e g] (if (or (% (e 0)) (% (e 1))) e)))) #{((first g) 0)}) (count g)))))

(defcheck solution-802620a4
  (fn connected? [G]
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
            ]
      (let [adj (adj-list2 G)
            ks (set (keys adj))
            reach (df-search3 adj (first ks) #{})]
        (= ks reach)))))

(defcheck solution-81553e
  (fn [g]
    (let [i (reduce
              (fn [x [k v]]
                (update-in x [k] conj v))
              {}
              g)
          j (reduce
              (fn [x [v k]]
                (update-in x [k] conj v))
              {}
              g)
          x (merge-with into i j)]
      (loop [m x
             l #{}
             a (conj #{} (ffirst m))]
        (if (= l a)
          (= (set (keys m)) a)
          (recur
            m
            a
            (reduce
              #(into % (m %2))
              a
              a)
            ))))))

(defcheck solution-8202f1a
  (fn [s]
    (letfn [(r [t v] (some (fn [[a b]] (= a b)) (for [x t y v] [x y])))
            (getA [t s] (some #(when (r t %) %) s))]
      ((fn f [t s]
         (let [a (getA t s)]
           (if a
             (f (distinct (concat t a)) (disj s a))
             (empty? s))))
       (first s) (disj s (first s))))))

(defcheck solution-826c430a
  (fn connected? [graph] (let [candidate (first (first graph))
                               nodes (into [] (disj (reduce #(into %1 %2) #{} graph) candidate))
                               getNext (fn [s x] (into s (filter #(some #{x} %1) graph)))
                               nextLevel (fn [level visited] (apply disj (into #{} (flatten (reduce getNext [] level))) (into level visited)))
                               connectedPair? (fn [root cand] (loop [visited #{cand} next #{cand}] (cond
                                                                                                     (empty? (nextLevel next visited)) false
                                                                                                     (some #{root} (nextLevel next visited)) true
                                                                                                     :default (recur (into visited next) (nextLevel next visited)))))

                               ] (if (= 1 (count graph)) true (every? #(connectedPair? candidate %1) nodes)))))

(defcheck solution-836bc6f4
  (fn [xs]
    (letfn [(prod [[x1 x2 :as xs] [y1 y2 :as ys]]
              (if (or (= x1 y1) (= x1 y2) (= x2 y1) (= x2 y2))
                (apply concat (for [x xs y ys] [[x y] [y x] [x x] [y y]])) #{}))
            (step [xs]
              (set (apply concat (for [x xs y xs] (prod x y)))))
            (trans [xs]
              (loop [n xs n+ (step xs)]
                (if (= n n+) n
                             (recur n+ (step n+)))))
            (expect [xs]
              (let [ns (set (apply concat xs)) s (count ns)] (* s s)))]
      (= (expect xs) (count (trans xs))))))

(defcheck solution-836e1cee
  (fn [s]
    (let [h (first s)]
      (loop [x (disj s h), i (into #{} h)]
        (or (empty? x)
            (let [e (some #(when (some i %) %) x)]
              (if e
                (recur (disj x e) (into i e))
                false)))))))

(defcheck solution-837330b5
  (fn [graph]
    (let [nodes (-> graph seq flatten set)
          first-node (first (first graph))
          one-step-away (fn [n]
                          (clojure.set/difference
                            (->> graph
                              (filter #(contains? (set %) n))
                              flatten
                              set)
                            #{n}))
          expand-visited-nodes (fn [visited-nodes]
                                 (->> visited-nodes
                                   (map one-step-away)
                                   (apply clojure.set/union)))]
      (loop [visited-nodes #{first-node}]
        (if (= visited-nodes nodes) true
                                    (let [new-nodes (expand-visited-nodes visited-nodes)]
                                      (if (empty? (clojure.set/difference new-nodes visited-nodes)) false
                                                                                                    (recur (clojure.set/union visited-nodes new-nodes)))))))))

(defcheck solution-837e6e5d
  (fn [g]
    (let [k reduce
          p (k (fn [m [a b]]
                 (assoc m
                   a (conj (get m a) b)
                   b (conj (get m b) a))) {} g)]
      (loop [n (into #{} (first g))
             c 0]
        (let [n (k
                  (fn [N n]
                    (into N (p n)))
                  n
                  n)
              d (count n)]
          (if (= d c)
            (= d (count p))
            (recur n d)))))))

(defcheck solution-83a0b278
  (fn connected-graph?
    [edges]
    (let [sorted-edges (into [] edges),
          heads (->> sorted-edges flatten set)]
      (loop [out #{}, done #{}, head (ffirst sorted-edges)]
        (if (nil? head)
          (if (= (count heads) (count done))
            true
            false)
          (let [new-out (into out (filter #(not= % head) (flatten (filter #(some #{head} %) sorted-edges)))),
                new-done (conj done head)
                new-head (first (clojure.set/difference new-out new-done))]
            (recur  new-out new-done new-head
              )))))
    ))

(defcheck solution-83be7516
  (fn [S]
    (let [g (apply merge-with concat
              (for [[x y] S
                    m [{x [y]} {y [x]}]]
                m))
          n (into #{} (apply concat S))
          f (fn f [A V]
              (if (empty? A) V
                             (->> (first A)
                               (g)
                               (filter #(not (V %)))
                               (concat (rest A))
                               (#(f % (conj V (first A)))))))

          ]
      (= n (f [(first n)] #{})))))

(defcheck solution-8475af5f
  (fn connected-graph [g]
    (let [verts (distinct (apply concat g))
          edges
                (reduce (fn [m [e1 e2]]
                          (assoc m
                            e1 (conj (m e1 []) e2)
                            e2 (conj (m e2 []) e1)))
                  {} g)]
      (loop [seen #{(first verts)} unvisited [(first verts)]]
        (cond
          (= seen (set verts)) true
          (empty? unvisited) false
          :else
          (let [v (first unvisited)
                unseen-neighbors (filter #(not (seen %)) (edges v))]
            (recur (clojure.set/union
                     seen
                     (set unseen-neighbors))
              (concat (rest unvisited) unseen-neighbors))))))))

(defcheck solution-84b2fbc1
  (fn [s]
    (letfn [(connected-nodes
              [nodes coll]
              (let [nodes' (into nodes
                             (flatten
                               (for [[a b] coll :when (or (contains? nodes a)
                                                          (contains? nodes b))]
                                 [a b])))]
                (if (= nodes nodes')
                  nodes
                  (recur nodes' coll))))]
      (= (connected-nodes #{(ffirst s)} s)
        (set (flatten (seq s)))))))

(defcheck solution-8542fa8d
  (fn [g]
    (let [vertices (reduce into #{} g)
          bfs (fn [g v]
                (loop [q (conj #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) v) seen #{v}]
                  (let [current (peek q)
                        vs (reduce into #{} (for [[a b] g :when (or (= a current) (= b current))] [a b]))]
                    (if (empty? q)
                      seen
                      (recur (into (pop q) (apply disj vs seen)) (into seen vs))))))]
      (= vertices (bfs g (ffirst g))))))

(defcheck solution-85c246a2
  (fn [e]
    (let [vmap (apply merge-with into
                 (map (fn [[v1 v2]] (if (= v1 v2) {v1 #{v1}} {v1 #{v2} v2 #{v1}})) e))]
      (= (-> vmap keys set)
        ((fn s [v m seen]
           #_(println v m seen)
           (if (empty? m) #{v}
                          (let [newm (dissoc m v)
                                newseen (conj seen v)
                                iter (filter (complement newseen) (m v))]
                            (apply clojure.set/union #{v}
                              (map #(s % newm newseen) iter)))))
         (ffirst vmap) vmap #{})))))

(defcheck solution-868989ab
  (fn [g]
    (letfn [
            (paths [[prev :as path] rests]
              (if (empty? rests)
                [path]
                (#(if (empty? %) [path] %)
                 (apply concat
                   (keep
                     #(if (or (nil? prev)
                              (> 4 (count (into #{} (concat prev %)))))
                        (paths (cons % path) (disj rests %)))
                     rests)))))]
      (let [ps (paths () g)]
        (every?
          (fn [[src dst]]
            (some
              (fn [p]
                (and (= src (last p))
                     (some #(= dst %) p)))
              ps))
          (for [src g dst (disj g src)] [src dst]))))))

(defcheck solution-86900c84
  (fn [edges]
    (let [union clojure.set/union
          difference clojure.set/difference
          vmap (->> edges
                 (mapcat (fn [[v1 v2]] [{v1 #{v2}} {v2 #{v1}}]))
                 (apply merge-with union))
          traverse
          (fn [vertex]
            (loop [vs (vmap vertex), res #{vertex}]
              (let [new (difference vs res)]
                (if (empty? new)
                  res
                  (recur (apply union (map vmap new)) (union res new))
                  ))))
          ]
      (every? #(= (count vmap) (count (traverse %))) (keys vmap))
      )))

(defcheck solution-86aee76c
  (fn conn [s]
    (let
     [m (some #(if (not= (first %) (second %)) %) s)]
      (if m
        (let [r (fn [v] (if (= v (first m)) (second m) v))]
          (conn (set (map #(vector (r (first %)) (r (second %))) s))))
        (= (count s) 1)))))

(defcheck solution-870b77c6
  (fn __ [s]
    (let [nodes (fn [s]
                  ;; all the a's and b's of a set of [a b] tuples
                  (into #{} (concat (map first s) (map second s))))
          sorted-uniq (fn [s]
                        (into #{} (filter (fn [[a b]] ((complement =) a b)) (map sort s))))
          num-possible-links (fn [n]
                               ;; n * (n - 1) / 2
                               (if (= n 1)
                                 0
                                 (/ (* n (dec n)) 2)))
          positions (fn [pred coll]
                      (keep-indexed (fn [idx x]
                                      (when (pred x)
                                        idx))
                        coll))
          rev (fn [[a b]]
                [b a])
          add-revs (fn [s]
                     ;; s is a set of [a b] tuples; add all the [b a] tups
                     (clojure.set/union s (into #{} (map rev s))))
          bfs (fn [s [a b]]
                ;; s is a graph as a set of [a b] tuples; find all the neighbors of [a b]
                (let [firsts (map first s)
                      seconds (map second s)
                      idxs (positions (partial = b) firsts)]
                  (if (seq idxs)
                    (map #(vector % (nth seconds %2)) (repeat a) idxs)
                    nil)))
          tr-cl (fn tr-cl [s]
                  ;; bfs on each node recursively to find all reachable nodes
                  (let [nexts (into #{} (filter (complement nil?) (apply concat (map (partial bfs s) s))))
                        s-plus (clojure.set/union s nexts)]
                    (if (= s s-plus)
                      s
                      (tr-cl s-plus))))
          t (sorted-uniq (tr-cl (add-revs s)))
          measured (count t)
          desired (num-possible-links (count (nodes s)))]
      (= measured desired))))

(defcheck solution-8779b08f
  (fn [s]
    (let [g (fn [s]
              (reduce (fn [m [f s]]
                        (assoc m
                          f (conj (m f []) s)
                          s (conj (m s []) f))) {} s))
          f (fn [s m]
              (set
                (filter
                  (comp not nil?)
                  (flatten
                    (take (+ 1 (count m))
                      (iterate #(flatten (map m %)) s))))))]
      (let [m (g s)]
        (= (count (f [(nth (keys m) 0)] m)) (count (set (flatten (vals m)))))))))

(defcheck solution-87cc6fb9
  (fn [pairs]
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
                (every? (partial = true) (map :explored (vals exp)))))]

      (->> pairs
        (into [])
        pairs-to-neighbor-list-map
        all-connected))))

(defcheck solution-87f0342
  (fn [g]
    ((fn f [e]
       (#(if (= e %) (= % g) (f %))
        (reduce (fn [a b] (into a (filter #(some (set b) %) g)))
          #{}
          e)))
     #{(first g)})))

(defcheck solution-880a750a
  (fn [g]
    (= (-> g seq flatten set)
      (nth (iterate #(if (seq %)
                       (reduce (fn [x y]
                                 (if (some (set y) x)
                                   (into x y)
                                   x))
                         % g)
                       (set (first g)))
             #{})
        9))))

(defcheck solution-88716134
  #(= ((fn [s0 es] (let [[m red] (reduce (fn step[[m s] e] (let [intr (keep s e)] (if-not (or (empty? intr) (= intr e)) [true (into s e)] [m s]))) [false s0] es)] (if m (recur red es) red))) (set (first %)) %) (set (flatten (seq %)))))

(defcheck solution-888ad817
  (fn f91 [edges]
    (letfn [(graph [edges]
              (->> edges
                (mapcat (fn [v]
                          (let [f (first v)
                                s (second v)]
                            [{f #{s}} {s #{f}}])))
                (apply merge-with clojure.set/union)))
            (dsf [graph node]
              (loop [seen #{node}
                     path [node]
                     todo (vec (get graph node))]
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
                (count (dsf g (ffirst g)))))]
      (-> edges
        graph
        connected-graph?))))

(defcheck solution-88e2eb36
  (fn prob91 [edges]
    (let [csets
          (reduce
            (fn [s tuple]
              (let [tset (set tuple)
                    isets (filter #(not (empty? (clojure.set/intersection % tset))) s)]
                (clojure.set/union
                 (clojure.set/difference s (set isets))
                  (hash-set (apply clojure.set/union tset isets)))))
            #{} edges)]
      (= 1 (count csets)))))

(defcheck solution-8917c89f
  (fn f [edges]
    (let [
          maps (map #(list (hash-map (first %) (rest %)) (hash-map (last %) (butlast %))) edges)
          all (apply merge-with concat (flatten maps))
          reachable (fn [self seen start]
                      (flatten
                        (cons start
                          (for [node (all start) :when (not (seen node))]
                            (self self (assoc seen node 1) node)))))
          ]
      (or (= 1 (count edges))
          (= (set (keys all)) (set (reachable reachable {} (key (first all)))))))))

(defcheck solution-8965d961
  (fn [edges]
    (loop [g (set (first edges)) [h & t] (rest edges) g* [] added 0]
      (let [conn? (fn [[x y]] (or (g x)(g y)))]
        (if (nil? h)
          (if (empty? g*) true
                          (if (= 0 added) false (recur g g* [] 0)))
          (if (conn? h)
            (recur (apply conj g h) t g* (inc added))
            (recur g t (conj g* h) added)))))))

(defcheck solution-8a1fed72
  (fn[x]
    (let [s (fn [se [x y]]
              (if (or (se x) (se y))
                (set (concat se [x y])) se))
          all (set (reduce concat #{} x))]
      (loop [r #{(first all)}]
        (let [m (reduce s r x)]
          (if (= m r) (= all m) (recur m)))))))

(defcheck solution-8a2ffd06
  (fn [e]
    (letfn [
            (v [e] (set (mapcat (fn [[a b]][a b]) e)))
            (id [e] (set (for [n (v e)] [n n])))
            (re [e] (set (for [[a b] e] [b a])))
            (q [e] (letfn [(f [x] (for [[a b] x [c d] x :when (= b c)] [a d]))] (let [e2 (set (f e))] (if (clojure.set/subset? e2 e) e (q (clojure.set/union e e2))))))
            (pi [e] (clojure.set/union e (id e) (re e)))]
      (let [n (count (v e)) t (count (q (pi e)))] (= (* n n) t)))))

(defcheck solution-8a33233f
  (fn[s](
          loop[ x (set(last (vec s))) y (pop (vec s)) l -1 ](
                                                              if(= (count y) l) (if(= l 0) true false)
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

(defcheck solution-8a43d813
  (fn [d]
    (let [a (fn [m k v] (assoc m k (conj (m k []) v)))
          g (reduce (fn [m [x y]] (a (a m x y) y x)) {} d)]
      (=
        (set (keys g))
        (loop [[h & t] [(ffirst d)] s #{}]
          (if h
            (recur (into t (remove s (g h))) (into s [h]))
            s))))))

(defcheck solution-8a53ba5a
  (fn [g] (
            letfn [
                   (add_edge[s e] ( conj s (first e) (last e)))
                   (sj[v f](
                            #(
                               if (= v f)
                               true
                               (if (= v %)
                                 false
                                 (sj % f)
                                 )
                               )
                            (reduce #(
                                       if (or (contains? %1 (first %2)) (contains? %1 (last %2)))
                                       (add_edge %1 %2)
                                       %1
                                       ) v g)

                            ))

                   ]

            (sj (add_edge #{} (first g))  (reduce add_edge #{} g))
            )))

(defcheck solution-8a589c56
  (fn [s1]
    (let [
          [m1 nodes]  (reduce (fn [[acc accn] [l r :as t]]
                                [(merge-with concat acc {l [r]} ) (into accn [ l r])]) [{} #{}] s1)
          m2 (reduce (fn [acc xxs] (let [vw (get m1 xxs)]
                                     (reduce (fn [a1 v1] (merge-with concat a1 {v1 [xxs]}))
                                       acc
                                       vw
                                       ))) m1 (keys m1))
          allxs (keys m1)
          k1 [(first allxs)]
          ]
      (loop [rs (into #{} k1) ]
        (let
         [k2 (reduce (fn [acc ls]  (into acc (get m2 ls))) rs rs)]
          (if (= (count k2) (count rs))
            (= (count k2) (count nodes))
            (recur k2 )
            ))))
    ))

(defcheck solution-8a5c0312
  (fn [graph]
    (let [nodes (set (flatten (seq graph)))
          in? (fn [seq elm] (some #(= elm %) seq))
          neighbors (fn [n] (map (fn [[a b]] (if (= a n) b a)) (filter #(in? % n) graph)))
          unvisited (fn [visisted n] (filter #(not (contains? visisted %)) (neighbors n)))]
      (loop [visited #{} component #{} queue [(first nodes)]]
        (if (empty? queue)
          (= component nodes)
          (recur
            (conj visited (peek queue))
            (conj component (peek queue))
            (into (pop queue) (unvisited visited (peek queue)))))))))

(defcheck solution-8a7c0e83
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
                            (if (= a b)
                              (fillFn mp b a)
                              (fillFn (fillFn mp a b) b a)
                              )
                            ) {}  pathcol)


         ]
      (if (nil? pathmap)
        false
        (loop[result [(first (keys pathmap))],pathMap pathmap]
          #_(println result)
          #_(println pathMap)
          (if (empty? pathMap)
            true
            (let[nextCol (reduce concat [] (map #(get pathMap %) result))]
              (if (empty? nextCol)
                false
                (recur (distinct nextCol) (reduce dissoc pathMap result) )
                )
              )
            )
          )
        )
      )
    ))

(defcheck solution-8aa0ef5
  (fn [xs] (let [nodes (set (distinct (apply concat (seq xs))))
                 aux (fn [f g z nd] (reduce #(if (= (f %2) nd) (into % (vector (g %2))) %) z xs))
                 go (fn [nd] (aux first second (aux second first #{} nd) nd))
                 succ (fn [nds] (reduce into (map go nds)))
                 ]
             (letfn [(limit [z] (into z (if (= z (succ z)) () (limit (succ z)))))]
               (= nodes (set (limit (set (first xs)))))))))

(defcheck solution-8af25973
  (fn [edges]
    (let [all-nodes (set (mapcat identity edges))]
      (loop [seen #{(ffirst edges)}]
        (if (= seen all-nodes)
          true
          (let [next-seen (into seen (mapcat (fn [node]
                                               (keep (fn [[a b]]
                                                       (cond
                                                         (= a node) b
                                                         (= b node) a)) edges)) seen))]
            (if (= seen next-seen)
              false
              (recur next-seen))))))))

(defcheck solution-8bf83a14
  (fn connected-graph? [edges]
    (let [in-component? (fn [component edge] (some #(contains? component %) edge))
          containing-components (fn [cs edge] (filter #(in-component? % edge) cs))
          components (reduce (fn [cs edge]
                               (if-let [ccs (not-empty (containing-components cs edge))]
                                 (-> (apply disj cs ccs)
                                   (conj (apply clojure.set/union (cons (set edge) ccs))))
                                 (conj cs (set edge))))
                       #{} edges)]
      (= 1 (count components)))))

(defcheck solution-8c9849bb
  (fn [P]
    (let [u clojure.set/union
          d clojure.set/difference
          G (reduce (partial merge-with u)
              (map (fn [[a b]] (into {} [[a #{b}] [b #{a}]])) P))
          N (set (keys G))]
      (loop [S #{(first N)}
             V #{}]
        (let [n (first S)]
          (if n
            (recur (d (u S (G n)) (conj V n))
              (conj V n))
            (= V N)))))))

(defcheck solution-8ccd34c5
  (fn [x] (let [nodes (set (apply concat x))
                j (fn [sets [a b]]
                    (let [sa (first (filter #(some #{a} %) sets))
                          sb (first (filter #(some #{b} %) sets))]
                      (clojure.set/union (disj sets sa sb) #{(clojure.set/union sa sb)})))]
            #_(println nodes (reduce j (set (map #(conj #{} %) nodes)) x))
            (= 1 (count (set (reduce j (set (map #(conj #{} %) nodes)) x)))))))

(defcheck solution-8d6ecb9e
  (letfn [(connected? [a b]
            (reduce (fn [acc x]
                      (or acc (< -1 (.indexOf b x))))
              false a))
          (connected-in? [graph node]
            (reduce #(or %1 (connected? node %2)) false graph))
          (path-to-any? [connected not-visited]
            (if (seq not-visited)
              (some true?
                (map #(when (connected-in? connected %)
                        (path-to-any? (conj connected %) (disj not-visited %)))
                  not-visited))
              true))]
    (fn [graph]
      (let [conn (first graph)]
        (true? (path-to-any? #{conn} (disj graph conn)))))))

(defcheck solution-8dae6eac
  (fn cg? ([g] (cg? g (reduce (fn [s [a b]] (conj (conj s a) b)) #{} g) #{} nil))
    ([g nl nt cl]
     (cond
       (not cl) (true? (some identity (map #(cg? g (disj nl %) #{%} %) nl)))
       (empty? nl) true
       :else
       (let [pnl (reduce (fn [s [a b]] (cond (= a cl) (conj s b)
                                             (= b cl) (conj s a) 1 s))
                   #{} g)
             nn (filter #(not (contains? nt %)) pnl)]
         (if (empty? nn) false
                         (some identity (map #(cg? g (disj nl %) (conj nt %) %) nn)))
         )))))

(defcheck solution-8dfbe8ee
  #(loop [s (set (first %))]
     (let [f flatten
           t (into s (f (for [i % :when (some s i)] i)))]
       (if (= t s)
         (= t (set (f (vec %))))
         (recur t)))))

(defcheck solution-8e270824
  (fn [coll]
    (->> (iterate
           (fn [[a b]]
             (let [r (filter #((complement empty?) (clojure.set/intersection b (set %))) a)]
               [(apply disj a r) (apply clojure.set/union b (map set r))]))
           [(disj coll (first coll)) (set (first coll))])
      (take (count coll))
      (last)
      (first)
      (empty?))))

(defcheck solution-8f16ba5c
  (fn connected?
    [graph]
    (letfn [
            (vertices [graph]
              (reduce #(conj (conj % (first %2)) (second %2)) #{} graph))

            (edges-with [vertex]
              (map
                #(if (= vertex (first %)) % (reverse %))
                (filter #(or (= vertex (first %)) (=  vertex (second %))) graph)))
            (walk [seen, edges]
              (cons (ffirst edges)
                (when-let [edge (seq(first (drop-while #(seen (second %)) edges)))]
                  (let [vertex1 (first edge) vertex2 (second edge)]
                    (walk (conj seen (ffirst edges)) (concat (edges-with vertex2) (drop-while #(seen (second %)) edges)  ))))))]
      (= (count (vertices graph)) (count (set(walk #{} (edges-with (ffirst graph)))))))))

(defcheck solution-8f391ca7
  (fn tg [s]
    (letfn [(nodes [s]
              (loop [s s,
                     result #{}]
                (if (empty? s)
                  result
                  (clojure.set/union (set (first s))
                    (nodes (rest s))))))
            (onecycle [s]
              (loop [s (sort s),
                     n1 (first (first s)),
                     n2 (second (first s)),
                     result #{}]
                (if (empty? s)
                  result
                  (if (or (contains? result n1)
                          (contains? result n2))
                    (recur (rest s) (first (first (rest s))) (second (first (rest s))) (conj result n1 n2))
                    (recur (rest s) (first (first (rest s))) (second (first (rest s))) (conj #{} n1 n2) )))))]
      (= (onecycle s) (nodes s)))))

(defcheck solution-8faabed3
  (fn connected? [graph]
    (let [edges (into graph (map reverse graph))
          edge-map (group-by first edges)
          nodes (set (keys edge-map))
          traverse (fn traverse [visited node]
                     (if (contains? visited node)
                       []
                       (into #{node}
                         (mapcat #(traverse (conj visited node) (second %)) (edge-map node)))))]
      (= nodes (traverse #{} (first nodes))))))

(defcheck solution-904b8d22
  (fn [edges]
    (let [n (count (set (flatten (into [] edges))))]
      (letfn [(neigh [v]
                (filter identity (for [[a b] edges]
                                   (cond
                                     (= a v) b
                                     (= b v) a))))]
        (loop [queue [ (first (first edges)) ]
               visited #{}]
          (if (empty? queue)
            (= (count visited) n)
            (let [v (peek queue)
                  neighbors (filter #(not (visited %)) (neigh v))]
              (recur
                (vec (into neighbors (pop queue)))
                (conj visited v)))))))))

(defcheck solution-91d79898
  (fn [graph] (letfn [ (vertices [edge-set] (sort (distinct (flatten (map flatten edge-set))))) (without [coll item] (cond (empty? coll) [] (= (first coll) item) (rest coll) :else (cons (first coll) (without (rest coll) item)))) (seq-contains? [coll item] (cond (empty? coll) false (= (first coll) item) true :else (seq-contains? (rest coll) item))) (edges-with [edge-set vertex] (filter #(seq-contains? % vertex) (map flatten edge-set))) (vertices-with-distance-1 [edge-set vertex] (mapcat #(without % vertex) (edges-with edge-set vertex))) (expand [edge-set vertices] (sort (distinct (concat vertices (mapcat #(vertices-with-distance-1 edge-set %) vertices))))) (iterate-while-changes [f x] (cons x (map second (take-while #(not= (first %) (second %)) (partition 2 1 (iterate f x)))))) (expand-full [edge-set] (last (iterate-while-changes (partial expand edge-set) (take 1 (vertices edge-set))))) (is-connected? [graph] (= (set (vertices graph)) (set (expand-full graph)))) ] (is-connected? graph))))

(defcheck solution-926d3cbc
  (fn [graph]
    (let [update (fn [m k f x] (assoc m k (f (get m k) x)))
          sconj (fnil conj #{})
          graph (reduce (fn [g [n m]]
                          (-> g
                            (update n sconj m)
                            (update m sconj n)))
                  {} graph)
          nodes (vec (keys graph))
          walk (fn walk [walked node]
                 (let [walked (conj walked node)]
                   (reduce walk walked (clojure.set/difference (graph node) walked))))]
      (== (count nodes) (count (walk #{} (peek nodes)))))))

(defcheck solution-929762ea
  (fn connected [xs]
    (loop [visited (conj #{} (first xs))
           unvisited (rest xs)]
      (or (= xs visited)
          (let [node-nums (reduce (fn [accum item]
                                    (apply conj accum item)) visited)
                neighbors (filter
                            (fn [item]
                              (some #(or (= (first item) %)
                                         (= (second item) %)) node-nums))
                            unvisited)]
            (if (empty? neighbors)
              false
              (recur (apply conj visited neighbors)
                (remove (apply conj #{} neighbors) unvisited))))))))

(defcheck solution-929d0ed8
  (fn [s c g]
    (let [n (s (apply c g))]
      (empty? (remove (nth (iterate #(s (c % (keep (fn [[a b]] (if (% a) b (if (% b) a))) g))) #{(first n)}) (count n)) n)))) set concat)

(defcheck solution-92fe1b68
  (fn all-connected? [paths]
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
                  sorted-paths)))]
      (= (all-nodes paths) (connect-paths paths)))))

(defcheck solution-935024b6
  (fn [s]
    (true?
      ((fn connected? [node-set reminder]
         (or (empty? reminder)
             (some #(when (some (fn [k] (node-set k)) %)
                      (connected? (into node-set %) (disj (set reminder) %))) reminder)))
       (into #{} (first s)) (rest s)))))

(defcheck solution-93a768e6
  (fn [connections graph]
    (if (empty? graph)
      (apply = (set (keys connections)) (vals connections))
      (let [[a b] (first graph)
            a-connected (get connections a #{a})
            b-connected (get connections b #{b})
            ab-connected (into a-connected b-connected)]
        (recur
          (reduce (fn [conn node] (assoc conn node ab-connected))
            connections
            ab-connected)
          (disj graph [a b]))))) {})

(defcheck solution-94252668
  (fn [edges]
    (letfn [(adjacent? [[v w] [x y]]
              (or (= v x) (= v y) (= w x) (= w y)))]
      (loop [frontier #{(first edges)}
             edges-unvisited (disj edges (first edges))]
        (if (empty? edges-unvisited)
          true
          (if (empty? frontier)
            false
            (recur (into (disj frontier (first frontier)) (filter #(adjacent? (first frontier) %) (disj edges-unvisited (first frontier))))
              (clojure.set/difference edges-unvisited frontier))))))))

(defcheck solution-94a211ff
  (fn connected? [graph]
    (letfn [(test [c remaining]
              (if (empty? remaining)
                true
                (let [node (some #(when (some c %) %) remaining)]
                  (if (nil? node)
                    false
                    (recur (into c node) (remove #(= node %) remaining))))))]
      (test (into #{} (first graph)) (rest graph)))))

(defcheck solution-9510dfc8
  (fn [edges]
    (let [[first-node & others] (flatten (seq edges))
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
                    unconnected)))))))

(defcheck solution-955a33ab
  #(let [graph (group-by first
                 (mapcat (fn [e]
                           [e (vec (reverse e))])
                   %))]
     ((fn traverse [v]
        (let [new-v (into v (map last (mapcat graph v)))]
          (if (= new-v v)
            (= v (set (keys graph)))
            (traverse new-v))))
      #{(first (keys graph))})))

(defcheck solution-967e5ffc
  (letfn [(component [sets v]
            (or (some #(if (% v) %) sets) #{v}))
          (update-components [sets [v1 v2]]
            (let [c1 (component sets v1)
                  c2 (component sets v2)]
              (conj (remove #(or (= % c1) (= % c2)) sets)
                (clojure.set/union c1 c2))))
          (components [vs]
            (reduce update-components #{} vs))]
    (fn [x] (= (count (components x)) 1))))

(defcheck solution-96ec7ddd
  (fn connected-graph? [graph]
    (letfn
     [(connects-with-any [edge connected]
        (some
          #(some (set edge) %)
          connected))
      (get-connections [connected rest-edges]
        (filter
          #(connects-with-any % connected)
          rest-edges))
      (is-connected? [connected rest-edges]
        #_(prn connected rest-edges)
        (if (empty? rest-edges)
          true
          (let [new-connecions (get-connections connected rest-edges)]
            (if (empty? new-connecions)
              false
              (is-connected?
                (reduce conj connected  new-connecions)
                (reduce disj rest-edges new-connecions))))))]
      (is-connected?
        #{(first graph)}
        (disj graph (first graph))))))

(defcheck solution-9723f326
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
                              (get m t))))])))]
      (let [g1 (g->m g)
            g2 (g->m (map (comp vec reverse) g))
            m (merge-with #(set (concat %1 %2)) g1 g2)
            size (count m)]
        (= size
          (-> (nth (iterate expand m) size) vals first count))))))

(defcheck solution-976e8f1a
  (fn [g]
    (let [u clojure.set/union
          g (apply merge-with u (mapcat (fn [[k v]] [{k #{v}} {v #{k}}]) g))
          f #(into {} (for [[k v] %] [k (reduce (fn [v k] (u v (% k))) v v)]))]
      (loop [g g]
        (if (= g (f g))
          (apply = (count g) (map (comp count val) g))
          (recur (f g)))))))

(defcheck solution-9780ca51
  (fn [g]
    (let [ gg (into g (map #(vector (last %) (first %)) g)) ]
      (letfn [ (f [ [a b] r]
                 (->>
                   (filter #(= (first %) b) r)
                   (map #(vector a (last %)))
                   ))
              (trans-closure [r]
                (let [x  (mapcat #(f % r) r)
                      y  (filter #(nil? (r %)) x )]
                  (if (empty? y) r (recur (into r y)))))]
        (let [ closure (trans-closure gg)
              nodes (distinct (map #(first %) gg))
              ] ( = (count closure) ( * (count nodes) (count nodes))))
        ))))

(defcheck solution-980d78e9
  (fn __ [d]
    (letfn [(isce [a b]
              (> (count (clojure.set/intersection (into #{} a) (into #{} b))) 0))
            (somcon [a ans]
              (not= 0 (count (filter #(true? %)
                               (for [b ans]
                                 (isce a b))))))
            (getgroup [ds]
              (let [vs (into [] ds)]
                (loop [v vs i 0 n (count vs) acc []]
                  (if (= i 0) (recur (rest v) (inc i) n (conj acc (first v)))
                              (if (= i n) acc
                                          (recur (rest v) (inc i) n
                                            (if (somcon (first v) acc)
                                              (conj acc (first v))
                                              acc)))))))
            (conup [s]
              (loop [m s acc []]
                (if (empty? m) acc
                               (let [ans (getgroup m)]
                                 (recur (clojure.set/difference m (set ans))
                                   (conj acc (distinct (flatten ans))))))))]
      (loop [m (into #{} (conup d)) prevn 0]
        (if (= (count m) prevn)
          (= 1 (count m))
          (recur (into #{} (conup m)) (count m)))))))

(defcheck solution-990ca5c
  (fn [s] (loop [c (vec s)]
            (cond
              (every? #(= (ffirst (vec s)) %)
                (flatten c))  true
              (some #(and
                      (not= (first %) (second %))
                      (some #{(ffirst (vec s))} %)) c)
              (let [x (some #(if (and
                                  (not= (first %) (second %))
                                  (some #{(ffirst (vec s))} %)) % ) c)
                    y (if (= (ffirst (vec s)) (first x))
                        (second x) (first x))]
                (recur (partition 2
                         (map #(if (= y %) (ffirst (vec s)) %)
                           (flatten c))
                         )))
              :else false

              )

            )))

(defcheck solution-9930d00a
  (fn graph-connectivity [coll]
    (let [ne (fn [coll p] (first (filter #(or (= (first p) (first %)) (= (first p) (second %))) coll)))
          replace (fn [coll p]
                    (let [rep (map #(vector (if (= (second p) (first % )) (first p) (first % ))
                                      (if (= (second p) (second %)) (first p) (second %))) coll)]
                      (filter #(not (and (= (first p) (first %)) (= (first %) (second %)))) rep)) )]
      (if (> 2 (count coll)) true
                             (loop [f (first coll) r coll]
                               (let [rep (replace r f)]
                                 (if (empty? rep) true
                                                  (let [f (ne rep f)] (if (nil? f) false (recur f rep))))))))))

(defcheck solution-9a1ad17d
  (fn connected? [graph]
    (let [first-node (first graph)
          nodes (rest graph)
          connected-nodes? (fn [[a b] [c d]] (cond (or (= a c)
                                                       (= a d)
                                                       (= b c)
                                                       (= b d)) true
                                                   :else false))]
      (loop [node first-node found [] nodes nodes]
        (cond (empty? nodes) true
              (empty? node) (if (empty? found) false
                                               (recur (first found) (rest found) nodes))
              :else (let [toadd (into #{} (filter #(connected-nodes? node %) nodes))]
                      (recur nil (into #{} (concat toadd found)) (remove toadd nodes))
                      )
              )
        )
      )
    ))

(defcheck solution-9a6bad83
  (fn __ [x]
    (let [my-assoc (fn [acc [k v :as element]]
                     (let [old (acc k)
                           nu (if old (conj old v) #{v})]
                       (assoc acc k nu)))
          read-in (fn [xs]
                    (reduce (fn [acc element]
                              (my-assoc
                                (my-assoc acc element)
                                (reverse element)))
                      {} xs))
          gcons (fn gcons
                  ([xs x] (gcons xs x #{}))
                  ([xs x seen]
                   (if (seen x)
                     seen
                     (reduce into #{}
                       (for [other (xs x)]
                         (gcons xs other (conj seen x)))))))
          graph (read-in x)
          conns (gcons graph (first (first graph)))]
      (= (count (keys graph)) (count conns)))))

(defcheck solution-9b997d85
  (fn [v]
    (let [sv (set (reduce (fn [s [x y]] (conj s x y)) [] v))]
      (= sv (last (take (count sv)
                    (iterate (fn [xs]
                               (reduce (fn [s [l r]]
                                         (cond (contains? s l) (conj s r)
                                               (contains? s r) (conj s l)
                                               :else s)) xs v)) #{(first sv)})))))
    ))

(defcheck solution-9bb0f05b
  #(letfn [(f [s]
             (let [t (into s (for [x s y % :when (some (set x) y)] y))]
               (if (= s t) t (recur t))))]
     (= (f #{(first %)}) %)))

(defcheck solution-9ce30d7a
  (fn [g]
    (letfn
     [(grow [g reached]
        (into reached (concat
                        (for [[x y] g :when (contains? reached y)] x)
                        (for [[x y] g :when (contains? reached x)] y))))
      (nodes [g] (into #{} (concat (map first g) (map second g))))
      (main [g]
        (loop [reached #{(first (first g))}]
          (if (= reached (nodes g))
            true
            (let [new-reached (grow g reached)]
              (if (= new-reached reached)
                false
                (recur new-reached))))))]
      (main g))))

(defcheck solution-9db226e5
  (fn  [c]
    (let [[f & r] (seq c)
          h #(apply concat %)]
      (= (set (h c))
        (reduce (fn [a x]
                  (into a (h (filter #(some a %) c))))
          (set f) r)))))

(defcheck solution-9e95eef3
  (fn [edges]
    (let [graph (group-by keys (mapcat (fn [[a b]] (list {a b} {b a})) edges))
          vert (keys graph)
          connected (fn [[h & t :as vlist] acc]
                      (if (nil? h)
                        acc
                        (if (acc h)
                          (recur t acc)
                          (recur (concat t (map vals (graph h))) (conj acc h)))))]
      (= (count (connected (list (first vert)) #{})) (count vert)))))

(defcheck solution-9ef915fb
  (fn [sn]

    (let [node-map (reduce (fn [m [n1 n2]] (assoc m n1 (conj (get m n1 #{}) n2) n2 (conj (get m n2 #{}) n1))) {} sn)]
      (loop [stack [(first (keys node-map))], visited #{}]

        (if (empty? stack)
          (= (count (keys node-map)) (count visited))
          (if (visited (peek stack))
            (recur (pop stack) visited)
            (recur (apply conj stack (node-map (peek stack))) (conj visited (peek stack)))))))))

(defcheck solution-9f2dfdca
  (fn [edges]
    (let [vertices (apply hash-set (distinct (flatten (seq edges))))]
      (loop [visited #{(first vertices)} remaining edges]
        (if (= visited vertices)
          true
          (let [{:keys [can-visit later]} (group-by #(if (some visited %) :can-visit :later) remaining)]
            (if (empty? can-visit)
              false
              (recur (apply conj visited (flatten can-visit)) (apply hash-set later)))))))))

(defcheck solution-9fa1c594
  (fn graph-connectivity [graph]
    (let [iterate-until
                    (fn [f init pred] (->> (iterate f init) (partition 2 1) (drop-while #(not (apply pred %))) first second))
          nodes (->> (seq graph) flatten set)
          adjacents (fn [node] (into #{} (for [[n m] graph :when (or (= n node) (= m node))] (if (= n node) m n))))
          step (fn [[visited last-visited]]
                 (let [new-visited (->> (map adjacents last-visited) (reduce clojure.set/union))]
                   [(clojure.set/union visited new-visited) new-visited]))
          stop (fn [[visited _] [next-visited _]] (= visited next-visited) )
          some-node (some nodes nodes)
          [accessible-from-node _] (iterate-until step [#{} #{some-node}] stop)]
      (= (count accessible-from-node) (count nodes)))))

(defcheck solution-9fd73e69
  (fn graph-connectivity[s]
    (let[ rs (reduce
               (fn[rr aaa]
                 (reduce
                   (fn[rs [a b]] (if(or (get rs a) (get rs b)) (conj rs a b) rs))
                   rr
                   (next s)))
               (set (first s))
               (range (count s)))
         #_#__ (prn )]
      (empty? (filter (fn[[a b]] (if(get rs a) false true)) s)))))

(defcheck solution-9fef3663
  (fn [g]
    (let [v (set (apply concat g))]
      (true? (some #(= v %) (vals
                              (loop [s (apply merge
                                         (map #(hash-map % #{%})
                                           v))]
                                (let [x (apply merge-with
                                          clojure.set/union
                                          (for [n  (keys s)
                                                nn (keep
                                                     #(cond (= (% 0) n) (% 1)
                                                            (= (% 1) n) (% 0)
                                                            true        nil)
                                                     g)]
                                            {nn (conj (s n) nn)}))]
                                  (if (= x s) x (recur x))))))))))

(defcheck solution-a0101db4
  (fn connectivity [edges]
    (loop [removed #{(first edges)}
           not-visited (set (rest edges))]
      (let [
            neighbors (set (for [edge not-visited
                                 :when
                                 (not (empty?
                                        (clojure.set/intersection
                                          (set (flatten (vec removed)))
                                          (set edge)
                                          )
                                        ))]
                             edge
                             ))
            ]
        (cond
          (and (empty? neighbors) (zero? (count not-visited))) true
          (empty? neighbors) false
          :else
          (recur neighbors (clojure.set/difference not-visited neighbors))
          )
        )
      )
    ))

(defcheck solution-a01eb664
  (let [closure (let [prod (fn [u v]
                             (set (reduce
                                    (fn [a x]
                                      (concat a
                                        (map #(vector x %) v)))
                                    [] u)))
                      nxt (fn nxt [ls]
                            (clojure.set/union ls
                              (into #{}
                                (map
                                  #(vector (first (first %)) (second (second %)))
                                  (filter
                                    #(= (second (first %)) (first (second %)))
                                    (prod ls ls))))))]
                  (fn [s]
                    (loop [s s
                           n (nxt s)]
                      (if (= s n)
                        s
                        (recur n (nxt n))))))]
    (fn [edges]
      (let [verts (count (distinct (flatten (into '() edges))))]
        (= (* verts verts)
          (count (closure
                   (clojure.set/union edges
                     (into #{} (map #(vector (second %) (first %)) edges))))))))))

(defcheck solution-a053edc
  (fn connected? [input]
    (letfn [(phun [acc
                   [l r]]
              (if (empty? acc)
                #{#{l r}}
                (let [grouped (group-by #(cond
                                           (and (contains? % l) (contains? % r)) :both
                                           (contains? % l) :has-a
                                           (contains? % r) :has-b
                                           :else :neither)
                                acc)
                      joined (set (apply concat (map #(into % [l r])
                                                  (concat (:has-a grouped)
                                                    (:has-b grouped)))))
                      possible-new-subset (if (and (empty? joined) (empty? (:both grouped)))
                                            #{l r}
                                            nil)]
                  (set (filter #(and (not (empty? %))
                                     (not (contains? (set (:has-a grouped)) %))
                                     (not (contains? (set (:has-b grouped)) %)))
                         (concat acc
                           #{joined}
                           #{possible-new-subset}
                           (:neither acc))))
                  )))]
      (if (= 1 (count input))
        true
        (= 1 (count (reduce phun #{} input)))))))

(defcheck solution-a0972ff9
  (fn connected?
    ([graph] (boolean (connected? (set (first graph)) (set (rest graph)))))
    ([connected-subgraph graph]
     (letfn [(conn-subgraph [[a b :as edge]]
               (and
                (boolean (or (connected-subgraph a) (connected-subgraph b)))
                (connected? (into connected-subgraph edge) (disj graph edge))))]
       (or
        (empty? graph)
        (some conn-subgraph graph))))))

(defcheck solution-a0ba39fc
  (fn [s]
    (let [closure (fn [s]
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

          s' (into s
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
      )))

(defcheck solution-a0d33615
  (fn connected? [graph]
    (letfn [(transitions [node graph]
              (set (map #(if (= node (first %)) % (reverse %))
                     (filter #(or (= node (first %)) (= node (second %))) graph))))

            (reachable-nodes [start-node explored-nodes graph]
              (let [explored-nodes (conj explored-nodes start-node)
                    unexplored-transistions (remove #(explored-nodes (second %)) (transitions start-node graph))]
                (if (empty? unexplored-transistions)
                  explored-nodes
                  (reduce #(into %1 (reachable-nodes (second %2) explored-nodes graph))
                    explored-nodes
                    unexplored-transistions))))

            (nodes [transitions]
              (reduce #(conj (conj %1 (first %2)) (second %2))
                #{}
                transitions))]

      (= (count (nodes graph))
        (count (reachable-nodes (ffirst graph) #{} graph))))))

(defcheck solution-a1049c65
  (fn [n]
    (= 1 (count (let [graph (vec n)
                      nodes (distinct (flatten graph))]
                  (loop [n 0 grps graph]
                    (if (= n (count nodes) ) grps
                                             (let [   x (nth nodes n)
                                                   grps (group-by #(some #{x} %) grps)
                                                   pgrps  (flatten (grps x))
                                                   ngrps  (grps nil)
                                                   ]
                                               (recur (inc n) (cons pgrps ngrps) )
                                               ))))))))

(defcheck solution-a18412d3
  (fn [edges]
    (let [nodes (reduce into #{} edges)
          edge-map (reduce (fn [m [x y]] (-> m (update-in [x] conj y) (update-in [y] conj x))) {} edges)]
      (loop [s #{(first nodes)}]
        (let [new-s (into s (flatten (vals (select-keys edge-map s))))]
          (cond
            (= new-s nodes) true
            (= new-s s) false
            :else (recur new-s)))))))

(defcheck solution-a25c97e7
  (fn graph-connect1? [graph]
    (= 1 (count
           (let [join-sets (fn[ss s]
                             (let [h (group-by #(some s %) ss)
                                   not-connected (h nil)
                                   connected (apply clojure.set/union
                                               (cons s (flatten (vals (dissoc h nil)))))]
                               (cons connected not-connected)))]
             (loop [ g (map set graph)]
               (let [size (count g)
                     ng (reduce join-sets
                          [(first g)] (rest g))]
                 (if (or (nil? (next ng)) (= size (count ng)))
                   ng
                   (recur ng)))))))))

(defcheck solution-a29b138a
  (fn [g]
    (not (next
           (reduce
             (fn [g [x y]]
               (let [l (fn [n] (some #(when (% n) %) g))
                     a (l x)
                     b (l y)]
                 (if a
                   (if b
                     (conj (disj g a b) (into a b))
                     (conj (disj g a) (conj a y)))
                   (if b
                     (conj (disj g b) (conj b x))
                     (conj g (set [x y]))))))
             #{}
             g)))))

(defcheck solution-a39fbb9c
  (fn [g]
    (= (reduce #(into % %2) #{} g)
      (loop [res (set (first g))]
        (let [res+1 (reduce #(if (some % %2) (into % %2) %) res g)]
          (if (= res res+1)
            res
            (recur res+1)))))))

(defcheck solution-a3d41af7
  (fn [graph]
    (< (count (reduce (fn [subraphs [a b]]
                        (let [matching-subgraphs (filter (fn [subgraph]
                                                           (or (contains? subgraph a) (contains? subgraph b)))
                                                   subraphs)]
                          (if (empty? matching-subgraphs)
                            (into subraphs (conj subraphs (set [a b])))
                            (let [with-removed-unmerged-subraphs (apply disj subraphs matching-subgraphs)
                                  with-merged-subraphgs (conj with-removed-unmerged-subraphs (into #{} (apply concat (set [a b]) matching-subgraphs)))]
                              with-merged-subraphgs))))
                #{}
                graph))
      2)))

(defcheck solution-a42751a
  (letfn [(findk [s x] (if (= x (s x)) x (recur s (s x))))
          (unionk [s [a b]] (let [fa (findk s a) fb (findk s b)] (if (= fa fb) s (assoc s fa fb))))
          (addk [s [a b]] (assoc s a a b b))
          (makeSet [c] (reduce unionk (reduce addk {} c) c))
          (connected? [s] (= 1 (count (distinct (map #(findk s %) (keys s))))))] (comp connected? makeSet)))

(defcheck solution-a437cc4a
  (letfn [(partition-grps [xs]
            (reduce (fn [a [p q]]
                      (let [p-grps (group-by #(% p) a)
                            q-grps (group-by #(% q) (p-grps nil))
                            pq-grp (concat [p q] (apply concat (p-grps p))
                                     (apply concat (q-grps q)))]
                        (cons (set pq-grp) (q-grps nil)))) #{} xs))]
    (fn [xs]
      (= (count (partition-grps xs)) 1))))

(defcheck solution-a47a5429
  (letfn
   [(remove-path [graph a b]
      (update-in (update-in graph [a] disj b)
        [b] disj a))
    (find-node [graph start node]
      (let [nbors (graph start)]
        (or (= start node)
            (nbors node)
            (some #(find-node (remove-path graph start %) % node) nbors))))]
    (fn [edges]
      (let [; '(:a :b ...)
            nodes (->> edges seq flatten distinct)
            ; map of {node #{neighbors}, ...}
            graph (reduce (fn [m [a b]]
                            (update-in
                              (update-in m [a] (comp set conj) b)
                              [b] (comp set conj) a)) {} edges)
            ]
        (every? identity
          (for [a nodes b nodes :when (not= a b)] (find-node graph a b)))))))

(defcheck solution-a4d48bf3
  (fn [s]
    (empty?
      ((fn f [e p]
         (let [p (disj p e)
               es (remove #(apply distinct? (concat e %)) p)]
           (reduce #(f %2 %) p es))) (first s) s))))

(defcheck solution-a506753a
  (fn [s]
    (loop [i s matched nil rem '() looped nil]
      (cond
        (empty? i)
        (cond
          (empty? rem) true
          (or (nil? looped) (> looped (count rem)))
          (recur rem matched '() (count rem))
          :else false)
        (nil? matched)
        (recur (rest i) (set (first i)) rem looped)
        (some matched (first i))
        (recur (rest i) (apply conj matched (first i)) rem looped)
        :else
        (recur (rest i) matched (conj rem (first i)) looped)))))

(defcheck solution-a583f469
  (fn [edges]
    (let [nodes (into #{} (concat (map first edges) (map second edges)))
          connections (->> (concat edges (map reverse edges))
                        (group-by first)
                        (map (fn [[k v]] [k (map second v)]))
                        (into {}))]
      (loop [to-visit (into #{} (take 1 nodes)) visited #{}]
        (if (seq to-visit)
          (let [head (first to-visit)
                remaining (disj to-visit head)
                remaining (->> (connections head)
                            (remove visited)
                            (into remaining))]
            (recur remaining (conj visited head)))
          (= visited nodes))))))

(defcheck solution-a68d82ad
  (fn [relations]
    (let [transitive-closure (loop [prev-relations relations]
                               (let [result (into
                                              prev-relations
                                              (apply
                                                concat
                                                (for [[a1 b1 :as r1] prev-relations]
                                                  (apply
                                                    concat
                                                    (for [[a2 b2 :as r2] prev-relations
                                                          :when (and (not= r1 r2)
                                                                     (or (= b1 a2)
                                                                         (= b1 b2)
                                                                         (= a1 a2)
                                                                         (= a1 b2)))]
                                                      [[a1 b2] [b1 a2] [a1 a2] [b1 b2]])))))]
                                 (if (= result prev-relations)
                                   result
                                   (recur result))))
          connections (reduce
                        (fn [acc [a b]]
                          (let [conn-a (get acc a)
                                conn-b (get acc b)]
                            (merge-with clojure.set/union
                              acc
                              {a #{b}}
                              {b #{a}})))
                        {}
                        transitive-closure)]
      (every? (fn [k]
                (= (set (keys connections)) (set (conj (get connections k) k))))
        (keys connections)))))

(defcheck solution-a6d414fc
  (fn connected-graph?
    [graph]
    (loop [edges #{(first graph)}
           nodes (set (first graph))
           remaining-edges (disj graph (first graph))]
      (if-let [edges-addition
               (seq (remove nil? (map #(if (some identity (map nodes %)) %)
                                   remaining-edges)))]
        (recur (apply conj edges edges-addition)
          (apply conj nodes (flatten edges-addition))
          (apply disj remaining-edges edges-addition))
        (empty? remaining-edges)))))

(defcheck solution-a7b75f7c
  (fn [edges]
    (first
      (drop-while sequential?
        (iterate
          (fn [[connected edges]]
            (if (empty? edges)
              true
              (let [nodes (filter #(some connected %) edges)]
                (if (empty? nodes)
                  false
                  (let [edges (filter #(not (some connected %)) edges)
                        connected (into connected (flatten nodes))]
                    [connected edges])))))
          (let [start-node (ffirst edges)]
            [#{start-node} edges]))))))

(defcheck solution-a90dd7f4
  (letfn [
          (to-adjacency-map [edges]
            (loop [m {}, es edges]
              (if-let [[[n1 n2] & es] (seq es)]
                (recur (merge-with clojure.set/union m {n1 #{n2}} {n2 #{n1}}) es)
                m)))

          (transitive-closure-1 [m]
            (apply merge-with clojure.set/union (for [[k vs] m, v vs] {k (clojure.set/union vs (get m v))})))

          (transitive-closure-* [m]
            (let [m2 (transitive-closure-1 m)]
              (if (= m2 m)
                m
                (recur m2))))]

    (fn is-connected [edges]
      (let [m (transitive-closure-* (to-adjacency-map edges))]
        (= (count (keys m)) (apply min (map count (vals m))))))))

(defcheck solution-a9200eba
  (fn [s]
    (apply = (vals (reduce
                     (fn [g [a b]]
                       (let [r (clojure.set/union (g a #{a}) (g b #{b}))]
                         (reduce #(assoc % %2 r) g r)))
                     {} s)))))

(defcheck solution-a938e0e5
  (fn [edges]
    (let [find (fn [m v] (first (clojure.set/select #(% v) m)))
          union (fn [m v1 v2]
                  (let [vs (clojure.set/union (find m v1) (find m v2))
                        m (remove (into #{} [(find m v1) (find m v2)]) m)]
                    (conj (set m) vs)))]
      (loop [forest (->> edges seq flatten (map hash-set) (into #{}))
             edges edges]
        (if (empty? edges)
          (= (count forest) 1)
          (recur (apply union forest (first edges)) (rest edges)))))))

(defcheck solution-a94d0e6d
  (fn [edges]
    (letfn [(neighbors [coll]
              (let [forward-map (reduce (fn [n-map edge]
                                          (if (contains? n-map (first edge))
                                            (update-in n-map [(first edge)] conj (second edge))
                                            (conj n-map [(first edge) #{(second edge)}]))) '{} coll)]
                (reduce (fn [n-map edge]
                          (if (contains? n-map (second edge))
                            (update-in n-map [(second edge)] conj (first edge))
                            (conj n-map [(second edge) #{(first edge)}]))) forward-map coll)))]
      (let [n-map (neighbors edges)]
        (loop [remaining [(first (first n-map))] visited #{}]
          (if (empty? remaining)
            (= (count visited) (count n-map))
            (if (contains? visited (first remaining))
              (recur (rest remaining) visited)
              (let [neighbors (get n-map (first remaining))]
                (recur (reduce (fn [to-visit neighbor]
                                 (if (contains? visited neighbor)
                                   to-visit
                                   (conj to-visit neighbor))) (rest remaining) neighbors) (conj visited (first remaining)))))))))))

(defcheck solution-a951051f
  (fn [x] (letfn [
                  (adjlist [e] (let [
                                     ee (concat e (map reverse e))
                                     ve (group-by first ee)]
                                 (into {} (for [[k v] ve] [k (map second v)]))))
                  (connected? [g] (letfn [
                                          (add-vertex [s v] (if (s v) s
                                                                      (reduce add-vertex (conj s v) (g v))))]
                                    (= (count (add-vertex #{} (ffirst g))) (count g))))]
            (connected? (adjlist x)))))

(defcheck solution-a98379f4
  (fn [g]
    (let [
          f first
          d (fn r [c v]
              (if (v c) v
                        (reduce
                          #(r (second %2) (r (f %2) %1))
                          (conj v c)
                          (filter #((set %) c) g))))
          v (mapcat (fn [n] n) g)
          ]
      (=
        (d (f v) #{})
        (set v)))))

(defcheck solution-ab27a0de
  (fn connected? [coll]
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

            (walk [node edges done]
              (if (empty? edges)
                (conj [] edges (distinct done))
                (loop [_current edges]
                  #_(println "debug!:" node edges _current done)
                  (if (empty? _current)
                    ; return value of path?
                    (conj [] edges (distinct done))
                    (let [e (first _current)]
                      (if (edge? node e)
                        (walk (next-node node e) (_remove edges e) (apply conj done e))
                        (recur (rest _current))))
                    ))))]

      (loop [start (first (nodes (vec coll)))
             edges coll]
        (let [ret (walk start (vec edges) [])
              yet-edge (first ret)
              done-node (last ret)
              startable (clojure.set/intersection (set (nodes yet-edge)) (set done-node))]

          #_(println yet-edge done-node startable)
          (if (empty? yet-edge)
            true
            (if (empty? startable)
              false
              (recur (first startable) yet-edge)))
          ))
      )))

(defcheck solution-ab289f0d
  (fn [es]
    (let [sets      (map set es)
          integrate (fn [es] (when-let [[f & r] (seq es)]
                               (set (map #(if-not (empty? (clojure.set/intersection % f)) (clojure.set/union % f) %) r))))]
      (->> sets
        (iterate integrate)
        (drop-while #(> (count %) 1))
        (ffirst)
        (= (apply clojure.set/union sets))))))

(defcheck solution-ab2c6dff
  (fn [xs]
    (let [cs (reduce (fn [cs [q p]]
                       (assoc cs q (conj (get cs q #{}) p)
                                 p (conj (get cs p #{}) q))) {} xs)]
      (loop [v #{}
             c [(ffirst cs)]]
        (if (seq c)
          (let [v2 (into v c)]
            (recur v2 (remove v2 (mapcat cs c))))
          (= v (into #{} (flatten (seq xs)))))))))

(defcheck solution-ab73fb05
  (fn connected? [edges]
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
          (= connected-nodes all-nodes))))))

(defcheck solution-abe881ba
  (fn [graph]
    (let [all-nodes (->> (apply concat graph)
                      set)]
      (->> (iterate (fn [nodes]
                      (->> (for [n nodes
                                 t (concat (->> (filter (comp (partial = n) first) graph)
                                             (map second))
                                     (->> (filter (comp (partial = n) second) graph)
                                       (map first)))]
                             t)
                        (concat nodes)
                        set))
             (set [(first all-nodes)]))
        (take (count all-nodes))
        last
        (= all-nodes)))))

(defcheck solution-abf35ceb
  (fn is-connected? [coll]
    (letfn [(census [ws]
              (let [mapped-vectors (group-by identity (flatten (vec ws)))
                    reducer (fn [bag tuple] (assoc bag (first tuple) (count (second tuple))))]
                (reduce reducer {} mapped-vectors)))
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
      (connected? coll))))

(defcheck solution-acd0f25c
  (fn [edges]
    (let [bfs (fn [start m]
                (loop [visited #{}
                       cands #{start}]
                  (if (empty? cands)
                    visited
                    (let [cur (first cands)
                          new-cands (concat (remove #(= % cur) cands)
                                      (clojure.set/difference (set (get m cur)) visited))]
                      (recur (conj visited cur) new-cands)))))

          make-graph (fn [edges]
                       (reduce (fn [m [a b]]
                                 (assoc m
                                   a (conj (m a []) b)
                                   b (conj (m b []) a))) {} edges))

          g (make-graph edges)]
      (= (count (bfs (nth (keys g) 0) g)) (count (set (flatten (vals g))))))))

(defcheck solution-acfad91
  (fn [s]
    ((fn [s v]
       (let [ss (group-by #(boolean (some v %)) s)
             vp (reduce into v (ss true))]
         (or (nil? (ss false))
             (and (> (count vp) (count v)) (recur (ss false) vp)))))
     (map set s) #{(ffirst s)})))

(defcheck solution-ad116039
  (fn [edges]
    (letfn [(separate [pred coll]
              (reduce #(if (pred %2) [(conj (first %) %2) (second %)]
                                     [(first %) (conj (second %) %2)])
                [[] []] coll))
            (in-path? [path vs]
              (let [t (separate #(or (contains? path (first %))
                                     (contains? path (second %)))
                        vs)
                    vs2 (second t)]
                (cond (empty? vs2) true
                      (= (count vs) (count vs2)) false
                      :else (in-path? (into path (apply concat (first t))) vs2))))]
      (in-path? (set (first edges)) (rest edges)))))

(defcheck solution-aeb58e10
  (fn [es] (let [ng (fn [es ps] (into ps (apply concat (for [p ps e es :when (e p)] e))))
                 n (count (set (apply concat es)))]
             (->> (iterate #(ng (map set es) %) (set (first es))) (take n) last count (= n)))))

(defcheck solution-aec6eec7
  (fn[tuples]
    (let [vertexes
          (into #{} (mapcat identity tuples))

          neighbors
          (fn[v]
            (let [edges
                  (filter #(or (= v (first %)) (= v (second %))) tuples)

                  outgoing
                  (fn[e] (if (= v (first e))
                           e (into (empty e) (reverse e))))]

              (->> edges
                (map outgoing)
                (map second)
                (into #{}))))

          reachable
          (fn[v]
            (loop [res #{v}]
              (let [next
                    (into (empty res)
                      (clojure.set/union res
                                         (set (mapcat neighbors res))))]
                (if (= res next)
                  res (recur next)))))]

      (every? identity (->> vertexes
                         (map reachable)
                         (map #(= vertexes %)))))))

(defcheck solution-af235242
  (fn is-connected?
    [g]
    (let [[x & xs] (sort (seq g))]
      (= (into #{} (mapcat identity g)) (reduce (fn [a e]
                                                  (if (seq (clojure.set/intersection a (set e)))
                                                    (into a e) a)) (set x) xs)))))

(defcheck solution-af4603bb
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

      (= visited (set (keys neighbors))))))

(defcheck solution-afe1a5c6
  (fn connected? [edges]
    (let [graph (reduce (fn [acc [a b]](merge-with clojure.set/union acc {a #{b}} {b #{a}})) {} edges)
          all-vertices (set (apply concat (seq edges)))
          ]
      (loop [to-check #{(first all-vertices)}
             visited #{}]
        (if (empty? to-check)
          (= visited all-vertices)
          (let [checked-vertex (first to-check)
                new-visited (conj visited checked-vertex)
                new-to-check (clojure.set/union (set (rest to-check)) (clojure.set/difference (get graph checked-vertex #{}) new-visited))]
            (recur new-to-check new-visited)
            )
          )
        )
      )))

(defcheck solution-b00f943
  (fn [graph]
    (letfn [(setify [coll] (set (map set coll)))
            (adjacent? [edge1 edge2]
              (not (empty? (clojure.set/intersection edge1 edge2))))
            (neighbors [edges graph]
              (set (apply concat
                     (for [e edges]
                       (filter #(adjacent? e %) graph)))))
            (step [from graph]
              (let [to (neighbors from graph)]
                {   :edges to
                 :subgraph (clojure.set/difference graph to)}))
            (rand-bf-walk [graph]
              (loop [edges (->> graph seq rand-nth set (conj #{}))
                     graph (-> graph setify (clojure.set/difference edges))
                     walk [edges]]
                (let [next-step (step edges graph)
                      edges (:edges next-step)]
                  (if (or (empty? edges) (empty? graph))
                    walk
                    (recur edges (:subgraph next-step) (conj walk edges))))))]
      (= (->> graph rand-bf-walk (map seq) flatten count)
        (count graph)))))

(defcheck solution-b065157b
  (fn gconn [x]
    (let [trans (fn [coll]
                  (loop [c coll acc (count coll)]
                    (let [nc (for [x c y c
                                   :when (and ((complement =) x y)
                                              (= (last x) (first y)))] [(first x) (last y)])
                          nnc (clojure.set/union (set nc) c)]
                      (cond
                        (= (count nnc) acc) nnc
                        :else (recur nnc (count nnc)))))
                  )
          n (count (set (mapcat identity x)))
          g (set (mapcat (fn [[x y]] [[x y] [y x]]) x))
          g' (trans g)
          ]
      (= (* n n) (count g')))))

(defcheck solution-b1bf8cc8
  (fn __ [someset]
    (= ((fn[q](set(apply concat (map #(map (partial vector %)  q) q)))) (distinct (flatten(concat  someset))))
      (loop [ a (reduce conj someset (map reverse someset))](let[newset (reduce
                                                                          (fn[x y](reduce conj x (filter #(not (nil? %)) (map #(if (= (first %) (last y)) (vector (first y) (last %) )) x))))
                                                                          a
                                                                          a)](if (= a newset) a (recur newset)))))))

(defcheck solution-b1ca80b0
  (letfn [(connected? [nodes e]
            (some (into #{} e) nodes))]
    (fn [coll]
      (loop [nodes (into #{} (first coll))
             edges (rest coll)]
        (if (empty? edges)
          true
          (let [new-edges (filter (partial connected? nodes) edges)]
            (if (empty? new-edges)
              false
              (recur (clojure.set/union nodes (into #{} (flatten new-edges)))
                (remove (partial connected? nodes) edges)))))))))

(defcheck solution-b1cc0541
  (fn [graph]
    (loop [visitied (set (first graph)) edges (rest graph)]
      (if (= (count edges) 0) true
                              (let [grouped (group-by #(and (nil? (visitied (first %)))
                                                            (nil? (visitied (last  %)))) edges)]
                                (if-let [near (grouped false)]
                                  (recur (into visitied (flatten near)) (grouped true))
                                  false))))))

(defcheck solution-b1f21b06
  (fn connected? [edges]
    (let [bidirectional (concat edges (map reverse edges))
          adjacencies (into {} (for [[k v] (group-by first bidirectional)] [k (map second v)]))]
      (letfn [(traverse [visited]
                (if (= (count adjacencies) (count visited)) true
                                                            (let [adjacent (into #{} (adjacencies (last visited)))
                                                                  visited-set (into #{} visited)
                                                                  to-traverse (clojure.set/difference adjacent visited-set)]
                                                              (or (some true? (map #(traverse (conj visited %)) to-traverse)) false))))]
        (or (some true? (map #(traverse (vector %)) (keys adjacencies))) false)))


    ))

(defcheck solution-b2a3fd2c
  (fn [e]
    (loop [[x & v] (first e) c #{} e e]
      (cond
        x (let [c (conj c x)
                {conn x, disconn nil} (group-by #(some #{x} %) e)
                n (remove c (flatten conn))]
            (recur (concat v n) c disconn))
        (seq e) false
        :else true))))

(defcheck solution-b309f6a5
  (letfn [(build-graph [tuples]
            (loop [tuples (seq tuples) acc {}]
              (if (seq tuples)
                (recur (rest tuples) (bi-connect acc (first tuples))  )
                acc)))
          (connect [graph a b]
            (update-in graph [a] #((fnil conj []) %1 b)))
          (bi-connect [graph [a b]]
            (connect (connect graph a b) b a))
          (visit
            [graph fringe visited]
            (let [extend-fringe (fn []
                                  (reduce #(if (visited %2) %1 (conj %1 %2))
                                    (pop fringe) (graph (peek fringe))))
                  ]
              (if (seq fringe )
                (recur graph (extend-fringe) (conj visited (peek fringe)))
                (count visited))
              ))

          (connected
            [tuples]
            (let [graph (build-graph tuples)
                  visited (visit graph (conj #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) (first (first tuples))) #{})]
              (= visited (count (keys graph))))
            )
          ]
    connected))

(defcheck solution-b34c8672
  (fn foo [edges] (let [find-node-for-edge (fn [e n] (not (empty? (clojure.set/intersection n (set e)))))]
                    (loop [e edges f [] nodes #{}]
                      (cond (empty? e) (empty? f)
                            (empty? nodes) (recur (rest e) f (into nodes (first e)))
                            true
                            (if (find-node-for-edge (first e) nodes)
                              (recur (concat (rest e) f) [] (into nodes (first e)))
                              (recur (rest e) (conj f (first e)) nodes)))))))

(defcheck solution-b42c3cf4
  (fn graph-connect [nodes]
    (letfn [(numnodes [nodes] (count (set (reduce concat nodes))))
            (to-node-map [nodes]
              ((fn !  [nodes node-map]
                 (if-let [n (seq nodes)]
                   (let [[key val] (first n)]
                     (if (contains? node-map key)
                       (! (rest n) (assoc node-map  key (conj (node-map key) val)))
                       (! (rest n) (assoc node-map key [val]))
                       )
                     )
                   node-map)
                 ) (concat nodes (map #(reverse %) nodes)) {}))
            (search [node node-map visited]
              (if (or (contains? visited  node) (empty? (node-map node)))
                visited
                (set (mapcat #(search % node-map (conj visited node)) (node-map node)))
                )
              )]
      (let [N (numnodes nodes)
            node-map (to-node-map nodes)
            searched-nodes (search (ffirst nodes) node-map #{})]
        (= N (count searched-nodes))))))

(defcheck solution-b5265254
  (fn [edges]
    (letfn [
            (connections [edges]
              (reduce (fn [C e]
                        (let [a (first e), b (second e)]
                          (if (= a b)
                            C ;ignore self-reference
                            (merge-with clojure.set/union C
                              {a #{b}, b #{a}}))))
                {} edges))

            (connected? [C]
              (if (empty? C) true
                             ((fn [U I D]
                                (loop [kwn (-> C first key list set),
                                       unk (-> C rest keys set)]
                                  (if (empty? unk)
                                    true
                                    (let [bnd (apply U (map C kwn))]
                                      (if (empty? (I bnd unk))
                                        false
                                        (recur (U kwn bnd)
                                          (D unk bnd)))))))
                              clojure.set/union
                              clojure.set/intersection
                              clojure.set/difference)))]

      (connected? (connections edges)))))

(defcheck solution-b527ac6d
  (fn [ls]
    (letfn [(c [x cs] (first (filter #(% x) cs)))]
      (->
        (reduce
          (fn [cs [a b]]
            (let [ac (c a cs) bc (c b cs)]
              (conj
                (disj cs ac bc)
                (set (concat ac bc [a b])))))
          #{} ls)
        count (= 1)))))

(defcheck solution-b55f6e8f
  (fn __
    [graph]
    (letfn [(dfs
              [g s]
              (loop [vertices [] explored #{s} frontier [s]]
                (if (empty? frontier)
                  vertices
                  (let [v (peek frontier)
                        neighbors (g v)]
                    (recur
                      (conj vertices v)
                      (into explored neighbors)
                      (into (pop frontier) (remove explored neighbors)))))))]

      (let [g (apply merge (map (fn [[k v]]
                                  {k (map last v)})
                             (group-by #(first %) graph)))
            all-pathes (map #(dfs g %) (keys g))
            vs (into #{} (flatten (into [] graph)))]

        (if (some #(= (count %) (count vs)) all-pathes)
          true
          false)
        ))))

(defcheck solution-b57bf3fd
  (fn connected? [graph]
    (= (-> graph seq flatten set)
      (nth (iterate
             #(set
                (flatten
                  (for [edge graph]
                    (if (or (-> edge second %)
                            (-> edge first %))
                      edge))))
             #{(-> graph first first)})
        (count graph)))))

(defcheck solution-b5a2ba65
  (fn [graph]
    (= 1
      (count
        (reduce (fn [components [a b]]
                  (let [connected-to-a (set (filter #(% a) components))
                        connected-to-b (set (filter #(% b) components))
                        components (clojure.set/difference components connected-to-a)
                        components (clojure.set/difference components connected-to-b)
                        new-component (set (concat [a b] (apply concat connected-to-a) (apply concat connected-to-b)))]
                    (conj components new-component))) #{} graph)))))

(defcheck solution-b71353b0
  (fn[g]
    (let
     [to-set (fn[s] (into #{} (mapcat identity s)))
      sz (count (to-set g))
      f (fn[s]
          (let [t (to-set (filter #(some s %) g)) z (count t)]
            (cond (= z sz) true
                  (= z (count s)) false
                  :else (recur t))))]
      (f #{(ffirst g)}))))

(defcheck solution-b872e42f
  (fn [g]
    (let [vs (set (mapcat identity g))
          es (set (mapcat (juxt identity reverse) g))
          newvs (fn [v] (map second (filter #(= v (first %)) es)))
          subset? (fn [s1 s2] (every? s1 s2))]
      (loop [curr #{(first vs)}
             news (mapcat newvs curr)]
        (if (subset? curr news) (= curr vs)
                                (recur (reduce conj curr news) (mapcat newvs news)))))))

(defcheck solution-b8f544b4
  (fn [g](let [looped (group-by #(= (first %)(second %)) g)
               loops  (looped true)
               edges  (map #(sort %) (looped false))
               points (partial reduce into #{})
               all    (count (points g))
               check  #(= (- all 1) (count (distinct (map % edges))))]
           (if (= 1 all) true
                         (and (every? (points edges) (points loops)) ;; points with loops connected with others
                              (or (check first)
                                  (check second))
                              )))
    ))

(defcheck solution-b93f78e2
  (fn [g]
    (loop [c #{(first g)} uc (set (rest g))]
      (if (empty? uc)
        true
        (let [cf (set (apply concat c))
              e (first (filter (fn [[v1 v2]] (or (contains? cf v1) (contains? cf v2))) uc))
              nuc (disj uc e)]
          (if e
            (recur (conj c e) nuc)
            false
            )
          )
        )
      )
    ))

(defcheck solution-b9d39fb9
  (fn [edges]
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
                            g1))))))))

(defcheck solution-b9f7cc6e
  (fn [edges]
    (letfn [
            (nodes [edges] (into #{} (mapcat identity edges)))
            (add-neighbor [nbrs [u v]] (assoc nbrs u (conj (get nbrs u) v)))
            (graph [edges]
              (let [nn (nodes edges)
                    nbrs (reduce add-neighbor (zipmap nn (repeat #{})) edges)
                    nbrs (reduce add-neighbor nbrs (for [[u v] edges] [v u]))]
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
                (walk #{start} (next-nodes graph start))))]
      (let [g (graph edges) nn (:nodes g) start (first nn)]
        (= nn (set (cons start (depth-first g start))))))))

(defcheck solution-b9f8b0c4
  (fn [se]
    (let [t (count (set (reduce concat se)))]
      (or
       (= 1 (count se))
       (loop [c 1 i (set (first se))]
         (if (>= c t)
           (= t (count i))
           (recur (inc c) (reduce #(if (some (partial contains? i) %2) (apply conj % %2) %) i se)) ))))))

(defcheck solution-ba01d3d5
  (fn sol [xset]

    (letfn[

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

           (cfunc [cset tset [a seta]]
             (let [nset (clojure.set/union seta (tset a))]
               [a nset]
               )
             )

           (tfunc [cset tset [a seta]]
             (let [nset (clojure.set/difference
                          (reduce clojure.set/union
                            (for [x seta]  (cset x))
                            )
                          (cset a)
                          )
                   ]
               [a nset]
               )

             )

           (propset [mset]
             (loop [
                    cset (into {} (for [x mset]
                                    [(first x) (set (second x))])
                           )
                    tset cset
                    ]
               (if  (every? true? (map #(empty? (second %)) tset) )
                 cset
                 (recur (into {} (map #(cfunc cset tset %) cset))
                   (into {} (map #(tfunc cset tset %) tset))
                   )
                 )

               )

             )


           ]

      (let [len (count xset)]
        (every? true? (map #(>= (count (second %)) (dec len)) (propset (initset {} xset))) )
        )
      )
    ))

(defcheck solution-bb30f1ce
  (fn graph-conn [edges]
    (let [nodes (reduce (fn [acc [n1 n2]] (conj acc n1 n2)) #{} edges)
          conn (fn [n1 n2] (or (edges [n1 n2]) (edges [n2 n1])))]
      (loop [queue [(first nodes)] visited #{}]
        (if (empty? queue) (= visited nodes)
                           (let [curr (first queue)
                                 to-add (filter #(and (not (visited %)) (conn curr %)) nodes)
                                 newq (reduce conj (rest queue) to-add)
                                 newvisit (conj visited curr)] (recur newq newvisit)))))))

(defcheck solution-bb642bb3
  (fn [s]
    (= 1 (count
           (reduce
             (fn [graphs [a b]]
               (reduce (fn [[r & rest] v]
                         (if (or (v a) (v b))
                           (cons (clojure.set/union v r) rest)
                           (cons r (cons v rest))))
                 [(set [a b])]
                 graphs))
             #{}
             s)))))

(defcheck solution-bce25aff
  (fn f
    ([s] (f #{} s))
    ([s xs]
     (if (seq xs)
       (if (empty? s)
         (f (into s (first xs)) (set (next xs)))
         (if-let [e (first (filter #(some s %) xs))]
           (f (into s e) (disj xs e))
           false))
       (not (empty? s))
       ))))

(defcheck solution-bd44860f
  (fn [z] (if (= 1 (count z)) true
                              (let [l1 (into '() z) l (concat l1 (map (fn [[p q]] [q p]) l1))]
                                (letfn [(remove-from-list [l-orginal to-be-removed] (filter #(not (some #{%} to-be-removed)) l-orginal))
                                        (link [element mid graph] (let [[vertex node] element result (keep #(if (or (= (% 1) vertex) (= (% 0) node)) % nil) graph)]
                                                                    (let [mid-step (concat mid result) f1 (first mid-step) r (rest mid-step)]
                                                                      (if (seq mid-step) (recur f1 r (remove-from-list graph (list element)))
                                                                                         (empty? graph)))))]
                                  (link (first l) '() l))))))

(defcheck solution-be2e9b18
  #(->> %
     (sort-by (fn [[a b]] (if (< 0 (compare a b)) a b)))
     (reduce (fn [a b]
               (if (some (set b) a)
                 (concat a b)
                 [])))
     (empty?)
     (not)))

(defcheck solution-be9b5f9d
  (fn [coll]
    (let [c (count coll)]
      (or
       (= c 7)
       (= c 1)))))

(defcheck solution-beb993
  (fn [s]
    (letfn [(merge [u v]
              (if (empty? (clojure.set/intersection u (set v)))
                u
                (clojure.set/union u (set v))))]
      (loop [acc (set (first s)) edges (rest s)]
        (let [newacc (set (reduce merge acc edges))
              newedges (remove #(clojure.set/subset? (set %) newacc) edges)]
          (if (= newacc acc)
            (empty? edges)
            (recur newacc newedges)))))))

(defcheck solution-c0386327
  (fn connected? [edges]
    (letfn [(tuple-to-graph [tuples]
              (reduce #(assoc %1 (first %2) (conj (get %1 (first %2) []) (second %2)) (second %2) (conj (get %1 (second %2) []) (first %2))) {} tuples))
            (unreachable?
              ([graph visited node-key]
               (if (contains? visited node-key)
                 visited
                 (set (apply concat (map #(unreachable? graph (conj visited node-key) %) (get graph node-key))))))
              ([graph]
               (not= (count (unreachable? graph #{} (first (keys graph)))) (count (keys graph)))))]
      (not (unreachable? (tuple-to-graph edges))))))

(defcheck solution-c0ab81bb
  (fn connected? [s]
    (if (= 1 (count s ))
      true
      (let [input (map set s)
            a (first input)
            bs (rest input)
            connectededges ((fn find-connected [edge more]
                              (cons edge
                                (mapcat #(if (or (contains? % (first edge)) (contains? % (second edge)))
                                           (find-connected %
                                             (filter (partial not= %) more))
                                           ())
                                  more))) a bs)]
        (= (set connectededges) (set input))
        ))))

(defcheck solution-c19df5d2
  (fn isConnected? [edges]
    (let [graph (reduce (fn [graph [a b]] (assoc graph a (conj (get graph a #{}) b))) {} (concat edges (map reverse edges)))
          nodes (set (keys graph))
          bfs (fn [node]
                (loop [queue [node] visited #{}]
                  (if-let [n (first queue)]
                    (recur (into (subvec queue 1) (clojure.set/difference (graph n) visited)) (conj visited n))
                    visited)))]

      (= nodes (bfs (first nodes))))))

(defcheck solution-c1e805db
  (fn [g]
    (= (set (apply concat g))
      ((fn clos
         ([g] (clos (set (first g)) (set (rest g))))
         ([stock graph]
          (let [ nodes
                (group-by
                  #(not (empty? (clojure.set/intersection stock (set %1))))
                  graph)]
            (if (empty? (get nodes true))
              stock
              (clos
                (into stock (apply concat (get nodes true)))
                (get nodes false)))))) g))))

(defcheck solution-c225c6bb
  (fn problema91 [input]
    (let [graph (set (map set input))
          pivot (first graph)
          outros-elementos (disj graph pivot)
          elementos-intersecao (filter #(seq (clojure.set/intersection pivot %)) outros-elementos)
          elementos-nao-intersecao (filter (complement #(seq (clojure.set/intersection pivot %))) outros-elementos)]
      (cond
        (empty? elementos-nao-intersecao) true
        (empty? elementos-intersecao) false
        :else (problema91 (apply hash-set (apply clojure.set/union pivot elementos-intersecao) elementos-nao-intersecao))))))

(defcheck solution-c326b0d3
  (fn [graph]
    (let [updated (fn [dict key val]
                    (conj (dict key []) val))
          add-edge (fn [adj [from to]]
                     (into adj [[from (updated adj from to)]
                                [to (updated adj to from)]]))
          adj (reduce add-edge {} graph)
          start (first (first graph))
          desired (count (keys adj))
          not-in #(not (%1 %2))
          neighbors (fn [v vis]
                      (filter (partial not-in vis) (adj v)))]
      (loop [[x & _ :as all] (list start) vis #{}]
        (cond
          (empty? all) (= (count vis) desired)
          :else (recur
                  (concat (rest all) (neighbors x vis))
                  (conj vis x)))))))

(defcheck solution-c638b9cd
  (fn [m s a]
    (let [c #(remove nil?
               (m (fn [[i j]] (if (= % i) j
                                          (if (= % j) i)))
                 (seq %2)))
          n #(s (flatten (seq %)))]
      (loop [r [(first (n a))]]
        (if (= (n a) (s r))
          true
          (if (= (n (m #(c % a) r)) (s r))
            false
            (recur (into r (n (m #(c % a) r))))))))) map set)

(defcheck solution-c76fe16f
  (fn [edges]
    (let [find (fn [union k] (or (some #(if (contains? % k) %) union) #{k}))]
      (= 1 (count
             (reduce (fn [r [a b]]
                       (let [ua (find r a)
                             ub (find r b)]
                         (-> r
                           (disj ua ub)
                           (conj (clojure.set/union ua ub)))))
               #{} edges))))))

(defcheck solution-c7aeb620
  (letfn
   [( closure [xys]
      (into
        xys
        (for [[x1 y1] xys
              [x2 y2] xys
              :when (= y1 x2)]
          [x1 y2])))

    ( tc [xys]
      (loop [count-so-far (count xys)
             step (closure xys)]
        (if (= count-so-far (count step))
          step
          (recur (count step) (closure step)))))

    ( vertices [graph]
      (distinct (flatten (vec graph))))

    ( normalize [graph]
      (set (apply concat (for [[x y] graph] [[x y] [y x]]))))]

    (fn [graph]
      (let [count-full (->> graph normalize tc count)
            count-vertices (count (vertices graph))]
        (= count-full (* count-vertices count-vertices))))))

(defcheck solution-c7f49627
  (fn [graph]
    (letfn [(walk [node edge]
              (cond
                (= node (first edge)) (second edge)
                (= node (second edge)) (first edge)
                :else nil))]
      (let [nodes (set (flatten (vec graph)))]
        (loop [walked (for [n nodes e graph :when (not (nil? (walk n e)))] (conj #{} n (walk n e)))]
          (let [nextwalked (for [n nodes e graph w walked :when (and (w n) (not (w (walk n e))) (not (nil? (walk n e))))]
                             (conj w n (walk n e)))]
            (cond
              (some #(= nodes %) walked) true
              (empty? nextwalked) false
              :else (recur nextwalked))))))))

(defcheck solution-c8f8652d
  (letfn [(intersects? [s1 s2]
            (seq (clojure.set/intersection s1 s2)))
          (add-edge [components edge]
            (if-not (some #(intersects? % edge) components)
              (conj components edge)
              (for [c components]
                (if (intersects? c edge)
                  (clojure.set/union c edge)
                  c))))
          (connected? [components]
            (let [pairs (partition 2 1 components)]
              (every? #(apply intersects? %) pairs)))]
    (fn [edges]
      (->> (map set edges)
        (reduce add-edge [])
        connected?))))

(defcheck solution-c91e5e2d
  (fn [se]
    (let [s (seq se)]
      (loop [nodes (into #{} (first s))]
        (let [next (into nodes (apply concat (filter (fn [[v1 v2]] (or (get nodes v1) (get nodes v2))) (rest s))))]
          (if (= next nodes)
            (= next (reduce (fn [s v] (into s v)) #{} s))
            (recur next)))))))

(defcheck solution-c932fc45
  (fn [ls]
    (loop [s (set (first ls)) r (rest ls)]
      (or (empty? r)
          (let [{linked true unlinked nil} (group-by #(some (comp boolean s) %) r)]
            (if (seq linked)
              (recur (reduce into s linked) unlinked)
              false))))))

(defcheck solution-ca0993b7
  (fn [ss]
    (= (count (reduce
                (fn [ss s]
                  (loop [ss ss s (set s) acc #{}]
                    (if (seq ss)
                      (if (empty? (clojure.set/intersection (first ss) s))
                        (recur (rest ss) s (conj acc (first ss)))
                        (recur (rest ss) (clojure.set/union (first ss) s) acc))
                      (conj acc s))))
                #{} ss)) 1)))

(defcheck solution-cad491a8
  (fn connected? [edges]
    (loop [remaining   edges
           clusters    []]
      (if (empty? remaining)
        (= 1 (count clusters))
        (let [connection (apply hash-set (distinct (first remaining)))
              merged (apply clojure.set/union (conj (filter #(some connection %) clusters) connection))
              others (remove #(some connection %) clusters)]
          (recur (rest remaining)
            (conj others merged)))))))

(defcheck solution-cbdba720
  #(let [link (mapcat (fn [[from to]] [[from to] [to from]]) %)
         node (distinct (flatten link))
         graph (group-by first link)]
     (loop [queue [(first node)] visit #{}]
       (if (empty? queue)
         (= (count node) (count visit))
         (let [postition (first queue)]
           (recur
             (concat
               (rest queue)
               (remove visit (map second (graph postition))))
             (conj visit postition)))))))

(defcheck solution-ccab6552
  #(let [numOfNodes (count (set (apply concat %)))
         undirectGraph (fn [g]
                         (apply clojure.set/union (for[[a b] g]
                                                    (set (list [a b] [b a])))))
         growGraph (fn [g]
                     (let [ng (into g (set (concat (for[ [a b] g [c d] g :when (= b c)]
                                                     [a d]))))]
                       (if (= g ng) ng (recur ng))))]
     (= (* numOfNodes numOfNodes) (count (growGraph (undirectGraph %))))))

(defcheck solution-ccc1f949
  (fn graph-conn? [edgs]
    (let [flip-edges (fn [xs] (map (fn [[a b]] [b a]) xs))
          nodes-build (fn [ys]
                        (reduce (fn [m [k v]] (assoc m k (conj (get m k #{}) v)))
                          {} ys))
          edges (seq edgs)
          ed-dups-sorted (sort (concat edges (flip-edges edges)))
          m-nodes (nodes-build ed-dups-sorted)
          full-node-set (set (keys m-nodes))]
      (letfn [(group-for [n]
                (loop [acc #{n} nxt (get m-nodes n)]
                  (if (empty? nxt)
                    acc
                    (let [neu-nxt
                          (clojure.set/difference
                            (apply clojure.set/union (map #(get m-nodes %) nxt))
                            acc)]
                      (recur (into acc nxt) neu-nxt)))))]

        (let [group-one (group-for (first full-node-set))
              extra-nodes (clojure.set/difference full-node-set group-one)]
          (empty? extra-nodes) )))))

(defcheck solution-ccc80f25
  (fn [g] (let [v (reduce #(into % %2) #{} g)
                vs (set (map (comp set list) v))
                connect (fn [vs [e1 e2]]
                          (let [es (filter #(or (% e1) (% e2)) vs)]
                            (cond (= 1 (count es)) vs
                                  :else (conj (apply disj vs es)
                                          (into (first es) (second es))))))]
            (= 1 (count (reduce connect vs g))))))

(defcheck solution-ccee643f
  (fn [graph]
    (let [graph (->> graph (map set) set)
          adjoining? (fn [e1 e2]
                       (not (empty? (clojure.set/intersection e1 e2))))
          adjoining-at-least-one? (fn [edge-collection edge]
                                    (boolean
                                      (some #(adjoining? edge %) edge-collection)))
          boolean->keyword {true :adjoining false :not-adjoining}
          group-by-adjoining (fn [edges candidate-edges]
                               (group-by
                                 (fn [candidate-edge]
                                   (boolean->keyword
                                     (adjoining-at-least-one? edges candidate-edge)))
                                 candidate-edges))
          first-edge (first graph)]
      (loop [visiting-edges #{first-edge}
             remaining-edges (disj graph first-edge)]
        (cond (empty? remaining-edges) true
              (empty? visiting-edges) false
              :else (let [grouped-edges (group-by-adjoining visiting-edges remaining-edges)]
                      (recur (:adjoining grouped-edges)
                        (:not-adjoining grouped-edges))))))))

(defcheck solution-cd10675b
  (letfn [(union [& xts] (reduce into #{} xts))
          (select [f? xt]
            (reduce #(if (f? %2) %1 (disj %1 %2)) xt xt))
          (isect [xt yt]
            (if (> (count xt) (count yt))
              (recur yt xt)
              (select (partial contains? yt) xt)))
          (isects? [xt yt] ((comp full? isect) xt yt))
          (popset [xt] (when-first [x xt] [x (disj xt x)]))
          (full? [x] ((comp not empty?) x))]
    ;
    (fn [gr]
      (and (full? gr))
      (loop [[path & paths] (map set gr)]
        (or (empty? paths)
            (let [pred (partial isects? path)
                  [nodes paths] ((juxt filter remove) pred paths)]
              (and (full? nodes)
                   (recur (cons (reduce union path nodes) paths)))))))))

(defcheck solution-cd544a06
  (fn [x]
    (loop [a (rest x) b (into #{} (first x)) f #{}]
      (if (and (or (empty? a) (nil? a)) (empty? f))
        true
        (if (or (empty? a) (nil? a))
          false
          (if (or (contains? b (first (first a))) (contains? b (last (first a))))
            (recur (concat f (rest a)) (into #{} (conj b (first (first a)) (last (first a)))) #{})
            (recur (rest a) b (conj f (first a)) )))))))

(defcheck solution-cdcfdf8a
  (fn __ [G]
    (let [G' (set (mapcat (fn [[p q]] (list [p q] [q p])) G))
          elems (set (mapcat (fn [[p q]] (list p q)) G))
          dfs (fn dfs [visited node]
                (if (contains? visited node)
                  visited
                  (->> G'
                    (filter #(= node (first %)))
                    (map second)
                    (mapcat #(dfs (conj visited node) %))
                    (concat visited)
                    set)))]
      (= elems (dfs #{} (first elems))))))

(defcheck solution-cdd798e6
  (fn fully-connected? [graph]
    (let [nodes (set (apply concat graph))
          full-graph (set (mapcat (fn [[a b :as n]] [n [b a]]) graph))
          children (into {} (for [[k v] (group-by first full-graph)] [k (set (map second v))]))
          connections (fn [node]
                        (->> (iterate #(into % (mapcat children %)) #{node})
                          (partition 2 1)
                          (drop-while #(apply not= %))
                          first first))]
      (every? #(= % nodes) (map connections nodes)))))

(defcheck solution-ce3e0592
  (comp
    (fn [[f & r]]
      (if (not r)
        true
        (if-let [n (first (filter #(some f %) r))]
          (recur (-> (set r) (disj n f) (conj (into f n)) seq))
          false)))
    #(map set %)))

(defcheck solution-cea5ddbe
  (fn connect? [g]
    (letfn [(trans? [v s]
              (or
               (not (empty? (take 1 (filter #(= % v) s))))
               (not (empty? (take 1 (filter #(and
                                              (= (first %) (first v))
                                              (trans? (vector (second %) (second v)) (disj s %))
                                              ) s))))))
            (final [st]
              (letfn [(trans? [v s]
                        (or
                         (not (empty? (take 1 (filter #(= % v) s))))
                         (not (empty? (take 1 (filter #(and
                                                        (= (first %) (first v))
                                                        (trans? (vector (second %) (second v)) (disj s %))
                                                        ) s))))))]
                (let [plain (reduce into #{} st)]
                  (into #{} (filter #(trans? % st)(mapcat #(map (fn[item] (vector % item)) plain) plain))))))]
      (let [graph (reduce conj g (into #{} (map reverse g)))
            nodes (into [] (into #{} (mapcat identity g)))
            transitive-closure (final graph)]
        (not (some nil? (for [x (range 0 (dec (count nodes)))
                              y (range (inc x) (count nodes))]
                          (if (transitive-closure [(nodes x) (nodes y)])
                            [(nodes x) (nodes y)]
                            nil))))))))

(defcheck solution-cec1e71e
  (fn [edge-set]
    (let [all-nodes (reduce (fn [acc pair] (-> acc
                                             (conj (first pair))
                                             (conj (second pair))))
                      #{} edge-set)
          reachable-from-node (fn [node]
                                (reduce (fn [acc pair] (cond
                                                         (= node (first pair)) (conj acc (second pair))
                                                         (= node (second pair)) (conj acc (first pair))
                                                         :else acc))
                                  #{} edge-set))
          reachable-from-set (fn [nodes]
                               (reduce clojure.set/union nodes (map reachable-from-node nodes)))

          ]
      (loop [reached #{(first all-nodes)}]
        (let [reachable (reachable-from-set reached)]
          (cond
            (= all-nodes reachable) true
            (= reached reachable) false
            :else (recur reachable)))))))

(defcheck solution-cfa10b5b
  (fn connected? [edges]
    (let [adj-list (apply merge-with
                     concat
                     (mapcat (fn [[u v]]
                               [{u [v]} {v [u]}])
                       edges))

          reachable-nodes (loop [queue [(ffirst adj-list)]
                                 seen #{(ffirst adj-list)}]
                            (if (seq queue)
                              (recur (concat (rest queue)
                                       (remove seen
                                         (adj-list (first queue))))
                                (into seen
                                  (adj-list (first queue))))
                              seen))]
      (= (into #{} (keys adj-list))
        reachable-nodes))))

(defcheck solution-cfa49f22
  (fn q91 [coll]
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
              ([m] (mapcat #(paths m [] %) (keys m)))) ]

      (let [ts (transits coll)
            num-nodes (count ts)]
        (if (some #(= num-nodes (count %)) (paths ts)) true false)))))

(defcheck solution-cffdbb66
  (fn [edges]
    (let [edge-map
                   (apply
                     merge-with
                     clojure.set/union
                     (for [[a b] edges]
                       (if (= a b)
                         {a #{b}}
                         {a #{b}, b #{a}})))
          findable (loop [queue [(first (first edge-map))]
                          seen #{(first queue)}]
                     (if-not (seq queue)
                       seen
                       (let [[top & rest-of-queue] queue
                             unseen (clojure.set/difference (edge-map top) seen)]
                         (recur (into rest-of-queue unseen) (into seen unseen)))))]
      (= (count findable) (count edge-map)))))

(defcheck solution-d1342c70
  (fn [graph]
    (let [V (set (apply concat graph))
          E (reduce (fn [m [a b]] (-> m (update-in [a] conj b) (update-in [b] conj a)))
              (zipmap V (repeat #{})) graph)]
      (loop [queue (conj #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) (first V))
             visited #{(first V)}]
        (if (empty? queue)
          (= visited V)
          (let [new-adj (->> queue peek E (remove visited))]
            (recur (-> queue pop (into new-adj))
              (into visited new-adj))))))))

(defcheck solution-d28de46
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
                    (let [node (first nodes) nodes (disj nodes node)]
                      (cond (nil? node) components
                            (components node) (recur nodes components)
                            :else (recur (clojure.set/union nodes (graph node)) (conj components [node index])))))))

          nodes (into #{} (flatten (seq xs)))
          graph (collect-edges xs (into {} (map #(vector % #{}) nodes)))]
      ;;(println graph)
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
        (= 1)))))

(defcheck solution-d2a9a150
  (fn [s]
    (letfn [(check-all [o s]
              (let [r (group-by #(or (= (first %) (second o)) (= (second %) (first o)) (= (first %) (first o))) s)]
                [(cons o (get r true)) (get r false)]))]
      (let [[a b] (check-all (first s) (rest s))]
        (loop [a a b b]
          (cond (empty? b) true
                (empty? a) false
                :else (let [[a1 b1] (check-all (first a) b)]
                        (recur (concat (rest a) (rest a1)) b1))))))))

(defcheck solution-d3e68016
  (fn [g]
    (let [start (ffirst g)
          num-nodes (count (into #{} (flatten (seq g))))
          edges (into #{} (clojure.set/union g (set (map reverse g))))
          orbit (fn orbit [n e already-seen]
                  (let [out-edges (filter #(= n (first %)) e)
                        successors (clojure.set/difference (into #{} (map last out-edges)) already-seen)]
                    (if (empty? successors)
                      already-seen
                      (let [other-edges (clojure.set/difference e (set out-edges))]
                        (apply clojure.set/union (map #(orbit % other-edges (into already-seen[%])) successors))))))
          component (orbit start edges #{start})]
      (= num-nodes (count component)))))

(defcheck solution-d3f32276
  (fn connected [edges]
    (let [vertices (set (apply concat edges))
          connected-vertices (fn connected-vertices
                               ([vertex] (connected-vertices vertex #{}))
                               ([vertex cache]
                                (let [adjacent-vertices (->> (apply concat (filter #(contains? (set %) vertex) edges))
                                                          (filter #(not (contains? cache %)))
                                                          )]
                                  (if (empty? adjacent-vertices)
                                    cache
                                    (set (mapcat #(connected-vertices % (set (conj cache %))) adjacent-vertices))
                                    )
                                  )
                                )
                               )
          ]
      (every? #(= vertices (connected-vertices %)) vertices)
      )
    ))

(defcheck solution-d4615499
  (fn graph-connected? [edges]
    (let [
          vertices-of (fn [edges]
                        (reduce #(apply conj %1 %2) #{} edges))
          vertices (vertices-of edges)
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
      (impl #{} #{arbitrary-vertex} (disj vertices arbitrary-vertex)))))

(defcheck solution-d4859fe8
  (fn [edge]
    (let [node (distinct (flatten (vec edge)))]
      (loop [conn-node (hash-set (first node))
             rset (rest node)]
        (if (empty? rset)
          true
          (let [redge (filter #(or (contains? conn-node (first %)) (contains? conn-node (second %))) edge)
                rnode (distinct (flatten (vec redge)))
                cset  (clojure.set/union (set rnode) conn-node)]
            (do
              #_(println conn-node rset rnode cset)
              (cond
                (=  cset conn-node)
                false
                :else (recur cset  (filter #(not (contains? cset %)) rset))
                )
              )
            ))
        ))
    ))

(defcheck solution-d4ce2ea9
  (fn f [g]
    (let [expand-graph (fn expand-graph [g]
                         (let [rev-g (map #(apply hash-map [(second %) [(first %)]]) g)
                               gm (map #(apply hash-map [(first %) [(second %)]]) g)]
                           (reduce (fn [col x]
                                     (merge-with concat col x))
                             (concat rev-g gm))))
          traverse     (fn traverse [visited n eg]
                         (let [all-children (eg n)
                               not-visited  (filter #(not (visited %)) all-children)
                               visited'     (conj visited n)
                               #_#__ (println n visited all-children not-visited)]
                           (if (not-empty not-visited)
                             (mapcat #(traverse visited' % eg) not-visited)
                             (do #_(println "Returning" visited')
                                 visited'))))
          eg           (expand-graph g)
          n            (first (first eg))
          all-nodes    (set (mapcat concat g))
          visited      (set (traverse #{} n eg))]
      #_(println eg all-nodes visited)
      (= all-nodes visited))))

(defcheck solution-d544dbe4
  (fn connected? [g]
    (let [nodes (set (mapcat flatten g))]
      (loop [[h & more :as rm] [(first nodes)] seen #{}]
        (if (= seen nodes)
          true
          (if (seq rm)
            (let [neighbors
                  (->> g (map (fn [[e1 e2]] (if (= e1 h) e2 (if (= e2 h) e1 nil))))
                    set (remove #(or (nil? %) (seen %))) vec)]
              (recur (concat more neighbors) (conj seen h)))
            false))))))

(defcheck solution-d669174c
  (fn [g]
    (loop [q (map set g)]
      (let [c (count q)
            z (reduce (fn [[h & t :as a] y]
                        (if (empty? (clojure.set/intersection h y))
                          (conj a y)
                          (into [(clojure.set/union h y)] t)))
                [(first q)] (-> q rest vec))]
        (if (= c (count z)) (= 1 c)
                            (recur z))))))

(defcheck solution-d729354c
  (fn [edges]
    (let [
          maps (map #(list (hash-map (first %) (rest %))
                       (hash-map (last %) (butlast %))) edges)
          all (apply merge-with concat (flatten maps))
          connected? (fn cn? [s e path all]
                       (cond
                         (some #{s} path) false
                         (= s e) true
                         :else (some true?
                                 (map #(cn? % e (conj path s) all)
                                   (all s)))))]
      (->>
        (for [s (keys all) e (keys all) :when (not= s e)]
          (connected? s e [] all))
        (#(conj % true))
        (every? true?)))))

(defcheck solution-d77b60a6
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
                                              (map (fn [x] (set (non-visited-nodes e x (remove #(= x %) nn)))) next-n))))))]

    (fn [e]
      (let [nn (set (apply concat e))]
        (empty? (non-visited-nodes e (first nn) (rest nn)))))))

(defcheck solution-d7df6762
  (fn connectivity
    [g]
    (let [all-nodes (set (apply concat g))
          graph (reduce (fn [m [a b]]
                          (update-in m [a] (fnil conj #{}) b))
                  {}
                  (concat g (map reverse g)))
          walk (fn walk
                 ([node] (walk node #{node}))
                 ([node visited]
                  (let [next-nodes (remove visited (graph node))]
                    (if (empty? next-nodes)
                      visited
                      (reduce into
                        visited
                        (map #(walk % (conj visited %))
                          next-nodes))))))]
      (apply = all-nodes (map walk all-nodes)))))

(defcheck solution-d7e93ddc
  (fn [edges]
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
                (merge-if-intersection new-partition edge)))]
      (= 1 (count
             (loop [edges edges
                    vertex-partition []]
               (if (empty? edges)
                 vertex-partition
                 (recur (next edges) (add-edge-to-partition vertex-partition (first edges))))))))))

(defcheck solution-d7f51faf
  (fn[edges]
    (let [[head & tail] (vec edges)]
      (loop [start (set head) tail tail]
        (let [components (group-by #(boolean (some start %)) tail)]
          (if (seq (components true))
            (recur (set (flatten (components true))) (components false))
            (not (seq (components false)))))))))

(defcheck solution-d8dd1a4f
  (fn [graph]
    (let [connect (fn [r g](first (for [[s e] g :when (some #(let [[s2 e2] %] (or (= s s2)(= s e2)(= e s2)(= e e2))) r)][s e])))]
      (let [ connect_graph (loop [r [(first graph)] g (disj graph (first graph))]
                             (if (empty? g)r
                                           (let [cnode (connect r g)]
                                             (if (empty? cnode) r
                                                                (recur (conj r cnode) (disj g cnode))
                                                                )
                                             )
                                           )
                             )]
        (= (count graph) (count connect_graph)))
      )
    ))

(defcheck solution-d8fddfeb
  (fn [edges]
    (->> (reduce
           (fn [graph edge]
             (let [sub-graph1 (some #(if (% (first edge)) %) graph)
                   sub-graph2 (some #(if (% (second edge)) %) graph)]
               (conj
                 (disj graph sub-graph1 sub-graph2)
                 (set (concat sub-graph1 sub-graph2 edge)))))
           #{}
           edges)
      (count)
      (= 1))))

(defcheck solution-d98c553a
  (fn [xset]
    (letfn [(con_xy [ret [x y]]
              (loop [one (set [x y]), nret #{}, [h & t :as oret] (seq ret)]
                (if (empty? oret)
                  (conj nret one)
                  (if (or (h x) (h y))
                    (recur (into one h) nret t)
                    (recur one (conj nret h) t)))))]
      (= 1 (count (reduce con_xy #{} xset))))))

(defcheck solution-d99f9594
  (fn [edges]
    (let [all-nodes (set (apply concat edges))
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
                           (matching-edges vertex edges)))]
      (= all-nodes
        (set
          (map first
            (tree-seq can-expand? get-children [start-node edges])))))))

(defcheck solution-da0903f
  (fn gconnected? [g]
    (let [
          allnodes (reduce (fn [acc [n1 n2]] (conj acc n1 n2) ) #{} g)
          start (first (first g))]
      (letfn [(extend [current] (reduce (fn [s [i1 i2 :as s2]] (if
                                                                (or (s i1) (s i2))
                                                                 (into s s2)
                                                                 s)) current g)) ]
        (loop [results #{start}]
          (let [extended_results (extend results)]
            (if (= extended_results results)
              (= allnodes results)
              (recur extended_results)
              )))))))

(defcheck solution-da88602e
  (fn solve [xs]
    (letfn [(to-graph [xs]
              (let [all-edges (mapcat (fn [[x y]] [[x y] [y x]]) xs)]
                (reduce (fn [acc [v es]]
                          (assoc acc v (map second es)))
                  {}
                  (group-by first all-edges))))
            (dfs [graph v [visited edges-to :as state]]
              (if (visited v)
                state
                (let [newvisited (conj visited v)]
                  (reduce (fn [[acc-visited acc-edges-to :as acc-state] w]
                            (if (acc-visited w)
                              acc-state
                              (dfs graph w [acc-visited (assoc acc-edges-to w v)])))
                    [newvisited edges-to]
                    (graph v)))))]
      (let [graph (to-graph xs)
            vertices (into #{} (flatten (into [] xs)))
            state-after-dfs (dfs graph (first (keys graph)) [#{} {}])
            visited (first state-after-dfs)]
        (= vertices visited)))))

(defcheck solution-dae2b7e1
  (fn [g]
    (= 1
      (count
        (reduce
          (fn [g n]
            (let [{a n r nil} (group-by #(% n) g)]
              (conj r (reduce clojure.set/union a))))
          (map set g)
          (set (flatten (seq g))))))))

(defcheck solution-daee6ddd
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
                (set (mapcat (fn [a] (map (fn [b] [a b]) nodes)) nodes))))]
      (= (transitive-closure (undirected graph))
        (connected graph)))))

(defcheck solution-db973844
  (fn [g]
    (letfn [(un [dicts [p q]]
              (let [has? #(or (% p) (% q))]
                (cons (apply clojure.set/union (set [p q]) (filter has? dicts)) (remove has? dicts))))]
      (->> (reduce un #{} g) count (= 1)))))

(defcheck solution-dbb8c856
  (fn [edges]
    (letfn [(touches [comp [x y]]
              (or (contains? comp x) (contains? comp y)))
            (add [comps edge]
              (let [to-merge (conj (set (filter #(touches % edge) comps)) (set edge))
                    others (clojure.set/difference comps to-merge)]
                (into #{(apply clojure.set/union to-merge)} others)))]
      (= 1 (count (reduce add #{} edges))))
    ))

(defcheck solution-dbbfa583
  (fn [s]
    (let [p (remove #{s} (reduce (fn [a e]
                                   (apply conj a #{e}
                                     (map #(conj % e) a)))
                           #{}
                           s))]
      (every? #(not= () (filter (set (apply concat %))
                          (flatten (remove % s))))
        p))))

(defcheck solution-dc3a6c0c
  #(let [s (into (sorted-set) (mapcat (fn [[a b]] [[a b] [b a]]) %))
         q (into #{} (apply concat s))]
     (loop [t (set (first s))
            r s]
       (if (empty? r)
         (= t q)
         (let [[a b :as c] (first r)]
           (if (or (t a) (t b))
             (recur (into t [a b]) (disj r [a b]))
             false))))))

(defcheck solution-dc68121d
  (letfn [(connected? [conn-nodes edges]
            (if (empty? edges)
              true
              (if-let [edge (some
                              (fn [[x y]]
                                (when (or (conn-nodes x) (conn-nodes y)) [x y]))
                              edges)]
                (recur (conj conn-nodes (first edge) (second edge)) (disj edges edge))
                false)))]
    #(let [edge (first %)]
       (connected? (conj #{(first edge)} (second edge)) (disj % edge)))))

(defcheck solution-dc7d2c5e
  (fn connected? [es]
    (let [makeAdj (fn [es]  ; construct an adjacency map: k = vertex, v = set of vertices adj to k
                    (let [addEdge (fn [m [u v]]
                                    (assoc m u (conj (m u #{}) v) v (conj (m v #{}) u)))]
                      (reduce addEdge {} es)))
          adjMap (makeAdj es)
          visited (zipmap (keys adjMap) (repeat false))]
      (loop [visited visited
             pending [(key (first adjMap))]] ; breadth-first search
        (if (empty? pending)
          (every? true? (vals visited))
          (recur (assoc visited (first pending) true)
            (into (rest pending) (for [v (adjMap (first pending) #{})
                                       :when (not (visited v))] v))))))))

(defcheck solution-dc91bc43
  (fn connected? [edges]
    (letfn [(edges->neighbours
              [coll]
              (letfn [(updater [s v]
                        (conj (or s #{}) v))
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
                 [[v] (dissoc g v)] (g v))))]
      (let [g (edges->neighbours edges)
            [_ left] (dfs g)]
        (empty? left)))))

(defcheck solution-dccf81c8
  (fn [edges]
    (let [all-nodes (set (mapcat identity edges))
          neighbours (fn [node]
                       (letfn [(get-neighbour [edge]
                                 (cond
                                   (= node (first edge)) (second edge)
                                   (= node (second edge)) (first edge)
                                   :else nil))]
                         (filter identity (map get-neighbour edges))))
          step (fn [visited]
                 (set (concat visited (mapcat neighbours visited))))]
      (loop [visited (into #{} (first edges))]
        (let [stepped (step visited)]
          (cond
            (= all-nodes stepped) true
            (= visited stepped) false
            :else (recur stepped)))))))

(defcheck solution-dddc25fc
  (letfn
   [(connect [[xs [[a b] & more]]]
      (cond
        (nil? a)    [xs]
        (or (xs a)
            (xs b)) [(conj xs a b) more]
        :else       [xs (concat more [[a b]])]))]

    #(nil?
       (second
         (last
           (take
             (* 2 (count %))
             (iterate connect [(set (first %)) (seq %)])))))))

(defcheck solution-de2b3c49
  (fn connected? [edges]
    (let [vertices (set (apply concat edges))]
      (letfn [(edge? [u v] (some (fn [[w z]] (or (= [w z] [u v]) (= [w z] [v u]))) edges) )
              (neighbors [v] (filter (partial edge? v) vertices) )
              (component [v]
                (loop [seen #{v}
                       toExplore [v]]
                  (if (empty? toExplore) seen
                                         (let [u (first toExplore)
                                               newGuys (filter #(not (seen %)) (neighbors u))]
                                           (recur (into seen newGuys) (concat (rest toExplore) newGuys))
                                           )
                                         )
                  ))]
        (= (count vertices) (count (component (first vertices))))

        )
      )
    ))

(defcheck solution-de704724
  (fn connected? [edges]
    (let [trees (reduce
                  (fn [ans edge]
                    (let [[l-root r-root] (map #(first (drop-while ans (iterate ans %))) edge)]
                      (if (= l-root r-root) ans (assoc ans l-root r-root))))
                  {}
                  edges)]
      (not (second (distinct (remove trees (apply concat edges))))))))

(defcheck solution-deeb0075
  (fn [g]
    (letfn [
            (make-edges [g]
              (->>
                (filter (fn [[x y]] (not= x y))  g)
                (set)
                (into [])))

            (make-nodes [g]
              (into #{} (flatten (into [] g))))

            (make-matrix [ns]
              (->> (for [n1 ns n2 ns :when (not= n1 n2)]
                     #{n1 n2})
                (map (fn [n] [(first n) (last n)]))
                (into #{})))

            (make-times [ns]
              (int (* 4 (Math/pow (count ns) 2))))

            (contains-node? [[x y] n]
              (or (= x n) (= y n)))

            (next-edge [es n]
              (first (filter #(contains-node? % n) es)))

            (remove-edge [es e]
              (if (empty? es)
                es
                (let [x (first es)
                      xs (into [] (rest es))]
                  (if (= x e) xs (remove-edge (conj xs x) e)))))

            (next-node [[x y] n]
              (if (= x n) y x))

            (walk? [es n1 n2]
              (cond
                (= n1 n2) true
                (empty? es) false
                :else
                (let [e (next-edge es n1)]
                  (cond
                    (nil? e) false
                    (contains-node? e n2) true
                    :else
                    (walk? (remove-edge es e) (next-node e n1) n2)))))

            (eval-edge? [es n1 n2 t]
              (cond
                (zero? t) false
                (walk? es n1 n2) true
                :else
                (eval-edge? (shuffle es) n1 n2 (dec t))))

            (eval-graph [es mx t]
              (reduce
                (fn [acc [n1 n2]]
                  (if (not acc) false (eval-edge? es n1 n2 t))) true mx))]

      (let [es (make-edges g)
            ns (make-nodes g)
            mx (make-matrix ns)
            t (make-times ns)]
        (eval-graph es mx t)))))

(defcheck solution-e01022b6
  #(boolean (#{1 7} (count %))))

(defcheck solution-e0b2a34a
  (fn c [g]
    (let [n (set (flatten (apply vector g)))
          m (reduce #(assoc %1 (first %2) (conj (get %1 (first %2) #{(first %2)}) (second %2))) {} g)
          r (loop [mm m p {}]
              (if (= mm p)
                mm
                (recur (reduce
                         #(assoc
                           %1
                            (first %2)
                            (set (clojure.set/union (second %2) (apply clojure.set/union (map (fn [e] (get mm e)) (second %2))) )))
                         {} mm)
                  mm)
                )
              )]
      (true? (some #(= (count n) (count %)) (vals r)))
      )
    ))

(defcheck solution-e1017156
  (fn connected-graph [graph]
    (letfn [(walk [graph seen]
              (loop [i (count graph) seen seen]
                (if (zero? i)
                  seen
                  (recur (dec i)
                    (walk-graph graph seen)))))
            (walk-graph [graph seen]
              (loop [g graph seen seen]
                (if (empty? g)
                  seen
                  (let [[a b] (first g)]
                    (recur (rest g)
                      (map (fn [seen]
                             (if (seen a)
                               (conj seen b)
                               (if (seen b)
                                 (conj seen a)
                                 seen)))
                        seen))))))]
      (let [seen (walk graph [#{(ffirst graph)}])]
        (not (nil?
               (some (fn [g] (= (count (set (flatten (vec graph))))
                               (count g)))
                 seen)))))))

(defcheck solution-e1819b18
  (fn [edges]
    (->
      (reduce
        (fn [cs [a b]]
          (let [ca (get cs a a)
                cb (get cs b b)
                cs (assoc cs a ca b cb)]
            (zipmap (keys cs) (replace {ca cb} (vals cs)))))
        {}
        edges)
      (vals)
      (distinct)
      (count)
      (= 1))))

(defcheck solution-e197093e
  #(let [c (fn [e v]
             (if-let [u (e v)]
               (recur e u)
               v))
         e (reduce
             (fn [e [u v]]
               (let [p (c e u) q (c e v)]
                 (if (= p q)
                   e
                   (assoc e p q))))
             {} %)]
     (or (empty? e)
         (apply = (map (partial c e) (keys e))))))

(defcheck solution-e19f889c
  (fn [g] (let [vs (set (flatten (vec g))),
                nes (fn [v xs] (filter #((set %) v) xs)),
                to (fn [v e] (if (= (first e) v) (second e) (first e))),
                path? (fn path? [a b h]
                        (cond (= a b) true,
                              (empty? h) false,
                              :else (boolean (seq (filter #(path? (to a %) b (disj h %)) (nes a h))))))]
            (every? #(path? (first vs) % g) vs))))

(defcheck solution-e2160c6
  (fn [graph]
    (letfn [(connect [connections connected]
              (let [group (reduce (fn [s e] (apply conj s (or (connections e) #{e}))) #{} connected)]
                (reduce (fn [m e] (assoc m e group)) connections group)))]
      (let [connections (reduce (fn [m pair] (connect m pair)) {} graph)
            nodes (set (keys connections))]
        (every? #(= nodes (connections %)) nodes)))))

(defcheck solution-e25d51cc
  (fn [coll]
    (let [sets (map set coll)]
      ((fn connectable? [h r]
         (if (empty? r)
           true
           (let [check (fn [s] (< (count (into h s)) (+ (count h) (count s))))
                 connectable (filter check r)
                 unconnectable (remove check r)
                 ]
             (if (empty? connectable)
               false
               (connectable? (reduce into h connectable) unconnectable)))))
       (first sets) (rest sets)))))

(defcheck solution-e316dc9d
  (fn is-connected? [edges]
    (if (empty? (rest edges))
      true
      (let [vertexes (-> edges vec flatten set)
            n        (count vertexes)
            get-reachable-from (fn [x edges]
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
                                           result)
                                         ))
                                     )))
            find-edges-with	(fn [x edges]
                               (loop [eds edges, result #{}]
                                 (if (empty? eds)
                                   result
                                   (let [edge (first eds)
                                         idx  (.indexOf edge x)]
                                     (if (= -1 idx)
                                       (recur (rest eds) result)
                                       (recur (rest eds) (conj result edge))
                                       ))
                                   )))
            get-vertexes-without (fn [y edges]
                                   (loop [eds edges, result #{}]
                                     (if (empty? eds)
                                       result
                                       (let [[a b] (first eds)]
                                         (cond
                                           (= a y) (recur (rest eds) (conj result b))
                                           (= b y) (recur (rest eds) (conj result a))
                                           :default
                                           (recur (rest eds) result))
                                         ))))
            fill-l-k  (fn [l k vers-without-y]
                        (loop [vwy vers-without-y, l1 (set l), k1 k]
                          (if (empty? vwy)
                            {:l l1, :k k1}
                            (let [z (first vwy)]
                              (if (contains? l1 z)
                                (recur (rest vwy) l1 k1)
                                (recur (rest vwy) (conj l1 z) (conj k1 z))

                                )
                              ))))
            ver-x 	(first vertexes)
            l 		[ver-x]
            k   		[ver-x]]
        (loop [k1 k, l1 k1]
          (if (empty? k1)
            (if (= n (count l1)) true false)
            (let [y      		 (first k1)
                  edgs-y 		 (find-edges-with y edges)
                  vers-without-y (get-vertexes-without y edgs-y)
                  r 			 (fill-l-k l1 (rest k1) vers-without-y)]
              (recur (r :k) (r :l))
              )))
        ))))

(defcheck solution-e3506641
  #(> 2 (count
          (reduce (fn [s [ea eb]]
                    (apply cons
                      (reduce (fn [[j d] cs]
                                (if (or (cs ea) (cs eb))
                                  [(into j cs) d]
                                  [j (conj d cs)]))
                        [#{} []] s)))
            (->> % vec flatten set (map hash-set))
            %))))

(defcheck solution-e39c8be9
  (fn
    ff
    ([s]
     (if (= 1 (count s))
       true
       (ff [(first s)] (rest s))))
    ([f r]
     (let [tr (mapcat #(filter (fn [x] (or (= (first x) (second %)) (= (second x) (first %)) (= (first x) (first %)) (= (second x) (second %)))) r) f)]
       (if (and (empty? tr) (empty? r))
         true
         (if (and (empty? tr) (not (empty? r)))
           false
           (recur tr (remove #(some (fn [x] (= % x)) tr) r))))))))

(defcheck solution-e3a18978
  (fn [graph]
    (let [available (reduce (partial merge-with clojure.set/union) (map (fn [tuple] {(first tuple) #{(second tuple)}}) (concat graph (map reverse graph))))
          all-available-routes ((fn all-routes [prev next]
                                  (cond (= prev next) prev
                                        :else (all-routes
                                                next
                                                (reduce (partial merge-with clojure.set/union) (map (fn [route]
                                                                                                      {(first route)
                                                                                                       (into #{}
                                                                                                         (concat (second route)
                                                                                                           (mapcat (fn [current-available]
                                                                                                                     (get next current-available))
                                                                                                             (second route))))}) next)))))
                                [] available)

          node-count (count (distinct (reduce concat graph)))]
      (= (reduce max (map count (vals all-available-routes))) node-count)
      )))

(defcheck solution-e48137ef
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
            (visit [graph current-node node-list]
              (cond
                (empty? node-list) true
                (empty? (get graph current-node)) false
                :else (some identity (map #(visit (rem-edge graph current-node %) % (disj node-list %)) (get graph current-node)))))
            (is-connected [graph]
              (not (nil? (some identity (map #(visit graph % (disj (into #{} (keys graph)) %)) (keys graph))))))]
      (is-connected (make-graph s)))))

(defcheck solution-e582f48
  (fn [graph]
    (letfn [(eliminate [s node]
              (let [m (group-by #(or (= node (first %)) (= node (last %))) s)]
                (reduce #(eliminate % (if (= node (first %2)) (last %2) (first %2))) (get m false) (get m true))))]
      (let [[start end] (first graph)]
        (empty? (eliminate (eliminate (rest graph) start) end))))))

(defcheck solution-e60066c5
  (fn connected? [graph]
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
                next-node))))))))

(defcheck solution-e61b7622
  (fn graph-connected? [g]
    (loop [visited #{}
           [c & r :as stck] (vector (first g))]
      (if (empty? stck)
        (= (count visited) (count g))
        (recur (conj visited c)
          (concat (filter #(and (not (visited %))
                                (not-empty (clojure.set/intersection (set %) (set c)))) g)
            r))))))

(defcheck solution-e8938e2a
  (fn connected? [nodes]
    (let
     [links (reduce (fn [coll [x y]]
                      (-> coll
                        (assoc x (conj (get coll x []) y))
                        (assoc y (conj (get coll y []) x))))
              {}
              nodes)
      gen-next (fn [curr]
                 (set (mapcat links curr)))
      results (loop [curr [(ffirst nodes)] seen #{(ffirst nodes)}]
                (let [next-unseen (remove seen (gen-next curr))]
                  (if (empty? next-unseen)
                    seen
                    (recur next-unseen (into seen next-unseen)))))]
      (= results (set (keys links))))))

(defcheck solution-e9df88f6
  (fn graph-connect
    ([s]
     (let [el (distinct (flatten (seq s)))]
       (every?
         #(graph-connect (first %) (second %) s)
         (for [i el j el] [i j]))))
    ([a b s]
     (letfn [(any? [f s] ((comp boolean some) f s))
             (includes? [s a] (any? (partial = a) s))
             (rm-one [s a]
               (concat
                 (remove (partial = a) s)
                 (rest (filter (partial = a) s))))]
       (if (= a b)
         true
         (if (empty? s)
           false
           (let [next (filter #(includes? % a) s)]
             (if (empty? next)
               false
               (any?
                 #(graph-connect
                    (if (= (second %) a) (first %) (second %))
                    b
                    (rm-one s %))
                 next)))))))))

(defcheck solution-ead4aaae
  (fn is-connected? [colls]
    (let [create-trans (fn create-trans [colls]
                         (let [trans (reduce (fn [x [a b]]
                                               (set (concat x (set (map #(vector a (second %)) (filter #(= b (first %))  colls))))))
                                       colls colls)]
                           (if (= colls trans)  trans (create-trans trans))))
          collapse (fn [s] (reduce (fn [x [a b]] (if (or (contains? x [b a]) (= a b)) x (conj x [a b]) )) #{} s))
          riemann (fn [n] (apply + (range 1 n)))
          nodes (set (flatten (list* colls)))]
      (= (riemann (count nodes)) (count (collapse (create-trans (set (concat colls (map (fn [[k l]] [l k]) colls))))))))))

(defcheck solution-eb4ac18e
  (fn [g]
    (loop [c (first g)
           t (next g)]
      (if (empty? c)
        (empty? t)
        (let [x (group-by #(nil? ((set %) (first c))) t)]
          (recur (concat (next c) (flatten (x false)))
            (x true)))))))

(defcheck solution-ebdc83f1
  (fn __ [edges]
    (letfn [(connected? [c1 c2]
              (not (empty? (clojure.set/intersection c1 c2))))
            (add-comp [c1 c2]
              (if (connected? c1 c2) (apply conj c1 c2) c1))
            (merge-comp [cs]
              (if (empty? cs) '()
                              (let [c (first cs), rs (rest cs)]
                                (if (true? (some true? (map #(connected? c %) rs)))
                                  (merge-comp (map #(add-comp % c) rs))
                                  (cons c (merge-comp rs))))))
            ]
      (->> (map #(into #{} %) edges)
        (merge-comp)
        (count)
        (#(= 1 %))))))

(defcheck solution-ec20bb0b
  (fn [x]
    (= 1 (count
           (letfn [(g [x y] (> (count (clojure.set/intersection (set x) (set y))) 0))
                   (f [x]
                     (set (map (fn [z]
                                 (reduce #(if (g % %2) (into % %2) %) (set z) x))
                            x)))]
             (-> x f f))))))

(defcheck solution-ec2f8ff1
  (fn connected-graph? [edges]
    (let [nodes (reduce #(apply conj %1 %2) #{} edges)]
      (loop [visited? #{} queue (conj #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) (first nodes))]
        (let [node (peek queue)
              rst  (pop queue)]
          (if (empty? queue)
            (empty? (clojure.set/difference nodes visited?))
            (recur (conj visited? node)
              (reduce
                (fn [q [v w]]
                  (cond
                    (and (not= v node) (not (visited? v))) (conj q v)
                    (and (not= w node) (not (visited? w))) (conj q w)
                    :else q))
                rst
                (filter (fn [e] (some #(= node %) e)) edges)))))))))

(defcheck solution-ec9a6102
  (fn [E]
    (let [V (set (reduce (fn [vs [a b]]
                           (->> vs
                             (cons a)
                             (cons b)))
                   #{} E))]
      (letfn [(rec [n vis]
                (reduce (fn [v [a b]]
                          (cond
                            (= a n) (if (get v b) v (rec b v))
                            (= b n) (if (get v a) v (rec a v))
                            :else v))
                  (conj vis n) E))]
        (= (count V) (count (rec (first V) #{})))))))

(defcheck solution-ecb46656
  (fn [ss]
    (let [s (vec ss)]
      (loop [acc (set (first s))]
        (let [new-acc (reduce #(if (or (%1 (first %2)) (%1 (second %2)))
                                 (clojure.set/union %1 (set %2))
                                 %1)
                        acc
                        s)
              ]
          (if (= (->> s flatten distinct count) (count new-acc))
            true
            (if (= (count new-acc) (count acc))
              false
              (recur new-acc)))))
      )))

(defcheck solution-ed48c645
  (fn [ee]
    (let [v (set (flatten (seq ee)))]
      (letfn [(visit [seen n]
                (let [seen (conj seen n)
                      neighbours (flatten (filter #(or (= n (% 0)) (= n (% 1))) ee))]
                  (reduce #(if (% %2) % (visit % %2)) seen neighbours)))]
        (= v (visit #{} (first (first ee))))))))

(defcheck solution-ed711ca3
  (fn is-connected [connections]
    (let [ add-connection (fn  [current-sets connection]
                            (let [get-existing-index (fn [current-sets node] (second (first (filter #(first %) (map-indexed (fn [index a] [(contains? a node) index]) current-sets) ))))
                                  in-current-indexes (set (remove nil? (map #(get-existing-index current-sets %) connection)))
                                  with-index (map-indexed #(vector %1 %2) current-sets)
                                  g (group-by  #(contains? in-current-indexes (first %)) with-index)
                                  these (apply concat (map second (g true)))
                                  others (map second (g false))
                                  new-set (conj others  (conj these (first connection) (second connection)))
                                  ]
                              (map set new-set)))]
      (= 1 (count (reduce #(add-connection %1 %2) [] connections))))))

(defcheck solution-ed7f894c
  (fn [vs]
    (let [nd
          (set
            (flatten
              (seq vs)))]
      (letfn [
              (cts [n]
                (for
                 [edge vs
                  :let [
                        myedge (remove
                                 #{n}
                                 edge)]
                  :when
                  (=
                    1
                    (count myedge))]
                  (first myedge)))]
        (loop [cntd #{}
               nts [(first nd)]
               unctd (rest nd)]
          (let
           [nxt
            (remove
              cntd
              (flatten
                (map
                  cts
                  nts)))]
            #_(prn cntd nts unctd nxt)
            (cond (empty? unctd)
                  true
                  (empty? nxt)
                  false
                  :else
                  (recur
                    (into
                      cntd
                      nxt)
                    nxt
                    (remove
                      (set nxt)
                      unctd))))))
      )))

(defcheck solution-ede9b915
  (fn grconn
    ([edges] (grconn (set (first edges)) (set (rest edges))))
    ([nds es]
     (letfn [(conn? [e] (or (nds (first e)) (nds (second e)))),
             (connpoint [e] (if (nds (first e)) (second e) (first e)))]
       (cond (empty? es) true,
             (empty? (filter conn? es)) false,
             :else (recur (set (concat nds (map connpoint (filter conn? es))))
                     (set (filter (complement conn?) es))))))))

(defcheck solution-edea600
  (fn [g]
    (let [V (set (flatten (seq g)))
          P (atom (into {} (map #(vector % 0) V)))
          nn #(reduce (fn [s [a b]]
                        (cond (= a %) (if (= (@P b) 0) (conj s b) s)
                              (= b %) (if (= (@P a) 0) (conj s a) s)
                              :else s))
                [] g) ]
      (letfn [(go [v]
                (swap! P assoc v 1)
                (doseq [i (nn v)]
                  (go i)))]
        (go (first V))
        (if (some #(= 0 (last %)) @P)
          false
          true)))))

(defcheck solution-ee0f4ae4
  (fn [es]
    (let [nodes (set (apply concat es))
          es' (filter (fn [[u v]] (not= u v)) es)
          edges (set (into es' (for [[u v] es'] [v u])))]
      (= (* (count nodes) (dec (count nodes)))
        (count
          (loop [cur edges]
            (let [addl (for [[u v] cur [w x] cur :when (and (= v w) (not= u x))] [u x])
                  nxt (set (into cur addl))]
              (if (= nxt cur) nxt (recur nxt)))))))))

(defcheck solution-ee6b44d1
  (fn [e o]
    (let [[h & r] (seq o)]
      ((fn f [c r]
         (or (e r)
             (let [n (mapcat #(filter (fn [p] (some (set %) p)) r) c)]
               (if (e n) false
                         (f (reduce conj c n)
                           (remove (set n) r))))))
       #{h} r))) empty?)

(defcheck solution-ef3f6251
  #(let [[x & e] (seq %),r reduce,g empty?]
     (loop [v (into #{} x), e %, f false]
       (if f
         (g e)
         (let [m (filter (fn [[x y]] (or (v x) (v y))) e)]
           (recur (r conj v (flatten m))
             (r disj e m)
             (g m)))))))

(defcheck solution-f0021ccd
  (fn connected?
    ([edge-set]
     (connected?
       #{(first (first edge-set))}
       (count (into #{} (flatten (map identity edge-set))))
       (into {}
         (map
           (fn [[k el]] (vector k (map last el)))
           (group-by first (mapcat #(vector % (reverse %)) edge-set))))))
    ([vd-set n vts]
     (if (= n (count vd-set)) true
                              (let [nv (clojure.set/union vd-set (into #{} (mapcat #(get vts %) vd-set)))]
                                (if (= vd-set nv) false (connected? nv n vts)))))))

(defcheck solution-f110b19c
  (fn [x]
    (loop [src (rest x) temp (first x)]
      (if (empty? src)
        true
        (let [found (filter #(some (set temp) %) src)]
          (if (empty? found)
            false
            (recur (remove (set found) src) (apply concat temp found))))))))

(defcheck solution-f1b71148
  (fn graph-connectivity
    [connections]
    (let [nodes (set (flatten (seq connections)))]
      (loop [edge [(first nodes)] unconnected (disj nodes edge)]
        (let [candidate (reduce into (map (fn [node] (reduce #(if (= node (first %2))
                                                                (conj %1 (second %2))
                                                                (if (= node (second %2))
                                                                  (conj %1 (first %2))
                                                                  %1))
                                                       #{}
                                                       connections)) edge))
              new-edge (clojure.set/intersection candidate unconnected)
              new-unconnected (reduce #(disj %1 %2) unconnected new-edge)]
          (if (empty? new-unconnected)
            true
            (if (empty? new-edge)
              false
              (recur new-edge new-unconnected))))))))

(defcheck solution-f1f4d28d
  (fn walk
    ([g a b]
     (if
      (empty? g) false
                 (some true?
                   (for [edge (filter #(= (% 0) a) g)]
                     (if (= (edge 1) b)
                       true
                       (walk (disj g edge) (edge 1) b)
                       )))))
    ([g]
     (let [coll (apply vector (distinct (apply concat g)))]
       (every? true?
         (for [a (range (count coll)), b (range (count coll))
               :when (< a b)]
           ;[(coll a) (coll b)
           (walk (clojure.set/union g (set (map (fn [[x y]] [y x]) g)))
             (coll a) (coll b))
           ))))))

(defcheck solution-f2aafe96
  (fn [graph]
    (letfn [(adjacent [k]
              (clojure.set/union
               (set (->> graph (filter #(= k (first %))) (map second)))
               (set (->> graph (filter #(= k (second %))) (map first)))))
            (dfs-connected? [[v & vs] remaining]
              (cond (empty? remaining) true
                    (nil? v) false
                    :else (let [adj (filter remaining (adjacent v))
                                next-vs (if (empty? adj) vs (apply conj vs adj))
                                next-rem (apply disj remaining adj)]
                            (dfs-connected? next-vs next-rem))))]
      (let [[v & vs] (distinct (flatten (vec graph)))]
        (dfs-connected? (list v) (apply hash-set vs))))))

(defcheck solution-f2fcc24c
  (fn [graph]
    (let [traverse (fn traverse [graph node acc]
                     (reduce (fn [acc [a b]]
                               (cond
                                 (and (= node a) (nil? (acc b))) (traverse graph b (conj acc b))
                                 (and (= node b) (nil? (acc a))) (traverse graph a (conj acc a))
                                 :else acc))
                       acc graph))
          start (first (first graph))]
      (= (count (traverse graph start #{start}))
        (count (set (flatten (vec graph))))))))

(defcheck solution-f3b60821
  (letfn [(adjs-of-node [node edges]
            (let [rfn (fn [adj [fe se]]
                        (cond
                          (= node fe) (conj adj se)
                          (= node se) (conj adj fe)
                          :else adj))]
              (reduce rfn #{} edges)))
          (all-nodes [edges] (into #{} (mapcat identity edges)))
          (next-adjacency [nodes edges]
            (apply clojure.set/union nodes (map #(adjs-of-node % edges) nodes)))
          (adj-seq
            ([edges] (adj-seq #{(ffirst edges)} edges))
            ([nodes edges] (cons nodes (lazy-seq (adj-seq (next-adjacency nodes edges) edges)))))]
    (fn [edges]
      (let [s (adj-seq edges)
            l (atom nil)
            [res] (drop-while #(not= @l (reset! l %)) s)]
        (= res (all-nodes edges))))))

(defcheck solution-f43e7bcc
  (fn [e]
    (let [C count
          m (atom #{})]
      (= (-> e vec flatten set C)
        (C (set (tree-seq
                  #(when-not (@m %) (swap! m conj %))
                  #(keep
                     (fn [[x y]] (when (and (= x %) (not= y %)) y))
                     (into e (map (fn [[x y]] [y x]) e)))
                  (ffirst e))))))))

(defcheck solution-f484873d
  (fn [edges]
    (let [assoconj (fn [m k v] (assoc m k (if (m k) (conj (m k) v) #{v})))
          nodes (reduce (fn [nodes [v c]] (-> nodes (assoconj v c) (assoconj c v)))
                  {} edges)
          visit (fn visit [visited v]
                  (if (visited v)
                    visited
                    (reduce into
                      (map #(visit (conj visited v) %) (nodes v)))))]
      (= (count (visit #{} (ffirst nodes))) (count (keys nodes))))))

(defcheck solution-f4ee7a65
  (fn [edges]
    (= 1 (count (reduce (fn [c [u v]]
                          (let [s (or (first (filter #(% u) c)) #{u})
                                t (or (first (filter #(% v) c)) #{v})]
                            (conj (disj c s t) (clojure.set/union s t))))
                  #{} edges)))))

(defcheck solution-f56853d6
  (fn [s]
    (let [con-to (fn [cc s] (into cc (for [x cc [a b] s] (cond (= x a) b (= x b) a :else x))))
          conn-comp (fn [cc s] (let [newcc (con-to cc s)] (if (= cc newcc) cc (recur newcc s))))]
      (= (set (flatten (seq s)))
        (conn-comp #{(ffirst s)} s)))))

(defcheck solution-f64201bd
  (fn [es]
    (let [
          ps (clojure.set/union (set (map first es)) (set (map last es)))
          n (count ps)
          es1 (atom es)]
      #_(println ps)
      (dorun
        (for [k ps]
          (swap! es1 conj [k k])))
      (dorun (for [k ps i ps j ps]
               (if (and (or (@es1 [i k]) (@es1 [k i])) (or (@es1 [k j]) (@es1 [j k])))
                 (swap! es1 conj [i j]))))
      (= (* n n) (count @es1)))))

(defcheck solution-f6bf537a
  (letfn [(connected? [x+ items old new-items steps]
            (let
             [new-old (clojure.set/union old new-items)]
              (cond (> steps (count items)) :should-never-happen
                    (= items old) true
                    (= new-items #{}) false
                    true
                    (recur
                      x+ items new-old
                      (clojure.set/difference
                        (set (for [[i j] x+ :when (contains? new-items i)] j))
                        new-old
                        ) (inc steps))))
            )] (fn [x] (let
                        [x+ (clojure.set/union
                              x (set (for [[i j] x] [j i]))) items (set (map first x+))]
                         (connected? x+ items #{} #{(first items)} 0)))))

(defcheck solution-f7964cbe
  #(let [n-lst
         (into {} (for [[k v] (merge-with concat (group-by first %)
                                (group-by second %))]
                    [k (into #{} (flatten v))]))]
     (loop [hs #{(ffirst n-lst)}]
       (let [n-hs (into #{} (mapcat n-lst hs))]
         (if (= n-hs hs)
           (every? hs (keys n-lst))
           (recur n-hs))))))

(defcheck solution-f7986b9b
  (fn [s]
    (let [nodes (set (concat (map first s) (map second s)))
          s     (into s (map (fn [[a b]] [b a]) s))]
      (= ((fn f [current visited]
            (let [visited   (conj visited current)
                  neighbors (filter
                              #(not (contains? visited %))
                              (map
                                second
                                (filter
                                  #(= (first %)
                                     current)
                                  s)))]
              (if (empty? neighbors)
                visited
                (apply clojure.set/union (map #(f % visited) neighbors)))))
          (first nodes)
          #{})
        nodes))))

(defcheck solution-f7d68651
  (fn connected? [g]
    (let [rf (fn rf [a b]
               (let [u (b 0)
                     v (b 1)
                     sets (reduce clojure.set/union (filter #(or (contains? % u) (contains? % v)) a))
                     rest (remove #(or (contains? % u) (contains? % v)) a)]
                 (conj rest (clojure.set/union #{u} #{v} sets))))]
      (= 1 (count (reduce rf [] g))))))

(defcheck solution-f8710aa0
  (fn [graph]
    (let [nodes (reduce (fn [acc x]
                          (let [[from to] x
                                temp (assoc acc from (conj (acc from []) to))]
                            (assoc temp to (conj (temp to []) from)))) {} graph)]
      (loop [tovisit [(first (keys nodes))] visited #{}]
        (if (empty? tovisit)
          (= (count visited) (count nodes))
          (if (contains? visited (peek tovisit))
            (recur (pop tovisit) visited)
            (recur  (into (pop tovisit) (nodes (peek tovisit)))
              (conj visited (peek tovisit)))))))))

(defcheck solution-f8e69183
  (fn connected? [g]
    {:pre [(coll? g),
           (every? (comp (partial = 2) count) g)]}
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
          :else              (recur new-ccom new-edges))))))

(defcheck solution-fa64bcc7
  #(let [vertex (set (apply concat %))]
     (loop [visited (set (first %))
            {left false connected true} (group-by
                                          (fn [[a b]]
                                            (or
                                             (contains? visited a)
                                             (contains? visited b)))
                                          (rest %))]
       (cond
         (= visited vertex) true
         (empty? connected) false
         :else
         (let [nv (clojure.set/union visited (set (apply concat connected)))
               subsets (group-by
                         (fn [[x y]]
                           (or (contains? nv x) (contains? nv y)))
                         left)]
           (recur nv subsets))))))

(defcheck solution-faee9002
  (fn [s]
    (let [s (set (concat s (map reverse s)))
          tmp (set (map first s))
          ds (fn [v]
               (->>
                 s
                 (filter #(= (first %) v))
                 (map second)))
          g (fn g [rt v]
              (let [rt (conj rt v)]
                (reduce
                  g rt
                  (filter (complement rt)
                    (ds v)))))]
      (= (g #{} (first tmp))
        tmp))))

(defcheck solution-fb44c365
  (fn gc [graph]
    (let [vertexes (set (mapcat identity graph))
          found? (fn [vert s] (some #(= % vert) s))
          get-pend (fn [p edge s] (if (found? (p edge) s) #{} (set(filter #(and(not= edge %)(found? (p edge) %)) graph))))
          ]
      (loop [pending #{(first graph)} found #{}]
        (cond (= found vertexes) true
              (empty? pending) false
              :else (recur
                      (concat (rest pending)
                        (get-pend first (first pending) found)
                        (get-pend last  (first pending) found))
                      (conj found (first (first pending)) (last (first pending)))
                      )
              )
        )
      )
    ))

(defcheck solution-fb527e99
  (fn [edges]
    (letfn [(nodes [edges] (set (mapcat identity edges)))
            (reachable-from [n]
              (disj (nodes (filter #(contains? (set %) n) edges)) n))
            (dfs [start]
              (loop [visited #{start}
                     unvisited (reachable-from start)]
                (if (empty? unvisited)
                  visited
                  (let [this-node (first unvisited)
                        next-nodes (apply disj (reachable-from this-node) (into visited unvisited))]
                    #_(prn visited unvisited)
                    (recur (conj visited this-node)
                      (into (rest unvisited) next-nodes))))))]
      (let [all-nodes (nodes edges)
            first-node (first (first edges))]
        (= all-nodes (dfs first-node))))))

(defcheck solution-fbea8093
  (fn bfs [col]
    (letfn [(nbmap [col]
              (let [edges (concat col (map (fn [e] [(last e) (first e)]) col))]
                (reduce (fn [ret e] (update-in ret [(first e)] (fnil conj #{}) (last e))) {} edges)))

            (stepHd [graph q color]       ;; deq Q head and process each node
              (when-let [hd (peek q)]  ;; while q is not empty
                ;; process node early, process each edge, then process node late.
                (cons hd           ;; bfs, all nodes in Q are reachable, add this round reachable to partial result
                  (lazy-seq        ;; stepHd fn rets a lazy seq of all reachable nodes
                    (let [hdnb (remove (fn [e] (contains? color e)) (graph hd))]  ;; at leaf level, an empty lazy-seq
                      ;;(prn "hd " hd " hdnb " hdnb " color " color q)
                      (stepHd graph (into (pop q) hdnb) (reduce #(conj %1 %2) color hdnb)))))))]
      (let [graph (nbmap col)             ;; transform edge list into neighbor map
            root (first (keys graph))]
        (= (count graph) (count (stepHd graph (conj #?(:clj clojure.lang.PersistentQueue/EMPTY :cljs #queue []) root) (conj #{} root))))))))

(defcheck solution-fc4b3653
  (fn graph-connect1?
    [coll]
    (letfn [(create-map
              [coll]
              (reduce (fn [m [k v]] (update-in (update-in m [k] conj v) [v] conj k)) {} coll))]
      (let [g (create-map coll)
            start (first (first g))]
        (= (set (keys g))
          (loop [[x & more :as se] (g start) v #{start}]
            (if (nil? se)
              v
              (if (contains? v x)
                (recur more v)
                (recur (concat more (g x))
                  (conj v x))))))))))

(defcheck solution-fce757a3
  (fn c91
    [s]
    (letfn [(walk [m kys r]
              (let [ks (reduce #(into %1 (m %2)) #{} kys)
                    newks (clojure.set/difference ks r)]
                (if (empty? newks)
                  (if (= r (set (keys m)))
                    true
                    false)
                  (walk m newks (into r newks)))))]
      (let [m (reduce (fn [r [v1 v2]]
                        (assoc (assoc r
                                 v2
                                 (conj (r v2 []) v1))
                          v1
                          (conj (r v1 []) v2))) {} s)]
        (walk m [(first (first m))] #{})))))

(defcheck solution-fcee9bfa
  (fn connected? [g]
    (let [transitiveclosure (fn [m]
                              (loop [storage #{} result m]
                                (if (= storage result)
                                  result
                                  (recur result (set (into result (for [x result
                                                                        y result
                                                                        :when (= (peek x) (first y))]
                                                                    (vector (first x) (peek y)))))))))]
      (reduce #(and %1 %2) (map sequential?
                             (for [q (set (mapcat flatten g))
                                   r (set (mapcat flatten g))]
                               (some #{(vector q r)} (transitiveclosure (into g
                                                                          (map #(vec (reverse %)) g))))))))))

(defcheck solution-fd378bbd
  (fn [edges]
    (loop [seen (set (first edges))
           remaining (rest edges)]
      (let [{reachable true
             unreachable false} (group-by (comp boolean (partial some seen)) remaining)]
        (cond
          (empty? unreachable) true
          (empty? reachable) false
          :else (recur (into seen (flatten reachable)) unreachable))))))

(defcheck solution-fdf95e7e
  (fn my-graph-connectivity
    [graph]
    (letfn [(end-element? [list end] (some #(= (set (key (last %))) (set end)) list))
            (remove-first [val coll] (let [[n m] (split-with (partial not= val) coll)] (concat n (rest m))))
            (get-pairs [graph] (filter #(= 2 (count %)) (set (for [x graph y graph] (conj #{} x y)))))
            (possible-paths [val coll]
              (remove nil? (map #(cond
                                   (= (second val) (first %)) (hash-map % (remove-first % coll))
                                   (= (second val) (second %)) (hash-map (reverse %) (remove-first % coll)))
                             coll)))
            (are-vertices-connected [start end elements]
              (loop [result (list (hash-map start (remove #(= % start) elements))) i 1]
                (if (or (end-element? result end) (= i (inc (count elements))))
                  (not (empty? result))
                  (recur (mapcat #(possible-paths (key (last %)) (val (last %))) result) (inc i)))))]
      (if (= 1 (count graph))
        true
        (not-any? false? (map #(or (are-vertices-connected (first %) (second %) graph)
                                   (are-vertices-connected (reverse (first %)) (second %) graph)) (get-pairs graph)))))))

(defcheck solution-ffb98799
  (fn connected? [g]
    (let [adj-list (fn [s]
                     (let [nodes (set (mapcat flatten s))
                           nbs (fn [curr]
                                 (mapcat (partial filter #(not= % curr))
                                   (filter (partial some #(= % curr)) s)))]
                       (zipmap nodes (map nbs nodes))))
          walk (fn walk [curr adj visited]
                 (let [nearby (adj curr)
                       unvisited (filter (complement visited) nearby)
                       new-visited (into visited unvisited)]
                   (if (empty? unvisited) visited
                                          (apply clojure.set/union (flatten
                                                                     (map #(walk % adj new-visited) unvisited))))))
          adj (adj-list g)
          nodes (keys adj)]
      (if (< (count nodes) 2) true
                              (= (count nodes) (count (walk (first nodes) adj #{})))))))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-91))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))

