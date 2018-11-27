(ns coal-mine.problem-10
  (:require [coal-mine.checks :refer [defcheck-10] :rename {defcheck-10 defcheck}]
            [clojure.test]))

(defcheck solution-11c29ef3
  20 20)

(defcheck solution-13ba94c6
  ({:a 10, :b 20, :c 30} :b ) (:b (hash-map :a 10, :b 20, :c 30)))

(defcheck solution-17716ea8
  20 20)

(defcheck solution-1c8f840e
  (*(+(*)(*)(*)(*))(+(*)(*)(*)(*)(*))))

(defcheck solution-1f399e0e
  (:b {:a 10, :b 20, :c 30}))

(defcheck solution-25787b36
  ({:a 10 :b 20 :c 30} :b))

(defcheck solution-30695846
  ({:b 20, :a 10} :b))

(defcheck solution-31739d4a
  20)

(defcheck solution-38278b2f
  (:b (hash-map :a 10, :b 20, :c 30)))

(defcheck solution-4f65f584
  20 20)

(defcheck solution-6faa137
  ((hash-map :a 10, :b 20, :c 30) :b))

(defcheck solution-762f0016
  (* 2 (+ 4 6)))

(defcheck solution-79d2af53
  (:b {:a 10 :b 20 :c 30}))

(defcheck solution-7edfb3e3
  (+ 1 2 3 4 5 5))

(defcheck solution-b2547f18
  (+ 10 10))

(defcheck solution-b2f9d726
  (:kaka {:kaka 20} ))

(defcheck solution-cda5eb46
  ({:a 10, :b 20, :c 30} :b))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-10))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
