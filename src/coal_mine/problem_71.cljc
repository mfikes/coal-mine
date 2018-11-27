(ns coal-mine.problem-71
  (:require [coal-mine.checks :refer [defcheck-71] :rename {defcheck-71 defcheck}]
            [clojure.test]))

(defcheck solution-18a2096c
  count)

(defcheck solution-380b760d
  (fn [v] (nth v 4)))

(defcheck solution-5b8eb60b
  (fn [xs] (nth xs 4)))

(defcheck solution-6183254c
  (partial apply max))

(defcheck solution-7c1faad3
  #(reduce max %))

(defcheck solution-84badb5a
  #(last %))

(defcheck solution-9cab99d5
  #(nth %1 4))

(defcheck solution-9cf461d5
  last)

(defcheck solution-aa7f271b
  (fn [_] 5))

(defcheck solution-d00e00a2
  (fn [x] 5))

(defcheck solution-d3870bc3
  #(nth % 4))

(defcheck solution-f0161194
  (fn [s] (last s)))

(defcheck solution-f07764c8
  (comp peek vec))

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-71))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
