(ns coal-mine.problem-17
  (:require [coal-mine.checks :refer [defcheck-17] :rename {defcheck-17 defcheck}]
            [clojure.test]))

(defcheck solution-1826e20c
  '( 6 7 8 ))

(defcheck solution-1ec03707
  (map #(+ % 5) '(1 2 3)))

(defcheck solution-2136f942
  [6 7 8])

(defcheck solution-2a6c40d4
  (take 3 (range 6 9)))

(defcheck solution-2da2bed
  (list 6 7 8))

(defcheck solution-39837430
  '(6,7,8))

(defcheck solution-50c01bcb
  '(6 7 8 ))

(defcheck solution-578220a5
  [6,7,8])

(defcheck solution-8afb8abf
  '(6 7 8) (let [ {:keys [head midd tail]} {:head 1 :midd 2 :tail 3}
                 inc-with-five (partial + 5)
                 res (map inc-with-five '(1 2 3) ) ]
             res))

(defcheck solution-8b9d0469
  '(6  7 8))

(defcheck solution-ae804055
  `(6 7 8))

(defcheck solution-b4900391
  (range 6 9))

(defcheck solution-b803bbba
  (vec (range 6 9)))

(defcheck solution-c2330912
  '( 6 7 8))

(defcheck solution-dbddba69
  '(6, 7, 8))

(defcheck solution-df92c3a4
  '(6 7 8))

(defcheck solution-e227f46a
  '[6 7 8])

(defcheck solution-f78bb947
  [6  7 8])

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-17))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
