(ns coal-mine.problem-8
  (:require [coal-mine.checks :refer [defcheck-8] :rename {defcheck-8 defcheck}]
            [clojure.test]
            [clojure.set]))

(defcheck solution-1550092
  #{:a :b :d :c })

(defcheck solution-27e70ccc
  #{:d :c :b :a})

(defcheck solution-2aa50037
  #{:a :c :b :d})

(defcheck solution-44052ae8
  #{:a :c :b :d} #{:a :c :b :d})

(defcheck solution-46943be8
  #{:b :c :d :a})

(defcheck solution-479643de
  #{:a :b :c :d})

(defcheck solution-4d6c1e7e
  #{:a :b :c :d} #{:a :b :c :d})

(defcheck solution-4daf81f6
  #{:a :c :d :b})

(defcheck solution-5db6422e
  '#{:a :b :c :d})

(defcheck solution-694a1f8e
  #{:a :b :d :c})

(defcheck solution-6ec44dea
  #{:a :b :c :d} #{:a :b :c :d})

(defcheck solution-6ffa5249
  #{:a :c :b :d})

(defcheck solution-8d7379b7
  (set '(:a :a :b :c :c :c :c :d :d)))

(defcheck solution-8ebcebe9
  (set '(:d :c :a :b)))

(defcheck solution-8f9110f3
  #{ :a :b :c :d })

(defcheck solution-966b5bee
  #{:c :b :d :a})

(defcheck solution-9bc1390c
  (set '(:a :b :c :d)))

(defcheck solution-a8eb82e0
  (set '(:a :b :c :d)) (set '(:a :b :c :d)))

(defcheck solution-c446f333
  (hash-set :d :c :b :a))

(defcheck solution-c5b999ea
  (hash-set :a :b :c :d))

(defcheck solution-d014c0a8
  (clojure.set/union #{:a :b :c} #{:b :c :d}))

(defcheck solution-da55b4
  #{:a :b :c :d} #{:a :b :c :d})

(defcheck solution-dcd6f9b1
  #{ :a :b :c :d})

(defcheck solution-e0afd95d
  #{:a :b :c :d } #{:a :b :c :d })

(defcheck solution-e1af8ef2
  (set '(:a :a :a :a :a :a :a :a :a :a :a :a :b :c :c :c :c :d :d)))

(defcheck solution-e58ed0f6
  (set [:a :b :c :d]))

(defcheck solution-f8d036c3
  #{:b :a :c :d})

(defcheck solution-fc863ace
  #{:a :b :c :d })