(ns coal-mine.problem-36
  (:require [coal-mine.checks :refer [defcheck-36] :rename {defcheck-36 defcheck}]
            [clojure.test]))

(defcheck solution-1682b85e
  [z 1 y 3 x 7 ])

(defcheck solution-194b769d
  [x 7 z 1 y 3])

(defcheck solution-1c8e327e
  [x 7 y 3 z 1 ] ;x + y = 10
  ;z + y = 4
  ;z = x OR y

  x)

(defcheck solution-1e546c99
  [z 1, y 3, x 7])

(defcheck solution-1f9a682a
  [ z 1 y 3 x 7 ])

(defcheck solution-26c5532a
  [y 3 x 7 z 1])

(defcheck solution-26f3e73f
  [ x 7 y 3 z 1])

(defcheck solution-28deb8ae
  [x 7, y 3, z 1 ])

(defcheck solution-2b096ac3
  [x 7 , y 3, z 1])

(defcheck solution-32c14945
  [z 1, y 3, x, 7])

(defcheck solution-413ff638
  [x 7
   y 3
   z 1])

(defcheck solution-476eca9f
  [x 7, y 3, z 1])

(defcheck solution-47c9ba01
  [ z 1
   y 3
   x 7])

(defcheck solution-48d641fd
  [ x 7
   y 3
   z 1 ])

(defcheck solution-4b4d3b9f
  [z 1, y (- 4 z), x (- 10 y)])

(defcheck solution-4ecc3edf
  [ x 7 y 3 z 1 ])

(defcheck solution-51255201
  [[z y x] [1 3 7]])

(defcheck solution-58916806
  [x 7  y 3 z 1])

(defcheck solution-5a6e0035
  [ x 7  y 3 z 1])

(defcheck solution-5df152d8
  [z 1
   y 3
   x 7])

(defcheck solution-5e005772
  [ z 1 y 3 x 7])

(defcheck solution-60de02a3
  [  z 1 x 7 y 3])

(defcheck solution-61f1418c
  [[x y z][7 3 1]])

(defcheck solution-729510d8
  [z 1,y 3, x 7 ])

(defcheck solution-7429edbd
  [
   x 7, y 3, z 1,
   ])

(defcheck solution-79d859af
  [ z 1
   y 3
   x 7 ])

(defcheck solution-7a2bddba
  [z 1 y 3 x  7])

(defcheck solution-7a748e21
  [x 7  y 3  z 1])

(defcheck solution-7a7ab94c
  [x 7 y 3 z 1])

(defcheck solution-7e003a46
  [z 1, y, 3, x, 7])

(defcheck solution-8394f8eb
  [y 3 z 1 x 7])

(defcheck solution-8d003b49
  [z 1
   y 3
   x 7])

(defcheck solution-926420b
  [z 1
   y 3
   x 7])

(defcheck solution-945c816a
  [x 7 y 3  z 1])

(defcheck solution-9a6a5238
  [x 7
   y 3
   z 1])

(defcheck solution-b15086c7
  [z (- (+ 1 (* 1 (- 1 (* 1 (- 1 1))))) 1)
   y (- (* (+ 1 (/ 1 1))(* 2 (/ 2 2))) 1)
   x (first (flatten [[[[[[[[[[[[[[[[[[[[[[[[[[[7]]]]]]]]]]]]]]]]]]]]]]]]]]]))])

(defcheck solution-b703b03
  [x 7, z 1, y 3])

(defcheck solution-b8a33398
  [[x y z] [7 3 1]])

(defcheck solution-b8ca146f
  [z 1 x 7 y 3])

(defcheck solution-bac51b30
  [ x 7
   y 3
   z 1])

(defcheck solution-c001021f
  [z 1,y 3,x 7])

(defcheck solution-c0b940d0
  [z 1 y (- 4 z) x (- 10 y)])

(defcheck solution-db97eef8
  [x 7
   y 3
   z 1])

(defcheck solution-e76a0466
  [[x y z]
   [7 3 1]])

(defcheck solution-e76f630e
  [ [x y z] [7 3 1] ])

(defcheck solution-ecf07dad
  [x 7, y 3,z 1])

(defcheck solution-f4fe8150
  [y 3
   z 1
   x 7])

(defcheck solution-f55154ab
  [x 7,y 3,z 1])

(defcheck solution-f59f0ad3
  [x 7 , y 3 , z 1])

(defcheck solution-f6db6bce
  [z 1 y 3 x 7])

(defcheck solution-f7b1847
  [x 7
   y 3
   z 1])

(defn run-tests []
  (clojure.test/run-tests 'coal-mine.problem-36))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
