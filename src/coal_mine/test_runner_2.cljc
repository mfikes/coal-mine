(ns coal-mine.test-runner-2
  (:require
   #?(:cljs [cljs.core.specs.alpha])
   [clojure.test]
   coal-mine.problem-66
   coal-mine.problem-67
   coal-mine.problem-68
   coal-mine.problem-69
   coal-mine.problem-70
   coal-mine.problem-71
   coal-mine.problem-72
   coal-mine.problem-73
   coal-mine.problem-74
   coal-mine.problem-75
   coal-mine.problem-76
   coal-mine.problem-77
   coal-mine.problem-78
   coal-mine.problem-79
   coal-mine.problem-80
   coal-mine.problem-81
   coal-mine.problem-82
   coal-mine.problem-83
   coal-mine.problem-84
   ))

(defn run-tests []
  (clojure.test/run-tests
    'coal-mine.problem-66
    'coal-mine.problem-67
    'coal-mine.problem-68
    'coal-mine.problem-69
    'coal-mine.problem-70
    'coal-mine.problem-71
    'coal-mine.problem-72
    'coal-mine.problem-73
    'coal-mine.problem-74
    'coal-mine.problem-75
    'coal-mine.problem-76
    'coal-mine.problem-77
    'coal-mine.problem-78
    'coal-mine.problem-79
    'coal-mine.problem-80
    'coal-mine.problem-81
    'coal-mine.problem-82
    'coal-mine.problem-83
    'coal-mine.problem-84))

(defn -main []
  (run-tests))

#?(:cljs (set! *main-cli-fn* -main))
