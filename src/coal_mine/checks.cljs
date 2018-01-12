(ns coal-mine.checks
  (:refer-clojure :exclude [system-time])
  (:require-macros coal-mine.checks))

(def system-time cljs.core/system-time)
