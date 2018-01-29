(ns coal-mine.problem-16
  (:require [coal-mine.checks :refer [defcheck-16] :rename {defcheck-16 defcheck}]
            [clojure.test]
            [clojure.pprint]))

(defcheck solution-10d8b6e9
  (fn [n] (clojure.string/join ["Hello, " n "!"])))

(defcheck solution-13df49c7
  (fn [name] (clojure.string/join ["Hello, " name \!])))

(defcheck solution-16041578
  (fn [n] (str "Hello, " n "!")))

(defcheck solution-162ca0c3
  #(str"Hello, "%\!))

(defcheck solution-1655c471
  (fn [x](str "Hello, " x "!")))

(defcheck solution-17f3b86e
  (fn [name]
    (str "Hello, " name "!")))

(defcheck solution-186e8ad8
  (fn [x] (str "Hello, " x  "!")))

(defcheck solution-1913532b
  (fn [name]
    (str "Hello, " name "!")))

(defcheck solution-19e2eda
  #(clojure.string/join ["Hello, " % "!"]))

(defcheck solution-1d14622a
  #(apply str (let [s ["Hello, " , "!"]]
                (interpose % s ))))

(defcheck solution-1ed1018d
  (fn [x] (str "Hello, " x \!)))

(defcheck solution-2005ac1c
  (fn [x] (. "Hello, %!" replace "%" x)))

(defcheck solution-22b2f209
  (fn [who] (str "Hello, " who "!")))

(defcheck solution-236f3ef5
  (fn[x](str "Hello, " x "!")))

(defcheck solution-25078be9
  #(str"Hello, " % "!"))

(defcheck solution-25ccbada
  #(str"Hello, "%1"!"))

(defcheck solution-27aeb3df
  (partial clojure.pprint/cl-format nil "Hello, ~a!"))

(defcheck solution-2a2be946
  (fn [x]
    (str "Hello, " x "!")
    ))

(defcheck solution-2d9bf61
  (fn[arg] (str "Hello, " arg "!")))

(defcheck solution-307612a6
  (fn yo [x] (str "Hello, " x "!")))

(defcheck solution-33553266
  (fn greeter [name]
    (str "Hello, " name "!")
    ))

(defcheck solution-33f0a53
  (fn [x] ( str "Hello, " x "!")))

(defcheck solution-348e5aa3
  (fn my-hello [name] (str "Hello, " name "!")))

(defcheck solution-37dcc062
  (fn hello [c] (str "Hello, " c "!")))

(defcheck solution-38e9b519
  (fn [m] (str "Hello, " m "!")))

(defcheck solution-39726ade
  (fn greeting [name] (str "Hello, " name "!")))

(defcheck solution-397a6490
  #(str "Hello, " % '!))

(defcheck solution-493f0e51
  #(str "Hello, " , %, "!"))

(defcheck solution-4edb4cb6
  (fn [msg] (str "Hello, " msg "!")))

(defcheck solution-557e6c79
  (fn [sx] (str "Hello, " sx "!")))

(defcheck solution-58255411
  (fn[n](str"Hello, "n"!")))

(defcheck solution-5af6db7
  (fn hello-world [name]
    (str "Hello, " name "!")))

(defcheck solution-5bd550f0
  (fn [x] (apply str ["Hello, " x "!"])))

(defcheck solution-5e3d3684
  #(str "Hello, " % "!" ))

(defcheck solution-5e54e4b8
  (fn [s]
    (str "Hello, " s "!")))

(defcheck solution-5ed713ce
  #(str "Hello, "
     %
     "!"))

(defcheck solution-61f37abe
  #(str "Hello, "
     % \!))

(defcheck solution-62af1b27
  (fn [person] (str "Hello, " person "!")))

(defcheck solution-65bd6596
  (fn [lname] (str "Hello, " lname "!")))

(defcheck solution-65fe98e5
  #(apply str (conj ["Hello, "] % "!")))

(defcheck solution-66ef4b34
  #(clojure.string/join "" ["Hello, " % "!"]))

(defcheck solution-67da3db0
  (fn hello_world [ name ]
    (str "Hello, " name "!")))

(defcheck solution-6be7d6a6
  (fn hello [name]
    (str "Hello, " name "!")))

(defcheck solution-6c757238
  (fn [name] (clojure.string/join ["Hello, " name "!"])))

(defcheck solution-6c7d28cf
  (fn [n]
    (str "Hello, " n "!")))

(defcheck solution-6d15d97b
  #(str "Hello, " % \!))

(defcheck solution-6da84d7d
  (fn [x] (str "Hello, " x "!")))

(defcheck solution-7af422b6
  (fn hello-ify [name] (str "Hello, " name "!")))

(defcheck solution-7af4b56e
  (fn [x] (clojure.string/join ["Hello, " x "!"])))

(defcheck solution-8362c69
  #(str "Hello, " %1 \!))

(defcheck solution-867589e6
  #(apply str (concat "Hello, " % "!")))

(defcheck solution-88881e89
  (fn[name] (clojure.string/join ["Hello, " name "!"])))

(defcheck solution-8a1c06ff
  #(str "Hello, " % "!"))

(defcheck solution-8d106e56
  (fn [fname] (str "Hello, " fname "!")))

(defcheck solution-8eeef688
  (fn [a] (str "Hello, " a "!")))

(defcheck solution-8fc8e485
  (fn[s] (str "Hello, " s "!")))

(defcheck solution-93ed9b11
  (fn hello [x] (str "Hello, " x "!")))

(defcheck solution-942718be
  (fn greet [name] (str "Hello, ",name, "!")))

(defcheck solution-954d1eb4
  (fn [x] (clojure.string/join "" ["Hello, " x "!"])))

(defcheck solution-95f906be
  (fn [x]
    (apply str ["Hello, " x "!"])))

(defcheck solution-964db12e
  (fn [who] (clojure.string/join ["Hello, " who "!"])))

(defcheck solution-981d296f
  (fn [name] ( .concat "Hello, "(.concat name "!" ))))

(defcheck solution-9a7cee0c
  #(apply str ["Hello, " % "!"]))

(defcheck solution-9cddd0db
  (fn [name] (.concat (.concat "Hello, " name) "!")))

(defcheck solution-9d70b5
  (fn [e] (str "Hello, " e "!")))

(defcheck solution-a0cf462e
  (fn hello [x] (clojure.string/join x ["Hello, " "!"])))

(defcheck solution-a21c7c98
  (fn [s] (str "Hello, " s "!")))

(defcheck solution-a2ff2f7
  ;;#(str "Hello, " % "!")
  (fn [s] (str "Hello, " s "!")))

(defcheck solution-a8abf2e5
  (fn [name](str "Hello, " name "!")))

(defcheck solution-a96b6df3
  #( str "Hello, ", %, "!"))

(defcheck solution-a9c9b885
  (fn [s] (clojure.string/replace "Hello, {name}!" "{name}" s)))

(defcheck solution-acd8e90d
  (fn my-func [x] (str "Hello, " x "!")))

(defcheck solution-ad8a673d
  #(apply str "Hello, " % "!"))

(defcheck solution-adc0368f
  #(str "Hello, ", %, "!"))

(defcheck solution-aeace420
  #(str "Hello, " %  "!"))

(defcheck solution-b14bb6f9
  (fn [nm] (clojure.string/join ", " ["Hello" (str nm "!")])))

(defcheck solution-b2d38c58
  #( str "Hello, " % "!"))

(defcheck solution-b531746
  (fn [nam] (str "Hello, " nam "!")))

(defcheck solution-b68eb2c9
  (fn [nm] (str "Hello, " nm "!")))

(defcheck solution-b6da0bd
  #(.concat "Hello, " (.concat % "!")))

(defcheck solution-b89c2d32
  (fn hello[x] (str "Hello, " x "!")))

(defcheck solution-bf6e2ef5
  #(str "Hello," \space % "!"))

(defcheck solution-bfb82408
  #(str "Hello, " %1 "!"))

(defcheck solution-c5916683
  (fn personalize-greeting [name] (str "Hello, " name "!")))

(defcheck solution-c5cb6d20
  (fn [st](str "Hello, " st "!")))

(defcheck solution-c6d3fcdb
  #(clojure.string/join ["Hello, ", %, "!"]))

(defcheck solution-cae46129
  (partial clojure.string/replace "Hello, #!" #"#"))

(defcheck solution-d32b3e93
  #(.concat (.concat "Hello, " %) "!"))

(defcheck solution-d91cfc0f
  (fn hello [nm] (str "Hello, " nm "!")))

(defcheck solution-dbee9300
  (fn [text] (str "Hello, " text "!")))

(defcheck solution-dd883b33
  (fn sol0016 [s]
    (str "Hello, " s "!")))

(defcheck solution-deacaed3
  (fn [x] (.concat(.concat "Hello, " x)"!")))

(defcheck solution-e6a7985b
  (fn [x]
    (str "Hello, " x "!")))

(defcheck solution-ecbe242d
  (fn [personName] (str "Hello, " personName "!")))

(defcheck solution-eef36413
  (fn [x] (clojure.string/join x ["Hello, " "!"])))

(defcheck solution-f1640d9f
  (fn [person] (str "Hello, " person \!)))

(defcheck solution-f1c6153a
  (fn [x] (.concat (.concat "Hello, " x) "!")))

(defcheck solution-f1f8394f
  (fn [ppl] (str "Hello, " ppl "!")))

(defcheck solution-f2acaff7
  (fn [name] (str "Hello, " name \!)))

(defcheck solution-f610836f
  #(str "Hello, "%"!"))

(defcheck solution-f8292504
  (fn hello [name] (str "Hello, " name "!")))

(defcheck solution-fd1cefb1
  (fn [name] (str "Hello, " name "!")))