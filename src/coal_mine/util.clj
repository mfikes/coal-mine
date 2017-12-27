(ns coal-mine.util
  (:require
   [clojure.string :as str]
   [clojure.tools.reader :as reader]
   [clojure.tools.reader.reader-types :as readers])
  (:import
   (java.io StringReader PushbackReader)))

(defn unescape [s]
  (-> s
    (str/replace "&gt;" ">")
    (str/replace "&lt;" "<")
    (str/replace "&amp;" "&")
    (str/replace "&quot;" "\"")))

(defn trim [s]
  (try
    (let [rdr (readers/source-logging-push-back-reader (PushbackReader. (StringReader. s)))]
      (binding [reader/*alias-map* identity]
        (->> (repeatedly #(-> (reader/read {:eof nil} rdr)
                            meta :source))
          (take-while some?)
          (str/join " "))))
    (catch Throwable e
      #_(throw (ex-info "Failed to trim" {:input s :root-cause e}))
      s)))

(defn parse-solutions [file]
  (->>
    (slurp file)
    (re-seq #"(?s)<div class=\"solution-username\">(.+?)'s solution:</div><pre class=\"solution-code\">(.*?)</pre>")
    (map (fn [[_ id solution]]
           [(str "solution-" (format "%x" (hash solution))) (unescape solution)]))
    (into {})
    (sort-by first)
    (remove (fn [[id solution]]
              (str/includes? solution "4clojure")))
    (run! (fn [[id solution]]
            (println (str "(defcheck " id))
            (println (str "  " (trim (unescape solution)) ")"))
            (println)))))
