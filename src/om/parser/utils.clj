(ns om.parser.utils
  (require [om.parser.zip :as z]))

(defn tok-length [tok]
  (if (string? tok)
    (.length ^String tok)
    (:length tok 0)))

(defn lengths [c]
  (let [tok-l (map tok-length c)]
    [(apply + tok-l) tok-l]))

(def whitespaces #{:whitespace :comment :discard})

(defn opening-tag? [tag]
  (try
    (= "open" (.substring (str tag) 1 5))
    (catch Exception _ false)))

(defn starting-tag? [tag]
  (try
    (= "start" (.substring (str tag) 1 6))
    (catch Exception _ false)))

(def starting-or-opening-tag?
  (some-fn opening-tag? starting-tag?))

(defn closing-tag? [tag]
  (try
    (= "close" (.substring (str tag) 1 6))
    (catch Exception _ false)))

(defn tag [n] (:tag (z/node n)))

(defn escape-str [body]
  (.replaceAll (.replace body "\\" "\\\\")  "([^\\\\]*?)\\\"(.*?)" "$1\\\\\"$2"))

(defn descape-str [body]
  (.replace (.replace body "\\\"" "\"") "\\\\" "\\"))
