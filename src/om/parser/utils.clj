(ns om.parser.utils
  (:require [om.parser.zip :as z]
            [om.parser.grammar :as g]))

(declare closing-tag?
         escape-str
         lengths)

(defn- tree-to-str [c]
  (cond
   (string? c) c
   (and (vector? (:content c))
        (= :string-body (:tag (get (:content c) 1)))
        (not (closing-tag? (:tag (get (:content c) 2)))))
   (let [c (:content c)
         frst (str (first (:content (first c))) (first (:content (second c))))
         lst (first (:content (last c)))
         body (drop 2 (butlast c))]
     (str (apply str frst (map #(-> % tree-to-str escape-str) body)) lst))
   :else
   (apply str (map tree-to-str (:content c)))))

(defrecord Node [tag content length]
  Object
  (toString [this]
    (tree-to-str this)))

(defn make-node [t c]
  (let [t (if (#{:net.cgrand.parsley/unfinished} t) :uncomplete t)]
    (Node. t c (lengths c))))

(defn make-unexpected [l]
  (make-node :unexpected [l]))

(defn length
  "Return token lenght"
  [tok]
  (if (string? tok)
    (.length ^String tok)
    (apply + (:length tok 0))))

(defn lengths
  "Return a vector with the lenghts of sub-expressions"
  [c]
  (map length c))

(def content-string?
  (comp string? first :content))

(defn- aim [node off]
  (let [sums (reductions + (:length node))]
    (loop [s sums i 0 p 0]
      (if (< (first s) off)
        (recur (rest s) (inc i) (first s))
        [i (- off p)]))))

(defn- zoom-in [zip offset]
  (if (content-string? (z/node zip))
    zip
    (if (= (count (:length (z/node zip))) 1)
      (recur (z/down zip) offset)
      (let [[pos new-off] (aim (z/node zip) offset)
            new-loc (-> zip z/down (z/right pos))]
        (recur new-loc new-off)))))

(defn node-from-offset
  "Given a tree and an offset, returns a zip pointing to the node corresponding to the offset."
  [tree offset]
  (if-not (<= (length tree) offset)
    (zoom-in (z/zip tree) (inc offset))))

(defn starting-offset
  "Given a zip returns the starting offset of the pointed node"
  [zip]
  (if zip
    (loop [z (z/up zip) pos (last (z/path zip)) off 0]
      (if (z/root? z)
        (apply + off (take pos (:length (z/node z))))
        (recur (z/up z) (last (z/path z)) (apply + off (take pos (:length (z/node z)))))))))

(defn next-offset
  "Returns the starting offset of the next node"
  [zip]
  (starting-offset (z/next zip)))

(defn prev-offset
  "Returns the starting offset of the previous node"
  [zip]
  (starting-offset (z/prev zip)))

(defn tag
  "Return the tag of the currently selected node in a zip"
  [zip]
  (:tag (z/node zip)))

(defn opening-tag?
  "Check if tag is a pair opener"
  [tag]
  (try
    (= "open" (.substring (str tag) 1 5))
    (catch Exception _ false)))

(defn starting-tag?
  "Check if tag is a read-macro"
  [tag]
  (try
    (= "start" (.substring (str tag) 1 6))
    (catch Exception _ false)))

(def starting-or-opening-tag?
  "Returns true either if tag is a read-macro or a pair opener"
  (some-fn opening-tag? starting-tag?))

(defn closing-tag?
  "Check if tag is pair closer"
  [tag]
  (try
    (= "close" (.substring (str tag) 1 6))
    (catch Exception _ false)))

(def opening-or-closing-tag?
  "Returns true if tag is a pair delimiter"
  (some-fn opening-tag? closing-tag?))

(defn opening-tag
  "Return a compound keyword of \"open-\" and type"
  [type]
  (if type
    (keyword (str "open-" (.substring (str type) 1)))))

(defn closing-tag
  "Return a compound keyword of \"close-\" and type"
  [type]
  (if type
   (keyword (str "close-" (.substring (str type) 1)))))

(defn in-string?
  "Checks if the cursor is inside a string/regex body, or selecting their closing paren"
  [zip]
  (#{:string-body :close-string :close-regex} (tag zip)))

(defn in-string-body?
  "Checks if the cursor is inside a string/regex body"
  [zip]
  (#{:string-body} (in-string? zip)))

(defn escape-str
  "Escape backslashes and double-quotes in a string"
  [^String body]
  (.replaceAll (.replace body "\\" "\\\\")  "([^\\\\]*?)\\\"(.*?)" "$1\\\\\"$2"))

(defn descape-str
  "Revert the effects of escape-str"
  [^String body]
  (.replace (.replace body "\\\"" "\"") "\\\\" "\\"))

(defn coll-type
  "Returns a keyword matching the collection type given the a closing or an opening tag"
  [tag]
  (if tag
    (let [k (keyword (apply str (interpose "-" (rest (.split (str tag) "-")))))]
      (if-not (#{(keyword "")} k)
        k))))

(defn opening-str
  "Returns a string representation of the opening paren of the specified type (type must be a valid keyword type)"
  [type]
  (str ((opening-tag type) g/parser-grammar)))

(defn closing-str
  "Returns a string representation of the closing paren of the specified type (type must be a valid keyword type)"
  [type]
  (str ((closing-tag type) g/parser-grammar)))

(defn uncomplete?
  "Checks if outer expression is uncomplete"
  [zip]
  (#{:uncomplete}
   (tag (z/up zip
              (if ((comp starting-or-opening-tag? tag)
                   (z/node zip)) 2)))))

(defn opening-paren
  "Returns a new zip pointing to the opening paren of the currently focused expression"
  [zip]
  (if zip
    (if (opening-tag? (tag (z/leftmost zip)))
      (if (= (z/path zip) (z/path (z/leftmost zip)))
        (loop [p (z/up zip)]
          (if (opening-tag? (tag (z/leftmost p)))
            (z/leftmost p)
            (if-not (z/root? p)
              (recur (z/up p)))))
        (z/leftmost zip))
      (recur (z/up zip)))))

(defn closing-paren
  "Returns a new zip pointing to the closing paren of the currently focused expression"
  [zip]
  (if zip
    (if (opening-tag? (tag (z/leftmost zip)))
      (if (= (z/path zip) (z/path (z/leftmost zip)))
        (loop [p (z/up zip)]
          (if (closing-tag? (tag (z/rightmost p)))
            (z/rightmost p)
            (if-not (z/root? p)
              (recur (z/up p)))))
        (z/rightmost zip))
      (recur (z/up zip)))))

(defn prev-matching-opening-paren
  "Returns a new zip pointing to the closing paren of the specified type if the focused expression is inside it"
  [zip type]
  (loop [p-paren (opening-paren zip)]
    (if p-paren
      (if (= (opening-tag type) (tag p-paren))
        p-paren
        (recur (opening-paren (z/up p-paren)))))))

(defn next-matching-closing-paren
  "Returns a new zip pointing to the closing paren of the specified type if the focused expression is inside it"
  [zip type]
  (loop [p-paren (closing-paren zip)]
    (if p-paren
      (if (= (closing-tag type) (tag p-paren))
        p-paren
        (recur (closing-paren (z/up p-paren)))))))

(defn adjust-lengths
  "Given a node, walk up to the root adjusting the node lenght, to be used after a tree edit"
  [node]
  (let [p (z/path node)]
    (loop [c (z/up node)]
      (if (z/up c)
        (recur (z/up (assoc-in c (concat (z/path c) [:length]) (map length (:content (z/node c))))))
        (z/update-path (assoc-in c [:length] (map length (:content c))) p)))))
