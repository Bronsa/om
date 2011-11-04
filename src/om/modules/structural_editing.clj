(ns om.modules.structural-editing
  (:use om.parser
        om.parser.utils
        om.parser.grammar)
  (:require [om.parser.zip :as z]))

;;; THIS IS PURE SHIT, REWRITE *EVERYTHING* possibly with a nice DSL, lisp has macros dammit, let's use them

;; DURR HANDLE comments and whitespaces
;; fff, encapsulate and decapsulate should swap how they behave with whitespace,

(defn encapsulate-next-expression [tree off]
  (if (starting-or-opening-tag? (:tag (z/up (node-from-offset tree off))))
    (if-not (> (count (node-from-offset tree off)) off)
        (recur tree (- off (count (node-from-offset tree off)))))
    (loop [node (z/rightmost (z/up (node-from-offset tree off)))
           n-node (z/next node 2)]
      (if (closing-tag? (:tag (z/node (z/up n-node))))
        (if (z/next n-node)
          (recur n-node (z/next n-node)))
        (when n-node
          (let [n-node (loop [n-node (z/up n-node)]
                         (if (starting-or-opening-tag? (:tag (z/node n-node)))
                           (recur (z/up n-node))
                           n-node))
                ins-off (starting-offset node)
                [n-node c] (if (and (#{:whitespace :comment} (-> n-node z/node :tag))
                                    (not (closing-tag? (-> n-node (z/next 2) z/up z/node :tag))))
                             (let [nn (encapsulate-next-expression
                                       (z/remove n-node) (starting-offset node))
                                   c (last nn)
                                   nn (str (z/node n-node) (first nn))]
                               [nn (+ c (count (str (z/node n-node))))])
                             [(str ((if (#{:string-body} (-> node z/left z/node :tag))
                                      escape-str identity) (str (z/node n-node)))
                                   (z/node node)) (inc (count (str (z/node n-node))))])]
            [n-node ins-off c]))))))

;;split on whitespace?, unescape
(defn decapsulate-last-expression [tree off]
  (let [off-node (z/up (node-from-offset tree off))
        node (z/left (z/rightmost off-node))]
    (let [[off node] (if (= (z/path off-node)
                                (z/path (z/leftmost off-node)))
                       (if (not (zero? off))
                         [(dec off) (z/left (z/rightmost (z/up off-node)))]
                         [nil nil])
                       [off node])]
      (if (and (closing-tag? (-> node z/rightmost z/node :tag))
               (not (= 2 (count (:tokens-length (z/node (z/up node))))))
               (not (#{:string-body} (-> node z/leftmost z/right z/node :tag))))
        (let [[n o] (if (#{:whitespace :comment} (:tag (z/node node)))
                      ((juxt #(str (first %) (str (z/node node))) second)
                       (decapsulate-last-expression (z/remove node) off))
                      [(str (z/node (z/rightmost node)) (z/node node)) (starting-offset node)])]
          [n o (.length n)])))))

(defn wrap-next-expression [tree off type]
  (let [node (z/up (node-from-offset tree off))]
    (when-not (#{:string-body} (:tag (z/node node)))
      (let [off-diff (- off (starting-offset node))
            node (if (starting-or-opening-tag? (:tag (z/node node)))
                   (z/up node) node)
            node-content (str (z/node node))
            opening ((keyword (str "open-" (.substring (str type) 1))) parser-grammar)
            closing ((keyword (str "close-" (.substring (str type) 1))) parser-grammar)
            partial-node-content (.substring node-content off-diff)]
        [(str opening ((if (#{:regex :string} type) escape-str identity) partial-node-content) closing) off (.length partial-node-content)]))))


(defn splice-next-expression [tree off]
  (let [node (z/up (node-from-offset tree off) 2)
        opening (z/down node 2)]
    (if-not (opening-tag? (-> tree (node-from-offset off) z/up z/node :tag))
      (if (opening-tag? (:tag (z/node (z/up opening))))
        (let [c (butlast (drop 1 (:content (z/node node))))
              f (if (= :string-body (:tag (z/node (z/right (z/up opening))))) descape-str identity)]
          [(f (str (->Node nil (vec c) nil nil))) (starting-offset node) (:length (z/node node))]))
      (if (opening-tag? (-> tree (node-from-offset off) z/prev z/up z/node :tag))
        (let [[t o s] (splice-next-expression
                       (z/insert-right (-> tree (node-from-offset (dec off)) z/up)
                                       (->Node :whitespace [" "] [1] 1)) off)]
          [(.substring t 1) o s])))))

(defn split-expression [tree off]
  (let [node (-> tree (node-from-offset off))
        opening-node-tag  (-> node z/up z/leftmost z/node :tag)]
    (if-not (opening-tag? (-> node z/up z/node :tag))
      (if (opening-tag? opening-node-tag)
        (let [type (.substring (str opening-node-tag) 5)
              closing-node-tag (keyword (.concat "close" type))]
          [(str (parser-grammar closing-node-tag) (parser-grammar opening-node-tag)) off 0])))))

(defn join-expression [tree off]
  (let [leftmost-paren (loop [n (-> tree (node-from-offset off))]
                         (if (closing-tag? (:tag (z/node (z/up n))))
                           n
                           (if (z/prev n)
                             (if-not (opening-tag? (:tag (z/node (z/up (z/prev n)))))
                               (recur (z/prev n))))))
        rightmost-paren (loop [n (-> tree (node-from-offset off))]
                          (if (opening-tag? (:tag (z/node (z/up n))))
                            n
                            (if (z/next n)
                              (let [t (:tag (z/node (z/up (z/next n))))]
                                (if-not (or (starting-tag? t) (closing-tag? t))
                                  (recur (z/next n)))))))]
    (if (and leftmost-paren rightmost-paren
         (= (.substring (-> leftmost-paren z/up z/node :tag str) 7)
            (.substring (-> rightmost-paren z/up z/node :tag str) 6)))
      [(loop [n (z/next leftmost-paren) s ""]
         (if (= (z/path n) (z/path rightmost-paren))
           s
           (recur (z/next n) (str s (z/node n)))))
       (starting-offset leftmost-paren) (inc (- (ending-offset rightmost-paren)
                                                (starting-offset leftmost-paren)))])))

(defn insert-pair [tree off type]
  (let [opening ((keyword (str "open-" (.substring (str type) 1))) parser-grammar)
        closing ((keyword (str "close-" (.substring (str type) 1))) parser-grammar)
        pair (str opening closing)]
    (if-not (#{:string-body :close-string :close-regex}
             (:tag (z/node (z/up (node-from-offset tree off)))))
      [pair off 0]
      [(escape-str (str opening)) off 0])))

(defn jump-to-closing [tree off type]
  (let [node (node-from-offset tree off)
        opening (str ((keyword (str "open-" (.substring (str type) 1))) parser-grammar))
        closing (str ((keyword (str "close-" (.substring (str type) 1))) parser-grammar))]
    (if (#{:string-body :close-string :close-regex} (:tag (z/node (z/up node))))
      [closing off 0 off]
      (let [node (if node node (node-from-offset tree (dec off)))]
        (if (and (= :uncomplete (:tag (z/node (z/up node 2))))
                 (= opening (z/node (z/down (z/leftmost (z/up node))))))
          [closing off 0 (dec off)]
          (if  (= opening (z/node (z/down (z/leftmost (z/up node)))))
            ["" off 0 (starting-offset (z/down (z/rightmost (z/up node))))]))))))
