(ns om.parser.grammar
  (:use om.parser.utils)
  (:require [net.cgrand.parsley :as p]))

(defn- tree-to-str [c]
  (cond
   (string? c) c
   (and (vector? (:content c))
        (= :string-body (:tag (get (:content c) 1 nil)))
        (not (closing-tag? (:tag (get (:content c) 2 nil)))))
   (let [c (:content c)
         frst (str (first (:content (first c))) (first (:content (second c))))
         lst (first (:content (last c)))
         body (drop 2 (butlast c))]
     (str (apply str frst (map #(-> % tree-to-str escape-str) body)) lst))
   :else
   (apply str (map tree-to-str (:content c)))))

(defrecord Node [tag content tokens-length length]
  Object
  (toString [this]
    (tree-to-str this)))

(defn make-node [t c]
  (let [[len tok-len] (lengths c)
        t (if (= t :net.cgrand.parsley/unfinished)
            :uncomplete
            t)]
    (Node. t c tok-len len)))

(defn- make-unexpected [l]
  (make-node :unexpected [l]))

(def parser-options
  {:root-tag :root
   :main :expr*
   :space (p/unspaced whitespaces :*)
   :make-node make-node
   :make-unexpected make-unexpected})

(def parser-grammar
  {:expr- #{:pairs :numbers :keyword :symbol :quote :meta :deprecated-meta :deref :syntax-quote :var :anon-arg :char :unquote :unquote-splicing :read-eval}
   :pairs- #{:list :vector :map :set :fn-literal :string :regex} ;; lacks #k{} and #k[] syntax
   :numbers- #{:int :float :ratio}
   :space #{:whitespace :comment :discard}
   :whitespace #"(?:,|\s)+"
   :comment #{(p/unspaced #"(?:\#\!|;)" #"[^\n]*\n?")}
   :start-discard "#_"
   :discard [:start-discard :expr]
   :open-list \(
   :close-list \)
   :list [:open-list :expr* :close-list]
   :open-vector \[
   :close-vector \]
   :vector [:open-vector :expr* :close-vector]
   :open-map \{
   :close-map \}
   :map [:open-map :expr* :close-map]
   :open-set "#{"
   :close-set \}
   :set [:open-set :expr* :close-set]
   :open-fn-literal "#("
   :close-fn-literal \)
   :fn-literal [:open-fn-literal :expr* :close-fn-literal]
   :string-body #"(?:\\.|[^\\\"])++(?=\")"
   :open-string \"
   :close-string \"
   :string (p/unspaced :open-string :string-body :? :close-string)
   :open-regex "#\""
   :close-regex \"
   :regex (p/unspaced :open-regex :string-body :? :close-regex)
   :int #"(?:[-+]?(?:0(?!\.)|[1-9][0-9]*+(?!\.)|0[xX][0-9A-Fa-f]+(?!\.)|0[0-7]+(?!\.)|[1-9][0-9]?[rR][0-9A-Za-z]+(?!\.)|0[0-9]+(?!\.))(?!/))"
   :ratio #"[-+]?[0-9]+/[0-9]*"
   :float #"[-+]?[0-9]+\.[0-9]*+(?:[eE][-+]?+[0-9]+)?+M?"
   :start-char \\
   :char (p/unspaced :start-char #"(?:newline|space|tab|backspace|formfeed|return|u[0-9|a-f|A-F]{4}|o[0-3]?+[0-7]{1,2}|.)")
   :start-quote \'
   :quote [:start-quote :expr]
   :start-meta \^
   :meta [:start-meta :expr #_:expr] ;;second expr is the one with the first expr as meta applied to it|| DURR THIS WAY EVERYTHING GETS BROKEN
   :start-deprecated-meta "#^"
   :deprecated-meta [:start-deprecated-meta :expr #_:expr]
   :start-syntax-quote \`
   :syntax-quote [:start-syntax-quote :expr]
   :start-deref \@
   :deref [:start-deref :expr]
   :start-var "#'"
   :var [:start-var :expr]
   :start-unquote #"~(?!@)"
   :unquote [:start-unquote :expr]
   :start-unquote-splicing "~@"
   :unquote-splicing [:start-unquote-splicing :expr]
   :anon-arg #"%[0-9]*"
   :symbol #"(?:[-+](?![0-9])[^^(\[#{\\\"~%:,\s;'@`)\]}]*)|(?:[^^(\[#{\\\"~%:,\s;'@`)\]}\-+;0-9][^^(\[#{\\\"~%:,\s;'@`)\]}]*)#?"
   :start-keyword #":{1,2}"
   :keyword (p/unspaced :start-keyword #"[^(\[{'^@`~\"\\,\s;)\]}]*")
   :start-read-eval "#="
   :read-eval [:start-read-eval :expr]})

(def parser
  (apply (partial p/parser parser-options) (reduce concat parser-grammar)))