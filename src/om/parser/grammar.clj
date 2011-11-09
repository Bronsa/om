(ns om.parser.grammar
  (:require [net.cgrand.parsley :as p]))

(def parser-grammar
  {:expr- #{:pairs :numbers :keyword :symbol :quote :meta :deprecated-meta :deref :syntax-quote :var :anon-arg :char :unquote :unquote-splicing :read-eval}
   :pairs- #{:list :vector :map :set :fn-literal :string :regex :record-vector-literal :record-map-literal}
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
   :meta [:start-meta :expr]
   :start-deprecated-meta "#^"
   :deprecated-meta [:start-deprecated-meta :expr]
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
   :symbol #"(?:(dec|inc|\+|-|\*)')|(?:[-+](?![0-9])[^^(\[#{\\\"~%:,\s;@`')\]}]*)|(?:[^^(\[#{\\\"~%:,\s;@`')\]}\-+;0-9][^^(\[#{\\\"~%:,\s;@`')\]}]*)#?"
   :open-record-vector-literal #"#(?:[^^(\[#{\\\"~%:,\s;@`')\]}/\-+;0-9]*)\["
   :close-record-vector-literal \]
   :open-record-map-literal #"#(?:[^^(\[#{\\\"~%:,\s;@`')\]}/\-+;0-9]+)\{"
   :close-record-map-literal \}
   :record-vector-literal (p/unspaced :open-record-vector-literal :expr :close-record-vector-literal)
   :record-map-literal (p/unspaced :open-record-map-literal :expr :close-record-map-literal)
   :start-keyword #":{1,2}"
   :keyword (p/unspaced :start-keyword #"[^(\[{'^@`~\"\\,\s;)\]}]*")
   :start-read-eval "#="
   :read-eval [:start-read-eval :expr]})
