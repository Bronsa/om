(ns om.parser
  (:use om.parser.utils
        om.parser.grammar)
  (:require [net.cgrand.parsley :as p]
            [om.parser.zip :as z]))

(def parser
  (apply (partial p/parser {:root-tag :root
                            :main :expr*
                            :space (p/unspaced (:space parser-grammar) :*)
                            :make-node make-node
                            :make-unexpected make-unexpected})
         (reduce concat parser-grammar)))

(defn buffer [text]
  (p/edit (p/incremental-buffer parser) 0 0 text))

(defn edit-buffer [buffer offset len text]
  (let [text (str (or text "") " ")]
    (p/edit (or buffer (p/incremental-buffer parser))
            offset len text)))

(defn buffer-tree [buffer]
  (p/parse-tree buffer))

(defn parse [^String text]
  (parser (str text " ")))
