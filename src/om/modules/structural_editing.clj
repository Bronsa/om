(ns om.modules.structural-editing
  (:use om.parser
        om.parser.utils
        clojure.core.incubator)
  (:require [om.parser.zip :as z]
            [om.parser.grammar :as g]))

(defn next-word [buff off]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]))

(defn prev-word [buff off]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]))

(defn split-expr [buff off]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]
    (if-let [t (-> current-node opening-paren tag coll-type)]
      (if-not (uncomplete? current-node)
       [(str (closing-str t) (opening-str t)) off 0 (inc off)]))))

(defn join-expr [buff off]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]))

(defn splice-expr [buff off]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]))

(defn wrap-expr [buff off]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]))

(defn insert-pair [buff off type]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]
    (if (in-string? current-node)
      [(escape-str (opening-str type)) off 0]
      [(str (opening-str type) (closing-str type)) off 0 (+ off (.length (opening-str type)))])))

(defn- remove-spaces [node]
  (if (#{:whitespace} (tag node))
    ["" (starting-offset node) (length (z/node node))]))

(defn close-pair [buff off type]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]
    (if (in-string? current-node)
      [(escape-str (closing-str type)) off 0]
      (if-let [closing-node-path (z/path (next-matching-closing-paren current-node type))]
        (loop [curr (closing-paren current-node) ret []]
          (let [edits (remove-spaces (z/prev curr))]
            (if (= (z/path curr) closing-node-path)
              (if edits
                (conj ret (conj edits (next-offset curr)))
                (conj ret ["" 0 0 (next-offset curr)]))
              (if edits
                (recur (-> curr z/prev z/remove adjust-lengths z/next closing-paren)
                       (conj ret edits))
                (recur (-> curr z/next closing-paren) ret)))))))))

(defn encapsulate-right-expr [buff off]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]))

(defn decapsulate-right-expr [buff off]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]))

(defn encapsulate-left-expr [buff off]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]))

(defn decapsulate-left-expr [buff off]
  (let [tree (buffer-tree buff)
        current-node (node-from-offset tree off)]))
