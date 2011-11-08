(ns om.modules.structural-editing
  (:use om.parser
        om.parser.utils
        clojure.core.incubator)
  (:require [om.parser.zip :as z]
            [om.parser.grammar :as g]))

(defn next-word [tree off]
  (let [current-node (node-from-offset tree off)]))

(defn prev-word [tree off]
  (let [current-node (node-from-offset tree off)]))

(defn split-expr [tree off]
  (let [current-node (node-from-offset tree off)]))

(defn join-expr [tree off]
  (let [current-node (node-from-offset tree off)]))

(defn splice-expr [tree off]
  (let [current-node (node-from-offset tree off)]))

(defn wrap-expr [tree off]
  (let [current-node (node-from-offset tree off)]))

(defn insert-pair [tree off type]
  (let [current-node (node-from-offset tree off)]
    (if (in-string-body? current-node)
      [(escape-str (opening-str type)) off 0]
      [(str (opening-str type) (closing-str type) off 0 (+ off (.length (opening-str type))))])))

(defn close-pair [tree off type]
  (let [current-node (node-from-offset tree off)]))

(defn encapsulate-right-expr [tree off]
  (let [current-node (node-from-offset tree off)]))

(defn decapsulate-right-expr [tree off]
  (let [current-node (node-from-offset tree off)]))

(defn encapsulate-left-expr [tree off]
  (let [current-node (node-from-offset tree off)]))

(defn decapsulate-left-expr [tree off]
  (let [current-node (node-from-offset tree off)]))
