(ns om.modules.structural-editing
  (:use om.parser
        om.parser.utils)
  (:require [om.parser.zip :as z]
            [om.parser.grammar :as g]))

(defn next-word [tree off])
(defn prev-word [tree off])
(defn split-expr [tree off])
(defn join-expr [tree off])
(defn splice-expr [tree off])
(defn wrap-expr [tree off])
(defn insert-pair [tree off type])
(defn close-pair [tree off type])
(defn encapsulate-right-expr [tree off])
(defn decapsulate-right-expr [tree off])
(defn encapsulate-left-expr [tree off])
(defn decapsulate-left-expr [tree off])
