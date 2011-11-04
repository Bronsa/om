(ns om.parser
  (:use om.parser.grammar
        om.parser.utils)
  (:require [net.cgrand.parsley :as p]
            [om.parser.zip :as z]))

(defn edit-buffer [buffer offset len text]
  (let [text (or text "")]
    (p/edit (or buffer (p/incremental-buffer parser))
            offset len text)))

(defn buffer-tree [buffer]
  (p/parse-tree buffer))

(defn parse
  [^String text]
  (parser text))

(defn- aim [node off]
  (let [sums (reductions + (:tokens-length node))]
    (loop [s sums i 0 p 0]
      (if (< (first s) off)
        (recur (rest s) (inc i) (first s))
        [i (- off p)]))))

(defn- zoom-in [zip offset]
  (if (string? (z/node zip))
    zip
    (if (= (count (:tokens-length (z/node zip))) 1)
      (recur (z/down zip) offset)
      (let [[pos new-off] (aim (z/node zip) offset)
            new-loc (-> zip z/down (z/right pos))]
        (recur new-loc new-off)))))

(defn node-from-offset [tree offset]
  (if-not (<= (:length tree) offset)
    (zoom-in (z/zip tree) (inc offset))))

(defn starting-offset [zip]
  (if zip
    (loop [z (z/up zip) pos (last (z/path zip)) off 0]
      (if (z/root? z)
        (apply + off (take pos (:tokens-length (z/node z))))
        (recur (z/up z) (last (z/path z)) (apply + off (take pos (:tokens-length (z/node z)))))))))

(defn ending-offset [zip]
  (if zip
    (dec (+ (starting-offset zip) (count (z/node zip))))))
