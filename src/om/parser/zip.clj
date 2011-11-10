(ns om.parser.zip
  (:refer-clojure :exclude [remove next]))

(defn update-path [tree path]
  (if tree
   (with-meta tree {:path (vec path)})))

(defmacro ^:private recur-times [times & body]
  `(let [r# ~@body
        t# (or ~times 1)]
    (if (= 1 t#)
      r#
      (recur r# (dec t#)))))

(defmacro defdir [dir [arg] & body]
  `(defn ~dir
     ([~arg] (~dir ~arg 1))
     ([~arg t#]
        (if (= 0 t#)
          ~arg
          (recur-times t# ~@body)))))

(defn zip [tree]
  (update-path tree []))

(defn path [tree]
  (:path (meta tree)))

(defn branch? [node]
  (and (:content node)
       (not (string? (first (:content node))))))

(defn node [tree]
  (loop [t tree p (path tree)]
    (if (empty? p)
      t
      (recur (get t (first p)) (rest p)))))

(def root? (comp empty? :path meta))

(defdir up [tree]
  (when-not (root? tree)
    (update-path tree
                 (drop-last 2 (path tree)))))

(defdir superior [tree]
  (loop [s tree]
    (if (and (up s) (not= :root (:tag (node (up s)))))
      (recur (up s))
      s)))

(defdir down [tree]
  (when (branch? (node tree))
    (update-path tree
                 (conj (path tree) :content 0))))

(defdir inferior [tree]
  (loop [i tree]
    (if (down i)
      (recur (down i))
      i)))

(defdir right [tree]
  (when (> (dec (count (:content (node (up tree)) [0])))
           (or (last (path tree)) 0))
    (update-path tree
                 (conj (vec (butlast (path tree)))
                       (inc (last (path tree)))))))

(defdir rightmost [tree]
  (loop [r tree]
    (if (right r)
      (recur (right r))
      r)))

(defdir left [tree]
  (when-not (#(or (nil? %) (zero? %)) (last (path tree)))
    (update-path tree
                 (conj (vec (butlast (path tree)))
                       (dec (last (path tree)))))))

(defdir leftmost [tree]
  (loop [l tree]
    (if (left l)
      (recur (left l))
      l)))

(defdir next-node [tree]
  (or (down tree)
      (or (right tree)
          (loop [t (up tree)]
            (or (right t)
                (if (up t)
                  (recur (up t))))))))

(defdir prev-node [tree]
  (when-not (root? tree)
    (if-let [t (left tree)]
      (if (down t)
        (loop [t (down t)]
          (cond
           (right t) (let [n (loop [t (right t)]
                               (if (right t)
                                 (recur (right t))
                                 t))]
                       (if (down n) (recur (down n)) n))
           (down t) (recur (down t))
           :else t))
        t)
      (up tree))))

(defdir next [tree]
  (loop [p (next-node tree)]
    (if (string? (first (:content (node p)))) p
        (if (next-node p)
         (recur (next-node p))))))

(defdir prev [tree]
  (loop [p (prev-node tree)]
    (if (string? (first (:content (node p)))) p
        (if (prev-node p)
         (recur (prev-node p))))))

(defn insert-left [tree node]
  (let [path (path tree)
        pos (last path)]
    (update-in tree (butlast path)
               (fn [old] (vec (concat (take pos old) [node] (drop pos old)))))))

(defn insert-right [tree node]
  (let [path (path tree)
        pos (last path)]
    (update-in tree (butlast path)
               (fn [old] (vec (concat (take pos old) [node] (drop pos old)))))))

(defn remove [tree]
  (let [path (path tree)
        pos (last path)]
    (update-path
     (update-in tree (butlast path)
                (fn [old] (vec (concat (take pos old) (drop (inc pos) old)))))
     path)))

(defn edit [tree f & args]
  (let [path (path tree)]
    (update-path
     (update-in tree (butlast path) f args) path)))
