(ns om.gui
  (:use [seesaw core chooser keystroke]
        clojure.pprint)
  (:require [om.parser :as p]
            [om.modules.structural-editing :as s-e]))

;; note this simple GUI is just for the mere purpose of testing, I will write the GUI when everything else is done, i hope

(declare app)

(def buf (atom (p/edit-buffer nil 0 0 nil)))
(def fopen (atom nil))
(def locked (atom false))

(defmacro locking-doc [& body]
  `(try (swap! locked not)
        ~@body
        (catch Exception e# (throw e#))
        (finally (swap! locked not))))

(defn open-file [e]
  (let [file (choose-file :type :open)
        file-content (slurp file)]
    (swap! fopen (constantly file))
    (locking-doc
     (text! (:editor @app) file-content)
     (swap! buf (fn [buf] (p/edit-buffer buf 0 0 file-content)))
     (send-off (agent nil) (fn [_] (p/buffer-tree @buf))))))

(defn save-file [e]
  (spit @fopen (text (:editor @app))))

;;; debugging faggotry
(def e (doto (editor-pane :text "") (.setEditable false)))
(def tree-viewer (frame :title "tree"
                        :width 500 :height 500
                        :content (scrollable e)))
;;;

(def app (atom {:editor (editor-pane :text "")
                :menu-items {:file (menu :text "File"
                                         :items [(action :handler open-file :name "Open")
                                                 (action :handler save-file :name "Save")])}}))

(swap! app assoc :frame (frame :title "title"
                               :width 500 :height 500
                               :menubar (menubar :items (vals (:menu-items @app)))
                               :content (scrollable (:editor @app))))

(defn caret-position []
  (.getDot (.getCaret (:editor @app))))

(defn process-edit [doc starting-off len type]
  (let [text (if (= "INSERT" type)
               (.getText doc (int starting-off) (int len)))
        len (if text 0 len)]
    (swap! buf (fn [buf] (p/edit-buffer buf starting-off len text)))))

(defn se-register-keystroke
  ([ks f]
     (se-register-keystroke ks f (fn [])))
  ([ks f add-f]
     (let [editor (:editor @app)
           im (.getInputMap editor)
           am (.getActionMap editor)
           id (str (gensym))]
       (.put im ks id)
       (.put am id
             (action :handler (fn [e]
                                (locking-doc
                                    (let [c (caret-position)]
                                      (when-let [[text off len & opt-c] (f (p/buffer-tree @buf) c)]
                                        (swap! buf (fn [buf] (p/edit-buffer buf off len text)))
                                        (doto (.getDocument editor)
                                          (.remove off len)
                                          (.insertString off text nil))
                                        (.setCaretPosition editor c)
                                        (add-f text off len opt-c))))))))))

(defn set-caret-position [p]
  (.setCaretPosition (:editor @app) p))

(defn next-caret-position
  ([] (fn [& _]
        (set-caret-position (inc (caret-position)))))
  ([o] (fn [& _] (set-caret-position (+ o (caret-position))))))

(se-register-keystroke (keystroke "ctrl RIGHT") s-e/encapsulate-next-expression)
(se-register-keystroke (keystroke "ctrl LEFT") s-e/decapsulate-last-expression)
(se-register-keystroke (keystroke "alt shift S") s-e/split-expression (next-caret-position))
(se-register-keystroke (keystroke "alt shift J") s-e/join-expression (next-caret-position -1))
(se-register-keystroke (keystroke "alt E") #(s-e/wrap-next-expression %1 %2 :list) (next-caret-position))
(se-register-keystroke (keystroke "alt S") s-e/splice-next-expression (next-caret-position -1))

(doseq [[s t f]  [["(" :list (next-caret-position)]
                  ["[" :vector (next-caret-position)]
                  ["{" :map (next-caret-position)]
                  ["\"" :string (fn [t _ _ _] (set-caret-position
                                               (+ (caret-position) (if (= \\ (first t)) 2 1))))]]]
  (se-register-keystroke (keystroke (str "typed " s))
                              #(s-e/insert-pair %1 %2 t) f))

(doseq [[s t] [[")" :list]
               ["]" :vector]
               ["}" :map]]] (se-register-keystroke
                             (keystroke (str "typed " s))
                             #(s-e/jump-to-closing %1 %2 t) (fn [_ _ _ [c]]
                                                              (set-caret-position (inc c)))))

(listen (:editor @app)
        :document (fn [event]
                    (if-not @locked
                      (process-edit (.getDocument event)
                                    (.getOffset event)
                                    (.getLength event)
                                    (.toString (.getType event))))
                    ;; debugging faggotry, broken too
                    (send-off (agent nil) (fn [_] (text! e (with-out-str (pprint (p/buffer-tree @buf))))))))

;; http://stackoverflow.com/questions/5139995/java-column-number-and-line-number-of-cursors-current-position
;; http://www.javaprogrammingforums.com/java-swing-tutorials/915-how-add-line-numbers-your-jtextarea.html
;; http://download.oracle.com/javase/1.5.0/docs/api/javax/swing/text/DefaultStyledDocument.html
;; http://download.oracle.com/javase/1.5.0/docs/api/javax/swing/text/AbstractDocument.html
;; http://download.oracle.com/javase/1.4.2/docs/api/javax/swing/JEditorPane.html
;; http://download.oracle.com/javase/1.4.2/docs/api/javax/swing/text/DefaultEditorKit.html
