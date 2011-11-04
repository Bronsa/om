(ns om.core
  (:use seesaw.core
        [om.gui :only [app tree-viewer]])
  (:gen-class))

(defn -main [& _]
  (-> (:frame @app)
      show!)
  (-> tree-viewer show!) ;; debug thing
)
