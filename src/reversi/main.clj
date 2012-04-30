(ns reversi.main
  (:require reversi.ui_text)
  (:require reversi.ui_swing)
  (:require reversi.ui_quil)
  (:gen-class))

(defn -main [& args]
  (let [mode (first args)]
    (cond
      (= "text" mode) (reversi.ui_text/reversi-text)
      (= "swing" mode) (reversi.ui_swing/reversi-swing)
      (= "quil" mode) (reversi.ui_quil/reversi-quil))))
