(ns reversi.datamodel)

(defn initialize-board []
  "Returns a new Reversi board initialized for a new game."
  [[nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil :w  :b  nil nil nil]
   [nil nil nil :b  :w  nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]
   [nil nil nil nil nil nil nil nil]])
