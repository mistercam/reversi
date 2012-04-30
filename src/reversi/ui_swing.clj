(ns reversi.ui_swing
  (:import (javax.swing JFrame JPanel JButton JOptionPane)
           (java.awt.event MouseListener)
           (java.awt Color Dimension))
  (:use reversi.datamodel)
  (:use reversi.functional))

(def footer-height 30) ; space below the board to show the current player
(def piece-size 70) ; the width and height of an individual board square
(def gap-size 5) ; the vertical and horizontal spacing between squares
(def padding 5) ; the spacing within a square that surrounds a piece
(def board-size (+ (* 8 piece-size) (* 7 gap-size))) ; the board size
(def color-white (Color. 255 255 255))
(def color-black (Color. 0 0 0))
(def color-green (Color. 0 136 0))

;; This function draws a black or white piece within the square specified by
;; the row and column
(defn draw-piece [g r c piece]
  "Draws a white or black piece in the square of the specified row
  and column."
  (if (not= nil piece)
    (let [color (if (= :w piece) color-white color-black)]
      (.setColor g color)
      (.fillOval g
        (+ (* c piece-size) (* c gap-size) padding) ; x
        (+ (* r piece-size) (* r gap-size) padding) ; y
        (- piece-size (* 2 padding)) ; width
        (- piece-size (* 2 padding)))))) ; height

;; This function draws the square at the specified row and column, and if a
;; piece resides in said square, draws the piece, too
(defn draw-square [g board r c]
  "Draws the square for the specified row and column."
  (.setColor g color-green)
  (.fillRect g
    (+ (* c piece-size) (* c gap-size)) ; x
    (+ (* r piece-size) (* r gap-size)) ; y
    piece-size ; width
    piece-size) ; height
  (draw-piece g r c ((board r) c)))

;; This function writes text in the footer below the main playing board
;; indicating who the current player is
(defn draw-player [g piece]
  "Draws a message below the board indicating the current player."
  (let [p (if (= :w piece) "WHITE" "BLACK")]
    (.setColor g color-white)
    (.drawString g (str "CURRENT PLAYER: " p) 10 (+ board-size (quot footer-height 2)))))

;; This function draws the square for every permutation of row and column. We also print
;; the active player in the footer
(defn draw-board [g board piece]
  "Draws the Reversi game board."
  (.setColor g color-black)
  (.fillRect g
    0 ; x
    0 ; y
    board-size ; width
    (+ board-size footer-height)) ; height
  (doseq [r (range 8) c (range 8)]
    (draw-square g board r c))
  (draw-player g piece))

;; This function accepts a mouse position and returns a vector of the
;; corresponding row and column. Note that an x-position translates to
;; a column, and a y-position translates to a row.
(defn parse-mouse-pos [x y]
  "Returns the row and column corresponding to the mouse position."
  [(quot y (+ piece-size gap-size))
   (quot x (+ piece-size gap-size))])

;; More for cleaner code
(defn popup [frame msg]
  "Displays a popup containing the specified message."
  (JOptionPane/showMessageDialog frame msg))

;; More for cleaner code
(defn announce-winner [frame board]
  "Displays a popup containing the winner of the game."
  (let [p (winner board)]
    (cond
      (= p :w) (popup frame "White Wins!")
      (= p :b) (popup frame "Black Wins!")
      :else
      (popup frame "It's a tie!"))))

;; This function builds our Reversi panel and defines its behavior. It implements
;; Java's MouseListener interface to provide behaviour to support mouse events.
(defn reversi-panel [frame board piece]
  "Builds a JPanel to represent our Reversi game board. The panel accepts
  mouse events to place pieces on the board.

  board and piece should both be atoms."
  (let [game-over (atom false)]
    (proxy [JPanel MouseListener] []
      (paintComponent [g] ; overriding JPanel's paintComponent method
        (proxy-super paintComponent g) ; call paintComponent on the JPanel
        (draw-board g @board @piece)) ; draw our board
      (getPreferredSize [] ; defines the size of our game board
        (Dimension. board-size (+ board-size footer-height)))
      (mouseClicked [e] ; when the user clicks the mouse
        (let [rc (parse-mouse-pos (.getX e) (.getY e))] ; converts the mouse position into row-column coords
          (if (not (move-valid? @board @piece rc))
            (popup frame "Invalid move") ; the user clicked outside the main playing area or on a square with a piece
            (do ; the user clicked in an empty square
              (swap! board #(place-piece % @piece rc)) ; play the piece and update the board
              (.repaint this)
              (cond
                (game-over? @board) (do
                                      (announce-winner frame @board)
                                      (reset! game-over true))
                (empty? (take 1 (valid-moves @board (opposite-piece @piece)))) (popup frame "Still your turn!")
                :else (do
                        (swap! piece opposite-piece)
                        (.repaint this))))))) ;; we've switched players, so let's redraw to update the active player text
      (mouseReleased [e]) ; we're not doing anything for this event
      (mousePressed [e]) ; we're not doing anything for this event
      (mouseEntered [e]) ; we're not doing anything for this event
      (mouseExited [e])))) ; we're not doing anything for this event

;; This function starts the swing-based Reversi game. It builds the Swing
;; container and then adds to it our game panel, which contains the core of
;; our GUI logic.
(defn reversi-swing []
  "Starts a Swing-based Reversi game."
  (let [board (atom (initialize-board)) ; the active board (mutable)
        piece (atom :b) ; the active piece (mutable)
        frame (JFrame. "Reversi") ; the outer Swing container. "Reversi" appears in the title bar.
        panel (reversi-panel frame board piece)] ; the Reversi board
    (doto panel
      (.setFocusable true) ; focus can be given to this panel
      (.addMouseListener panel)) ; have the panel listen for mouse events
    (doto frame
      (.add panel) ; add our Reversi panel to the frame
      (.pack) ; size the frame to fit the preferred size of our panel
      (.setVisible true)))) ; make the frame visible