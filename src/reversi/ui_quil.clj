(ns reversi.ui_quil
  (:use quil.core)
  (:use reversi.datamodel)
  (:use reversi.functional))

(def footer-height 30) ; space below the board to show the current player
(def piece-size 70) ; the width and height of a board square
(def gap-size 5) ; the vertical and horizontal spacing between squares
(def board-size (+ (* 8 piece-size) (* 7 gap-size))) ; the board size
(def padding 5) ; the spacing within a square that surrounds a piece
(def message-width (- board-size 80)) ; the width of the message box
(def message-height 80) ; the height of the message box
(def message-time 3) ; amount of time in seconds to display a message
(def fps 10) ; the number of times per second are sketch will be redrawn
(def color-black [0 0 0])
(def color-grey [200 200 200])
(def color-white [255 255 255])
(def color-green [0 136 0])

;; If the counter in msg is not 0, then we draw the message to the screen
;; and decrement the counter.
(defn draw-message [msg]
  "Displays the specified message in the middle of the sketch."
  (if (not= 0 (@msg 0))
    (let [mx (quot (- board-size message-width) 2)
          my (quot (- board-size message-height) 2)]
      (do
        (apply fill color-grey)
        (rect mx my message-width message-height) ; make a black box centered on the board
        (apply fill color-black) ; make the text yellow
        (text-size 60)
        (text-align :center :center ) ; center text horizontally and vertically within box
        (text (@msg 1) mx my message-width message-height) ; write text in box
        (swap! msg #(assoc % 0 (dec (% 0))))))))

(defn draw-player [piece]
  "Draws a message below the board indicating the current player."
  (let [p (if (= :w piece) "WHITE" "BLACK")]
    (apply fill color-white) ; make the text white
    (text-size 14)
    (text-align :left :baseline)
    (text (str "CURRENT PLAYER: " p) 10 (+ board-size (quot footer-height 2)))))

(defn draw-piece [r c piece]
  "Draws a white or black piece in the square of the specified row
  and column."
  (if (not= nil piece)
    (let [color (if (= :w piece) color-white color-black)]
      (apply fill color) ; sets the current fill-color to white or black
      (ellipse-mode :corner ) ; the (x,y)-coord in our call to ellipse below specifies the top left corner of bounding box
      (ellipse
        (+ (* c piece-size) (* c gap-size) padding) ; x
        (+ (* r piece-size) (* r gap-size) padding) ; y
        (- piece-size (* 2 padding)) ; width
        (- piece-size (* 2 padding)))))) ; height

(defn draw-square [board r c]
  "Draws the square for the specified row and column."
  (apply fill color-green) ; sets the current fill-color to green
  (rect
    (+ (* c piece-size) (* c gap-size)) ; x
    (+ (* r piece-size) (* r gap-size)) ; y
    piece-size ; width
    piece-size) ; height
  (draw-piece r c ((board r) c)))

;; This function draws the square for every permutation of row and column. We also print
;; the active player in the footer.
(defn draw-board [board piece msg]
  "Draws the Reversi game board."
  (apply background color-black)
  (doseq [r (range 8) c (range 8)]
    (draw-square @board r c))
  (draw-player @piece)
  (draw-message msg))

;; Our sketch is configured to be redrawn at a rate of 'fps' frames per second.
;; If we want a message to appear on the screen for 5 seconds, then it must be
;; printed to the screen over the course of 5*fps redraws
(defn set-message [m message]
  "Specifies a new message to be drawn to the screen, and the length of time it
  remain."
  (let [seconds (* message-time fps)]
    (reset! m [seconds message])))

;; This method stores the winning-game text into 'msg' so draw-board can
;; display it
(defn announce-winner [msg board]
  "Sets the winner of the Reversi game"
  (let [p (winner board)]
    (cond
      (= :w p) (set-message msg (str "White Wins!"))
      (= :b p) (set-message msg (str "Black Wins!"))
      :else
      (set-message msg "It's a Tie!"))))

;; This function accepts a mouse position and returns a vector of the
;; corresponding row and column. Note that an x-position translates to
;; a column, and a y-position translates to a row.
(defn parse-mouse-pos [x y]
  "Returns the row and column corresponding to the mouse position."
  [(quot y (+ piece-size gap-size))
   (quot x (+ piece-size gap-size))])

;; This function fires when the user clicks the mouse. When this happens
;; the mouse position is mapped to a row and column on the game board. If the
;; row and column are valid, and the move is valid, the piece is played
;; and the board is updated.
(defn mouse-clicked [board piece game-over msg]
  "Attempts to play the specified piece in the square the user clicked."
  (if (= 0 (@msg 0)) ; handle mouse click only if we're not showing a message
    (let [move (parse-mouse-pos (mouse-x) (mouse-y))]
      (if (move-valid? @board @piece move)
        (do
          (swap! board #(place-piece % @piece move))
          (cond
            (game-over? @board) (do
                                  (announce-winner msg @board)
                                  (reset! game-over true))
            (empty? (take 1 (valid-moves @board (opposite-piece @piece)))) (set-message msg "Still your turn!")
            :else (swap! piece opposite-piece)))
        (set-message msg "Invalid move!")))))

(defn setup []
  "Configures the properties of our sketch."
  (smooth) ; turns on anti-aliasing
  (frame-rate fps) ; the number of times per second we'll redraw the board
  (apply background color-black)) ; sets the background

(defn reversi-quil []
  "Starts a quil-based Reversi game."
  (let [board (atom (initialize-board))
        piece (atom :b )
        game-over (atom false)
        msg (atom [0 nil])]
    (defsketch reversi ; Define a new Reversi sketch
      :title "Reversi"
      :setup setup ; specifies the function used to initializes the sketch
      :draw (partial draw-board board piece msg) ; specifies the draw function
      :mouse-clicked (partial mouse-clicked board piece game-over msg) ; specifies the mouse clicked handler
      :size [board-size (+ board-size footer-height)]))) ; specifies the size of the sketch