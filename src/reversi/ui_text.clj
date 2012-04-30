(ns reversi.ui_text
  (:require [clojure.string :as string])
  (:use reversi.datamodel)
  (:use reversi.functional))

;; We only require character labels for columns in the text UI because the
;; user will be required to enter in a move through the console. If we used numbers
;; to represent columns, then the user could easily forget what it means to specify
;; a move of [3 5]. Did they intend to place a piece on the 4th row and 6th column?
;; or the 6 row and 4th column? With a numeric row and character column, we can let
;; the user enter in input such as 2A, E5, 3f, d4.
(def columns
  {"A" 0 "B" 1 "C" 2 "D" 3 "E" 4 "F" 5 "G" 6 "H" 7})

;; This simply recurs over the specified row, printing the text representation
;; of the head of the row on each pass.
(defn print-cols [row]
  "Prints the contents of the each square in the specified row to standard
  out."
  (if (not (empty? row))
    (let [c (first row)]
      (cond
        (= :w c) (print "W")
        (= :b c) (print "B")
        :else (print "."))
      (recur (rest row)))))

;; This prints each row to standard out. We prepend and append a row id to
;; each row to assist the user in determining what row their potential move
;; resides in. E.g. The 4th row of a new board will be printed as "3 [...WB...] 3"
(defn print-rows [board]
  "Prints each row on the specified board to standard out, along with a
  numerical row identifier."
  (let [r (first board) row-id (- 8 (count board))]
    (if (not (empty? r)) ; if there's still rows to print
      (do
        (print row-id "[ ") ; prepend the row number
        (print-cols r) ; print the row contents
        (print " ]" row-id) ; append the row number
        (newline) ; print a new line
        (recur (rest board))))))

;; This prints the entire board to standard out. Before and after printing the board,
;; we print an additional header and footer which lists the column identifiers. This
;; will assist the user in determining what column their potential move resides in.
(defn print-board [board]
  "Prints the specified board to standard out"
  (let [header (str "    " (apply str (keys columns)))]
    (println header) ; print column labels
    (print-rows board) ; print all of the rows
    (println header))) ; print column labels

;; The user is supposed to enter something such as 2E, E2, 2e, or e2, but there's
;; nothing stopping them from entering something else. This simply determines if the
;; user's input is valid
(defn malformed-input? [input]
  "Returns true if the input is valid, otherwise false."
  (not (re-find (re-matcher #"^([0-7][A-Ha-h])$|^([A-Ha-h][0-7])$" input))))

;; While the user enters in moves such as 2E, our functional layer views moves
;; in terms of vectors of numbers. Here we convert input such as 2E to its
;; numerical coordinate vector ([1 4] in the case of 2E)
(defn parse-input [input]
  "Converts the user input into a coordinate vector"
  (let [f (str (first input)) l (str (last input))]
    (if (Character/isDigit (first input))
      [(Integer/parseInt f) (columns (string/upper-case l))] ; e.g 2E
      [(Integer/parseInt l) (columns (string/upper-case f))]))) ; e.g E2

;; This function prompts the user for input, and keeps prompting them until they
;; enter a valid move, or "quit"
(defn prompt-for-move [piece board]
  "Prompts the user for a move. Returns the move, if-valid. Returns nil
  if user has quit."
  (loop [input (atom "")]
    (if (= :w piece) ; Prints an input prompt
      (print "White's Turn. Enter move: ")
      (print "Black's Turn. Enter move: "))
    (flush) ; We printed the prompt with a 'print' instead of 'println'. Flushing forces the prompt to appear.
    (reset! input (str (read-line))) ; Read input and store back into 'input'
    (cond
      (= "quit" (string/lower-case @input)) nil
      (malformed-input? @input) (do
                                  (println "Malformed input! Try again.")
                                  (recur input))
      (not (move-valid? board piece (parse-input @input))) (do
                                                             (println "Invalid move! Try again.")
                                                             (recur input))
      :else (parse-input @input)))) ; Input is OK, so return a move

;; This function analyzes the board to determine who the winner is, and then
;; prints this to standard out
(defn print-winner [board]
  "Prints game over to standard out and declares the winner"
  (let [p (winner board)] ; 'p' is the piece with the most coverage
    (cond
      (= p :b ) (println "Game over. Black Wins!")
      (= p :w ) (println "Game over. White Wins!")
      :else (println "Game over. Tie!"))))

;; This function starts the text-based Reversi game.
(defn reversi-text []
  "Starts a text-based Reversi game."
  (let [board (atom (initialize-board)) ; the active board (mutable)
        piece (atom :b ) ; the active player (mutable)
        game-over (atom false) ; game state (mutable)
        execute-move (fn [move] (if move
                                  (do
                                    (swap! board (fn [b] (place-piece b @piece move))) ; make the move and update board
                                    (cond
                                      (game-over? @board) (do
                                                            (print-board @board)
                                                            (print-winner @board)
                                                            (reset! game-over true))
                                      (empty? (take 1 (valid-moves @board (opposite-piece @piece)))) (println "Still your turn!")
                                      :else (swap! piece opposite-piece)))
                                  (reset! game-over true)))]
    (while (not @game-over)
      (do
        (print-board @board)
        (execute-move (prompt-for-move @piece @board))))))