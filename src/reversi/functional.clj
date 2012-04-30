(ns reversi.functional)

;; These are direction vectors that will help us navigate the game board. For any
;; position [r,c] on the board, we can travel from said position in a specific
;; direction by adding the appropriate direction vector to the position.
;;
;; E.g. If we're in row 3 and column 5 (represented by [2,4]) we can travel one
;;      position to the right (i.e. E) by adding [0,1] to [2,4] to give us [2,5]
(def directions
  {:N [-1 0] :NE [-1 1] :E [0 1] :SE [1 1] :S [1 0] :SW [1 -1] :W [0 -1] :NW [-1 -1]})

;; In order for opponent pieces to be flipped, they must be flanked by the
;; specified move and another piece belonging to the active player. Starting
;; at the position specified by 'move', we'll iteratively move outward in the
;; specified direction, checking each piece in turn. The only condition in
;; which we have a flippable piece is if we've encountered at least one
;; opponent-piece before coming across a piece belonging to the active player.
;; If we come across an empty square, then the move will not cause an opponent
;; piece to be flipped in the specified direction. Likewise, if we come across
;; a piece belonging to the active player, and we're only one square-away from
;; the originating move (in any direction,) then there's no opponent-pieces to
;; be flipped in the specified direction.
(defn flippable? [board piece move direction]
  "Examines each square on the board moving outward from the specified
  move in the specified direction, looking for an opponent-piece that
  could be flipped as a result of the move. If a flippable piece is found,
  returns true, else returns false."
  (loop [m (vec (map + move direction))] ; m is the current square
    (let [r (m 0)
          c (m 1)
          rd (Math/abs (- (move 0) r)) ; num rows between originating square and current square
          cd (Math/abs (- (move 1) c))] ; num cols between originating square and current square
      (cond
        (or (> r 7) (< r 0) (> c 7) (< c 0)) false ; 'm' is outside the board
        (= nil ((board r) c)) false ; 'm' is empty
        (= piece ((board r) c)) (or (> rd 1) (> cd 1)) ; return false if we're 1 move away from origin, else true
        :else (recur (vec (map + m direction))))))) ; start next iteration of loop with 'm' set to next square

;; With the flippable? method defined, we can check to see if a move is valid simply
;; by checking each direction moving outward from the specified move to see if any
;; will result in a piece being flipped. Since we only care whether there *is a* piece
;; that can be flipped, and not how many can be flipped, we make each call to
;; flippable? within an or-statement, which will return as soon as one of the calls
;; to flippable? returns true
(defn move-valid? [board piece move]
  "Returns true if the specified move for the specified piece will cause
  a piece to be flipped in any direction."
  (let [r (move 0) c (move 1)]
    (cond
      (or (> r 7) (< r 0) (> c 7) (< c 0)) nil
      (not= nil ((board r) c)) nil
      :else (or
              (flippable? board piece move (directions :N ))
              (flippable? board piece move (directions :NE ))
              (flippable? board piece move (directions :E ))
              (flippable? board piece move (directions :SE ))
              (flippable? board piece move (directions :S ))
              (flippable? board piece move (directions :SW ))
              (flippable? board piece move (directions :W ))
              (flippable? board piece move (directions :NW ))))))

;; This returns the set of squares where 'piece' could be placed that would cause
;; an opponent-piece to be flipped
(defn valid-moves [board piece]
  "Returns the set of valid moves for the given piece"
  (for [r (range 8) c (range 8) ; for each permutation of r and c
        :let [move [r c]]
        :when (move-valid? board piece move)]
    move))

;; This takes a board, and if the specified move and piece can be played, returns
;; a *new* board with the piece played and all resulting opponent-pieces flipped
;; moving outward in the specified direction from where the piece was played
(defn flip [board piece move direction]
  "Returns a new board with results of playing the specified move and piece"
  (if (flippable? board piece move direction)
    (loop [b board m (vec (map + move direction))] ; bind board to b so we can use it in recur
      (let [r (m 0)
            c (m 1)
            p ((board r) c)]
        (if (= p piece)
          b ; we've come to a piece the same colour as the active piece, so we're done flipping. Return the modified board.
          (recur (assoc b r (assoc (b r) c piece)) (vec (map + m direction)))))) ; more flipping to be done, so flip and recur.
    board)) ; move+piece won't cause a flip, so just return the board

;; To avoid using a lexical-scoped atom (which we could then change after each call to
;; the flip function,) we instead create a new function via 'comp' that will modify
;; the board as it's passed into and returned from functions.
;;
;; For the sake of argument, let f_N, f_NE, f_E, f_SE, f_S, f_SW, f_W, and f_NW be
;; functions. Each function accepts a board, flips the appropriate pieces on the
;; board that results from playing 'piece' and 'move', and then returns the new, modified
;; board. Then the function we've made through 'comp' would be like calling:
;;
;; X(f_N(f_NE(f_E(f_SE(f_S(f_SW(f_W(f_NW(board)))))))))
;;
;; The 'X' function in our example call above is the assoc call we make below, which
;; actually places the piece on the board before returning it.
;;
;; #(...) is a reader macro (i.e. short form) for the fn(...) function, and the % is a
;; place-holder for the first argument passed into the function.
(defn place-piece-2 [board piece move]
  "Returns a new board that results from playing the specified piece and move."
  (if (move-valid? board piece move)
    (let [r (move 0) c (move 1)]
      ((comp
         #(assoc % r (assoc (% r) c piece))
         #(flip % piece move (directions :N ))
         #(flip % piece move (directions :NE ))
         #(flip % piece move (directions :E ))
         #(flip % piece move (directions :SE ))
         #(flip % piece move (directions :S ))
         #(flip % piece move (directions :SW ))
         #(flip % piece move (directions :W ))
         #(flip % piece move (directions :NW )))
        board)))) ; the move and piece were not valid, so just return the board unchanged

(defn place-piece [board piece move]
  "Returns a new board that results from playing the specified piece and move."
  (if (move-valid? board piece move)
    (let [r (move 0) c (move 1) b (atom board)]
      (swap! b #(assoc % r (assoc (% r) c piece)))
      (swap! b #(flip % piece move (directions :N )))
      (swap! b #(flip % piece move (directions :NE )))
      (swap! b #(flip % piece move (directions :E )))
      (swap! b #(flip % piece move (directions :SE )))
      (swap! b #(flip % piece move (directions :S )))
      (swap! b #(flip % piece move (directions :SW )))
      (swap! b #(flip % piece move (directions :W )))
      (swap! b #(flip % piece move (directions :NW ))))
    board))

;; The game is over when neither player has any remaining moves to make
(defn game-over? [board]
  "Returns true if there are no more moves to be made on the board, else returns false"
  (and
    (empty? (valid-moves board :w ))
    (empty? (valid-moves board :b ))))

;; We need to know how many pieces of a specified colour are on the board so we can
;; declare who the winner is. We do this by collapsing all of the nested vectors in
;; our board, discarding all elements that != our piece, then taking the size of the
;; resulting collection
(defn count-pieces [board piece]
  "Returns the number of squares occupied by the specified piece"
  (count (filter #(= % piece) (flatten board))))

;; Straightforward
(defn winner [board]
  "Returns the winner, or nil if there's a tie"
  (let [w (count-pieces board :w ) b (count-pieces board :b )]
    (cond
      (> w b) :w (> b w) :b :else nil)))

;; Straightforward
(defn opposite-piece [piece]
  "Returns the opposing-piece of the specified piece"
  (if (= :w piece) :b :w ))