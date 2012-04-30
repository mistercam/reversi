(ns reversi.test.functional
  (:use [reversi.datamodel])
  (:use [reversi.functional])
  (:use [clojure.test]))

(defn set-piece [board r c p]
  (assoc board r (assoc (board r) c p)))

(def fresh-board (initialize-board))
(def majority-black-board (set-piece (set-piece fresh-board 3 3 :b) 3 2 :b))
(def majority-white-board (set-piece (set-piece fresh-board 4 4 :w) 5 3 :w))

(deftest game-over-test
  (is (false? (game-over? fresh-board )) "A game is not over with a new board"))

(deftest count-pieces-test
  (is (= 2 (count-pieces fresh-board :w)) "A new board has two white pieces")
  (is (= 2 (count-pieces fresh-board :b)) "A new board has two black pieces"))

(deftest winner-test
  (is (= :w (winner majority-white-board)) "If white has more pieces, white wins")
  (is (= :b (winner majority-black-board)) "If black has more pieces, black wins")
  (is (= nil (winner fresh-board)) "If white and black have equal piece counts, then tie"))

(deftest opposite-piece-test
  (is (= :w (opposite-piece :b)) "Black is opposite of white")
  (is (= :b (opposite-piece :w)) "White is opposite of black"))