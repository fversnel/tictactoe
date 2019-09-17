(ns org.fversnel.tictactoe.core
  (:require [org.fversnel.tictactoe.board :as b]))

(defn initial-game-state
  ([]
   (initial-game-state {}))

  ([{:keys [starting-player board-size]
     :or {starting-player :x
          board-size 3}}]
   (let [initial-board (b/empty-board board-size)]
    {:initial-board initial-board
     :starting-player starting-player
     :board initial-board
     :moves []
     :active-player starting-player})))
    
(defn empty-spot? [spot] (= spot :_))

(defn swap-player [player]
  (case player
    :x :o
    :o :x))

(defn apply-move-to-board 
  [board move player]
  (b/fill-spot board move player))

(defn apply-move
  [{:keys [board moves active-player] :as game-state}
   move]
  (assoc
   game-state
   :board (apply-move-to-board board move active-player)
   :moves (conj moves move)
   :active-player (swap-player active-player)))

(defn valid-move?
  [{:keys [board]} move]
  (empty-cell? (b/spot board move)))

(defn player-wins? 
  [player {:keys [board]}]
  (when (b/filled-line? board player) player))

(defn tie?
  [{:keys [board]}]
  (when (b/full-board? board) :tie))

(defn winner
  [{:keys [board] :as game-state}]
  (or
    (player-wins? :o game-state)
    (player-wins? :x game-state)
    (tie? game-state)
    :none))

(defn finished? [game-state]
  (not= (winner game-state) :none))

(defn possible-moves
  [{:keys [board] :as game-state}]
  (for [x (range (count board))
        :let [row (nth board x)]
        y (range (count row))
        :let [cell (nth row y)]
        :when (empty-cell? cell)]
    {:x x :y y}))

(defn possible-states [game-state]
  (for [move (possible-moves game-state)]
    (apply-move game-state move)))

; (defn possible-moves
;   [{:keys [board]}]
;   (eduction
;     (filter 
;       (fn [coord]
;         (let [spot (b/spot board coord)]
;           (empty-spot? spot))))
;     (b/coords board)))


; clojure.zip/zipper
; ([branch? children make-node root])
;   Creates a new zipper structure. 

;   branch? is a fn that, given a node, returns true if can have
;   children, even if it currently doesn't.

;   children is a fn that, given a branch node, returns a seq of its
;   children.

;   make-node is a fn that, given an existing node and a seq of
;   children, returns a new branch node with the supplied children.
;   root is the root node.



(zipper
  finished?
  possible-states
  
  (initial-game-state)
  
  )



;(def print-board b/print-board)