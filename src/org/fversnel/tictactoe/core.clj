(ns org.fversnel.tictactoe.core
  (:require [org.fversnel.tictactoe.board :as b]))

;(def print-board b/print-board)

(defn initial-game-state
  [& {:keys [starting-player board-size]
       :or {starting-player :x
            board-size 3}}]
   (let [initial-board (b/empty-board board-size)]
    {:initial-board initial-board
     :starting-player starting-player
     :board initial-board
     :board-size board-size
     :moves []
     :active-player starting-player}))
    
(defn empty-spot? [spot] (= spot :_))

(defn swap-player [player]
  (case player
    :x :o
    :o :x))

(defn valid-move?
  [{:keys [board]} move]
  (empty-spot? (b/spot board move)))

(defn- player-wins? 
  [player board]
  (when (b/filled-line? board player) player))

(defn- tie?
  [board]
  (when (b/full-board? board) :tie))

(defn winner
  [board]
  (or
    (player-wins? :o board)
    (player-wins? :x board)
    (tie? board)))

(defn finished? [{:keys [winner]}]
  (not (nil? winner)))

(defn possible-moves
  [{:keys [board] :as game-state}]
  (for [coord (b/coords board)
        :when (empty-spot? (b/spot board coord))]
    coord))

(defn apply-move
  [{:keys [board moves active-player] :as game-state}
   move]
  (let [new-board (b/fill-spot board move active-player)]
    (assoc
      game-state
      :board new-board
      :moves (conj moves move)
      :active-player (swap-player active-player)
      ; TODO Only assoc a winner if we have one
      :winner (winner new-board))))

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