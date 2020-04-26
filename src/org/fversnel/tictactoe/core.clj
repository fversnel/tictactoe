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
     :moves []}))

(def empty-spot?
  (partial = :_))

(def swap-player
  {:x :o
   :o :x})

(defn active-player
  [{:keys [starting-player moves]}]
  (if (even? (count moves))
    starting-player
    (swap-player starting-player)))

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
  [{:keys [board]}]
  (or
   (player-wins? :o board)
   (player-wins? :x board)
   (tie? board)))

(defn finished?
  [game-state]
  (some? (winner game-state)))

(defn possible-moves
  [{:keys [board]}]
  (for [coord (b/coords board)
        :when (empty-spot? (b/spot board coord))]
    coord))

(defn apply-move
  [{:keys [board moves] :as game-state}
   move]
  (let [active-player (active-player game-state)
        new-board (b/fill-spot board move active-player)]
    (assoc
     game-state
     :board new-board
     :moves (conj moves move))))

(defn possible-states
  [game-state]
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