(ns org.fversnel.tictactoe.ai.stupid
  (:require
   [org.fversnel.tictactoe.core :refer [possible-moves]]
   [org.fversnel.tictactoe.player :refer [Player]]))

(defn random-move [game-state]
  (rand-nth (possible-moves game-state)))

(def stupid-but-legal-ai
  (reify Player

    (player-name [_] "StupidButLegalAI")

    (play
      [_ game-state play-move]
      (play-move (random-move game-state)))))