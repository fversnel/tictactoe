(ns org.fversnel.tictactoe.player)

(defprotocol Player
  (player-name [player])
  (play [player game-state play-turn]))