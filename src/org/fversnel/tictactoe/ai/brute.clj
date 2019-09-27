(ns org.fversnel.tictactoe.ai.brute
  (:require [org.fversnel.tictactoe.ai.treewalking :as t]
            [org.fversnel.tictactoe.player :refer [Player]]))

(def brute-force-ai
  (reify Player

    (player-name [_] "BruteForceAI")

    (play
      [_ game-state play-move]
      (play-move (t/best-move game-state)))))
