(ns org.fversnel.tictactoe.game
  (:require [clojure.core.async :as async :refer [chan go go-loop put! <! >!]]
            [org.fversnel.tictactoe.board :refer [board->str]]
            [org.fversnel.tictactoe.core :as core :refer [initial-game-state finished? winner apply-move valid-move? swap-player]]
            [org.fversnel.tictactoe.player :refer [play]]
            [org.fversnel.tictactoe.util.logging :as l :refer [log logln]]))

(defn play-game
  [& {:keys [players starting-player finish-chan logger]
      :or {starting-player :x
           finish-chan (chan 1)
           logger l/system-out-logger}}]
  (go-loop [game-state (initial-game-state {:starting-player starting-player})]
    (logln logger)
    (let [{:keys [board active-player]} game-state]
      (logln logger (board->str board))
      (if-not (finished? game-state)

        (let [move-chan (chan 1)
              play-move #(put! move-chan %)
              player (players active-player)]
          (go (play player game-state play-move))
          (let [move (<! move-chan)]
            (if (valid-move? game-state move)
              (recur (apply-move game-state move))
              (>! finish-chan {:error :invalid-move}))))

        ; else
        (>! finish-chan game-state)))))

(defn play-games
  [& {:keys [players number-of-games logger]
      :or {number-of-games (do (println "games") 1)
           logger l/system-out-logger}}]

  (let [finish-chan (chan 1)]
    (go-loop [state {:starting-player :x
                     :finished-games 0
                     :wins {:x 0 :o 0 :tie 0}}]

      (let [{:keys [starting-player finished-games wins]} state]

        (if (< finished-games number-of-games)

          (do
            (play-game
             :players players
             :starting-player (:starting-player state)
             :finish-chan finish-chan
             :logger logger)
            (let [{:keys [winner] :as game-result} (<! finish-chan)]
              (logln logger "winner" winner)
              (recur
               {:starting-player (swap-player starting-player)
                :finished-games (inc finished-games)
                :wins (update-in wins [winner] inc)})))

          ; else
          (logln logger wins))))))

(comment


  (use :reload 'org.fversnel.tictactoe.ai.brute)
  (use :reload 'org.fversnel.tictactoe.ai.stupid)
  (use :reload 'org.fversnel.tictactoe.ai.treewalking)
  (use :reload 'org.fversnel.tictactoe.game)

  (play-games :players {:x brute-force-ai :o stupid-but-legal-ai} :)
  
  (defn configure [& {:keys [debug verbose]
                      :or {debug false, verbose false}}]
    (println " debug =" debug " verbose =" verbose))


    (defn play-game2
      [& {:keys [players starting-player finish-chan logger]}
          ; :or {starting-player :x
          ;      finish-chan (do (println "aap chan") (chan 1))
          ;      logger l/system-out-logger}}
               ]
        players)
  )