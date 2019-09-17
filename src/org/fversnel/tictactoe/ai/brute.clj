(ns org.fversnel.tictactoe.ai.brute
  (:require 
    [clojure.walk :as w]
    [org.fversnel.tictactoe.core :refer [possible-states swap-player winner]]
    [org.fversnel.tictactoe.player :refer [Player]]))

(defn best-move [moves]
  (first (sort-by :score < moves)))

(defn worst-move [moves]
  (first (sort-by :score > moves)))

; game-state -> {:score number :move {:x number :y number}}
(defn score 
  ([game-state]
    (let [us (:active-player game-state)
          initial-depth 0]
      (score game-state us initial-depth)))
  
  ([game-state us depth]
    (let [opponent (swap-player us)
          move (last (:moves game-state))]

      (condp = (winner game-state)

        us 
        {:score 100 :move move} ;(- 100 depth)

        opponent 
        {:score -100 :move move} ;(- depth 100)
      
        :tie 
        {:score 0 :move move}

        :none
        (let [scored-moves 
              (map 
                (fn [state]
                  (score state us (inc depth)))
                (possible-states game-state))]
          (condp = (:active-player game-state)
            us (best-move scored-moves)
            opponent (worst-move scored-moves)))))))

(comment
  (doseq [next-state (:possible-states (possible-states g1))
          :let [move (last (:moves next-state))
                move-score (score next-state)]]
    (println move "scores" move-score))
)

(def brute-force-ai
  (reify Player

    (player-name [_] "BruteForceAI")

    (play
			[_ game-state play-move]
			(play-move (:move (score game-state))))))
