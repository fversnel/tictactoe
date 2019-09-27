(ns org.fversnel.tictactoe.ai.treewalking
  (:require [clojure.zip :as z]
            [org.fversnel.tictactoe.core :as core]))

(defn game-state-zipper [game-state]
  (z/zipper
    (fn [game-state] (not (:winner game-state)))
    (fn [game-state] (core/possible-states game-state))
    (fn [node _] node)
    game-state))

(defn game-verdict
  [us {:keys [moves winner] :as game-state}]
  (let [depth (count moves)
        opponent (core/swap-player us)
        score (condp = winner
                us (- 100 depth) 
                opponent (- depth 100)
                :tie depth
                (throw (Exception. "Cannot score a game that isn't over yet")))]
    {:moves moves :score score}))

(defn minimax 
  "game-state->verdict is function that takes a node (a game state) and produces a map {:score number}
  
   game-state-zipper is a zipper around the game-state such the minimax knows how to walk the game tree
   
   The game-state itself must be presented as a map"
  [game-state->verdict game-state-zipper]
  (let [choose-verdict 
        (fn [maximizing? row-nodes]
          (->> row-nodes
            (sort-by 
              (comp :score ::verdict)
              (if maximizing? > <))
            first
            ::verdict))

        assoc-verdict
        (fn [loc verdict]
          (z/edit loc assoc ::verdict verdict))]

    (loop [tree game-state-zipper]
      
      (let [current-node (z/node tree)
            verdict (::verdict current-node)
            depth (count (z/path tree))
            ; Starting from the root, the next level in the tree is always
            ; the move of the maximizing player
            ; Thus the maximizing player is on all the odd levels (1, 3 etc.) of the tree
            ; and the minimizing player is on all the even levels (0, 2 etc.) of the tree
            maximizing? (odd? depth)
            right-node (z/right tree)
            down-node (z/down tree)
            right-most-sibling? (nil? right-node)
            leaf? (nil? down-node)
            root? (= depth 0)]

        (cond
          (and verdict root?)
            (::verdict current-node)

          (and verdict right-most-sibling?)
            (let [row-nodes (conj (z/lefts tree) current-node)
                  verdict (choose-verdict maximizing? row-nodes)]
              (recur (assoc-verdict (z/up tree) verdict)))

          (and verdict)
            (recur right-node)

          ; Determine if you want to calculate a verdict at this position
          (and (not verdict) leaf?)
            (let [verdict (game-state->verdict current-node)]
              (recur (assoc-verdict tree verdict)))

          (not verdict)
            (recur down-node)

          :else 
            (throw (Exception. (str "Cannot handle node " current-node))))))))

(defn αβ-pruning 
  "TODO"
  []
  (let [α ##-Inf
        β ##Inf]
    β))

(defn best-move [{:keys [active-player moves] :as game-state}]
  (let [verdict (minimax 
                  (partial game-verdict active-player) 
                  (game-state-zipper game-state))
        next-move-index (count moves)
        move (nth (:moves verdict) next-move-index)]
    (println "verdict:" verdict)
    move))

(comment
  (use :reload 'org.fversnel.tictactoe.ai.treewalking)

  (def example-game 
    (reduce
      core/apply-move
      (core/initial-game-state)
      ; Best next move should be {:x 2 :y 0}
      [{:x 0 :y 0} {:x 1 :y 1} {:x 1 :y 0} {:x 2 :y 2}]))
  
  
  )