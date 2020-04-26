(ns org.fversnel.tictactoe.ai.treewalking
  (:require [clojure.zip :as z]
            [org.fversnel.tictactoe.core :as core]))

(defn game-state-zipper [game-state]
  (z/zipper
   (complement core/finished?)
   core/possible-states
   (fn [node _] node)
   game-state))

(defn game-verdict
  [us depth {:keys [moves] :as game-state}]
  (let [winner (core/winner game-state)
        opponent (core/swap-player us)
        score (condp = winner
                us (- 100 depth) 
                opponent (- depth 100)
                :tie depth
                (throw (Exception. "Cannot score a game that isn't over yet")))]
    {:moves moves :score score}))

(defn minimax 
  "game-state->verdict is function that takes the current depth in the tree and a node (a game state) and produces a map 
   with at least {:score number}. You're free to add more information to that map as you see fit.
  
   game-state-zipper is a zipper around the game-state such the minimax knows how to walk the game tree"
  [game-state->verdict game-state-zipper]
  (let [get-verdict (comp ::verdict meta)
        
        choose-verdict 
        (fn [maximizing? row-nodes]
          (->> row-nodes
               (sort-by 
                (comp :score get-verdict)
                (if maximizing? > <))
               first
               get-verdict))

        assoc-verdict
        (fn [loc verdict]
          (z/edit loc with-meta {::verdict verdict}))]

    (loop [tree game-state-zipper
           {:keys [α β] :as state} {:α ##-Inf :β ##Inf}]
      ;; (println state)
      
      (let [current-node (z/node tree)
            verdict (get-verdict current-node)
            depth (count (z/path tree))
            ;; Starting from the root, the next level in the tree is always
            ;; the move of the maximizing player
            ;; Thus the maximizing player is on all the odd levels (1, 3 etc.) of the tree
            ;; and the minimizing player is on all the even levels (0, 2 etc.) of the tree
            maximizing? (odd? depth)
            right-node (z/right tree)
            down-node (z/down tree)
            right-most-sibling? (nil? right-node)
            leaf? (nil? down-node)
            root? (= depth 0)]

        (if verdict
          (cond
            root?
            verdict
            
            right-most-sibling?
            (let [row-nodes (conj (z/lefts tree) current-node)
                  verdict (choose-verdict maximizing? row-nodes)]
              (recur (assoc-verdict (z/up tree) verdict) state))
            
            :else
            (recur right-node state))
          
          ;; else
          (if leaf?
            ;; Determine if you want to calculate a verdict at this position
            (let [verdict (game-state->verdict depth current-node)]
              (recur (assoc-verdict tree verdict) state))
            (recur down-node state)))))))
        

(defn αβ-pruning 
  "TODO"
  []
  (let [α ##-Inf
        β ##Inf]
    β))

(defn best-move [{:keys [moves] :as game-state}]
  (let [active-player (core/active-player game-state)
        verdict (minimax 
                 (partial game-verdict active-player) 
                 (game-state-zipper game-state))
        next-move-index (count moves)
        move (nth (:moves verdict) next-move-index)]
    (println "verdict:" verdict)
    move))

(comment
  (require :reload '[org.fversnel.tictactoe.core :as core])
  (use :reload 'org.fversnel.tictactoe.ai.treewalking)

  (def example-game 
    (reduce
     core/apply-move
     (core/initial-game-state)
      ; Best next move should be {:x 2 :y 0}
     [{:x 0 :y 0} {:x 1 :y 1} {:x 1 :y 0} {:x 2 :y 2}]))
  
  (best-move example-game)
  
  )