
(defn tree-test []

  (let [tree {:moves []
              :nodes 
               [{:moves [:w]
                 :nodes 
                  [{:moves [:w :x1]
                    :nodes
                    [{:moves [:w :x1 :y]
                      :nodes 
                      [{:verdict {:score 7
                                  :moves [:w :x1 :y :z1]}} 
                       {}
                       {:verdict {:score 8
                                  :moves [:w :x1 :y :z2]}}]}
                    {:verdict {:score -5
                               :moves [:w :x2]}}
                    {:verdict {:score -7
                               :moves [:w :x3]}}]}]}
                 {:verdict {:score 6
                            :moves [:w1]}}]}
        zipped-tree (z/zipper 
                      :nodes
                      :nodes
                      (fn [branch children] 
                        (assoc branch :nodes children))
                      tree)

        propagate-verdict 
        (fn [current-node up-loc row-nodes]
          ; (println \newline "current-node " current-node \newline "row-nodes" row-nodes)
          ; Select the best move (move with highest/lowest score)
          (let [best-verdict (->> row-nodes
                              (sort-by (comp :score :verdict) >)
                              first)]
            ; (println "best verdict" best-verdict)
            (z/edit up-loc merge best-verdict)))

        calculate-verdict
        (fn [current-node]
          (assoc current-node :verdict {:score 42}))
          
        ; row-score (fn [nodes]
        ;             (transduce
        ;               (map :score)
        ;               +
        ;               nodes))
        ; assoc-score (fn [node score]
        ;               (assoc node :score score))
                      ]
    (loop [tree zipped-tree]
      ; Is there a score attribute?
      (let [current-node (z/node tree)
            has-verdict? (contains? current-node :verdict)
            right-most-sibling? (nil? (z/right tree))
            leaf? (nil? (z/down tree))
            root? (nil? (z/up tree))
            depth (count (z/path tree))]

        ; (println {:has-verdict? has-verdict?
        ;           :right-most-sibling? right-most-sibling?
        ;           :leaf? leaf?
        ;           :root? root?
        ;           :current-node current-node})

        (cond
          (and has-verdict? root?)
            (:verdict current-node)

          (and has-verdict? right-most-sibling?)
            (let [up-loc (z/up tree)
                  row-nodes (conj (z/lefts tree) current-node)
                  new-up-loc (propagate-verdict current-node up-loc row-nodes)]
              ; (println "new node" (z/node new-up-loc) \newline)
              (recur new-up-loc))

          (and has-verdict?)
            (recur (z/right tree))

          ; Determine if you want to calculate a verdict at this position
          (and (not has-verdict?) leaf?)
          (let [new-tree (z/edit tree calculate-verdict)]
            (recur new-tree))

          (not has-verdict?)
            (recur (z/down tree))

          :else 
            (throw (Exception. (str "Cannot handle node " current-node)))))))

          ; end of function
            )



