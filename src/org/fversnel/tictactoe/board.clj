(ns org.fversnel.tictactoe.board
  (:require [org.fversnel.tictactoe.util.transducers :as t]
            [org.fversnel.tictactoe.util.memoize :refer [defn-memoize]]))

;
; Board creation
;

(def empty-token :_)

(def player-tokens [:x :o])

(def tokens (concat player-tokens [empty-token]))

(defn create-board [on-spot board-size]
  (mapv
    (fn [y] 
      (mapv 
        (fn [x] (on-spot {:x x :y y})) 
        (range board-size)))
    (range board-size)))

(def empty-board 
  (partial create-board (constantly empty-token)))

(def random-board
  (partial create-board (fn [_] (rand-nth tokens))))

(def random-full-board
    (partial create-board (fn [_] (rand-nth player-tokens))))

;
; Lookup and manipulation
;

(defn fill-spot 
  [board {:keys [x y]} token]
  (assoc-in board [y x] token))

(defn spot 
  [board {:keys [x y]}]
  (-> board (nth y) (nth x)))


;
; Coordinate functions
;

(defn- row-coords [board-size]
  (map
    (fn [y]
      (map 
        (fn [x] {:x x :y y})
        (range board-size)))
    (range board-size)))

(defn- diagonal-coords [board-size]
  [(map
    (fn [x] {:x x :y x})
    (range board-size))
   (map
    (fn [x] {:x x :y (- board-size 1 x)})
    (range board-size))])

(defn- column-coords [board-size]
  (map
    (fn [x]
      (map 
        (fn [y] {:x x :y y})
        (range board-size)))
    (range board-size)))

(defn-memoize coords [board]
  (let [board-size (count board)]
    (vec 
      (for [x (range board-size)
            y (range board-size)]
        {:x x :y y}))))


;
; Board test functions
;

(defn- token-line? 
  "Determines if a line on the board solely consists of one token type (e.g. :x)"
  [board token line-coords]
  (t/every?
   (map (partial spot board))
   (partial = token)
   line-coords))

(defn-memoize line-coords 
  [board-size]
  (concat 
    (row-coords board-size)
    (column-coords board-size)
    (diagonal-coords board-size)))

(defn filled-line? 
  "Returns true if one of the lines on the board solely consist of one token type (e.g. :x)"
  [board token]
  (let [board-size (count board)]
    (t/some
      (map (partial token-line? board token))
      identity
      (line-coords board-size))))

(defn full-board?
  "Returns true if the board doesn't contain an empty token (i.e. :_)"
  [board]
  (t/every?
    cat
    (partial not= empty-token)
    board))

;
; String functions
;

(def ^{:private true} cell->str name)

(def ^{:private true} row->str-xf
  (comp
   (map cell->str)
   (interpose \space)))

(def ^{:private true} board->str-xf
  (comp
   (map (partial eduction row->str-xf))
   (interpose [\newline])
   cat))

(defn board->str [board]
  (transduce
    board->str-xf
    str
    board))

(defn print-board [board]
  (println 
    (board->str board)))