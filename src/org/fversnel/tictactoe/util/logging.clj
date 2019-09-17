(ns org.fversnel.tictactoe.util.logging)

(defprotocol Logger
  (-log [logger more])
  (-enabled? [logger]))

(def system-out-logger
  (reify Logger
    (-log [_ more]
      (doseq [e more]
        (print e))
      (flush))
    (-enabled? [_] true)))

(def no-op-logger
  (reify Logger
    (-log [_ _])
    (-enabled? [_] false)))

(defmacro log [logger & more]
  `(when (-enabled? ~logger)
     (-log ~logger [~@more])))

(defmacro logln [logger & more]
  `(log ~logger ~@(conj (vec more) \newline)))