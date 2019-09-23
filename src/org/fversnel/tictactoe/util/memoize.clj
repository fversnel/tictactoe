(ns org.fversnel.tictactoe.util.memoize)

(defmacro defn-memoize
  [name args-list & body]
  `(def ~name
     (memoize
      (fn ~args-list ~@body))))