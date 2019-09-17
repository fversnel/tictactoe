(ns org.fversnel.tictactoe.util.transducers
  (:refer-clojure :exclude [every? not-every? some not-any? empty?]))

(defn every?
  "Like clojure.core/every? but uses a transducer instead"
  ([pred coll]
   (every? identity pred coll))

  ([xform pred coll]
   (transduce
    xform
    (completing
     (fn [acc value]
       (and acc (pred value))))
    true
    coll)))

(defn not-every?
  "Like clojure.core/not-every? but uses a transducer instead"
  ([pred coll]
   (not-every? identity pred coll))

  ([xform pred coll]
   (not (every? xform pred coll))))

(defn some
  "Like clojure.core/some but uses a transducer instead"
  ([pred coll]
   (some identity pred coll))

  ([xform pred coll]
   (transduce
    xform
    (completing
     (fn [_ value]
       (when
        (pred value)
         (reduced value))))
    nil
    coll)))

(defn not-any?
  "Like clojure.core/not-any? but uses a transducer instead"
  ([pred coll]
   (not-any? identity pred coll))

  ([xform pred coll]
   (not (some xform pred coll))))

(defn empty?
  "Like clojure.core/empty? but uses a transducer instead"
  ([coll]
   (empty? identity coll))

  ([xform coll]
   (transduce
    xform
    (completing
     (fn [_ _] (reduced false)))
    true
    coll)))