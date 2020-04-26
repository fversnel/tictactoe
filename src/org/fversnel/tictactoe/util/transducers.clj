(ns org.fversnel.tictactoe.util.transducers
  (:refer-clojure :exclude [distinct? every? not-every? some not-any? empty?]))

(defn distinct?
  "Returns true if no two of the arguments are =.
  Like clojure.core/distinct? but uses a transducer instead."
  ([coll]
   (distinct? identity coll))

  ([xform coll]
   (transduce
    xform
    (completing
     (fn [{:keys [seen] :as state} value]
       (if (seen value)
         (reduced {:distinct? false})
         (do (conj! seen value)
             state)))
     :distinct?)
    {:seen (transient #{}) :distinct? true}
    coll)))

(defn empty?
  "Returns true if coll has no items.
  Like clojure.core/empty? but uses a transducer instead."
  ([coll]
   (empty? identity coll))

  ([xform coll]
   (transduce
    xform
    (completing
     (fn [_ _]
       (reduced false)))
    true
    coll)))

(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false. Like clojure.core/every? but uses a transducer instead."
  ([pred coll]
   (every? identity pred coll))

  ([xform pred coll]
   (transduce
    xform
    (completing
     (fn [_ value]
       (if (pred value)
         true
         (reduced false))))
    true
    coll)))

(defn not-every?
  "Returns false if (pred x) is logical true for every x in
  coll, else true.
  Like clojure.core/not-every? but uses a transducer instead."
  ([pred coll]
   (not-every? identity pred coll))

  ([xform pred coll]
   (not (every? xform pred coll))))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil.  One common idiom is to use a set as pred, for example
  this will return :fred if :fred is in the sequence, otherwise nil:
  (some #{:fred} coll)
  Like clojure.core/some but uses a transducer instead."
  ([pred coll]
   (some identity pred coll))

  ([xform pred coll]
   (transduce
    xform
    (completing
     (fn [_ value]
       (when (pred value)
         (reduced value))))
    nil
    coll)))

(defn not-any?
  "Returns false if (pred x) is logical true for any x in coll
  else true.
  Like clojure.core/not-any? but uses a transducer instead"
  ([pred coll]
   (not-any? identity pred coll))

  ([xform pred coll]
   (not (some xform pred coll))))

