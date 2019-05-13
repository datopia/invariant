(ns invariant.core)

(defn invariant-dispatch [connection _ _]
  (cond (and (:schema connection)
             (:rschema connection))
        :datahike

        (= (str (type connection)) "class datomic.peer.LocalConnection")
        :datomic))

(defmulti invariant invariant-dispatch)
