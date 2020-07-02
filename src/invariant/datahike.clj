(ns invariant.datahike
  (:refer-clojure :exclude [+])
  (:require [clojure.edn    :as edn]
            [datahike.api   :as d]
            [datahike.core  :as dc]
            [datahike.query :as dq]
            [invariant.core]
            [invariant.query
             :refer [assert-valid-query invariant-query]]))

(alter-var-root #'dq/built-ins assoc 'subquery datahike.api/q)

(defn + [db eid attr delta]
  (let [m (d/pull db [attr] eid)
        v (attr m 0)]
    [[:db/add eid attr (clojure.core/+ v delta)]]))

(defn get-attribute-dispatch [v]
  (let [[a b] v]
    (cond (= :db.fn/call a) [:db.fn/call b]
          :else             a)))

(defmulti get-attribute get-attribute-dispatch)

(defmethod get-attribute [:db.fn/call +]
  [[_ _ eid attr delta]]
  attr)

(defmethod get-attribute :db/add
  [[_ e a v]]
  a)

(defn- invariant-holds? [inv-qs conn tx-data schema]
  (d/q (edn/read-string inv-qs)
       ;; current state
       @conn
       ;; apply transaction to current state
       (dc/db-with @conn tx-data)
       ;; empty database with only transaction applied
       (dc/db-with (dc/empty-db) (concat schema tx-data))
       tx-data))

(defn assert-invariants [conn tx-data schema]
  (let [attr-txs (for [tx tx-data]
                   [(get-attribute tx) tx])
        attrs    (distinct (map first attr-txs))]
    (doseq [[a tx] attr-txs
            :when  (= a :invariant/query)
            :let   [[_ _ _ v] tx]]
      (assert-valid-query (edn/read-string v)))

    (doseq [a     attrs
            :let  [inv-qs (d/q invariant-query @conn a)]
            :when inv-qs]
      (when-not (invariant-holds? inv-qs conn tx-data schema)
        (throw (ex-info "Invariant mismatch."
                 {:type      :invariant/invariant-mismatch
                  :attribute a
                  :invariant (edn/read-string inv-qs)
                  :tx-data   tx-data}))))
    true))

(defmethod invariant.core/invariant :datahike
  [conn schema tx-data]
  (assert-invariants conn tx-data schema))
