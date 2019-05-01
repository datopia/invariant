(ns invariant.datahike
  (:refer-clojure :exclude [+])
  (:require [datahike.api :as d]
            [datahike.core :as dc]
            [datahike.db :as ddb]
            [datahike.query :as dq]
            [invariant.core :as ic]
            [invariant.query :refer [assert-valid-query
                                     invariant-query]]
            [clojure.edn :as edn]))

(alter-var-root #'dq/built-ins (fn [old] (conj old ['subquery datahike.api/q])))

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

(defn assert-invariants [connection tx-data]
  (let [attribute-txs (for [tx tx-data]
                        [(get-attribute tx) tx])
        attributes    (distinct (map first attribute-txs))
        schema        (:schema @connection)]
    (doseq [[a tx] attribute-txs
            :when (= a :invariant/query)
            :let  [[_ _ _ v] tx]]
      (assert-valid-query (edn/read-string v)))

    (doseq [a attributes]
      (when-let [inv-qs (d/q invariant-query @connection a)]
        (when-not (d/q (edn/read-string inv-qs)
                       ;; current state
                       @connection
                       ;; apply transaction to current state
                       (dc/db-with @connection tx-data)
                       ;; empty database with only transaction applied
                       (dc/db-with (dc/empty-db schema) tx-data)
                       tx-data)
          (throw (ex-info "Invariant mismatch."
                          {:type :invariant/invariant-mismatch
                           :attribute a
                           :invariant (edn/read-string inv-qs)
                           :tx-data tx-data})))))
    true))

(defmethod invariant.core/invariant :datahike
  [connection schema tx-data]
  (assert-invariants connection tx-data))
