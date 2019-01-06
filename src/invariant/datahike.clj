(ns invariant.datahike
  (:require [datahike.api :as d]
            [datahike.core :as dc]
            [datahike.db :as ddb]
            [invariant.core :as ic]
            [invariant.query :refer [valid-query?]]
            [clojure.edn :as edn]))


(defn get-attribute-dispatch [v]
  (let [[a b] v]
    (cond (= :db.fn/call a)
          [:db.fn/call b]
          :else a)))

(defmulti get-attribute get-attribute-dispatch)

(defn +v [db eid attr delta]
  (let [m (d/pull db [attr] eid)
        v (attr m 0)]
    [[:db/add eid attr (+ v delta)]]))


(defmethod get-attribute [:db.fn/call +v]
  [[_ _ eid attr delta]]
  attr)


(defmethod get-attribute :db/add
  [[_ e a v]]
  a)



(defn ensure-invariances [connection tx-data]
  (let [attribute-txs (map (fn [tx] [(get-attribute tx) tx])
                           tx-data)
        attributes (distinct (map first attribute-txs))
        schema (:schema @connection)]
    (doseq [[a tx] attribute-txs
            :when (= a :invariant/query)
            :let [[_ _ _ v] tx]]
      (valid-query? (edn/read-string v)))

    (doseq [a attributes]
      (when-let [inv-qs (d/q '[:find ?q .
                              :in $ ?a
                              :where
                               [?e :invariant/rule ?a]
                               [?e :invariant/query ?q]]
                            @connection
                            a)]
        (when-not (d/q (edn/read-string inv-qs)
                       ;; current state
                       @connection
                       ;; apply transaction to current state
                       (dc/db-with @connection tx-data)
                       ;; empty database with only transaction applied
                       (dc/db-with (dc/empty-db schema) tx-data)
                       tx-data)
          (throw (ex-info "Invariant mismatch." {:type :invariant/invariant-mismatch
                                                 :attribute a
                                                 :invariant (edn/read-string inv-qs)
                                                 :tx-data tx-data})))))
    true))


(defmethod invariant.core/invariant :datahike
  [connection schema tx-data]
  (ensure-invariances connection tx-data))
