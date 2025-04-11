(ns invariant.datahike
  (:refer-clojure :exclude [+])
  (:require [datahike.api   :as d]
            [datahike.core  :as dc]
            [datahike.query :as dq]
            [invariant.query
             :refer [assert-valid-query invariant-query]]
            [clojure.edn :as edn]))

(alter-var-root #'dq/built-ins assoc 'q datahike.api/q)

(defn + [db selector attr delta]
  (let [m (d/entity db selector)
        eid (or (:db/id m) (d/tempid :db.part/user))
        v (attr m 0M)] 
    (concat (when (and (neg? eid) (seq selector))
              [[:db/add eid (first selector) (second selector)]])
            [[:db/add eid attr (clojure.core/+ v delta)]])))

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
  (let [;; create empty memory database with only schema
        empty-cfg (d/create-database (dissoc (:config @(:wrapped-atom conn)) :store))
        empty-conn (d/connect empty-cfg)
        empty+datoms (dc/db-with @empty-conn (concat schema tx-data))
        _ (d/release empty-conn)]
    (d/q (edn/read-string inv-qs)
       ;; current state
         @conn
       ;; apply transaction to current state
         (dc/db-with @conn tx-data)
       ;; empty database with only transaction applied
         empty+datoms
         tx-data)))

(defn assert-invariants [conn tx-data schema]
  (let [attr-txs (for [tx tx-data]
                   [(get-attribute tx) tx])
        attrs    (distinct (map first attr-txs))]
    (doseq [[a tx] attr-txs
            :when (= a :invariant/query)
            :let  [[_ _ _ v] tx]]
      (assert-valid-query (edn/read-string v)))

    (doseq [a attrs
            :let  [inv-qs (d/q invariant-query @conn a)]
            :when inv-qs]
      (println "Checking invariant" a inv-qs)
      (when-not (invariant-holds? inv-qs conn tx-data schema)
        (throw (ex-info "Invariant mismatch."
                        {:type      :invariant/invariant-mismatch
                         :attribute a
                         :invariant (edn/read-string inv-qs)
                         :tx-data   tx-data}))))
    true))


(defn transact-with-invariants
  "Transaction wrapper that enforces invariants before committing.
   
   Takes a Datahike connection, transaction data, and schema transactions.
   Checks all relevant invariants before committing the transaction.
   
   Parameters:
   - conn: Datahike connection
   - tx-data: Transaction data (vector of datoms or map with :tx-data key)
   - schema: Schema transactions required for invariant checking
   
   Returns:
   - The direct result from datahike.api/transact, if invariants are satisfied
   - Throws an exception if any invariant is violated
   
   Note: For operations where invariants should be skipped (e.g., schema updates),
   use datahike.api/transact directly instead."
  [conn tx-data schema]
  (let [tx-data (if (map? tx-data) (:tx-data tx-data) tx-data)]
    (assert-invariants conn tx-data schema)
    (d/transact conn {:tx-data tx-data})))