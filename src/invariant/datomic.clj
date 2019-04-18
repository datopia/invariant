(ns invariant.datomic
  (:refer-clojure :exclude [+])
  (:require [datomic.api :as api]
            [datahike.parser :as p]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [invariant.core]
            [invariant.query :refer [assert-valid-query
                                     invariant-query]]
            [invariant.unparse :refer [unparse]]))


(defn get-attribute-dispatch [v]
  (first v))

(defmulti get-attribute get-attribute-dispatch)

(defmethod get-attribute :+
  [[_ eid attr delta]]
  attr)

(defmethod get-attribute :db/add
  [[_ e a v]]
  a)



(defn unnest-deep-queries [[_ [_ query] & sources]]
  (let [res (p/parse-query query)
        clean-clauses (->> (:qwhere res)
                         (filter (fn [f] (not= (:symbol (:fn f)) 'subquery)))
                         vec)
        nested-functions (->> (:qwhere res)
                            (filter (fn [c]
                                      (let [t (type c)]
                                        (= datahike.parser.Function t))))
                            (filter (fn [f] (= (:symbol (:fn f)) 'subquery))))]
    (concat
     (list 'datomic.api/q
           (list 'quote
                 (unparse
                  (-> res
                     (assoc :qwhere clean-clauses)
                     (update :qin concat
                             (map :binding nested-functions))))))
     sources
     (map (comp unnest-deep-queries
                ;; subquery first argument
                #(concat (list 'api/q (list 'quote (second %))) sources)
                first
                unparse)
          nested-functions))))

(defn unnest-query [query sources]
  (unnest-deep-queries (concat ['_ (list 'quote query)] sources)))

(defn + [db eid attr delta]
  (let [m (api/pull db [attr] eid)
        v (attr m 0)]
    [[:db/add eid attr (+ v delta)]]))

(def tx-fns [{:db/id (api/tempid :db.part/user)
              :db/ident :+
              :db/fn (api/function {:lang "clojure"
                                    :params '[db eid attr delta]
                                    :code '(let [m (d/pull db [attr] eid)
                                                 v (attr m 0M)]
                                             [[:db/add eid attr (+ v delta)]])})}])

(let [counter (atom 0)]
  (defn datomic-empty-db [schema]
    (let [uri (str "datomic:mem:///temp-invariant-" (swap! counter inc))
          _ (api/create-database uri)
          conn (api/connect uri)]
      @(api/transact conn schema)
      @(api/transact conn tx-fns)
      (api/db conn))))

(defn assert-invariants [connection schema tx-data]
  (let [attribute-txs (map (fn [tx] [(get-attribute tx) tx])
                           tx-data)
        attributes (distinct (map first attribute-txs))]
    (doseq [[a tx] attribute-txs
            :when (= a :invariant/query)
            :let [[_ _ _ v] tx]]
      (assert-valid-query (edn/read-string v)))

    (doseq [a attributes]
      (when-let [inv-qs (api/q invariant-query (api/db connection) a)]
        (when-not (binding [*ns* (find-ns 'invariant.datomic)]
                    ((eval
                      (list 'fn '[$before $after $empty+txs $txs]
                            (unnest-query (read-string inv-qs)
                                          '[$before $after $empty+txs $txs])))
                     (api/db connection)
                     ;; apply transaction to current state
                     (:db-after (api/with (api/db connection) tx-data))
                     ;; empty database with only transaction applied
                     (:db-after (api/with (datomic-empty-db schema) tx-data))
                     tx-data))
          (throw (ex-info "Invariant mismatch."
                          {:type :invariant/invariant-mismatch
                           :attribute a
                           :invariant (edn/read-string inv-qs)
                           :tx-data tx-data})))))
    true))

(defmethod invariant.core/invariant :datomic
  [connection schema tx-data]
  (assert-invariants connection schema tx-data))








