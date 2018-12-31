(ns invariant.datahike
  (:require [datahike.api :as d]
            [datahike.core :as dc]
            [datahike.parser :as p]
            [datahike.db :as ddb]
            [invariant.core :as ic]
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


(def allowed-fns (atom (into #{'datahike.api/q
                              'd/q}
                             (keys datahike.query/built-ins))))


(defn valid-query? [query]
  (let [res (p/parse-query query)
        called-fns (->>
                    (:qwhere res)
                    (filter (fn [c]
                              (let [t (type c)]
                                (or
                                 (= datahike.parser.Function t)
                                 (= datahike.parser.Predicate t))))))]
    (doseq [c called-fns]
      (let [f (:symbol (:fn c))]
        (when (#{'datahike.api/q 'd/q} f)
          (let [q (:value (first (:args c)))]
            (valid-query? q)))
        (when-not (@allowed-fns f)
          (throw (ex-info "Function not allowed." {:type ::invalid-function-call
                                                   :call c})))))))



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
          (throw (ex-info "Invariant mismatch." {:type ::invariant-mismatch
                                                 :attribute a
                                                 :invariant (edn/read-string inv-qs)
                                                 :tx-data tx-data})))))
    true))



(defmethod invariant.core/invariant :datahike
  [connection tx-data]
  (ensure-invariances connection tx-data))
