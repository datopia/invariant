(ns invariant.datomic
  (:refer-clojure :exclude [+])
  (:require [datomic.api     :as api]
            [datalog.parser  :as p]
            [clojure.edn     :as edn]
            [invariant.core]
            [invariant.query
             :refer [assert-valid-query invariant-query]]
            [datalog.unparser :refer [unparse]]))

(defn get-attribute-dispatch [v]
  (first v))

(defmulti get-attribute get-attribute-dispatch)

(defmethod get-attribute :+
  [[_ eid attr delta]]
  attr)

(defmethod get-attribute :db/add
  [[_ e a v]]
  a)

(let [subq-selector (comp #{'subquery} :symbol :fn)
      fn-selector   (comp #{datalog.parser.type.Function} type)]
  (defn unnest-deep-queries [[_ [_ query] & sources]]
    (let [res              (p/parse query)
          clean-clauses    (remove subq-selector (:qwhere res))
          nested-functions (->> (:qwhere res)
                                (filter fn-selector)
                                (filter subq-selector))]
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
            nested-functions)))))

(defn unnest-query [query sources]
  (unnest-deep-queries (concat ['_ (list 'quote query)] sources)))

(defn + [db eid attr delta]
  (let [m (api/pull db [attr] eid)
        v (attr m 0)]
    [[:db/add eid attr (+ v delta)]]))

(def ^:private tx-fns
  [{:db/id    (api/tempid :db.part/user)
    :db/ident :+
    :db/fn    (api/function {:lang   "clojure"
                             :params '[db eid attr delta]
                             :code   '(let [m (d/pull db [attr] eid)
                                            v (attr m 0M)]
                                        [[:db/add eid attr (+ v delta)]])})}])

(let [counter (atom 0)]
  (defn datomic-empty-db [schema]
    (let [uri (str "datomic:mem:///temp-invariant-" (swap! counter inc))]
      (api/create-database uri)
      (let [conn (api/connect uri)]
        @(api/transact conn schema)
        @(api/transact conn tx-fns)
        (api/db conn)))))

(defn- invariant-holds? [inv-qs conn tx-data schema]
  (binding [*ns* (find-ns 'invariant.datomic)]
    ((eval
      (list 'fn '[$before $after $empty+txs $txs]
            (unnest-query (read-string inv-qs)
                          '[$before $after $empty+txs $txs])))
     (api/db conn)
     ;; apply transaction to current state
     (:db-after (api/with (api/db conn) tx-data))
     ;; empty database with only transaction applied
     (:db-after (api/with (datomic-empty-db schema) tx-data))
     tx-data)))

(defn assert-invariants [conn schema tx-data]
  (let [attr-txs (for [tx tx-data]
                   [(get-attribute tx) tx])
        attrs    (distinct (map first attr-txs))]
    (doseq [[a tx] attr-txs
            :when (= a :invariant/query)
            :let [[_ _ _ v] tx]]
      (assert-valid-query (edn/read-string v)))

    (doseq [a attrs
            :let [inv-qs (api/q invariant-query (api/db conn) a)]
            :when inv-qs]
      (when-not (invariant-holds? inv-qs conn tx-data schema)
        (throw (ex-info "Invariant mismatch."
                        {:type      :invariant/invariant-mismatch
                         :attribute a
                         :invariant (edn/read-string inv-qs)
                         :tx-data   tx-data}))))
    true))

(defmethod invariant.core/invariant :datomic
  [conn schema tx-data]
  (assert-invariants conn schema tx-data))
