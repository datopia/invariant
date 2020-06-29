(ns invariant.datomic-test
  (:require [invariant.datomic           :as invariant.d]
            [invariant.test.common       :as common]
            [invariant.test.util
             :refer [read-resource]]
            [datomic.api                 :as d]
            [invariant.backend           :as backend]))

(comment
  ;; TODO check truthy (backend/assert-invariants backend txn)

  ;; brittleness of ensuring to match?

  (let [uri "datomic:mem:///invariant-test"]
    (d/create-database uri)
    (binding [conn (d/connect uri)]
      @(d/transact conn schema)
      @(d/transact
        conn [#:db{:id                    #db/id[:db.part/db]
                   :ident                 :invariant/query-test
                   :valueType             :db.type/string
                   :cardinality           :db.cardinality/one
                   :invariant/query       "[:find ...]"
                   :db.install/_attribute :db.part/db}])
      @(d/transact conn common/example-txs)
      @(d/transact
        conn [#:db{:id    (d/tempid :db.part/user)
                   :ident :+
                   :fn    (d/function
                           {:lang   "clojure"
                            :params '[db eid attr delta]
                            :code   '(let [m (d/pull db [attr] eid)
                                           v (attr m 0M)]
                                       [[:db/add eid attr (+ v delta)]])})}])

      (let [q '[:find ?matches .
                :in $before $after $empty+txs $txs
                :where
                ;; run the sub-query
                [(subquery [:find (sum ?balance-before) (sum ?balance-after) (sum ?balance-change)
                            :with ?affected-entity
                            :in $before $after $empty+txs $txs
                            :where
                            ;; Unify data from databases and transactions with affected-entity
                            [$after      ?affected-entity         :account/balance    ?balance-after]
                            [$empty+txs  ?affected-entity         :account/balance    ?balance-change]
                            [(get-else $before ?affected-entity :account/balance 0) ?balance-before]

                            ;; 1. Zero-Sum
                            [(+ ?balance-change ?balance-before) ?computed-balance-after]
                            [(= ?balance-after ?computed-balance-after)]

                            ;; 2. Positivity
                            [(>= ?balance-after 0)]

                            ;; 3. Sender spending
                            [$txs _ _ :transaction/signed-by ?sender]
                            [(= ?sender ?affected-entity) ?is-sender]
                            [(>= ?balance-change 0) ?pos-change]
                            [(or ?is-sender ?pos-change)]]
                           $before $after $empty+txs $txs)
                 [[?sum-before ?sum-after ?sum-change]]]
                [(= ?sum-before ?sum-after)]
                [(= ?sum-change 0M) ?matches]]
            tid (backend/tempid backend :db.part/user)
            txn [[:+ 0 :account/balance  +1]
                 [:+ 3 :account/balance  -3]
                 [:+ 1 :account/balance -50]
                 [:+ 2 :account/balance +52]
                 [:db/add 0 :transaction/signed-by 1]
                 [:db/add 0 :transaction/signed-by 3]]
            _ @(d/transact conn txn)
            res (binding [*ns* (find-ns 'invariant.datomic)]
                  ((eval
                    (list 'fn '[$before $after $empty+txs $txs]
                          (invariant.d/unnest-query q '[$before $after $empty+txs $txs])))
                   (d/db conn)
                   ;; apply transaction to current state
                   (:db-after (d/with (d/db conn) txn))
                   ;; empty database with only transaction applied
                   (:db-after (d/with (invariant.d/datomic-empty-db schema) txn))
                   txn))]
        (d/delete-database uri)
        res))))
