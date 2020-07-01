(ns invariant.datahike-scratch
  (:refer-clojure :exclude [+])
  (:require [invariant.datahike          :as invariant.d
             :refer [+]]
            [invariant.test.common       :as common]
            [invariant.query
             :refer [assert-valid-query assert-safe-query]]
            [invariant.test.util
             :refer [read-resource]]
            [datahike.api :refer [q]               :as d]
            [datahike.core  :as dc]
            [invariant.backend           :as backend]))

(comment
  ;; TODO check truthy (backend/assert-invariants backend txn)

  ;; brittleness of ensuring to match?

  (let [uri "datahike:mem:///invariant-test-2"]
    (d/create-database uri)
    (binding [conn (d/connect uri)]
      (d/transact conn schema)
      (d/transact conn common/example-txs)
      (let [q '[:find ?matches .
                :in $before $after $empty+txs $txs
                :where
                ;; run the sub-query
                [(subquery [:find (sum ?balance-before) (sum ?balance-after) (sum ?balance-change)
                            :with ?affected-account
                            :in $before $after $empty+txs $txs
                            :where
                            ;; Unify data from databases and transactions with affected-account
                            [$after      ?affected-account         :account/balance    ?balance-after]
                            [$empty+txs  ?affected-account         :account/balance    ?balance-change]
                            [(get-else $before ?affected-account :account/balance 0) ?balance-before]

                            ;; 2. Positivity
                            [(>= ?balance-after 0)]

                            ;; 3. Sender spending
                            [$txs _ _ :transaction/signed-by ?sender]
                            [(= ?sender ?affected-account) ?is-sender]
                            [(>= ?balance-change 0) ?pos-change]
                            [(or ?is-sender ?pos-change)]]
                           $before $after $empty+txs $txs)
                 [[?sum-before ?sum-after ?sum-change]]]
                ;; 1. Zero-Sum aggregated
                [(= ?sum-before ?sum-after)]
                [(= ?sum-change 0) ?matches]]
            txn [[:db.fn/call + 2 :account/balance  +52]
                 [:db.fn/call + 3 :account/balance  -2]
                 [:db.fn/call + 1 :account/balance +1]
                 [:db/add 1 :transaction/signed-by 3]]

            #_[[:db.fn/call + 2 :account/balance +52]
               [:db.fn/call + 3 :account/balance  -2]
               [:db.fn/call + 4 :account/balance  +1]
               [:db/add 1 :transaction/signed-by 3]]

            _ (d/transact conn txn)
            res
            (d/q q
                 ;; current state
                 @conn
                 ;; apply transaction to current state
                 (dc/db-with @conn txn)
                 ;; empty database with only transaction applied
                 (dc/db-with (dc/empty-db) (concat schema txn))
                 txn)]
        (d/delete-database uri)
        res)))

  (require '[datahike.api :as d])

  (def uri "datahike:mem://simple_recursion")

  (d/delete-database uri)

  (d/create-database uri :schema-on-read true)

  (def conn (d/connect uri))

  (d/transact conn [{:db/id 1
                     :ancestor 2}
                    {:db/id 2
                     :ancestor 3}
                    {:db/id 3
                     :ancestor 4}])

  (def rule '[[(ancestor ?e1 ?e2)
               [?e1 :ancestor ?e2]]
              [(ancestor ?e1 ?e2)
               [?e1 :ancestor ?t]
               (ancestor ?t ?e2)]])

  (d/q '[:find  ?u1 ?u2
         :in    $ %
         :where (ancestor ?u1 ?u2)]
       @conn
       rule))
