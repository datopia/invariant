(ns invariant.datahike-test
  (:refer-clojure :exclude [+])
  (:require [clojure.test
             :refer [deftest testing is] :as test]
            [invariant.datahike          :as invariant.d
             :refer [+]]
            [invariant.test.common       :as common]
            [invariant.query
             :refer [assert-valid-query]]
            [invariant.test.util
             :refer [read-resource]]
            [datahike.api                :as d]
            [invariant.backend           :as backend]))

(deftest attribute-test
  (testing "Attribute extraction."
    (is (= :foo
           (invariant.d/get-attribute [:db/add 1 :foo 2])))

    (is (= :foo
           (invariant.d/get-attribute [:db.fn/call invariant.datahike/+ 1 :foo 3])))))

(deftest valid-query-test
  (testing "Valid queries."
    (is (= '{:type :invariant/invalid-function-call,
             :call #datahike.parser.Predicate{:fn #datahike.parser.PlainSymbol{:symbol nested-evil},
                                              :args [#datahike.parser.Variable{:symbol ?a}
                                                     #datahike.parser.Constant{:value 5}]}}

           (try
             (assert-valid-query '[:find ?a
                                   :in   $a $b $c $d
                                   :where
                                   [(subquery [:find  ?a
                                               :in    $a $b $c $d
                                               :where [(nested-evil ?a 5)]]
                                              $a $b $c $d) ?a]])
             (catch Exception e
               (ex-data e)))))))

(def schema (read-resource "datahike_schema.edn"))

(def ^:dynamic conn nil)

(def backend
  (reify backend/Backend
    (tempid [_ v]
      (d/tempid v))
    (unnest-query [_ q sources]
      nil)
    (assert-invariants [_ txs]
      (invariant.d/assert-invariants conn txs))
    (transact [_ txs]
      (d/transact conn txs))))

(let [uri "datahike:mem:///invariant-test"]
  (defn datahike-db-fixture [f]
    (d/create-database-with-schema uri schema)
    (binding [conn (d/connect uri)]
      (d/transact conn common/example-txs)
      (f)
      (d/delete-database uri))))

(test/use-fixtures :each datahike-db-fixture)

(deftest bad-invariant-deployment
  (testing "Testing deployment of bad invariant."
    (is (common/bad-invariant-deployment? backend))))

(deftest invariant-deployment
  (testing "Testing deployment of valid invariant."
    (is (common/deployed-valid-invariant? backend))

    (let [txn [[:db.fn/call + 3 :account/balance  +1]
               [:db.fn/call + 1 :account/balance  -3]
               [:db.fn/call + 2 :account/balance -50]
               [:db.fn/call + 3 :account/balance +52]
               [:db/add 1 :transaction/signed-by 1]
               [:db/add 1 :transaction/signed-by 2]]]
      (is (backend/assert-invariants backend txn)))

    ;; non-zero
    (let [txn [[:db.fn/call + 2 :account/balance +52]
               [:db.fn/call + 3 :account/balance  -2]
               [:db.fn/call + 4 :account/balance  +1]
               [:db/add 1 :transaction/signed-by 3]]]
      (is (common/balance-mismatch? backend txn)))

    ;; negative
    (let [txn [[:db.fn/call + 2 :account/balance +5000]
               [:db.fn/call + 3 :account/balance -5000]
               [:db/add 1 :transaction/signed-by 3]]]
      (is (common/balance-mismatch? backend txn)))

    ;; sender spending
    (let [txn [[:db.fn/call + 3 :account/balance  +1]
               [:db.fn/call + 1 :account/balance  -3]
               [:db.fn/call + 2 :account/balance -50]
               [:db.fn/call + 3 :account/balance +52]
               [:db/add 1 :transaction/signed-by 3]]]
      (is (common/balance-mismatch? backend txn)))))


(comment
  ;; TODO check truthy (backend/assert-invariants backend txn)

  ;; brittleness of ensuring to match?

  (require '[datahike.core  :as dc]) 

  (let [uri "datahike:mem:///invariant-test"]
      (d/create-database-with-schema uri schema)
      (binding [conn (d/connect uri)]
        (d/transact conn common/example-txs)
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
                  [(= ?sum-change 0) ?matches]]
              tid (backend/tempid backend :db.part/user)
              txn [[:db.fn/call + 2 :account/balance  +1]
                   [:db.fn/call + 3 :account/balance  -3]
                   [:db.fn/call + 1 :account/balance -50]
                   [:db.fn/call + 2 :account/balance +52]
                   [:db/add 1 :transaction/signed-by 1]
                   [:db/add 1 :transaction/signed-by 3]]
              _ @(d/transact conn txn)
              res
              (d/q q
                   ;; current state
                   @conn
                   ;; apply transaction to current state
                   (dc/db-with @conn txn)
                   ;; empty database with only transaction applied
                   (dc/db-with (dc/empty-db schema) txn)
                   txn)
              ]
          (d/delete-database uri)
          res))) 


  )
