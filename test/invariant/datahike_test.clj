(ns invariant.datahike-test
  (:require [clojure.test :refer [deftest testing is] :as test]
            [invariant.datahike :refer :all]
            [datahike.api :as d]))


(deftest attribute-test
  (testing "Attribute extraction."
    (is (= :foo
           (get-attribute [:db/add 1 :foo 2])))

    (is (= :foo
           (get-attribute [:db.fn/call invariant.datahike/+v 1 :foo 3])))))


(deftest valid-query-test
  (testing "Valid queries."
    (is (= '{:type :invariant.datahike/invalid-function-call,
            :call #datahike.parser.Predicate{:fn #datahike.parser.PlainSymbol{:symbol nested-evil},
                                             :args [#datahike.parser.Variable{:symbol ?a}
                                                    #datahike.parser.Constant{:value 5}]}}
           (try
            (valid-query? '[:find ?a
                            :in $
                            :where
                            [(datahike.api/q [:find ?a
                                               :where
                                               [(nested-evil ?a 5)]]) ?a]]) 
            (catch Exception e
              (ex-data e)))))))




(def schema
  {:account/balance #:db{:cardinality :db.cardinality/one},
   :voting/to #:db{:valueType :db.type/ref, :cardinality :db.cardinality/one},
   :transfer/unit #:db{:cardinality :db.cardinality/one},
   :voting/from #:db{:valueType :db.type/ref, :cardinality :db.cardinality/one},
   :transfer/amount #:db{:cardinality :db.cardinality/one},
   :account/unit #:db{:cardinality :db.cardinality/one},
   :transfer/to #:db{:valueType :db.type/ref, :cardinality :db.cardinality/one},
   :transfer/from #:db{:valueType :db.type/ref, :cardinality :db.cardinality/one},
   :voting/delegate #:db{:valueType :db.type/ref,
                         :cardinality :db.cardinality/one},
   :block/height #:db{:cardinality :db.cardinality/one},
   :account/name #:db{:cardinality :db.cardinality/one,
                      :unique :db.unique/identity}})

(def example-txs [{:db/id 1,
                   :account/name "Moe",
                   :account/balance 5000M,
                   :account/unit :datom}
                  {:db/id 2,
                   :account/name "Christian",
                   :account/balance 100M,
                   :account/unit :datom}
                  {:db/id 3,
                   :account/name "Danny",
                   :account/balance 3000M,
                   :account/unit :datom}])

(def ^:dynamic conn nil)

(defn datahike-db-fixture [f]
  (let [uri "datahike:mem:///invariant-test"
        _ (d/create-database-with-schema uri schema)]
    (def conn (d/connect uri))
    (d/transact conn example-txs)
    (f)
    (d/delete-database uri)))

(test/use-fixtures :each datahike-db-fixture)

(deftest bad-invariant-deployment
  (testing "Testing deployment of bad invariant."
    (let [invariant-txs
          [[:db/add (d/tempid -1) :invariant/rule :account/balance]
           [:db/add (d/tempid -1) :invariant/query
            (pr-str '[:find ?matches .
                      :in $before $after $txn
                      :where
                      ;; run the sub-query
                      [(d/q [:find (sum ?balance-before) (sum ?balance-after) (sum ?balance-change)
                             :with ?affected-entity
                             :in $before $after $txn
                             :where
                             [(evil-haha 1 2 3)]
                             ;; Unify data from databases and transactions with affected-entity
                             [$after    ?affected-entity         :account/balance    ?balance-after]
                             [$txn      ?affected-entity         :account/balance    ?balance-change]
                             [(get-else $before ?affected-entity :account/balance 0) ?balance-before]

                             ;; 1. Zero-Sum
                             [(+ ?balance-change ?balance-before) ?computed-balance-after]
                             [(= ?balance-after ?computed-balance-after)]

                             ;; 2. Positivity
                             [(>= ?balance-after 0)]

                             ;; 3. Sender spending
                             #_[$txn    _                 :transaction/signed-by ?sender]
                             #_[(datopia.attribute-invariants/balance-check
                                ?sender ?affected-entity ?balance-before ?balance-after)]]
                            $before $after $txn)
                       [[?sum-before ?sum-after ?sum-change]]]
                      [(= ?sum-before ?sum-after)]
                      [(= ?sum-change 0) ?matches]])]]]
      (is (= '{:type :invariant.datahike/invalid-function-call,
               :call #datahike.parser.Predicate{:fn #datahike.parser.PlainSymbol{:symbol evil-haha},
                                                :args [#datahike.parser.Constant{:value 1}
                                                       #datahike.parser.Constant{:value 2}
                                                       #datahike.parser.Constant{:value 3}]}}
             (try
               (ensure-invariances conn invariant-txs)
               (catch Exception e
                 (ex-data e))))))))


(deftest invariant-deployment
  (testing "Testing deployment of valid invariant."
    (let [tid (d/tempid 1) 
          invariant-txs
          [[:db/add tid :invariant/rule :account/balance]
           [:db/add tid :invariant/query
            (pr-str '[:find ?matches .
                      :in $before $after $txn
                      :where
                      ;; run the sub-query
                      [(d/q [:find (sum ?balance-before) (sum ?balance-after) (sum ?balance-change)
                             :with ?affected-entity
                             :in $before $after $txn
                             :where
                             ;; Unify data from databases and transactions with affected-entity
                             [$after    ?affected-entity         :account/balance    ?balance-after]
                             [$txn      ?affected-entity         :account/balance    ?balance-change]
                             [(get-else $before ?affected-entity :account/balance 0) ?balance-before]

                             ;; 1. Zero-Sum
                             [(+ ?balance-change ?balance-before) ?computed-balance-after]
                             [(= ?balance-after ?computed-balance-after)]

                             ;; 2. Positivity
                             [(>= ?balance-after 0)]

                             ;; 3. Sender spending
                             #_[$txn    _                 :transaction/signed-by ?sender]
                             #_[(datopia.attribute-invariants/balance-check
                                ?sender ?affected-entity ?balance-before ?balance-after)]]
                            $before $after $txn)
                       [[?sum-before ?sum-after ?sum-change]]]
                      [(= ?sum-before ?sum-after)]
                      [(= ?sum-change 0) ?matches]])]]]
      (is (ensure-invariances conn invariant-txs))
      ;; install them
      (d/transact conn invariant-txs)

      ;; test a valid transaction
      (let [transfer-transaction
            [[:db/add (d/tempid -1) :transaction/signed-by 1]
             [:db/add (d/tempid -1) :transaction/signed-by 3]
             [:db.fn/call +v 0 :account/balance  +1]
             [:db.fn/call +v 3 :account/balance  -1]
             [:db.fn/call +v 1 :account/balance -50]
             [:db.fn/call +v 2 :account/balance +52]
             [:db.fn/call +v 3 :account/balance  -2]]]
        (is (ensure-invariances conn transfer-transaction)))


      ;; test non-zero balance change
      (let [invalid-transaction
            [[:db/add (d/tempid -1) :transaction/signed-by 1]
             [:db/add (d/tempid -1) :transaction/signed-by 3]
             [:db.fn/call +v 0 :account/balance  +1]
             [:db.fn/call +v 2 :account/balance +52]
             [:db.fn/call +v 3 :account/balance  -2]]]
        (is (= {:type :invariant.datahike/invariant-mismatch,
                :attribute :account/balance}
               (try
                 (ensure-invariances conn invalid-transaction)
                 (catch Exception e
                   (select-keys (ex-data e) #{:type :attribute}))))))

      ;; negative balance
      (let [invalid-transaction
            [[:db/add (d/tempid -1) :transaction/signed-by 1]
             [:db/add (d/tempid -1) :transaction/signed-by 3]
             [:db.fn/call +v 2 :account/balance +5000]
             [:db.fn/call +v 3 :account/balance -5000]]]
        (is (= {:type :invariant.datahike/invariant-mismatch,
                :attribute :account/balance}
               (try
                 (ensure-invariances conn invalid-transaction)
                 (catch Exception e
                   (select-keys (ex-data e) #{:type :attribute})))))))))



