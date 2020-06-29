(ns invariant.datahike-test
  (:refer-clojure :exclude [+])
  (:require [clojure.test
             :refer [deftest testing is] :as test]
            [invariant.datahike          :as invariant.d
             :refer [+]]
            [invariant.test.common       :as common]
            [invariant.query :refer [assert-valid-query assert-safe-query]]
            [invariant.test.util
             :refer [read-resource]]
            [datahike.api :refer [q]     :as d]
            [datahike.core  :as dc]
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
             :call #datalog.parser.type.Predicate{:fn   #datalog.parser.type.PlainSymbol{:symbol nested-evil},
                                                  :args [#datalog.parser.type.Variable{:symbol ?a}
                                                         #datalog.parser.type.Constant{:value 5}]}}

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
    (assert-invariants [_ txs schema]
      (invariant.d/assert-invariants conn txs schema))
    (transact [_ txs]
      (d/transact! conn txs))))

(let [uri "datahike:mem:///invariant-test"]
  (defn datahike-db-fixture [f]
    (d/create-database uri)
    (binding [conn (d/connect uri)]
      (d/transact conn schema)
      (d/transact conn common/example-txs)
      (f)
      (d/delete-database uri))))

(test/use-fixtures :each datahike-db-fixture)

(deftest bad-invariant-deployment
  (testing "Testing deployment of bad invariant."
    (is (common/bad-invariant-deployment? backend schema))))

(deftest cycle-invariant-test
  (testing "A test checking a graph for cycles."
    ;; match cycles in all graphs
    (is (= 3
           (let [uri "datahike:mem:///invariant-test"]
             (d/create-database uri :schema-on-read true)
             (binding [conn (d/connect uri)]
               (d/transact conn [{:db/id    1
                                  :ancestor 2}
                                 {:db/id    2
                                  :ancestor 3}])
               (let [q   '[:find (count ?a) .
                           :in $before $after $empty+txs $txs %
                           :where
                           ($after ancestor ?a ?b)
                           [(= ?a ?b)]]
                     txn [{:db/id    3
                           :ancestor 1}]
                     res
                     (d/q q
                          ;; current state
                          @conn
                          ;; apply transaction to current state
                          (:db-after (d/with @conn txn))
                          ;; empty database with only transaction applied
                          (:db-after (d/with (dc/empty-db) txn))
                          txn
                          '[[(ancestor ?e1 ?e2)
                             [?e1 :ancestor ?e2]]
                            [(ancestor ?e1 ?e2)
                             [?e1 :ancestor ?t]
                             (ancestor ?t ?e2)]])]
                 (d/delete-database uri)
                 res)))))

    (is (nil?
         (let [uri "datahike:mem:///invariant-test"]
           (d/create-database uri :schema-on-read true)
           (binding [conn (d/connect uri)]
             (d/transact conn [{:db/id    1
                                :ancestor 2}
                               {:db/id    2
                                :ancestor 3}])
             (let [q   '[:find (count ?a) .
                         :in $before $after $empty+txs $txs %
                         :where
                         ($after ancestor ?a ?b)
                         [(= ?a ?b)]]
                   txn [{:db/id    3
                         :ancestor 4}]
                   res
                   (d/q q
                        ;; current state
                        @conn
                        ;; apply transaction to current state
                        (:db-after (d/with @conn txn))
                        ;; empty database with only transaction applied
                        (:db-after (dc/empty-db) txn)
                        txn
                        '[[(ancestor ?e1 ?e2)
                           [?e1 :ancestor ?e2]]
                          [(ancestor ?e1 ?e2)
                           [?e1 :ancestor ?t]
                           (ancestor ?t ?e2)]])]
               (d/delete-database uri)
               res)))))))

(deftest invariant-deployment
  (testing "Testing deployment of valid invariant."
    (is (common/deployed-valid-invariant? backend schema))

    (let [txn [[:db.fn/call + 3 :account/balance  +1]
               [:db.fn/call + 1 :account/balance  -3]
               [:db.fn/call + 2 :account/balance -50]
               [:db.fn/call + 3 :account/balance +52]
               [:db/add 1 :transaction/signed-by 1]
               [:db/add 1 :transaction/signed-by 2]]]
      (is (backend/assert-invariants backend txn schema)))

    ;; non-zero
    (let [txn [[:db.fn/call + 2 :account/balance +52]
               [:db.fn/call + 3 :account/balance  -2]
               [:db.fn/call + 1 :account/balance  +1]
               [:db/add 1 :transaction/signed-by 3]]]
      (is (common/balance-mismatch? backend txn schema)))

    ;; negative
    (let [txn [[:db.fn/call + 2 :account/balance +5000]
               [:db.fn/call + 3 :account/balance -5000]
               [:db/add 1 :transaction/signed-by 3]]]
      (is (common/balance-mismatch? backend txn schema)))

    ;; sender spending
    (let [txn [[:db.fn/call + 3 :account/balance  +1]
               [:db.fn/call + 1 :account/balance  -3]
               [:db.fn/call + 2 :account/balance -50]
               [:db.fn/call + 3 :account/balance +52]
               [:db/add 1 :transaction/signed-by 3]]]
      (is (common/balance-mismatch? backend txn schema)))))
