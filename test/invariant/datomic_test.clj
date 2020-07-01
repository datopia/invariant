(ns invariant.datomic-test
  (:require [clojure.test                :as test
             :refer [deftest testing is]]
            [invariant.datomic           :as invariant.d]
            [invariant.test.common       :as common]
            [invariant.backend           :as backend]
            [invariant.test.util
             :refer [read-resource]]
            [datomic.api                 :as d]))

(def ^:dynamic conn nil)

(def schema (read-resource "datomic_schema.edn"))

(def backend
  (reify backend/Backend
    (tempid [_ v]
      (d/tempid v))
    (unnest-query [_ q sources]
      (invariant.d/unnest-query q sources))
    (assert-invariants [_ txs schema]
      (invariant.d/assert-invariants conn schema txs))
    (transact [_ txs]
      (d/transact conn txs))))

(deftest unnest-query-test
  (is (common/unnesting? backend 'datomic.api/q)))

(defn datomic-db-fixture [f]
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
                   :db.install/_attribute :db.part/db}
              #:db{:id                    #db/id[:db.part/db]
                   :ident                 :ancestor
                   :valueType             :db.type/long
                   :cardinality           :db.cardinality/one
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
      (f)
      (d/delete-database uri))))

(test/use-fixtures :each datomic-db-fixture)

(deftest bad-invariant-deployment
  (testing "Testing deployment of bad invariant."
    (is (common/bad-invariant-deployment? backend schema))))

(let [q       '[:find  (count ?a) .
                :in    $before $after $empty+txs $txs %
                :where
                ($after ancestor ?a ?b)
                [(= ?a ?b)]]
      pre-txn [{:db/id 1 :ancestor 2}
               {:db/id 2 :ancestor 3}]]
  (defn cycle-query [txn]
    (let [empty-db (d/db conn)]
      @(d/transact conn pre-txn)
      (d/q q
           ;; current state
           (d/db conn)
           ;; apply transaction to current state
           (:db-after (d/with (d/db conn) txn))
           ;; empty database with only transaction applied
           (:db-after (d/with empty-db txn))
           txn
           '[[(ancestor ?e1 ?e2)
              [?e1 :ancestor ?e2]]
             [(ancestor ?e1 ?e2)
              [?e1 :ancestor ?t]
              (ancestor ?t ?e2)]]))))

(deftest cycle-invariant-test
  (testing "A test checking a graph for cycles."
    ;; match cycles in all graphs
    (is (= 3 (cycle-query [{:db/id 3 :ancestor 1}])))

    (is (nil? (cycle-query [{:db/id 3 :ancestor 4}])))))

(deftest invariant-deployment
  (testing "Testing deployment of valid invariant."
    (is (common/deployed-valid-invariant? backend {:bigdec? true}))

    ;; valid
    (let [txn [[:+ 0 :account/balance  +1]
               [:+ 3 :account/balance  -3]
               [:+ 1 :account/balance -50]
               [:+ 2 :account/balance +52]
               [:db/add 0 :transaction/signed-by 1]
               [:db/add 0 :transaction/signed-by 3]]]
      (is (backend/assert-invariants backend txn schema)))

    ;; non-zero
    (let [txn [[:+ 0 :account/balance  +1]
               [:+ 2 :account/balance +52]
               [:+ 3 :account/balance  -2]
               [:db/add 0 :transaction/signed-by 1]
               [:db/add 0 :transaction/signed-by 3]]]
      (is (common/balance-mismatch? backend txn schema)))

    ;; negative
    (let [txn [[:+ 2 :account/balance +5000]
               [:+ 3 :account/balance -5000]
               [:db/add 0 :transaction/signed-by 1]
               [:db/add 0 :transaction/signed-by 3]]]
      (is (common/balance-mismatch? backend txn schema)))

    (let [txn [[:+ 0 :account/balance  +1]
               [:+ 3 :account/balance  -3]
               [:+ 1 :account/balance -50]
               [:+ 2 :account/balance +52]
               [:db/add 0 :transaction/signed-by 3]]]
      (is (common/balance-mismatch? backend txn schema)))))
