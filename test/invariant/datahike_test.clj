(ns invariant.datahike-test
  (:refer-clojure :exclude [+])
  (:require [clojure.test
             :refer [deftest testing is] :as test]
            [invariant.datahike          :as invariant.d
             :refer [+]]
            [invariant.test.common       :as common]
            [invariant.query
             :refer [assert-valid-query assert-safe-query]]
            [invariant.backend           :as backend]
            [invariant.test.util
             :refer [read-resource]]
            [datahike.api                :as d
             :refer [q]]
            [datahike.core  :as dc]))

(deftest attribute-test
  (testing "Attribute extraction."
    (is (= :foo
           (invariant.d/get-attribute [:db/add 1 :foo 2])))

    (is (= :foo
           (invariant.d/get-attribute [:db.fn/call invariant.datahike/+ 1 :foo 3])))))

(deftest valid-query-test
  (testing "Valid queries."
    (is (= :invariant/invalid-function-call
           (try
             (assert-valid-query '[:find ?a
                                   :in   $a $b $c $d
                                   :where
                                   [(subquery [:find  ?a
                                               :in    $a $b $c $d
                                               :where [(nested-evil ?a 5)]]
                                              $a $b $c $d) ?a]])
             (catch Exception e
               (-> e ex-data :type)))))))

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

(let [uri "datahike:mem:///invariant-test"]
  (defn cycle-query [txn]
    (d/create-database uri :schema-on-read true)
    (binding [conn (d/connect uri)]
      (d/transact conn [{:db/id 1 :ancestor 2}
                        {:db/id 2 :ancestor 3}])
      (let [res (d/q '[:find  (count ?a) .
                       :in    $before $after $empty+txs $txs %
                       :where
                       ($after ancestor ?a ?b)
                       [(= ?a ?b)]]
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
        res))))

(deftest cycle-invariant-test
  (testing "A test checking a graph for cycles."
    ;; match cycles in all graphs
    (is (= 3 (cycle-query [{:db/id 3 :ancestor 1}])))

    (is (nil? (cycle-query [{:db/id 3 :ancestor 4}])))))

(deftest invariant-upheld
  (testing "Testing deployment of valid invariant."
    (is (common/deployed-valid-invariant? backend schema)))

  (testing "Valid transactions."
    (let [txn [[:db.fn/call + 3 :account/balance  +1]
               [:db.fn/call + 1 :account/balance  -3]
               [:db.fn/call + 2 :account/balance -50]
               [:db.fn/call + 3 :account/balance +52]
               [:db/add 1 :transaction/signed-by 1]
               [:db/add 1 :transaction/signed-by 2]]]
      (is (backend/assert-invariants backend txn schema))))

  (testing "Invalid transactions."
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
      (is (common/balance-mismatch? backend txn schema))))

  (testing "Expansion of map forms."
    (let [invalid-txs [[{:db/id           1
                         :account/balance 0M}]
                       [{:db/id                 1
                         :account/balance       0M
                         :transaction/signed-by 1}]
                       [{:account/balance 1M}]
                       [{:db/id        99
                         :account/name "New User"}
                        {:db/id                 99
                         :account/balance       1M
                         :transaction/signed-by 99}]]]
      (doseq [tx invalid-txs]
        (is (common/balance-mismatch? backend tx schema))))

    (let [valid-txs [[{:account/name "New User"}]
                     [{}]]]
      (doseq [tx valid-txs]
        (is (backend/assert-invariants backend tx schema))))))
