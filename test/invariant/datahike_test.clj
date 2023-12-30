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
           (invariant.d/get-attribute [:db/add -1 :foo 2])))

    (is (= :foo
           (invariant.d/get-attribute [:db.fn/call invariant.datahike/+ -1 :foo 3])))))

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
      (d/transact! conn {:tx-data txs}))))

(let [uri "datahike:mem:///invariant-test"]
  (defn datahike-db-fixture [f]
    (d/delete-database uri)
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

(let [ cfg {:store {:backend :mem :id     "invariant-test" }
           :schema-flexibility :read}]
  (defn cycle-query [txn]
    (d/delete-database cfg)
    (d/create-database cfg)
    (binding [conn (d/connect cfg)]
      (let [empty @conn]
        (d/transact conn [{:db/id 1001 :ancestor 1002}
                          {:db/id 1002 :ancestor 1003}])
        (let [res (d/q '[:find  (count ?a) .
                         :in    $before $after $empty+txs $txs %
                         :where
                         ($after ancestor ?a ?b)
                         [(= ?a ?b)]]
                     ;; current state
                       @conn
                     ;; apply transaction to current state
                       (d/db-with @conn txn)
                     ;; empty database with only transaction applied
                       (d/db-with empty txn)
                       txn
                       '[[(ancestor ?e1 ?e2)
                          [?e1 :ancestor ?e2]]
                         [(ancestor ?e1 ?e2)
                          [?e1 :ancestor ?t]
                          (ancestor ?t ?e2)]])]
          (d/release conn true)
          (d/delete-database cfg)
          res)))))

(deftest cycle-invariant-test
  (testing "A test checking a graph for cycles."
    ;; match cycles in all graphs
    (is (= 3 (cycle-query [{:db/id 1003 :ancestor 1001}])))

    (is (nil? (cycle-query [{:db/id 1003 :ancestor 1004}])))))

(deftest invariant-deployment
  (testing "Testing deployment of valid invariant."
    (is (common/deployed-valid-invariant? backend schema))

    (let [txn [[:db.fn/call + [:account/name "Danny"] :account/balance  +1]
               [:db.fn/call + [:account/name "Moe"] :account/balance  -3]
               [:db.fn/call + [:account/name "Christian"] :account/balance -50]
               [:db.fn/call + [:account/name "Danny"] :account/balance +52]
               [:db/add 1001 :datopia/signed-by "Moe"]
               [:db/add 1001 :datopia/signed-by "Christian"]]]
      (is (backend/assert-invariants backend txn schema)))

    ;; non-zero
    (let [txn [[:db.fn/call + [:account/name "Christian"] :account/balance +52]
               [:db.fn/call + [:account/name "Danny"] :account/balance  -2]
               [:db.fn/call + [:account/name "Moe"] :account/balance  +1]
               [:db/add 1001 :datopia/signed-by "Danny"]]]
      (is (common/balance-mismatch? backend txn schema)))

    ;; negative
    (let [txn [[:db.fn/call + [:account/name "Christian"] :account/balance +5000]
               [:db.fn/call + [:account/name "Danny"] :account/balance -5000]
               [:db/add 1001 :datopia/signed-by "Danny"]]]
      (is (common/balance-mismatch? backend txn schema)))

    ;; sender spending
    (let [txn [[:db.fn/call + [:account/name "Danny"] :account/balance  +1]
               [:db.fn/call + [:account/name "Moe"] :account/balance  -3]
               [:db.fn/call + [:account/name "Christian"] :account/balance -50]
               [:db.fn/call + [:account/name "Danny"] :account/balance +52]
               [:db/add 1001 :datopia/signed-by "Danny"]]]
      (is (common/balance-mismatch? backend txn schema)))))
