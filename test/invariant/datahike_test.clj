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
      (d/tempid (if (keyword? v) 1 v)))
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
               [:db.fn/call + 3 :account/balance +52]]]
      (is (backend/assert-invariants backend txn)))

    ;; non-zero
    (let [txn [[:db.fn/call + 2 :account/balance +52]
               [:db.fn/call + 3 :account/balance  -2]
               [:db.fn/call + 4 :account/balance  +1]]]
      (is (common/balance-mismatch? backend txn)))

    ;; negative
    (let [txn [[:db.fn/call + 2 :account/balance +5000]
               [:db.fn/call + 3 :account/balance -5000]]]
      (is (common/balance-mismatch? backend txn)))))
