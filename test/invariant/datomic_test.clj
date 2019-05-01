(ns invariant.datomic-test
  (:require [clojure.test
             :refer [deftest testing is] :as test]
            [invariant.datomic           :as invariant.d]
            [invariant.test.common       :as common]
            [invariant.test.util
             :refer [read-resource]]
            [datomic.api                 :as d]
            [invariant.backend           :as backend]))

(def ^:dynamic conn nil)

(def schema (read-resource "datomic_schema.edn"))

(def backend
  (reify backend/Backend
    (tempid [_ v]
      (d/tempid v))
    (unnest-query [_ q sources]
      (invariant.d/unnest-query q sources))
    (assert-invariants [_ txs]
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
    (is (common/bad-invariant-deployment? backend))))

(deftest invariant-deployment
  (testing "Testing deployment of valid invariant."
    (is (common/deployed-valid-invariant? backend {:bigdec? true}))

    ;; valid
    (let [txn [[:+ 0 :account/balance  +1]
               [:+ 3 :account/balance  -3]
               [:+ 1 :account/balance -50]
               [:+ 2 :account/balance +52]]]
      (is (backend/assert-invariants backend txn)))

    ;; non-zero
    (let [txn [[:+ 0 :account/balance  +1]
               [:+ 2 :account/balance +52]
               [:+ 3 :account/balance  -2]]]
      (is (common/balance-mismatch? backend txn)))

    ;; negative
    (let [txn [[:+ 2 :account/balance +5000]
               [:+ 3 :account/balance -5000]]]
      (is (common/balance-mismatch? backend txn)))))
