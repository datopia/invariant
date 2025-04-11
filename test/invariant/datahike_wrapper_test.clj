(ns invariant.datahike-wrapper-test
  (:refer-clojure :exclude [+])
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [invariant.datahike :as id :refer [transact-with-invariants +]]
            [invariant.test.common :as common]
            [invariant.test.util :refer [read-resource]]
            [datahike.api :as d]))

;; Use the same schema as in the main tests
(def schema (read-resource "datahike_schema.edn"))

(def ^:dynamic conn nil)

(let [uri "datahike:mem:///invariant-wrapper-test"]
  (defn datahike-db-fixture [f]
    (d/delete-database uri)
    (d/create-database uri)
    (binding [conn (d/connect uri)]
      (d/transact conn {:tx-data schema})
      (d/transact conn {:tx-data common/example-txs})
      (f)
      (d/delete-database uri))))

(use-fixtures :each datahike-db-fixture)

(deftest test-basic-wrapper
  (testing "Basic usage of transaction wrapper"
    ;; Deploy a valid invariant
    (let [valid-invariant (read-resource "valid_invariant.edn")
          tid (d/tempid :db.part/user)
          deploy-tx [[:db/add tid :invariant/rule :account/balance]
                     [:db/add tid :invariant/query (pr-str valid-invariant)]]]
      
      ;; This should succeed (valid invariant deployment using d/transact for schema additions)
      (d/transact conn {:tx-data deploy-tx})
      
      ;; Valid money transfer (zero-sum, everyone has authority)
      (let [valid-tx [[:db.fn/call + [:account/name "Danny"] :account/balance  +1]
                      [:db.fn/call + [:account/name "Moe"] :account/balance  -3]
                      [:db.fn/call + [:account/name "Christian"] :account/balance -50]
                      [:db.fn/call + [:account/name "Danny"] :account/balance +52]
                      [:db/add 1001 :datopia/signed-by "Moe"]
                      [:db/add 1001 :datopia/signed-by "Christian"]]]
        (is (map? (transact-with-invariants conn valid-tx schema))
            "Valid transaction should succeed"))
      
      ;; Invalid money transfer (not zero-sum)
      (let [invalid-tx [[:db.fn/call + [:account/name "Christian"] :account/balance +5000]
                        [:db.fn/call + [:account/name "Danny"] :account/balance -4000]
                        [:db/add 1001 :datopia/signed-by "Danny"]]]
        (is (thrown? Exception (transact-with-invariants conn invalid-tx schema))
            "Invalid transaction should throw an exception")))))

;; Note: Automatic schema extraction is available but not tested here
;; as it requires additional database setup.