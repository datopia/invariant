(ns invariant.datahike-wrapper-test
  (:refer-clojure :exclude [+])
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [invariant.datahike :as id :refer [transact-with-invariants +]]
            [invariant.test.common :as common]
            [invariant.test.util :refer [read-resource]]
            [datahike.api :as d]))

;; Same as datahike-test: schema fixture is already canonical.
(def schema (read-resource "datahike_schema.edn"))

(def ^:dynamic conn nil)

(defn datahike-db-fixture [f]
  (let [cfg {:store              {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :read}
        cfg (d/create-database cfg)]
    (binding [conn (d/connect cfg)]
      (d/transact conn schema)
      (d/transact conn common/example-txs)
      (try (f) (finally (d/delete-database cfg))))))

(use-fixtures :each datahike-db-fixture)

(deftest test-basic-wrapper
  (testing "Basic usage of transaction wrapper (3-arg back-compat form)."
    (let [valid-invariant (read-resource "valid_invariant.edn")
          deploy-tx [{:invariant/rule  :account/balance
                      :invariant/query (pr-str valid-invariant)}]]

      ;; Schema-style installs use raw d/transact (no invariant checking).
      (d/transact conn deploy-tx)

      (let [valid-tx [[:db.fn/call + [:account/name "Danny"]     :account/balance  +1]
                      [:db.fn/call + [:account/name "Moe"]       :account/balance  -3]
                      [:db.fn/call + [:account/name "Christian"] :account/balance -50]
                      [:db.fn/call + [:account/name "Danny"]     :account/balance +52]
                      [:db/add 1001 :datopia/signed-by "Moe"]
                      [:db/add 1001 :datopia/signed-by "Christian"]]]
        (is (map? (transact-with-invariants conn valid-tx schema))
            "Valid transaction should succeed"))

      (let [invalid-tx [[:db.fn/call + [:account/name "Christian"] :account/balance +5000]
                        [:db.fn/call + [:account/name "Danny"]     :account/balance -4000]
                        [:db/add 1001 :datopia/signed-by "Danny"]]]
        (is (thrown? Exception (transact-with-invariants conn invalid-tx schema))
            "Invalid transaction should throw an exception")))))

(deftest test-2-arity-wrapper
  (testing "The 2-arity transact-with-invariants reads schema from @conn —
            consumers don't need to thread the schema vector explicitly."
    (let [valid-invariant (read-resource "valid_invariant.edn")]
      (d/transact conn
                  [{:invariant/rule  :account/balance
                    :invariant/query (pr-str valid-invariant)}])
      (let [valid-tx [[:db.fn/call + [:account/name "Danny"]     :account/balance  +1]
                      [:db.fn/call + [:account/name "Moe"]       :account/balance  -3]
                      [:db.fn/call + [:account/name "Christian"] :account/balance -50]
                      [:db.fn/call + [:account/name "Danny"]     :account/balance +52]
                      [:db/add 1001 :datopia/signed-by "Moe"]
                      [:db/add 1001 :datopia/signed-by "Christian"]]]
        (is (map? (transact-with-invariants conn valid-tx))
            "2-arg call resolves schema from conn"))
      (let [invalid-tx [[:db.fn/call + [:account/name "Christian"] :account/balance +5000]
                        [:db.fn/call + [:account/name "Danny"]     :account/balance -4000]
                        [:db/add 1001 :datopia/signed-by "Danny"]]]
        (is (thrown? Exception (transact-with-invariants conn invalid-tx))
            "Invalid tx still throws under the 2-arg form")))))
