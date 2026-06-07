(ns invariant.datahike-test
  (:refer-clojure :exclude [+])
  (:require [clojure.test
             :refer [deftest testing is] :as test]
            [invariant.datahike          :as invariant.d
             :refer [+]]
            [invariant.test.common       :as common]
            [invariant.query
             :refer [assert-valid-query]]
            [invariant.backend           :as backend]
            [invariant.test.util
             :refer [read-resource]]
            [datahike.api                :as d]))

(deftest attribute-test
  (testing "Attribute extraction."
    (is (= :foo
           (invariant.d/get-attribute [:db/add -1 :foo 2])))

    (is (= :foo
           (invariant.d/get-attribute [:db/retract -1 :foo 2])))

    (is (= :foo
           (invariant.d/get-attribute [:db.fn/call invariant.datahike/+ -1 :foo 3])))

    (testing "Entity-map tx forms return the SET of touched attrs
              (excluding :db/id)."
      (is (= #{:foo :bar}
             (invariant.d/get-attribute {:db/id -1 :foo 1 :bar 2})))
      (is (= #{}
             (invariant.d/get-attribute {:db/id -1}))))

    (testing "Destructive tx-data shapes return nil — assert-invariants
              has nothing per-attr to schedule for them."
      (is (nil? (invariant.d/get-attribute [:db.fn/retractEntity 1])))
      (is (nil? (invariant.d/get-attribute [:db/retractEntity 1])))
      (is (nil? (invariant.d/get-attribute [:db/purge 1 :foo 2])))
      (is (nil? (invariant.d/get-attribute [:db.purge/entity 1])))
      (is (nil? (invariant.d/get-attribute [:db.purge/attribute 1 :foo])))
      (is (nil? (invariant.d/get-attribute [:db.fn/cas 1 :foo 2 3]))))

    (testing "Unknown tx-forms return nil via the :default method —
              consumer custom tx-fns don't crash the pipeline."
      (is (nil? (invariant.d/get-attribute [:db.fn/some-custom-fn 1 2 3]))))))

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

;; Schema fixture is already a canonical vector of `{:db/ident _ :db/valueType _
;; :db/cardinality _}` maps (post-f90b3bf #16). Use as-is.
(def schema (read-resource "datahike_schema.edn"))

(def ^:dynamic conn nil)

(def backend
  (reify backend/Backend
    (tempid [_ v]
      (d/tempid v))
    (unnest-query [_ _q _sources]
      nil)
    (assert-invariants [_ txs schema]
      ;; pass schema through for back-compat — assert-invariants now
      ;; resolves schema from the conn when nil, so either works
      (invariant.d/assert-invariants conn txs schema))
    (transact [_ txs]
      (d/transact conn txs))))

;; Datahike 0.8.x's store config requires a UUID :id (per
;; konserve.store/validate-store-config) and uses :backend :memory
;; rather than the legacy URI shape. A fresh UUID per fixture
;; invocation keeps the in-memory store isolated across tests.
(defn datahike-db-fixture [f]
  (let [cfg {:store              {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :read}
        cfg (d/create-database cfg)]
    (binding [conn (d/connect cfg)]
      (d/transact conn schema)
      (d/transact conn common/example-txs)
      (try (f) (finally (d/delete-database cfg))))))

(test/use-fixtures :each datahike-db-fixture)

(deftest bad-invariant-deployment
  (testing "Testing deployment of bad invariant."
    (is (common/bad-invariant-deployment? backend schema))))

;; ---------------------------------------------------------------------------
;; cycle-invariant-test — exercises the 4-source query model directly
;; (no `assert-invariants` machinery). Kept for upstream parity.
;; ---------------------------------------------------------------------------

(defn- cycle-query [txn]
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :read}
        cfg (d/create-database cfg)
        c   (d/connect cfg)]
    (try
      (let [empty @c]
        (d/transact c [{:db/id 1001 :ancestor 1002}
                       {:db/id 1002 :ancestor 1003}])
        (d/q '[:find  (count ?a) .
               :in    $before $after $empty+txs $txs %
               :where
               ($after ancestor ?a ?b)
               [(= ?a ?b)]]
             @c                 ;; before
             (d/db-with @c txn) ;; after
             (d/db-with empty txn) ;; empty + txs
             txn
             '[[(ancestor ?e1 ?e2)
                [?e1 :ancestor ?e2]]
               [(ancestor ?e1 ?e2)
                [?e1 :ancestor ?t]
                (ancestor ?t ?e2)]]))
      (finally (d/delete-database cfg)))))

(deftest cycle-invariant-test
  (testing "A test checking a graph for cycles."
    ;; match cycles in all graphs
    (is (= 3 (cycle-query [{:db/id 1003 :ancestor 1001}])))

    (is (nil? (cycle-query [{:db/id 1003 :ancestor 1004}])))))

(deftest invariant-deployment
  (testing "Testing deployment of valid invariant."
    (is (common/deployed-valid-invariant? backend schema))

    (let [txn [[:db.fn/call + [:account/name "Danny"]      :account/balance  +1]
               [:db.fn/call + [:account/name "Moe"]        :account/balance  -3]
               [:db.fn/call + [:account/name "Christian"]  :account/balance -50]
               [:db.fn/call + [:account/name "Danny"]      :account/balance +52]
               [:db/add 1001 :datopia/signed-by "Moe"]
               [:db/add 1001 :datopia/signed-by "Christian"]]]
      (is (backend/assert-invariants backend txn schema)))

    ;; non-zero
    (let [txn [[:db.fn/call + [:account/name "Christian"] :account/balance +52]
               [:db.fn/call + [:account/name "Danny"]     :account/balance  -2]
               [:db.fn/call + [:account/name "Moe"]       :account/balance  +1]
               [:db/add 1001 :datopia/signed-by "Danny"]]]
      (is (common/balance-mismatch? backend txn schema)))

    ;; negative
    (let [txn [[:db.fn/call + [:account/name "Christian"] :account/balance +5000]
               [:db.fn/call + [:account/name "Danny"]     :account/balance -5000]
               [:db/add 1001 :datopia/signed-by "Danny"]]]
      (is (common/balance-mismatch? backend txn schema)))

    ;; sender spending
    (let [txn [[:db.fn/call + [:account/name "Danny"]      :account/balance  +1]
               [:db.fn/call + [:account/name "Moe"]        :account/balance  -3]
               [:db.fn/call + [:account/name "Christian"]  :account/balance -50]
               [:db.fn/call + [:account/name "Danny"]      :account/balance +52]
               [:db/add 1001 :datopia/signed-by "Danny"]]]
      (is (common/balance-mismatch? backend txn schema)))))
