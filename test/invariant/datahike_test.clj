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

;; ============================================================================
;; Lookup-ref handling in the $empty+txs source
;; ============================================================================
;;
;; Business writes typically reference existing entities via lookup-refs:
;;
;;   {:posting/account [:account/code "1000"]
;;    :posting/commodity [:commodity/symbol "USD"]}
;;
;; The third invariant source `$empty+txs` (built by `db-with` on a
;; fresh empty-db) would throw `:entity-id/missing` on this shape — the
;; empty-db had the schema but no entities to resolve the lookup-ref
;; against. The seeding pass added in invariant-holds? fixes this.

(def ^:private collect-lookup-refs #'invariant.d/collect-lookup-refs)
(def ^:private lookup-ref?         #'invariant.d/lookup-ref?)

(deftest lookup-ref-predicate-shape-test
  (testing "Plain 2-vec with keyword head is a lookup-ref"
    (is (true? (lookup-ref? [:account/code "1000"])))
    (is (true? (lookup-ref? [:commodity/symbol "USD"]))))
  (testing "Tuples and non-2-element vectors are NOT lookup-refs"
    (is (false? (lookup-ref? [:db/add 1 :foo 2])))
    (is (false? (lookup-ref? [:db/retractEntity 1])))
    (is (false? (lookup-ref? [:db.fn/cas 1 :foo 2 3])))
    (is (false? (lookup-ref? [:a :b :c])))
    (is (false? (lookup-ref? [42 "USD"])))
    (is (false? (lookup-ref? "string"))))
  (testing "MapEntries are 2-element vectors but explicitly rejected"
    (is (false? (lookup-ref? (first {:db/id [:account/code "1000"]}))))))

(deftest collect-lookup-refs-walks-all-positions-test
  (testing "Lookup-refs at entity-map value positions"
    (is (= #{[:account/code "1000"] [:commodity/symbol "USD"]}
           (collect-lookup-refs
            [{:db/id "p1"
              :posting/account  [:account/code "1000"]
              :posting/commodity [:commodity/symbol "USD"]}]))))
  (testing "Lookup-refs at :db/id positions"
    (is (= #{[:account/code "1000"]}
           (collect-lookup-refs
            [{:db/id [:account/code "1000"] :account/active false}]))))
  (testing "Lookup-refs in tuple-tx value positions"
    (is (= #{[:account/code "1000"]}
           (collect-lookup-refs
            [[:db/add "p1" :posting/account [:account/code "1000"]]]))))
  (testing "Lookup-refs inside cardinality-many vectors"
    (is (= #{[:entity/code "A"] [:entity/code "B"]}
           (collect-lookup-refs
            [{:db/id "e"
              :entity/family [[:entity/code "A"] [:entity/code "B"]]}]))))
  (testing "Tx-data without lookup-refs returns empty"
    (is (= #{} (collect-lookup-refs [{:db/id 1 :foo 2}])))
    (is (= #{} (collect-lookup-refs [[:db/add 1 :foo 2]])))))

;; ---------------------------------------------------------------------------
;; End-to-end: assert-invariants with lookup-refs in tx-data.
;;
;; Uses a minimal schema (account + ref-typed posting attr) so the test
;; is independent of the main fixture. The invariant query mirrors what
;; real consumers write: read postings from $empty+txs, then look up
;; account properties via $after.
;; ---------------------------------------------------------------------------

(def ^:private lookup-ref-mini-schema
  [{:db/ident :lr.account/code
    :db/valueType :db.type/string
    :db/unique :db.unique/identity
    :db/cardinality :db.cardinality/one}
   {:db/ident :lr.account/active
    :db/valueType :db.type/boolean
    :db/cardinality :db.cardinality/one}
   {:db/ident :lr.posting/account
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :invariant/rule
    :db/valueType :db.type/keyword
    :db/unique :db.unique/identity
    :db/cardinality :db.cardinality/one}
   {:db/ident :invariant/query
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(defn- lookup-ref-conn []
  (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [c (d/connect cfg)]
      (d/transact c lookup-ref-mini-schema)
      (d/transact c [{:lr.account/code "1000" :lr.account/active true}])
      (d/transact c
                  [{:invariant/rule  :lr.posting/account
                    :invariant/query
                    (pr-str
                     '[:find ?ok .
                       :in $before $after $empty+txs $txs
                       :where
                       [(q [:find ?p
                            :in $after $empty+txs
                            :where
                            [$empty+txs ?p :lr.posting/account ?a]
                            [$after ?a :lr.account/active false]]
                           $after $empty+txs)
                        ?violators]
                       [(count ?violators) ?n]
                       [(= 0 ?n) ?ok]])}])
      c)))

(deftest empty-db-source-resolves-lookup-refs-no-crash-test
  (testing "tx-data with a lookup-ref no longer crashes the $empty+txs source"
    (let [c  (lookup-ref-conn)
          tx [{:db/id "p1" :lr.posting/account [:lr.account/code "1000"]}]]
      (is (true? (invariant.d/assert-invariants c tx))))))

(deftest empty-db-source-still-fires-invariant-on-violation-test
  (testing "Flipping the referenced account inactive in the same tx
            still triggers the registered invariant — the fix preserves
            invariant semantics."
    (let [c  (lookup-ref-conn)
          tx [{:db/id "p1" :lr.posting/account [:lr.account/code "1000"]}
              {:db/id [:lr.account/code "1000"] :lr.account/active false}]
          ex (try (invariant.d/assert-invariants c tx)
                  nil
                  (catch clojure.lang.ExceptionInfo e e))]
      (is (some? ex))
      (is (= :invariant/invariant-mismatch (:type (ex-data ex))))
      (is (= :lr.posting/account (:attribute (ex-data ex)))))))

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
