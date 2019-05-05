(ns invariant.test.common
  (:require [datahike.parser]
            [invariant.test.util :refer [read-resource]]
            [invariant.backend   :as backend]
            [clojure.walk        :refer [prewalk-replace]]))

(def example-txs (read-resource "example_txs.edn"))

(def unnested-query
  '(QUERY-SYM
    '[:find ?matches .
      :in   $before $after $empty-with-txs $tx-ops
      [[?sum-before ?sum-after ?sum-change] ...]
      :where
      [(= ?sum-before ?sum-after)]
      [(= ?sum-change 0) ?matches]]
    $before $after $empty-with-txs $tx-ops
    (QUERY-SYM
     '[:find  (sum ?balance-before)
       :in    $before $after $empty-with-txs $tx-ops ?sum
       :where [(= ?balance-before 42)]]
     $before $after $empty-with-txs $tx-ops
     (QUERY-SYM
      '[:find  (sum ?balance-before)
        :in    $before $after $empty-with-txs $tx-ops
        :where [(= ?balance-before 45)]]
      $before $after $empty-with-txs $tx-ops))))

(def nested-query
  '[:find ?matches .
    :in   $before $after $empty-with-txs $tx-ops
    :where
    [(subquery
      [:find (sum ?balance-before)
       :in   $before $after $empty-with-txs $tx-ops
       :where
       [(= ?balance-before 42)]
       [(subquery [:find  (sum ?balance-before)
                   :in    $before $after $empty-with-txs $tx-ops
                   :where [(= ?balance-before 45)]]
                  $before $after $empty-with-txs $tx-ops) ?sum]]
      $before $after $empty-with-txs $tx-ops)
     [[?sum-before ?sum-after ?sum-change]]]
    [(= ?sum-before ?sum-after)]
    [(= ?sum-change 0) ?matches]])

(defn unnesting? [backend fn-sym]
  (= (prewalk-replace {'QUERY-SYM fn-sym} unnested-query)
     (backend/unnest-query
      backend nested-query
      '[$before $after $empty-with-txs $tx-ops])))

(let [bad-invariant (read-resource "bad_invariant.edn")]
  (defn bad-invariant-deployment? [backend]
    (let [tid (backend/tempid backend -1)
          txn [[:db/add tid :invariant/rule  :account/balance]
               [:db/add tid :invariant/query (pr-str bad-invariant)]]]
      (= '{:type :invariant/invalid-function-call,
           :call #datahike.parser.Predicate
           {:fn #datahike.parser.PlainSymbol{:symbol evil-haha},
            :args [#datahike.parser.Constant{:value 1}
                   #datahike.parser.Constant{:value 2}
                   #datahike.parser.Constant{:value 3}]}}
         (try
           (backend/assert-invariants backend txn)
           (catch Exception e
             (ex-data e)))))))

(defn- adjust-invariant [q {bigdec? :bigdec?}]
  (prewalk-replace {'sum-change-expected (cond-> 0 bigdec? bigdec)} q))

(defn deployed-valid-invariant? [backend & [opts]]
  (let [q   (adjust-invariant (read-resource "valid_invariant.edn") opts)
        tid (backend/tempid backend :db.part/user)
        txn [[:db/add tid :invariant/rule  :account/balance]
             [:db/add tid :invariant/query (pr-str q)]]]
    (backend/assert-invariants backend txn)
    @(backend/transact backend txn)
    true))

(defn balance-mismatch? [backend txs]
  (= {:type      :invariant/invariant-mismatch,
      :attribute :account/balance}
     (try
       (backend/assert-invariants backend txs)
       (catch Exception e
         (select-keys (ex-data e) #{:type :attribute})))))
