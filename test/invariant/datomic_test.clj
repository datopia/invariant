(ns invariant.datomic-test
  (:require [clojure.test :refer [deftest testing is] :as test]
            [invariant.datomic :refer :all]
            [datomic.api :as d]))


(deftest unnest-query-test
  (is (= '(datomic.api/q '[:find ?matches .
                           :in $before $after $empty-with-txs $tx-ops [[?sum-before ?sum-after ?sum-change] ...]
                          :where
                          [(= ?sum-before ?sum-after)]
                          [(= ?sum-change 0) ?matches]]
                         $before $after $empty-with-txs $tx-ops
                         (datomic.api/q '[:find (sum ?balance-before)
                                         :in $before $after $empty-with-txs $tx-ops ?sum
                                         :where [(= ?balance-before 42)]]
                                        $before $after $empty-with-txs $tx-ops
                                        (datomic.api/q '[:find (sum ?balance-before)
                                                        :in $before $after $empty-with-txs $tx-ops
                                                        :where [(= ?balance-before 45)]]
                                                      $before $after $empty-with-txs $tx-ops)))
         (unnest-query '[:find ?matches .
                         :in $before $after $empty-with-txs $tx-ops
                         :where
                         [(subquery [:find (sum ?balance-before)
                                     :in $before $after $empty-with-txs $tx-ops
                                     :where
                                     [(= ?balance-before 42)]
                                     [(subquery [:find (sum ?balance-before)
                                                 :in $before $after $empty-with-txs $tx-ops
                                                 :where
                                                 [(= ?balance-before 45)]]
                                                $before $after $empty-with-txs $tx-ops) ?sum]]
                                    $before $after $empty-with-txs $tx-ops)
                          [[?sum-before ?sum-after ?sum-change]]]
                         [(= ?sum-before ?sum-after)]
                         [(= ?sum-change 0) ?matches]]
                       '[$before $after $empty-with-txs $tx-ops]))))

(def ^:dynamic conn nil)


(def schema [{:db/id #db/id[:db.part/db]
              :db/ident :invariant/rule
              :db/valueType :db.type/keyword
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}
             {:db/id #db/id[:db.part/db]
              :db/ident :invariant/query
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}
             {:db/id #db/id[:db.part/db]
              :db/ident :account/name
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}
             {:db/id #db/id[:db.part/db]
              :db/ident :account/balance
              :db/valueType :db.type/bigdec
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}
             {:db/id #db/id[:db.part/db]
              :db/ident :account/unit
              :db/valueType :db.type/keyword
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}
             {:db/id #db/id[:db.part/db]
              :db/ident :transaction/signed-by
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/many
              :db.install/_attribute :db.part/db}])


(defn datomic-db-fixture [f]
  (let [uri "datomic:mem:///invariant-test"
        _ (d/create-database uri)
        example-txs [{:db/id 1,
                      :account/name "Moe",
                      :account/balance 5000M,
                      :account/unit :datom}
                     {:db/id 2,
                      :account/name "Christian",
                      :account/balance 100M,
                      :account/unit :datom}
                     {:db/id 3,
                      :account/name "Danny",
                      :account/balance 3000M,
                      :account/unit :datom}]]

    (binding [conn (d/connect uri)]
      @(d/transact conn schema) 
      @(d/transact conn example-txs)
      @(d/transact conn [{:db/id (d/tempid :db.part/user)
                          :db/ident :+v
                          :db/fn (d/function {:lang "clojure"
                                              :params '[db eid attr delta]
                                              :code '(let [m (d/pull db [attr] eid)
                                                           v (attr m 0M)]
                                                       [[:db/add eid attr (+ v delta)]])})}])
      (f)
      (d/delete-database uri))))

(test/use-fixtures :each datomic-db-fixture)

(deftest bad-invariant-deployment
  (testing "Testing deployment of bad invariant."
    (let [tid (d/tempid -1)
          invariant-txs
          [[:db/add tid :invariant/rule :account/balance]
           [:db/add tid :invariant/query
            (pr-str '[:find ?matches .
                      :in $before $after $txn $txs
                      :where
                      ;; run the sub-query
                      [(subquery [:find (sum ?balance-before) (sum ?balance-after) (sum ?balance-change)
                                  :with ?affected-entity
                                  :in $before $after $txn $txs
                                  :where
                                  [(evil-haha 1 2 3)]
                                  ;; Unify data from databases and transactions with affected-entity
                                  [$after    ?affected-entity         :account/balance    ?balance-after]
                                  [$txn      ?affected-entity         :account/balance    ?balance-change]
                                  [(get-else $before ?affected-entity :account/balance 0) ?balance-before]

                                  ;; 1. Zero-Sum
                                  [(+ ?balance-change ?balance-before) ?computed-balance-after]
                                  [(= ?balance-after ?computed-balance-after)]

                                  ;; 2. Positivity
                                  [(>= ?balance-after 0)]

                                  ;; 3. Sender spending
                                  #_[$txn    _                 :transaction/signed-by ?sender]
                                  #_[(datopia.attribute-invariants/balance-check
                                     ?sender ?affected-entity ?balance-before ?balance-after)]]
                                 $before $after $txn $txs)
                       [[?sum-before ?sum-after ?sum-change]]]
                      [(= ?sum-before ?sum-after)]
                      [(= ?sum-change 0) ?matches]])]]]
      (is (= '{:type :invariant/invalid-function-call,
               :call #datahike.parser.Predicate{:fn #datahike.parser.PlainSymbol{:symbol evil-haha},
                                                :args [#datahike.parser.Constant{:value 1}
                                                       #datahike.parser.Constant{:value 2}
                                                       #datahike.parser.Constant{:value 3}]}}
             (try
               (ensure-invariants conn schema invariant-txs)
               (catch Exception e
                 (ex-data e))))))))


(deftest invariant-deployment
  (testing "Testing deployment of valid invariant."
    (let [tid (d/tempid :db.part/user) 
          invariant-txs
          [[:db/add tid :invariant/rule :account/balance]
           [:db/add tid :invariant/query
            (pr-str '[:find ?matches .
                      :in $before $after $empty+txs $txs
                      :where
                      ;; run the sub-query
                      [(subquery [:find (sum ?balance-before) (sum ?balance-after) (sum ?balance-change)
                                  :with ?affected-entity
                                  :in $before $after $empty+txs $txs
                                  :where
                                  ;; Unify data from databases and transactions with affected-entity
                                  [$after      ?affected-entity         :account/balance    ?balance-after]
                                  [$empty+txs  ?affected-entity         :account/balance    ?balance-change]
                                  [(get-else $before ?affected-entity :account/balance 0) ?balance-before]

                                  ;; 1. Zero-Sum
                                  [(+ ?balance-change ?balance-before) ?computed-balance-after]
                                  [(= ?balance-after ?computed-balance-after)]

                                  ;; 2. Positivity
                                  [(>= ?balance-after 0)]

                                  ;; 3. Sender spending
                                  #_[$txn    _                 :transaction/signed-by ?sender]
                                  #_[(datopia.attribute-invariants/balance-check
                                     ?sender ?affected-entity ?balance-before ?balance-after)]]
                                 $before $after $empty+txs $txs)
                       [[?sum-before ?sum-after ?sum-change]]]
                      [(= ?sum-before ?sum-after)]
                      [(= ?sum-change 0M) ?matches]])]]]
      (is (ensure-invariants conn schema invariant-txs))
      ;; install them
      @(d/transact conn invariant-txs)

      ;; test a valid transaction
      (let [tid (d/tempid :db.part/user)
            transfer-transaction
            [[:db/add tid :transaction/signed-by 1]
             [:db/add tid :transaction/signed-by 3]
             [:+v 0 :account/balance  +1]
             [:+v 3 :account/balance  -3]
             [:+v 1 :account/balance -50]
             [:+v 2 :account/balance +52]]]
        (is (ensure-invariants conn schema transfer-transaction)))


      ;; test non-zero balance change
      (let [tid (d/tempid :db.part/user)
            invalid-transaction
            [[:db/add tid :transaction/signed-by 1]
             [:db/add tid :transaction/signed-by 3]
             [:+v 0 :account/balance  +1]
             [:+v 2 :account/balance +52]
             [:+v 3 :account/balance  -2]]]
        (is (= {:type :invariant/invariant-mismatch,
                :attribute :account/balance}
               (try
                 (ensure-invariants conn schema invalid-transaction)
                 (catch Exception e
                   (select-keys (ex-data e) #{:type :attribute}))))))

      ;; negative balance
      (let [tid (d/tempid :db.part/user)
            invalid-transaction
            [[:db/add tid :transaction/signed-by 1]
             [:db/add tid :transaction/signed-by 3]
             [:+v 2 :account/balance +5000]
             [:+v 3 :account/balance -5000]]]
        (is (= {:type :invariant/invariant-mismatch,
                :attribute :account/balance}
               (try
                 (ensure-invariants conn schema invalid-transaction)
                 (catch Exception e
                   (select-keys (ex-data e) #{:type :attribute})))))))))





