(ns invariant.datomic
  (:require [datomic.api :as api]
            [datahike.parser :as p]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [invariant.query :refer [valid-query?]]
            [invariant.unparse :refer [unparse]]))


(defn unnest-deep-queries [[_ query & sources]]
  (let [res (p/parse-query query)
        clean-clauses (->> (:qwhere res)
                         (filter (fn [f] (not= (:symbol (:fn f)) 'datomic.api/q)))
                         vec)
        nested-functions (->> (:qwhere res)
                            (filter (fn [c]
                                      (let [t (type c)]
                                        (= datahike.parser.Function t))))
                            (filter (fn [f] (= (:symbol (:fn f)) 'datomic.api/q))))]
    (concat
     (list 'datomic.api/q
           (unparse
            (-> res
                (assoc :qwhere clean-clauses)
                (update :qin concat (map :binding nested-functions)))))
     sources
     (map (comp unnest-deep-queries unparse) nested-functions))))


(defn unnest-query [query sources]
  (unnest-deep-queries (concat ['api/q query] sources)))

(defn get-attribute-dispatch [v]
  (first v))

(defmulti get-attribute get-attribute-dispatch)

(defn +v [db eid attr delta]
  (let [m (api/pull db [attr] eid)
        v (attr m 0)]
    [[:db/add eid attr (+ v delta)]]))


(defmethod get-attribute :+v
  [[_ _ eid attr delta]]
  attr)


(defmethod get-attribute :db/add
  [[_ e a v]]
  a)


(let [counter (atom 0)]
  (defn datomic-empty-db [schema]
    (let [uri (str "datomic:mem:///temp-invariant-" (swap! counter inc))
          _ (api/create-database uri)
          conn (api/connect uri)]
      @(api/transact conn schema)
      @(api/transact conn [{:db/id (api/tempid :db.part/user)
                            :db/ident :+v
                            :db/fn (api/function {:lang "clojure"
                                                  :params '[db _ attr eid delta]
                                                  :code '(let [m (d/pull db [attr] eid)
                                                               v (attr m 0M)]
                                                           [[:db/add eid attr (+ v delta)]])})}])
      (api/db conn)
      )))


(defn ensure-invariances [connection schema tx-data]
  (let [attribute-txs (map (fn [tx] [(get-attribute tx) tx])
                           tx-data)
        attributes (distinct (map first attribute-txs))]
    (doseq [[a tx] attribute-txs
            :when (= a :invariant/query)
            :let [[_ _ _ v] tx]]
      (valid-query? (edn/read-string v)))

    (doseq [a attributes]
      (when-let [inv-qs (api/q '[:find ?q .
                                 :in $ ?a
                                 :where
                                 [?e :invariant/rule ?a]
                                 [?e :invariant/query ?q]]
                               (api/db connection)
                               a)]
        (when-not (api/q (unnest-query (read-string inv-qs))
                         ;; current state
                         (api/db connection)
                         ;; apply transaction to current state
                         (api/with (api/db connection) tx-data)
                         ;; empty database with only transaction applied
                         (api/with (datomic-empty-db schema) tx-data)
                         tx-data)
          (throw (ex-info "Invariant mismatch." {:type :invariant/invariant-mismatch
                                                 :attribute a
                                                 :invariant (edn/read-string inv-qs)
                                                 :tx-data tx-data})))
        ))
    true))


(defmethod invariant.core/invariant :datomic
  [connection schema tx-data]
  (ensure-invariances connection schema tx-data))




(comment

;; original


(d/q '[:find ?matches .
      :in $before $after $txn $txs
      :where
      ;; run the sub-query
      [(d/q [:find (sum ?balance-before) (sum ?balance-after) (sum ?balance-change)
             :with ?affected-entity
             :in $before $after $txn $txs
             :where
             [(evil-haha 1 2 3)]]
            $before $after $txn $txs)
       [[?sum-before ?sum-after ?sum-change]]]
      [(= ?sum-before ?sum-after)]
       [(= ?sum-change 0) ?matches]]
     $before $after $txn $txs )






;; transform



(d/q '[:find ?matches .
       :in $before $after $txn $txs ?sum-before ?sum-after ?sum-change
       :where
       [(= ?sum-before ?sum-after)]
       [(= ?sum-change 0) ?matches]]
     $before $after $txn $txs
     (d/q '[:find (sum ?balance-before) (sum ?balance-after) (sum ?balance-change)
            :with ?affected-entity
            :in $before $after $txn $txs
            :where
            [(evil-haha 1 2 3)]]
          $before $after $txn $txs)
     )








;; TODO
;; cache create empty DB with schema


  (def uri "datomic:mem:///datopia-transfer-example") 


(try
  ;; initialize database
  (api/create-database uri)
  (catch Exception e
    (prn e))) 

(api/delete-database uri)

(def conn (api/connect uri)) 



(def example-txs [{:db/id 1,
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
                   :account/unit :datom}]) 

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
              :db/valueType :db.ktype/keyword
              :db/cardinality :db.cardinality/one
              :db.install/_attribute :db.part/db}]) 

@(api/transact conn schema)

@(api/transact conn example-txs)

(let [counter (atom 0)]
  (defn datomic-empty-db [schema]
    (let [uri (str "datomic:mem:///temp-invariant-" (swap! counter inc))
          _ (api/create-database uri)
          conn (api/connect uri)]
      @(api/transact conn schema)
      @(api/transact conn [{:db/id (api/tempid :db.part/user)
                           :db/ident :+v
                           :db/fn (api/function {:lang "clojure"
                                                 :params '[db _ attr eid delta]
                                                 :code '(let [m (d/pull db [attr] eid)
                                                              v (attr m 0M)]
                                                          [[:db/add eid attr (+ v delta)]])})}])
      (api/db conn)
      )))


(defn ensure-invariances-datomic [connection schema tx-data]
  ;; TODO explode map representations temporarily
  ;; TODO properly handle db.call
  (let [invariant-attributes (filter (fn [[_ e a v]]
                                       (= a :invariant/query))
                                     tx-data)
        attributes (disj (into #{} (map (fn [[_ _ a _]] a) tx-data))
                         :invariant/rule)]
    (prn "matching attributes" attributes)
    (doseq [[_ e a v] invariant-attributes]
      (valid-query? (edn/read-string v)))

    (doseq [a (seq attributes)]
      ;; TODO cache
      (when-let [inv-qs (api/q '[:find ?q .
                              :in $ ?a
                              :where
                               [?e :invariant/rule ?a]
                               [?e :invariant/query ?q]]
                               (api/db connection)
                            a)]
        (prn "verifying" a inv-qs)
        (when-not (api/q (read-string inv-qs)
                       ;; current state
                         (api/db connection)
                       ;; apply transaction to current state
                       (api/with (api/db connection) tx-data)
                       ;; empty database with only transaction applied
                       ;; TODO schema
                       (api/with (datomic-empty-db schema) tx-data)
                       tx-data)
          (throw (ex-info "Invariant mismatch." {:type ::invariant-mismatch
                                                 :attribute a
                                                 :invariant (read-string inv-qs)
                                                :tx-data tx-data})))))))

(ensure-invariances conn schema example-txs) 

@(api/transact conn [[:db/add 123 :invariant/rule :account/balance]
                   [:db/add 123 :invariant/query
                    (pr-str '[:find ?matches .
                              :in $before $after $txn
                              :where
                              ;; run the sub-query
                              [(invariant.core/subquery "multi-transfer-sub"
                                                        $before $after $txn)
                               [[?sum-before ?sum-after ?sum-change]]]
                              [(= ?sum-before ?sum-after)]
                              [(= ?sum-change 0) ?matches]])]])

(ensure-invariances-datomic conn
                            schema
                            [[:db/add 123 :invariant/rule :account/balance]
                             [:db/add 123 :invariant/query
                              (pr-str '[:find ?matches .
                                        :in $before $after $txn
                                        :where
                                        ;; run the sub-query
                                        [(invariant.core/subquery "multi-transfer-sub"
                                                                  $before $after $txn)
                                         [[?sum-before ?sum-after ?sum-change]]]
                                        [(= ?sum-before ?sum-after)]
                                        [(= ?sum-change 0) ?matches]])]])


(def invalid1-transaction
  [[:+v -1 :account/balance 0  +1]
   [:+v -1 :account/balance 2 +52]
   [:+v -1 :account/balance 3  -2]])


(defn +v [db attr eid delta]
  (let [m (d/pull db [attr] eid)
        v (attr m 0)]
    [[:db/add eid attr (+ v delta)]]))

(api/transact conn [{:db/id (api/tempid :db.part/user)
                     :db/ident :+v
                     :db/fn (api/function {:lang "clojure"
                                           :params '[db _ attr eid delta]
                                           :code '(let [m (d/pull db [attr] eid)
                                                        v (attr m 0M)]
                                                    [[:db/add eid attr (+ v delta)]])})}])

@(api/transact conn invalid1-transaction)

(api/invoke (api/db conn) :+v)

;; tx-data to install the function

(api/transact conn [{:db/id (api/tempid :db.part/user)
                     :db/ident :hello
                     :db/fn (api/function {:lang "clojure"
                                           :params []
                                           :code '(println :hello)})}])

(api/invoke (api/db conn) :hello)

;; tx-data to call the function
[[:add-doc "foo" "this is foo's doc"]]


(ensure-invariances-datomic conn schema invalid1-transaction)

(api/q '[:find ?a
         :in $ $foo
         :where
         [(datomic.api/q '[:find ?a
                           :in $ $foo
                           :where
                           [(nested-fn-call ?a 5)]]
                         $ $foo)
          ?a]
         ]
       (api/db conn)
       :foo)

 

)
