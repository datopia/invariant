(ns invariant.transaction
  "WIP, supposed to support datomic/datahike map transaction syntax."
  (:require [datahike.db :as ddb]))

(defn- maybe-wrap-multival [a vs]
  (cond
    ;; not a multival context
    (not (or (ddb/reverse-ref? a)
             #_(multival? db a)))
    [vs]

    ;; not a collection at all, so definitely a single value
    (not (or #_(da/array? vs)
             (and (coll? vs) (not (map? vs)))))
    [vs]

    ;; probably lookup ref
    (and (= (count vs) 2)
         #_(is-attr? db (first vs) :db.unique/identity))
    [vs]

    :else vs))

(defn- explode [entity]
  (let [eid (:db/id entity)]
    (for [[a vs] entity
          :when  (not= a :db/id)
          :let   [reverse?   (ddb/reverse-ref? a)
                  straight-a (if reverse? (ddb/reverse-ref a) a)]
          v      (maybe-wrap-multival a vs)]
      (if (and #_(ref? db straight-a) (map? v)) ;; another entity specified as nested map
        (assoc v (ddb/reverse-ref a) eid)
        (if reverse?
          [:db/add v   straight-a eid]
          [:db/add eid straight-a v])))))

(comment
  (mapcat explode [#:account{:db/id   1
                             :name    "Moe"
                             :balance 5000M
                             :unit    :datom}
                   #:account{:db/id   2
                             :name    "Christian"
                             :balance 100M
                             :unit    :datom}
                   #:account{:db/id   3
                             :name    "Danny"
                             :balance  3000M
                             :unit    :datom}])

  (require '[datahike.api :refer :all])

  (def uri "datahike:mem:///test")

  ;; create a database at this place
  (create-database uri)

  (def conn (connect uri))

  ;; lets add some data and wait for the transaction
  @(transact conn [{:db/id 1 :name "Ivan" :age 15}
                   {:db/id 2 :name "Petr" :age 37}
                   {:db/id 3 :name "Ivan" :age 37}
                   {:db/id 4              :age 15}])

  (require '[clojure.string :as str])

  (q '[:find ?name (count ?e)
       :where
       [?e :name ?name]
       [(str/includes? ?name "Iv")]]
     @conn))
