(ns invariant.transaction
  "WIP, supposed to support datomic/datahike map transaction syntax."
  (:require [datahike.db :as ddb]))

(defn maybe-wrap-multival [a vs]
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

(defn explode [entity]
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
  (mapcat explode [{:db/id 1,
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
                    :account/unit :datom}]))
