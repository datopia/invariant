(ns invariant.datomic-test
  (:require [clojure.test :refer [deftest testing is] :as test]
            [invariant.datomic :refer :all]
            [datahike.api :as d]))



(deftest unnest-query-test
  (is (= '(datomic.api/q [:find ?matches .
                          :in $before $after $empty-with-txs $tx-ops [?sum-before ?sum-after ?sum-change]
                          :where
                          [(= ?sum-before ?sum-after)]
                          [(= ?sum-change 0)]]
                         $before $after $empty-with-txs $tx-ops
                         (datomic.api/q [:find (sum ?balance-before)
                                         :in $before $after $empty-with-txs $tx-ops ?sum :where [(= ?balance-before 42)]]
                                        $before $after $empty-with-txs $tx-ops
                                        (datomic.api/q [:find (sum ?balance-before)
                                                        :in $before $after $empty-with-txs $tx-ops
                                                        :where [(= ?balance-before 45)]]
                                                       $before $after $empty-with-txs $tx-ops)))
         (unnest-query '[:find ?matches .
                         :in $before $after $empty-with-txs $tx-ops
                         :where
                         ;; run the sub-query
                         [(datomic.api/q [:find (sum ?balance-before)
                                          :in $before $after $empty-with-txs $tx-ops
                                          :where
                                          [(= ?balance-before 42)]
                                          [(datomic.api/q [:find (sum ?balance-before)
                                                           :in $before $after $empty-with-txs $tx-ops
                                                           :where
                                                           [(= ?balance-before 45)]]
                                                           $before $after $empty-with-txs $tx-ops) ?sum]]
                                         $before $after $empty-with-txs $tx-ops)
                          [[?sum-before ?sum-after ?sum-change]]]
                         [(= ?sum-before ?sum-after)]
                         [(= ?sum-change 0) ?matches]]
                       '[$before $after $empty-with-txs $tx-ops])))


  (let []
    )

  )



