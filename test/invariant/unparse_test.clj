(ns invariant.unparse-test
  (:require [invariant.unparse :refer [unparse]]
            [datahike.parser :refer [parse-query]]
            [clojure.test :refer [deftest testing is] :as test]))


(deftest unparse-test
  (testing "Datahike query unparsing."
    (is (= '[:find (sum ?balance-before)
             :in $before $after $txn $txs
             :where
             [(= ?balance-before 42)]
             [(d/q [:find (sum ?balance-before)
                    :in $before $after $txn $txs
                    :where
                    [(= ?balance-before 42)]]
                   $before $after $txn $txs)]]
         (unparse (parse-query '[:find (sum ?balance-before)
                                   :in $before $after $txn $txs
                                   :where
                                   [(= ?balance-before 42)]
                                   [(d/q [:find (sum ?balance-before)
                                          :in $before $after $txn $txs
                                          :where
                                          [(= ?balance-before 42)]]
                                         $before $after $txn $txs)]]))))))





