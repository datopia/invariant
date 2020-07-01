(ns invariant.query-test
  (:refer-clojure :exclude [+])
  (:require [clojure.test :as test
             :refer [deftest testing is]]
            [invariant.query
             :refer [assert-valid-query assert-safe-query]]))

(deftest valid-safe-test
  (testing "Queries safe to run."
    (is (nil? (assert-safe-query '[:find ?a
                                   :in   $a
                                   :where
                                   [(subquery [:find  ?a
                                               :in    $a
                                               :where [(= ?a 5) ?b]]
                                              $a) ?a]])))
    (is (= 'nested-evil
           (try
             (assert-safe-query '[:find ?a
                                  :in   $a
                                  :where
                                  [(subquery [:find  ?a
                                              :in    $a
                                              :where [(nested-evil ?a 5)]]
                                             $a) ?a]])
             (catch Exception e
               (get-in (ex-data e) [:call :fn :symbol])))))

    (is (= 'nested-evil
           (try
             (assert-safe-query '[:find ?a
                                  :in   $a
                                  :where
                                  [(subquery [:find  ?a
                                              :in    $a
                                              :where [(nested-evil ?a 5) ?b]]
                                             $a) ?a]])
             (catch Exception e
               (get-in (ex-data e) [:call :fn :symbol])))))))
