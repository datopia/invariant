(ns invariant.query
  (:require [datalog.parser :as p]
            [datalog.parser.type]
            [datahike.query]))

(def invariant-query '[:find ?q .
                       :in $ ?a
                       :where
                       [?e :invariant/rule ?a]
                       [?e :invariant/query ?q]])

(def ^:dynamic *allowed-fns*
  (into #{'subquery} (keys datahike.query/built-ins)))

(def fn-selector (comp #{datalog.parser.type.Function
                         datalog.parser.type.Predicate} type))

(defn assert-safe-query [query]
  (doseq [c (filter fn-selector (:qwhere (p/parse query)))
          :let [f (:symbol (:fn c))]]
    (when (#{'subquery} f)
      (let [q (:value (first (:args c)))]
        (assert-safe-query q)))
    (when-not (*allowed-fns* f)
      (throw (ex-info "Function not allowed."
                      {:type :invariant/invalid-function-call
                       :call c})))))

(defn assert-valid-query [query]
  (let [res        (p/parse query)
        called-fns (filter fn-selector (:qwhere res))]
    (when-not (= (count (:qin res)) 4)
      (throw (ex-info "The query operates on exactly 4 sources: $before, $after, $empty-with-txs, $tx-seq"
                      {:type :invariant/number-of-sources-not-4
                       :sources (:qin res)})))
    (doseq [c called-fns
            :let [f (:symbol (:fn c))]]
      (when (#{'subquery} f)
        (let [q (:value (first (:args c)))]
          (assert-valid-query q)))
      (when-not (*allowed-fns* f)
        (throw (ex-info "Function not allowed."
                        {:type :invariant/invalid-function-call
                         :call c}))))))





