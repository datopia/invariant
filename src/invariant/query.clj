(ns invariant.query
  (:require [datahike.parser :as p]
            [datahike.query]))

(def allowed-fns (atom (into #{'subquery}
                             (keys datahike.query/built-ins))))

(defn valid-query? [query]
  (let [res (p/parse-query query)
        called-fns (->>
                    (:qwhere res)
                    (filter (fn [c]
                              (let [t (type c)]
                                (or
                                 (= datahike.parser.Function t)
                                 (= datahike.parser.Predicate t))))))]
    (when-not (= (count (:qin res)) 4)
      (throw (ex-info "The query operates on exactly 4 sources: $before, $after, $empty-with-txs, $tx-seq"
                      {:type :invariant/number-of-sources-not-4
                       :sources (:qin res)})))
    (doseq [c called-fns]
      (let [f (:symbol (:fn c))]
        (when (#{'subquery} f)
          (let [q (:value (first (:args c)))]
            (valid-query? q)))
        (when-not (@allowed-fns f)
          (throw (ex-info "Function not allowed." {:type :invariant/invalid-function-call
                                                   :call c})))))))



