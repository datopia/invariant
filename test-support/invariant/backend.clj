(ns invariant.backend)

(defprotocol Backend
  (tempid            [_ v])
  (unnest-query      [_ q sources])
  (assert-invariants [_ txs])
  (transact          [_ txs]))
