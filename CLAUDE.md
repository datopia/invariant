# CLAUDE.md

Project guidance for AI assistants working in this repository.

## What this library is

A data-integrity layer for Datahike: define invariants as 4-source
Datalog queries on attributes; transactions that would violate any
registered invariant for an attribute they touch are rejected before
commit.

The core primitive is `assert-invariants` (validates without
committing) and the wrapper `transact-with-invariants` (validates then
commits). Both come in 2-arg (schema-from-conn) and 3-arg (explicit
schema) forms.

The 4 query sources are `$before`, `$after`, `$empty+txs` (a fresh
empty-db with only the tx applied), and `$txs` (the raw tx ops).
Source 3 is the load-bearing one for properties intrinsic to the tx
itself (zero-sum balance transfers, uniqueness-within-tx, etc.).

## Single dependency

Targets datahike 0.8.x. No other runtime deps beyond datahike + Clojure.

## Project layout

```
src/invariant/
  datahike.clj   — the meat: `+`, `get-attribute` multimethod,
                   `invariant-holds?`, `assert-invariants`,
                   `transact-with-invariants`
  query.clj      — query validator (4-source check + allowed-fn
                   whitelist), with `assert-valid-query` +
                   `assert-safe-query`

test/invariant/
  datahike_test.clj         — attribute extraction, valid-query
                              shape, lookup-ref handling, end-to-end
                              backend tests via `common.clj`
  datahike_wrapper_test.clj — transact-with-invariants 2-arg + 3-arg
  query_test.clj            — query.clj coverage
  test/common.clj           — backend-protocol-driven shared tests

dev-resources/
  datahike_schema.edn       — canonical schema fixture
  example_txs.edn           — Alice/Bob/Christian/Danny seed
  valid_invariant.edn       — the balance-transfer invariant
  bad_invariant.edn         — fails 4-source shape check

bin/
  run-tests        — `clojure -X:test`
  run-single-test  — `clojure -X:test :only <test-sym>`
```

## Testing

```bash
bin/run-tests                                       # all tests
bin/run-single-test invariant.datahike-test/attribute-test
```

The suite uses `cognitect.test-runner` (configured in deps.edn).

## When to use invariants vs schema

Datahike's schema validates structural shape (types, cardinality,
uniqueness). Invariants validate cross-entity and tx-shape properties
that the schema can't see: "the postings sum to zero", "no new
balance is negative", "every signed-by sender authorised every
posting under them". The query model gives you access to before / after
/ tx-only views to express these declaratively.

## Provenance

The lookup-ref-seed fix in `invariant-holds?` and the destructive-form
`get-attribute` defaults were upstreamed from kontor's vendored copy
of this library (kontor research note 160 §T-3) — they surfaced as
this library was put under real workload and the gaps were closed at
the consumer side first. The upstream backport in this repo is the
canonical implementation now.
