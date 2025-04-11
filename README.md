# Invariant

A library for enforcing data integrity in Datalog databases like [Datahike](https://github.com/replikativ/datahike) by defining and validating invariants on attributes.

## Overview

Invariant extends Datalog databases with a powerful system to enforce domain-specific constraints on transactions. Unlike simple schema validation, invariants in this library can:

- Validate relationships across multiple entities
- Ensure consistent state transitions
- Enforce complex business rules
- Prevent data corruption in multi-step transactions

All invariants are defined as Datalog queries that run against multiple database states to validate transactions before they're committed.

## Key Features

- **Declarative Invariants**: Define constraints using familiar Datalog query syntax
- **Transaction Validation**: Automatically check transactions against relevant invariants
- **Context-Aware**: Evaluate invariants against current state, future state, and transaction data
- **Safety Checks**: Prevent unsafe queries from being used as invariants
- **Extensible**: Designed to work with multiple Datalog database implementations

## How It Works

Invariants are stored in the database as entities with `:invariant/rule` and `:invariant/query` attributes. When a transaction is submitted:

1. The system identifies which attributes are affected
2. Relevant invariants for those attributes are loaded
3. For each invariant:
   - A query is run against multiple database views:
     - `$before`: The current database state
     - `$after`: The database after applying the transaction
     - `$empty+datoms`: An empty database with only the transaction data
     - `$datoms`: The raw transaction operations
   - If any invariant query returns `false`, the transaction is rejected

This approach allows enforcing complex constraints like zero-sum transfers, referential integrity, and business rules.

## Example: Balance Transfer Invariant

Here's a real-world example of an invariant that ensures valid money transfers between accounts:

```clojure
[:find ?matches .
 :in $before $after $empty+datoms $datoms
 :where
 [(q [:find (sum ?balance-before) (sum ?balance-after) (sum ?balance-change)
      :with ?affected-entity
      :in $before $after $empty+datoms $datoms
      :where
      ;; 1. Match account entities with their balances
      [$after        ?affected-entity   :account/balance  ?balance-after]
      [$after        ?affected-entity   :account/name     ?account-name]
      [$empty+datoms ?empty-account-id  :account/name     ?account-name]
      [$empty+datoms ?empty-account-id  :account/balance  ?balance-change]
      [(get-else $before ?affected-entity :account/balance 0M) ?balance-before]

      ;; 2. Ensure balance changes are correctly computed
      [(+ ?balance-change ?balance-before) ?computed-balance-after]
      [(= ?balance-after ?computed-balance-after)]

      ;; 3. No negative balances allowed
      [(>= ?balance-after 0)]

      ;; 4. Only signers can have negative balance changes
      [$datoms _ _ :datopia/signed-by ?sender]
      [(= ?sender ?account-name) ?is-sender]
      [(>= ?balance-change 0) ?pos-change]
      [(or ?is-sender ?pos-change)]]
     $before $after $empty+datoms $datoms)
  [[?sum-before ?sum-after ?sum-change]]]
 ;; 5. Ensure zero-sum: total money in system doesn't change
 [(= ?sum-before ?sum-after)]
 [(== ?sum-change 0) ?matches]]
```

This invariant enforces several rules simultaneously:
- Money transfer must be zero-sum (total money in system remains constant)
- Account balances must never go negative
- Only transaction signers can have negative balance changes

## Usage

Add the dependency to your project and follow this basic pattern:

```clojure
;; 1. Define an invariant as a Datalog query
(def my-invariant-query '[:find ?valid .
                          :in $before $after $empty+datoms $datoms
                          ;; ... your logic here ...])

;; 2. Deploy the invariant to your database
(d/transact! conn [[:db/add -1 :invariant/rule :my-attribute]
                  [:db/add -1 :invariant/query (pr-str my-invariant-query)]])

;; 3. Use normal transactions - they'll be validated against the invariant
;; If a transaction would violate the invariant, it will throw an exception
```

### Using the Transaction Wrapper

The library provides a transaction wrapper that automatically checks invariants before committing:

```clojure
(require '[invariant.datahike :as id]
         '[datahike.api :as d])

;; First, store your schema for later use
(def schema-txs [...])  ;; Your database schema transactions

;; Deploy schema and invariants using normal transact (no invariant checks)
(d/transact conn {:tx-data schema-txs})

;; Use the wrapper for normal operations to check invariants
(id/transact-with-invariants conn transaction-data schema-txs)
```

The `transact-with-invariants` function returns the transaction result directly, just like `datahike.api/transact`. This makes it easier to compose with other operations in your codebase.

For operations where you need to bypass invariant checks (like schema updates), use `datahike.api/transact` directly.

The `transact-with-invariants` function:
1. Takes your connection, transaction data, and schema transactions
2. Checks if any invariants apply to attributes in your transaction
3. Validates that your transaction maintains all invariants
4. Only commits the transaction if all invariants are satisfied

#### Complete Example

Here's a complete example showing how to use invariants with the transaction wrapper:

```clojure
(ns my-app.core
  (:require [datahike.api :as d]
            [invariant.datahike :as id]))

;; 1. Create and connect to a database
(def uri "datahike:mem:///my-db")
(d/create-database uri)
(def conn (d/connect uri))

;; 2. Define your schema with invariant support
(def schema
  [{:db/ident :invariant/rule
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}
   {:db/ident :invariant/query
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :account/name
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :account/balance
    :db/valueType :db.type/bigdec
    :db/cardinality :db.cardinality/one}
   {:db/ident :tx/signedBy
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

;; 3. Deploy schema using direct transaction (no invariant checks)
(def schema-tx schema)
(d/transact conn {:tx-data schema-tx})

;; 4. Create a zero-sum invariant for account balances
(def balance-invariant
  '[:find ?valid .
    :in $before $after $empty+tx $tx
    :where
    [(q [:find (sum ?before) (sum ?after) (sum ?delta)
         :in $before $after $empty+tx $tx
         :where
         [$after ?e :account/balance ?after]
         [$empty+tx ?e :account/balance ?delta]
         [(get-else $before ?e :account/balance 0M) ?before]]
       $before $after $empty+tx $tx)
     [[?before-sum ?after-sum ?delta-sum]]]
    [(= ?before-sum ?after-sum)]
    [(= ?delta-sum 0M) ?valid]])

;; 5. Deploy the invariant
(def invariant-tx
  [[:db/add (d/tempid :db.part/user) :invariant/rule :account/balance]
   [:db/add (d/tempid :db.part/user) :invariant/query (pr-str balance-invariant)]])

(id/transact-with-invariants conn invariant-tx schema-datoms)

;; 6. Create initial accounts
(def accounts-tx
  [{:account/name "Alice" :account/balance 1000M}
   {:account/name "Bob" :account/balance 500M}])

(id/transact-with-invariants conn accounts-tx schema-datoms)

;; 7. Valid transaction - zero sum transfer
(def valid-tx
  [[:db.fn/call id/+ [:account/name "Alice"] :account/balance -100]
   [:db.fn/call id/+ [:account/name "Bob"] :account/balance +100]
   [:db/add (d/tempid :db.part/tx) :tx/signedBy "Alice"]])

(id/transact-with-invariants conn valid-tx schema-datoms)

;; 8. Invalid transaction - creates money out of nowhere
(def invalid-tx
  [[:db.fn/call id/+ [:account/name "Alice"] :account/balance +100]
   [:db.fn/call id/+ [:account/name "Bob"] :account/balance +50]])

;; This will throw an exception:
;; (id/transact-with-invariants conn invalid-tx schema-datoms)
```

By using this pattern, you ensure that all transaction operations preserve your domain invariants. For a real-world application, you may want to modify the transaction wrapper to extract the schema internally or cache it for performance.

For more complex examples, look at the `datahike_test.clj` file in the tests directory.

## Development

```bash
# Run all tests
bin/run-tests

# Run a single test
bin/run-single-test invariant.datahike-test/invariant-deployment
```

## License

Copyright © 2018-2025 Christian Weilbach, Moe Aboulkheir, 2018 Danny Wilson

Distributed under the MIT License.
