(ns invariant.datahike
  (:refer-clojure :exclude [+])
  (:require [clojure.edn :as edn]
            [clojure.walk :as walk]
            [datahike.api   :as d]
            [datahike.core  :as dc]
            [datahike.query :as dq]
            [invariant.query
             :refer [assert-valid-query invariant-query]]))

;; Datahike's built-in datalog query fn whitelist + a `subquery` alias.
;; `assert-valid-query` validates user queries against this set; users
;; can `binding` it to add extra pure fns their invariants need.
(alter-var-root #'dq/built-ins assoc 'subquery datahike.api/q)
;; Keep the `q` alias too — preserves source-compatibility with consumers
;; of pre-port releases that used `(q ...)` inside invariant queries.
(alter-var-root #'dq/built-ins assoc 'q datahike.api/q)

;; ============================================================================
;; `+` — transactor-side balance arithmetic helper.
;;
;; Usage: `[:db.fn/call invariant.datahike/+ <selector> <attr> <delta>]`
;; where `selector` is either an eid or a lookup-ref `[:unique/attr v]`.
;; Builds the `[:db/add eid attr (new-balance)]` tuple. If the selector
;; is a lookup-ref pointing at a non-existent entity, also creates the
;; entity by ALSO emitting `[:db/add eid <unique-attr> <v>]`.
;;
;; Datahike 0.8.x assigns eids implicitly via tempid strings or negative
;; integers; the pre-0.8 `(d/tempid :db.part/user)` Datomic-style call
;; never existed in datahike. Use a string tempid keyed off the selector
;; so multiple `+` calls against the same not-yet-existing entity collapse
;; onto the same tempid (preserves the old "+ creates the entity on
;; first reference" semantics).
;; ============================================================================

(defn + [db selector attr delta]
  (let [m   (d/entity db selector)
        eid (or (:db/id m)
                ;; Stable tempid per (lookup-ref-attr, lookup-ref-value)
                ;; so sibling `+` ops on the same selector resolve to
                ;; the same new entity. Falls back to a generated
                ;; sentinel string for non-lookup-ref selectors.
                (if (and (vector? selector) (= 2 (count selector)))
                  (str "invariant/" (first selector) "/" (second selector))
                  (str "invariant/eid-" (hash selector))))
        v   (attr m 0M)]
    (concat (when (and (string? eid) (vector? selector) (= 2 (count selector)))
              ;; Brand-new entity — also emit the unique-attr that makes
              ;; the lookup-ref resolve, so the entity exists.
              [[:db/add eid (first selector) (second selector)]])
            [[:db/add eid attr (clojure.core/+ v delta)]])))

;; ============================================================================
;; get-attribute multimethod — which attribute(s) does a tx-form touch?
;; assert-invariants uses this to schedule the right invariant per tx.
;; ============================================================================

(defn get-attribute-dispatch [v]
  (cond
    (map? v) :entity-map
    :else
    (let [[a b] v]
      (cond (= :db.fn/call a) [:db.fn/call b]
            :else             a))))

(defmulti get-attribute
  "Returns either a single attribute keyword (for `[:db/add …]` /
   `[:db/retract …]` / `[:db.fn/call …]` tuples) OR a set of attribute
   keywords for entity-map tx forms (`{:db/id _ :foo 1 :bar 2}`).

   The single-vs-set return is what `assert-invariants` uses to schedule
   the right invariants per tx. Multimethod is open: consumers can
   `defmethod` it for custom tx-fn shapes."
  get-attribute-dispatch)

(defmethod get-attribute [:db.fn/call +]
  [[_ _ _eid attr _delta]]
  attr)

(defmethod get-attribute :db/add
  [[_ _e a _v]]
  a)

(defmethod get-attribute :db/retract
  [[_ _e a _v]]
  a)

(defmethod get-attribute :entity-map
  [m]
  (->> (keys m)
       (remove #{:db/id})
       set))

;; All "destructive" tx-data shapes — none asserts a new attribute
;; value, so per-attribute invariants have nothing to schedule against.
;; Returning nil keeps these tx-forms from crashing the dispatch when
;; they ride inside a `transact-with-invariants` call (e.g. consumers
;; that mix purge / retractEntity / cas with regular asserts in the
;; same tx). `:db.fn/cas` is conceptually an assertion but its
;; (eid, attr, old, new) tuple gives no clean attr to lift; left nil
;; for now — a future revision could schedule the CAS attr like
;; `:db/add` does.
(defmethod get-attribute :db/retractEntity     [_] nil)
(defmethod get-attribute :db.fn/retractEntity  [_] nil)
(defmethod get-attribute :db/purge             [_] nil)
(defmethod get-attribute :db.purge/entity      [_] nil)
(defmethod get-attribute :db.purge/attribute   [_] nil)
(defmethod get-attribute :db.fn/cas            [_] nil)

;; Defensive default — unknown tx-forms (consumer-registered custom
;; tx-fns) don't crash the invariant pipeline. The trade-off is that
;; per-attr invariants don't fire for them; consumers can `defmethod`
;; explicitly if they want coverage.
(defmethod get-attribute :default [_] nil)

;; ============================================================================
;; Invariant checking — the assert-invariants pipeline
;; ============================================================================

(defn- internal-schema->tx
  "Convert datahike's internal `:schema` map (ident → spec) back into
   a tx-data vector of canonical schema maps that can be `db-with`'d
   into a fresh empty-db.

   The internal map carries three kinds of entries — the genuine
   user-defined attrs, the reverse-lookup integer keys, and partial
   system/bootstrap entries (`:db/ident`, `:db/txInstant`, …). Keep
   only entries that resolve to a complete canonical spec."
  [schema-map]
  (->> schema-map
       (keep (fn [[k v]]
               (when (and (keyword? k)
                          (map? v)
                          (contains? v :db/valueType)
                          (contains? v :db/cardinality))
                 (assoc v :db/ident k))))
       vec))

(defn- ->schema-tx
  "Coerce a schema input to a tx-data vector:
   - vector → assume already-canonical install form, pass through
   - map    → reconstruct from datahike's internal :schema shape
   - nil    → nil"
  [schema]
  (cond
    (nil? schema) nil
    (vector? schema) schema
    (map? schema) (internal-schema->tx schema)))

(defn- resolve-schema
  "Schema sources, in priority order:
   1. Explicit `schema` arg from the back-compat 3-arg call shape.
   2. Live `@conn`'s internal `:schema` map.

   The 0.8.x runtime exposes the schema directly on the value of the
   conn ref, so consumers no longer have to pass it explicitly — but
   the 3-arg form is preserved for source-compat with pre-0.8 callers."
  [conn schema-or-nil]
  (->schema-tx (or schema-or-nil (:schema @conn))))

;; ============================================================================
;; Lookup-ref resolution for the $empty+txs source
;;
;; Real-world tx-data references existing entities via lookup-refs:
;;
;;   {:posting/account [:account/code "1000"]
;;    :posting/commodity [:commodity/symbol "USD"]}
;;
;; Without seeding, the third source ($empty+txs) — built by `dc/db-with`
;; on a fresh empty-db — throws `:entity-id/missing` on any tx-data
;; containing a lookup-ref: the empty-db has the schema but no entities
;; to resolve `[:account/code "1000"]` against.
;;
;; The fix: walk tx-data for lookup-refs, resolve each against the LIVE
;; conn, seed the empty-db with stub `{:db/id <eid> <unique-attr> <val>}`
;; maps BEFORE applying tx-data. The user's tx-data is preserved verbatim
;; in $empty+txs (so any invariant that reads `[$empty+txs ?p :a ?v]` +
;; joins via `[$after ?v :b _]` keeps working — eid pinning keeps the
;; `?v` binding consistent across the two sources).
;; ============================================================================

(defn- lookup-ref?
  "A 2-element non-MapEntry vector whose first element is a USER
   attribute keyword. Excludes 2-element tx-tuples like
   `[:db/retractEntity 1]` (where the head is reserved `db.*`)."
  [x]
  (and (vector? x)
       (not (map-entry? x))
       (= 2 (count x))
       (keyword? (first x))
       (let [ns (namespace (first x))]
         (or (nil? ns)
             (and (not= ns "db")
                  (not (.startsWith ^String ns "db.")))))))

(defn- collect-lookup-refs
  "Walk `tx-data` and return the set of every lookup-ref `[:attr value]`
   encountered. Visits every position via `postwalk` so it catches
   refs at entity-map values, tuple-tx value positions, `:db/id`
   positions, and within cardinality-many vectors."
  [tx-data]
  (let [refs (atom #{})]
    (walk/postwalk
     (fn [x]
       (when (lookup-ref? x) (swap! refs conj x))
       x)
     tx-data)
    @refs))

(defn- seed-from-lookup-refs
  "For each lookup-ref `[attr val]` in `refs`, produce a seed map the
   empty-db can apply so the user's tx-data's lookup-ref to the same
   `[attr val]` resolves cleanly.

   - Lookup-ref resolves in the live db → seed with `{:db/id <eid> attr
     val}`. Pinning to the live eid keeps `?ref` bindings consistent
     across `$after` and `$empty+txs` for join-by-eid invariant queries.
   - Lookup-ref doesn't resolve (entity being CREATED by this same tx)
     → seed with a tempid-keyed map so db-with creates it. The eid
     won't match the live-db side, but for an entity that doesn't exist
     in `$after` either, joins via that eid wouldn't fire anyway."
  [db refs]
  (->> refs
       (map-indexed
        (fn [i [a v :as ref]]
          (if-let [eid (:db/id (d/entity db ref))]
            {:db/id eid a v}
            {:db/id (str "invariant/seed-" i) a v})))
       vec))

(defn- invariant-holds? [inv-qs conn tx-data schema-tx]
  ;; Build $empty+txs by spinning up a fresh in-memory db, transacting
  ;; the schema INTO it, seeding entities referenced by lookup-refs in
  ;; tx-data, and applying tx-data. The seed pass is the key correctness
  ;; bit: without it, any user tx-data referencing existing entities via
  ;; lookup-refs (the realistic majority of business writes) crashes the
  ;; third source with :entity-id/missing.
  (let [flex (or (:schema-flexibility @conn)
                 (get-in @conn [:config :schema-flexibility])
                 :read)
        db           @conn
        refs         (collect-lookup-refs tx-data)
        seed         (seed-from-lookup-refs db refs)
        empty-db     (dc/empty-db nil {:schema-flexibility flex})
        empty+schema (if (seq schema-tx)
                       (dc/db-with empty-db schema-tx)
                       empty-db)
        empty+seed   (if (seq seed)
                       (dc/db-with empty+schema seed)
                       empty+schema)]
    (d/q (edn/read-string inv-qs)
         ;; current state
         db
         ;; apply transaction to current state
         (dc/db-with db tx-data)
         ;; empty database with schema + lookup-ref seeds + tx applied
         (dc/db-with empty+seed tx-data)
         tx-data)))

(defn- spread-attrs
  "`get-attribute` returns either a single attribute (for tuple tx forms)
   or a set of attributes (for entity-map tx forms). Spread to a
   sequence of `[attr tx]` pairs in either case."
  [tx]
  (let [a (get-attribute tx)]
    (if (set? a)
      (map (fn [k] [k tx]) a)
      [[a tx]])))

(defn assert-invariants
  "For each attribute mentioned in `tx-data`, run any registered
   `:invariant/query` against the (before, after, empty+txs, txs)
   4-source datalog form. Throw `{:type :invariant/invariant-mismatch
   :attribute …}` on any failed invariant.

   Also validates that any `:invariant/query` BEING REGISTERED via
   `tx-data` parses as a valid 4-source query first — registering an
   invalid invariant is caught up-front, not at first-fire.

   ## Arities

   - `[conn tx-data]` — recommended. Schema is read from `@conn`.
   - `[conn tx-data schema]` — back-compat with pre-0.8.x consumers
     that passed schema explicitly. The schema arg overrides the conn's
     own; useful when conn doesn't have the schema installed yet (e.g.
     bootstrapping)."
  ([conn tx-data]
   (assert-invariants conn tx-data nil))
  ([conn tx-data schema]
   (let [schema   (resolve-schema conn schema)
         attr-txs (mapcat spread-attrs tx-data)
         attrs    (distinct (map first attr-txs))]
     (doseq [[a tx] attr-txs
             :when  (= a :invariant/query)
             :let   [v (cond
                         ;; flat tuple: [:db/add e :invariant/query "..."]
                         (and (vector? tx) (= 4 (count tx))) (nth tx 3)
                         ;; entity map: {:invariant/query "..."}
                         (map? tx)                            (:invariant/query tx))]]
       (assert-valid-query (edn/read-string v)))

     (doseq [a     attrs
             :let  [inv-qs (d/q invariant-query @conn a)]
             :when inv-qs]
       (when-not (invariant-holds? inv-qs conn tx-data schema)
         (throw (ex-info "Invariant mismatch."
                         {:type      :invariant/invariant-mismatch
                          :attribute a
                          :invariant (edn/read-string inv-qs)
                          :tx-data   tx-data}))))
     true)))

(defn transact-with-invariants
  "Transaction wrapper that enforces invariants before committing.

   Takes a Datahike connection + transaction data; checks all relevant
   invariants, then transacts via `datahike.api/transact` if every check
   passes.

   ## Arities

   - `[conn tx-data]` — recommended. Schema read from `@conn`.
   - `[conn tx-data schema]` — back-compat shim for the pre-0.8.x
     signature.

   ## Parameters

   - `conn`     — Datahike connection.
   - `tx-data`  — Either the raw tx-data vector, OR a map with `:tx-data`
                  key (the datahike-native shape) — both accepted for
                  source-compat with consumers that already wrap.

   ## Returns

   - The tx-report from `datahike.api/transact` on success.
   - Throws `ex-info` with `:type :invariant/invariant-mismatch` (or
     `:invariant/invalid-function-call`) on any failed check.

   ## When to bypass

   For operations where invariants should be skipped (e.g. schema-only
   installs, bulk seed loads on bootstrap), call `datahike.api/transact`
   directly."
  ([conn tx-data]
   (transact-with-invariants conn tx-data nil))
  ([conn tx-data schema]
   (let [tx-data (if (map? tx-data) (:tx-data tx-data) tx-data)]
     (assert-invariants conn tx-data schema)
     (d/transact conn tx-data))))
