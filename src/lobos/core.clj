; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.core
  "The `core` namespace provide the basic interface to interact with a
  database. It contains a set of functions and *actions* (a special kind
  of functions acting on abstract schemas or their elements) used to
  manipulate database schemas in an implementation agnostic way.

  To find out more about **Lobos**, check out:

   * [Lobos website](http://budu.github.com/lobos/)
   * [Lobos repo](https://github.com/budu/lobos)
   * [Lobos wiki](https://github.com/budu/lobos/wiki)"
  {:author "Nicolas Buduroi"}
  (:refer-clojure :exclude [alter defonce drop])
  (:require (lobos [analyzer :as analyzer]
                   [compiler :as compiler]
                   [connectivity :as conn]
                   [schema :as schema]))
  (:use (clojure.contrib [def :only [name-with-attributes]])
        (clojure [pprint :only [pprint]])
        lobos.utils))

;; -----------------------------------------------------------------------------

;; ## Debugging Interface

(defonce debug-level
  (atom nil)
  "This atom keeps the currently set debug level, see
  `set-debug-level`. *For internal use*.")

(defn set-debug-level
  "Set the current debugging level. The level argument can be one of
  `:schema`, `:ast` or `:sql`. Currently only `:sql` is supported for
  all actions. e.g.:

    user> (set-debug-level :sql)"
  [level]
  (swap! debug-level (constantly level)))

;; -----------------------------------------------------------------------------

;; ## Helpers

(defn debug
  "Prints useful information on the given combination protocol method
  and schema (or elements). For the available methods, see the
  `lobos.schema` namespace. For methods taking extra argument use the
  optional `args` argument, which takes a sequence. You can also supply
  a `connection-info` and `level` argument. Use the default connection
  and the `:sql` level when not specified.  *For debugging purpose*. e.g.:

    user> (debug build-create-statement
                 (sample-schema))"
  [method object-or-fn & [args connection-info level]]
  (let [level (or level @debug-level :sql)
        object (if (fn? object-or-fn)
                 (object-or-fn)
                 object-or-fn)
        db-spec (conn/get-db-spec connection-info)
        ast (when-not (= :schema level)
              (apply method object (conj args db-spec)))]
    (case level
      :sql (println (compiler/compile ast))
      :ast (do (println (type ast))
               (pprint ast))
      :schema (do (println (type object))
                  (pprint object)))))

(defn- execute*
  "Execute the given SQL string or sequence of strings. Prints them if
  the `debug-level` is set to `:sql`."
  [sql]
  (doseq [sql-string (if (seq? sql) sql [sql])]
    (when (= :sql @debug-level) (println sql-string))
    (with-open [stmt (.createStatement (conn/connection))]
      (.execute stmt sql-string))))

(defn execute
  "Executes the given statement(s) using the specified connection
  information, the bound one or the default connection. It will executes
  an extra *mode* statement if defined by the backend compiler. *For
  internal purpose*."
  [statements & [connection-info]]
  (let [statements (if (seq? statements)
                     statements
                     [statements])
        db-spec (conn/get-db-spec connection-info)
        mode (compiler/compile (compiler/mode db-spec))]
    (conn/with-connection connection-info
      (require (symbol (str "lobos.backends."
                            (:subprotocol connection-info))))
      (when mode (execute* mode))
      (doseq [statement statements]
        (let [sql (if (string? statement)
                           statement
                           (compiler/compile statement))]
          (when sql (execute* sql)))))) nil)

;; -----------------------------------------------------------------------------

;; ## Action Helpers

;; The following helpers are used internally to defince **Lobos**
;; *actions*. You can skip this section.

(defn connection?
  "Checks if the given argument is a named connection or a db-spec. *For
  internal use*."
  [cnx]
  (or ((set (keys @conn/global-connections)) cnx)
      (and (map? cnx)
           ((comp not schema/definition?) cnx))))

(defmacro defaction
  "Defines an action applicable to an optional abstract schema or
  database connection. *Actions* are simply a special kind of
  functions. They will have an augmented argument list, which is the
  given one prepended by the optional `cnx-or-schema` argument.

  All actions must return a built statement (or list of statements)
  using one of the protocol method available.

  The defined actions will have access to two extra local variables. The
  `schema` variable will contain the given schema if `cnx-or-schema` is
  one else it be nil. The `db-spec` argument will contain the db-spec
  map found with the given connection or in the given schema. *For
  internal use*."
  {:arglists '([name doc-string? attr-map? [params*] & body])}
  [name & args]
  (let [params (seq (first (filter vector? args)))
        [name args] (name-with-attributes name args)
        [params* & body] args]
    `(do
       (defn ~name [& params#]
         (let [[cnx-or-schema# params#]
               (optional #(or (schema/schema? %)
                              (connection? %)) params#)
               ~params* params#
               ~'schema (when (schema/schema? cnx-or-schema#) cnx-or-schema#)
               cnx# (or (conn/find-connection)
                        (-> ~'schema :options :db-spec)
                        (when-not ~'schema cnx-or-schema#)
                        :default-connection)
               ~'db-spec (merge (conn/get-db-spec cnx#)
                                (when ~'schema
                                  {:schema (-> ~'schema :sname name)}))]
           (execute
            ~@body
            ~'db-spec)))
       (.setMeta #'~name
                 (merge (.meta #'~name)
                        {:arglists '(~(vec (conj params 'cnx-or-schema?)))})))))

;; -----------------------------------------------------------------------------

;; ## Actions

(defaction create
  "Builds a create statement with the given schema element and execute
  it. See the `lobos.schema` namespace for more details on schema
  elements definition. e.g.:

    user> (create (table :foo (integer :a)))"
  [element]
  (schema/build-create-statement (or element schema) db-spec))

(defaction alter
  "Builds an alter statement with the given schema element and execute
  it. There's four types of alter actions: `:add`, `:drop`, `:modify`
  and `:rename`. See the `lobos.schema` namespace for more details on
  schema elements definition. e.g.:

    user> (alter :add (table :foo (integer :a)))
    user> (alter :modify (table :foo (column :a [:default 0])))
    user> (alter :rename (table :foo (column :a :to :b)))"
  [action element]
  (schema/build-alter-statement element action db-spec))

(defaction drop
  "Builds a drop statement with the given schema element and execute
  it. It can take an optional `behavior` argument, when `:cascade` is
  specified drops all elements relying on the one being dropped. e.g.:

    user> (drop (table :foo) :cascade)"
  [element & [behavior]]
  (schema/build-drop-statement element behavior db-spec))
