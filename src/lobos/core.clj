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

;; ## Global Schema Map

;; This section contains mainly functions for internal use. You can skip
;; most of it if you only mean to use **Lobos** as a library. If that's
;; the case, have a look at the `defschema` macro and the
;; `set-default-schema` and `get-default-schema` functions.

(defonce global-schemas
  (atom {})
  "This atom keeps a map of all schemas in use. See `schema-key` for
  more details on how keys are generated. *For internal use*.")

(defonce default-schema
  (atom nil)
  "This atom keeps the key of the default schema. *For internal use*.")

(defn schema-key
  "Returns an unique schema key for the given schema. This schema must
  have its db-spec option properly set. When given a keyword instead of
  a schema, it look-up this schema by name in the `global-schemas` map
  using the supplied db-spec argument. *For internal use*."
  [schema-or-name & [db-spec]]
  (str (:subname (or db-spec (-> schema-or-name
                                 :options
                                 :db-spec)))
       (if (keyword? schema-or-name)
         schema-or-name
         (:sname schema-or-name))))

(defn set-global-schema
  "Associates the given schema in the `global-schemas` map with its key
  and returns the given argument. *For internal use*."
  [schema]
  (swap! global-schemas assoc (schema-key schema) schema)
  schema)

(defn get-global-schema
  "Returns the specified global schema by searching for it using the
  given name and optional connection-info or default connection.  For
  internal use. *For debugging purpose*."
  [sname & [connection-info]]
  (let [db-spec (conn/get-db-spec connection-info)]
    (@global-schemas (schema-key sname db-spec))))

(defn update-global-schema
  "If given a schema, associates it in the global schemas map, else
  analyze the specified schema from the database specified by the given
  `db-spec` and associates it in the global schemas map. *For internal
  use*."
  ([schema] (set-global-schema schema))
  ([schema-name db-spec]
     (set-global-schema
      (assoc-in
       (analyzer/analyze-schema schema-name db-spec)
       [:options :db-spec]
       db-spec))))

(defn remove-global-schema
  "Removes the given schema from the global schema map. *For internal
  use*."
  [schema]
  (swap! global-schemas dissoc (schema-key schema))
  nil)

(defn get-or-create-schema
  "Returns the given schema, the schema returned by the given function
  or create an abstract schema definition with the given keyword
  depending on the argument's type, else nil. Associates the db-spec
  obtained from the optional `connection-info` in its options map if
  there's not already one. *For internal use*."
  [schema-or-name & [connection-info]]
  (let [db-spec (conn/get-db-spec connection-info)
        schema (cond (schema/schema? schema-or-name) schema-or-name
                     (fn? schema-or-name) (schema-or-name)
                     (keyword? schema-or-name) (schema/schema schema-or-name))]
    (if (and (not connection-info)
             (-> schema :options :db-spec))
      schema
      (assoc-in schema [:options :db-spec] db-spec))))

(defmacro defschema
  "This macro take two mandatory argument, the name of the variable
  being defined and the name of the schema it represent. When used
  without specifying elements, it defines a var containing the specified
  schema constructed from database meta-data. e.g.:

    user> (defschema sample-schema :tmp)

  If used with elements, the defined var will keep the abstract schema
  definition to be later created using the `create-schema` action. e.g.:

    user> (defschema sample-schema :tmp
             (table :foo (integer :a)))

  It can also take an optional `connection-info` argument from which the
  db-spec map will be used."
  {:arglists '([connection-info? var-name schema-name & elements])}
  [& args]
  (let [[connection-info args] (optional map? args)
        var-name (first args)
        schema-name (second args)
        elements (nnext args)]
    `(let [db-spec# (conn/get-db-spec ~connection-info)
           options# {:db-spec db-spec#}
           elements# (list ~@elements)
           key# (schema-key
                 (if elements#
                   (update-global-schema
                    (apply schema/schema ~schema-name options# elements#))
                   (update-global-schema ~schema-name db-spec#)))]
       (defn ~var-name [] (@global-schemas key#)))))

(defn set-default-schema
  "Set the given schema as the default one."
  [schema]
  (swap! default-schema (constantly (schema-key (schema)))))

(defn get-default-schema
  "Returns the default schema."
  []
  (@global-schemas @default-schema))

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
  using one of the protocol method available. This statement(s) will get
  executed and if a schema as been given as argument, it will be updated
  using the `update-global-schema` function.

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
               cnx-or-schema# (or cnx-or-schema# (get-default-schema))
               ~'schema (cond (schema/schema? cnx-or-schema#) cnx-or-schema#
                              (fn? cnx-or-schema#) (cnx-or-schema#))
               cnx# (or (conn/find-connection)
                        (-> ~'schema :options :db-spec)
                        cnx-or-schema#
                        :default-connection)
               ~'db-spec (merge (conn/get-db-spec cnx#)
                                (when ~'schema
                                  {:schema (-> ~'schema :sname name)}))]
           (execute
            ~@body
            ~'db-spec)
           (when ~'schema (update-global-schema (:schema ~'db-spec)
                                                ~'db-spec))))
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
  (schema/build-create-statement element db-spec))

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

(defn create-schema
  "Create a new schema with the given name or create one using the given
  abstract schema definition. See the `lobos.schema` namespace for more
  details on schema definition. If given a name but no `connection-info`
  it will use the default connection. e.g.:

    user> (create-schema :tmp)
    user> (create-schema sample-schema)"
  [schema-or-name & [connection-info]]
  (let [schema (get-or-create-schema schema-or-name connection-info)
        db-spec (-> schema :options :db-spec)]
    (execute (schema/build-create-statement schema db-spec) db-spec)
    (update-global-schema schema)))

(defn drop-schema
  "Drop the specified schema or the named one using the given optional
  behavior. If given a name but no `connection-info` it will use the
  default connection. When the `:cascade` behavior is specified, it
  drops all elements relying it."
  [schema-or-name & [behavior connection-info]]
  (let [schema (get-or-create-schema schema-or-name connection-info)
        db-spec (-> schema :options :db-spec)]
    (execute (schema/build-drop-statement schema behavior db-spec) db-spec)
    (remove-global-schema schema)))
