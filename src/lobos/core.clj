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
  (:require (lobos [compiler :as compiler]
                   [connectivity :as conn]
                   [migration :as mig]
                   [schema :as schema]))
  (:use (clojure.tools [macro :only [name-with-attributes]])
        (clojure [pprint :only [pprint]])
        lobos.internal
        lobos.utils))

;; -----------------------------------------------------------------------------

;; ## Helpers

(defmacro without-migration [& body]
  `(binding [mig/*record* nil]
     ~@body))

;; -----------------------------------------------------------------------------

;; ## Debugging Interface

(defn set-debug-level
  "Set the current debugging level. The level argument can be one of
  `:schema`, `:ast` or `:sql`. Currently only `:sql` is supported for
  all actions. e.g.:

    user> (set-debug-level :sql)"
  [level]
  (swap! debug-level (constantly level)))

(defn debug
  "Prints useful information on the given combination protocol method
  and schema (or elements). For the available methods, see the
  `lobos.schema` namespace. For methods taking extra argument use the
  optional `args` argument, which takes a sequence. You can also supply
  a `level` and `connection-info` argument. Use the default connection
  and the `:sql` level when not specified.  *For debugging purpose*. e.g.:

    user> (debug build-create-statement
                 (sample-schema))"
  [method object-or-fn & [args level connection-info]]
  (let [level (or level @debug-level :sql)
        object (if (fn? object-or-fn)
                 (object-or-fn)
                 object-or-fn)
        db-spec (conn/get-db-spec connection-info)
        ast (when-not (= :schema level)
              (apply method object (conj args db-spec)))]
    (case level
      :sql (doseq [ast-element ast]
             (pprint (compiler/compile ast-element)))
      :ast (do (println (type ast))
               ;; TODO: walk to remove db-spec
               (pprint ast))
      :schema (do (println (type object))
                  (pprint object)))))

;; -----------------------------------------------------------------------------

;; ## Lobos Action

;; ### Action Macro

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
        name* (symbol (str name \*))
        [name args] (name-with-attributes name args)
        [params* & body] args]
    `(do
       (defn ~name* [self# & params#]
         (let [[~'db-spec ~'schema ~params*]
               (optional-cnx-or-schema params#)]
           (execute
            (do ~@body)
            ~'db-spec)
           (mig/record self#)))
       (defmacro ~name [~'& args#]
         `(~~name* (quote ~~'&form) ~@args#))
       (.setMeta #'~name
                 (merge (.meta #'~name)
                        {:arglists '(~(vec (conj params 'cnx-or-schema?)))})))))

;; ### Actions

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
  (check-valid-options action :add :drop :modify :rename)
  (schema/build-alter-statement element action db-spec))

(defaction drop
  "Builds a drop statement with the given schema element and execute
  it. It can take an optional `behavior` argument, when `:cascade` is
  specified drops all elements relying on the one being dropped. e.g.:

    user> (drop (table :foo) :cascade)"
  [element & [behavior]]
  (schema/build-drop-statement element behavior db-spec))

(defaction exec
  "Execute the given statements as an action."
  [& statements]
  statements)

;; -----------------------------------------------------------------------------

;; ## Lobos Migration

;; ### Migration Command Macro

(defmacro defcommand
  [name & args]
  (let [params (seq (first (filter vector? args)))
        [name args] (name-with-attributes name args)
        [params* & body] args]
    `(do
       (defn ~name [& params#]
         (require mig/*migrations-namespace*)
         (let [[~'db-spec ~'sname ~params*]
               (optional-cnx-and-sname params#)]
           (mig/create-migrations-table ~'db-spec ~'sname)
           ~@body))
       (.setMeta #'~name
                 (merge (.meta #'~name)
                        {:arglists
                         '(~(vec (conj params
                                       'sname?
                                       'cnx-or-schema?)))})))))

;; ### Migration Commands

(defn print-stash []
  (when (.exists mig/*stash-file*)
    (print (slurp mig/*stash-file*))))

(defcommand print-done []
  (doseq [name (mig/query-migrations-table db-spec sname)]
    (println name)))

(defcommand print-pending []
  (doseq [name (mig/pending-migrations db-spec sname)]
    (println name)))

(defcommand migrate [& names]
  (let [names (if (empty? names)
                (mig/pending-migrations db-spec sname)
                names)]
    (mig/do-migrations db-spec sname :up names)))

(defcommand rollback [& args]
  (let [names (cond
               (empty? args)
               [(last (mig/query-migrations-table db-spec sname))]
               (= 1 (count args))
               (let [arg (first args)
                     migs (mig/query-migrations-table db-spec sname)]
                 (cond
                  (integer? arg) (take arg (reverse migs))
                  (= arg :all) migs
                  :else args))
               :else args)]
    (mig/do-migrations db-spec sname :down names)))

(defcommand reset [& args]
  (apply rollback args)
  (migrate))

(defcommand generate-migration [name & [msg]]
  (let [name (symbol (if (string? name)
                       name
                       (clojure.core/name name)))]
    (when-not name
      (throw (IllegalArgumentException.
              "Migration must be named.")))
    (when ((set (mig/list-migrations-names)) (str name))
      (throw (IllegalArgumentException.
              "Migration name is already taken.")))
    (mig/generate-migration* db-spec sname name msg
                             (mig/read-stash-file))
    (mig/clear-stash-file)))
