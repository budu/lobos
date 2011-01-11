;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.core
  "Main interface to interact with Lobos."
  (:refer-clojure :exclude [alter drop])
  (:require (lobos [analyzer :as analyzer]
                   [compiler :as compiler]
                   [connectivity :as conn]
                   [schema :as schema]))
  (:use (clojure.contrib [def :only [name-with-attributes]])
        (clojure [pprint :only [pprint]])
        lobos.utils))

;;;; Globals

(defonce debug-level (atom nil))

(defonce global-schemas (atom {}))

(defonce default-schema (atom nil))

;;;; Debugging interface

(defn set-debug-level
  "Set the current debugging level."
  [level]
  (swap! debug-level (constantly level)))

;;;; Schema definition

(defn schema-key
  "Returns a unique schema key for the given schema. This schema must
  have its db-spec option properly set. When given a keyword instead of
  a schema look-up this schema by name using the db-spec argument."
  [schema-or-name & [db-spec]]
  (str (:subname (or db-spec (-> schema-or-name
                                 :options
                                 :db-spec)))
       (if (keyword? schema-or-name)
         schema-or-name
         (:sname schema-or-name))))

(defn set-global-schema
  "Set the given schema in the global schemas map and returns it."
  [schema]
  (swap! global-schemas assoc (schema-key schema) schema)
  schema)

(defn get-global-schema
  "Returns the specified schema for the given optional connection-info,
  use default connection if no missing."
  [sname & [connection-info]]
  (let [db-spec (conn/get-db-spec connection-info)]
    (@global-schemas (schema-key sname db-spec))))

(defn update-global-schema
  "If given a schema, update it in the global schemas map, else analyze
  the specified schema found with the given connection."
  ([schema] (set-global-schema schema))
  ([schema-name db-spec]
     (set-global-schema
      (assoc-in
       (analyzer/analyze-schema schema-name
                                db-spec)
       [:options :db-spec]
       db-spec))))

(defn remove-global-schema
  "Removes the given schema from the global schema map."
  [schema]
  (swap! global-schemas dissoc (schema-key schema))
  nil)

(defn set-default-schema
  "Set the default schema."
  [schema]
  (swap! default-schema (constantly (schema-key (schema)))))

(defn get-default-schema
  "Returns the default schema."
  []
  (@global-schemas @default-schema))

(defn get-or-create-schema
  "Returns the given schema, the schema returned by the given function
  or create an abstract schema definition with the given keyword
  depending on the argument's type, else nil. Associates the db-spec
  obtained from the optional connection-info in its options map if
  there's not already one."
  [schema-or-name & [connection-info]]
  (let [db-spec (conn/get-db-spec connection-info)
        schema (cond (schema/schema? schema-or-name) schema-or-name
                     (fn? schema-or-name) (schema-or-name)
                     (keyword? schema-or-name) (schema/schema schema-or-name))]
    (if (-> schema :options :db-spec)
      schema
      (assoc-in schema [:options :db-spec] db-spec))))

(defmacro defschema
  "Defines a var containing the specified schema constructed from
  database meta-data."
  {:arglists '([connection-info? var-name schema-name & elements])}
  [& args]
  (let [[connection-info args] (optional map? args)
        var-name (first args)
        schema-name (second args)
        elements (nnext args)]
    `(let [db-spec# (conn/get-db-spec ~connection-info)
           options# {:db-spec db-spec#}
           elements# (list ~@elements)]
       (defn ~var-name []
         (@global-schemas
          (schema-key
           (if elements#
             (update-global-schema
              (apply schema/schema ~schema-name options# elements#))
             (update-global-schema ~schema-name db-spec#))))))))

;;;; Action helpers

(defn connection?
  "Checks if the given argument is a named connection or a db-spec."
  [cnx]
  (or ((set (keys @conn/global-connections)) cnx)
      (and (map? cnx)
           ((comp not schema/definition?) cnx))))

(defmacro defaction
  "Define an action applicable to an optional abstract schema."
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
               cnx# (or (-> ~'schema :options :db-spec)
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
                 (merge (.meta #'name)
                        {:arglists '(~(vec (conj params 'cnx-or-schema?)))})))))

;;;; Actions

(defn debug
  "Prints useful information on the given action/object combination."
  [action object-or-fn & [args connection-info level]]
  (let [level (or level @debug-level :sql)
        object (if (fn? object-or-fn)
                 (object-or-fn)
                 object-or-fn)
        db-spec (conn/get-db-spec connection-info)
        ast (when-not (= :schema level)
              (apply action object (conj args db-spec)))]
    (case level
      :sql (println (compiler/compile ast))
      :ast (do (println (type ast))
               (pprint ast))
      :schema (do (println (type object))
                  (pprint object)))))

(defn execute
  "Execute the given statement using the specified connection
  information or the bound one."
  [statement & [connection-info]]
  (when-let [sql-string (not-empty (if (string? statement)
                                     statement
                                     (compiler/compile statement)))]
    (when (= :sql @debug-level)
      (println sql-string))
    (let [db-spec (conn/get-db-spec connection-info)
          mode (compiler/compile (compiler/mode db-spec))]
      (conn/with-connection connection-info
        (when mode
          (with-open [stmt (.createStatement (conn/connection))]
            (.execute stmt mode)))
        (with-open [stmt (.createStatement (conn/connection))]
          (.execute stmt sql-string))))
    nil))

(defaction create
  "Builds a create statement with the given schema element and execute it."
  [odef]
  (schema/build-create-statement odef db-spec))

(defaction alter
  "Builds an alter statement with the given schema element and execute it."
  [subaction odef]
  (schema/build-alter-statement odef subaction db-spec))

(defaction drop
  "Builds a drop statement with the given schema element and execute it."
  [odef & [behavior]]
  (schema/build-drop-statement odef behavior db-spec))

(defn create-schema
  "Create a new schema."
  [schema-or-name & [connection-info]]
  (let [schema (get-or-create-schema schema-or-name connection-info)
        db-spec (-> schema :options :db-spec)]
    (execute (schema/build-create-statement schema db-spec) db-spec)
    (update-global-schema schema)))

(defn drop-schema
  "Drop the specified schema using the given behavior."
  [schema-or-name & [behavior connection-info]]
  (let [schema (get-or-create-schema schema-or-name connection-info)
        db-spec (-> schema :options :db-spec)]
    (execute (schema/build-drop-statement schema behavior db-spec) db-spec)))
