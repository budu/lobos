;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.core
  "Main interface to interact with Lobos."
  (:refer-clojure :exclude [drop])
  (require (lobos [analyzer :as analyzer]
                  [compiler :as compiler]
                  [connectivity :as conn]
                  [schema :as schema]))
  (use (clojure [pprint :only [pprint]])))

;;;; Globals

(defonce debug-level (atom nil))

(defonce global-schemas (atom {}))

(defonce default-schema (atom nil))

;;;; Schema definition

(defn schema-key [schema]
  (str (-> schema
           :options
           :connection-info
           conn/get-db-spec
           :subname)
       (:sname schema)))

(defn set-global-schema [schema]
  (swap! global-schemas assoc (schema-key schema) schema)
  schema)

(defn update-schema [schema-or-name & [connection-info]]
  (set-global-schema
   (analyzer/analyse-schema (if (keyword? schema-or-name)
                              schema-or-name
                              (:sname schema-or-name))
                            (or connection-info
                                (-> schema-or-name
                                    :options
                                    :connection-info)))))

(defn set-default-schema [schema]
  (swap! default-schema (constantly (schema-key (schema)))))

(defn get-default-schema []
  (@global-schemas @default-schema))

(defmacro defschema
  "Defines a var containing the specified schema constructed from
  database meta-data."
  [var-name schema-name & [connection-info]]
  (let [connection-info (or connection-info :default-connection)
        options {:connection-info connection-info}]
    `(let [schema# (update-schema ~schema-name ~connection-info)]
         (defn ~var-name []
           (@global-schemas (schema-key schema#))))))

;;;; Actions

(defn debug
  "Prints useful information on the given action/object combination."
  [action object-or-fn & [args connection-info level]]
  (let [level (or level @debug-level :output)
        object (if (fn? object-or-fn)
                 (object-or-fn)
                 object-or-fn)
        ast (when-not (= :schema level)
              (apply action object (conj args connection-info)))]
    (case level
      :output (println (compiler/compile ast))
      :ast (do (println (type ast))
               (pprint ast))
      :schema (do (println (type object))
                  (pprint object)))))

(defn execute
  "Execute the given statement using the specified connection
  information or the bound one."
  [statement & [connection-info]]
  (let [sql-string (compiler/compile statement)]
    (conn/with-connection connection-info
      (with-open [stmt (.createStatement (conn/connection))]
        (.execute stmt sql-string))))
  nil)

(defn create
  "Builds a create statement with the given schema object and execute it."
  [cnx-or-schema tdef]
  (let [cnx-or-schema (or cnx-or-schema (get-default-schema))
        schema (cond (schema/schema? cnx-or-schema) cnx-or-schema
                     (fn? cnx-or-schema) (cnx-or-schema))
        cnx (or (-> schema :options :connection-info)
                cnx-or-schema
                :default-connection)]
    (execute (schema/build-create-statement tdef nil) ; HACK: no backend yet
             cnx)
    (when schema (update-schema schema))))

(defn drop
  "Builds a drop statement with the given schema object and execute it."
  [cnx-or-schema odef & [behavior]]
  (let [cnx-or-schema (or cnx-or-schema (get-default-schema))
        schema (cond (schema/schema? cnx-or-schema) cnx-or-schema
                     (fn? cnx-or-schema) (cnx-or-schema))
        cnx (or (-> schema :options :connection-info)
                cnx-or-schema
                :default-connection)]
    (execute (schema/build-drop-statement
              odef behavior nil) ; HACK: no backend yet
             cnx)
    (when schema (update-schema schema))))
