; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.backends.mysql
  "Compiler implementation for MySQL."
  (:refer-clojure :exclude [compile defonce])
  (:require (lobos [schema :as schema]
                   [ast :as ast]))
  (:use (clojure.contrib [def :only [defvar-]])
        (lobos [schema :only [build-definition]]
               analyzer
               compiler
               metadata
               utils))
  (:import (lobos.schema Column
                         DataType
                         UniqueConstraint)))

(ast/import-all)

;; -----------------------------------------------------------------------------

;; ## Analyzer

(defvar- analyzer-data-type-aliases
  {:bit :boolean
   :int :integer
   :text :clob
   :tinyblob :blob
   :tinytext :clob})

(defmethod analyze [:mysql DataType]
  [_ column-meta]
  (let [dtype (-> column-meta :type_name as-keyword)
        dtype (first (replace analyzer-data-type-aliases [dtype]))]
    (schema/data-type
     dtype
     (if (#{:time :timestamp} dtype)
       []
       (analyze-data-type-args dtype column-meta)))))

(defmethod analyze [:mysql UniqueConstraint]
  [_ sname tname cname index-meta]
  (let [pkeys (primary-keys sname tname)
        pkey (pkeys (keyword cname))
        columns (vec (map #(-> % :column_name keyword)
                          index-meta))]
    (UniqueConstraint.
     (if pkey
       (make-constraint-name tname :primary-key columns)
       (keyword cname))
     (if pkey
       :primary-key
       :unique)
     columns)))

(defn- analyze-column [sname tname cname]
  (analyze Column
    (first
     (resultset-seq
      (.getColumns (db-meta) (name sname) nil (name tname) (name cname))))))

;; -----------------------------------------------------------------------------

;; ## Compiler

(defmethod compile [:mysql Identifier]
  [identifier]
  (let [{:keys [db-spec value level]} identifier
        schema (:schema db-spec)]
    (if (and (= level :schema) schema)
      (str (when schema (str (as-identifier db-spec schema) "."))
           (as-identifier db-spec value))
      (as-str \` value \`))))

(defvar- compiler-data-type-aliases
  {:clob :text
   :nclob :text})

(defmethod compile [:mysql DataTypeExpression]
  [expression]
  (let [{:keys [dtype args options]} expression
        {:keys [encoding collate]} options
        encoding (when (= dtype :nclob) "UTF8")
        dtype (first (replace compiler-data-type-aliases [dtype]))
        args (if (#{:time :timestamp} dtype) [] args)]
    (unsupported (= dtype :real)
      "Use double instead.")
    (unsupported (:time-zone options)
      "Time zones not supported.")
    (join \space
      (str (as-sql-keyword dtype) (as-list args))
      (when encoding (str "CHARACTER SET " (as-str encoding)))
      (when collate (str "COLLATE " (as-str collate))))))

(defmethod compile [:mysql AutoIncClause]
  [_]
  "AUTO_INCREMENT")

(defmethod compile [:mysql CreateSchemaStatement]
  [statement]
  (let [{:keys [db-spec sname elements]} statement
        [elements foreign-keys] (extract-foreign-keys elements)
        alters (map compile (build-alter-add-statements
                             (assoc db-spec :schema sname)
                             foreign-keys))]
    (conj (concat (map (comp compile
                             #(assoc-in % [:db-spec :schema] sname))
                       elements)
                  alters)
          (str "CREATE SCHEMA "
               (as-identifier db-spec sname)))))

(defmethod compile [:mysql DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (join \space
      "DROP"
      (as-sql-keyword otype)
      (as-identifier db-spec oname :schema)
      (when (and behavior (#{:table} otype))
        [(as-sql-keyword behavior)]))))

(defmethod compile [:mysql AlterDropAction]
  [action]
  (let [{:keys [db-spec element]} action
        is-unique (instance? UniqueConstraintDefinition element)
        is-pkey (and is-unique (= (:type element) :primary-key))]
    (join \space
          "DROP"
          (cond (instance? ColumnDefinition element) "COLUMN"
                (instance? ForeignKeyConstraintDefinition element) "FOREIGN KEY"
                (and is-unique (= (:ctype element) :unique)) "INDEX"
                is-pkey "PRIMARY KEY")
          (when-not is-pkey
            (as-identifier db-spec (:cname element))))))

(defmethod compile [:mysql AlterRenameAction]
  [action]
  (let [{:keys [db-spec element]} action
        old-name (:cname element)
        new-name (:others element)
        column (with-db-meta db-spec
                 (assoc (analyze-column (:sname element)
                                        (:tname element)
                                        old-name)
                   :cname new-name))]
    (join \space
          "CHANGE"
          (as-identifier db-spec old-name)
          (compile (build-definition column db-spec)))))
