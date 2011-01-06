;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.analyzer
  "Analyze a database's meta-data to contruct an abstract schema."
  (:refer-clojure :exclude [replace])
  (:require (lobos [schema :as schema]))
  (:use (clojure.contrib [def :only [defvar-]])
        (clojure [string :only [replace]])
        lobos.metadata
        lobos.utils)
  (:import (lobos.schema Column
                         Constraint
                         DataType)))

;;;; Constraints analysis

(defn indexes
  "Returns all indexes for the specified schema and table names, results
  are sorted by ordinal position, grouped by index name and can be
  filtered by the given function f."
  [sname tname & [f]]
  (try
    (group-by :index_name
      (sort-by :ordinal_position
        (filter #(and (> (:type %) 0)
                      ((or f identity) %))
          (resultset-seq
           (.getIndexInfo (db-meta)
                          (when-not (supports-schemas) (name sname))
                          (when (supports-schemas) (name sname))
                          (name tname)
                          false
                          false)))))
    (catch java.sql.SQLException _ nil)))

(defn primary-keys
  "Returns primary key names as a set of keywords."
  [sname tname]
  (set
   (map #(-> % :pk_name keyword)
        (resultset-seq
         (.getPrimaryKeys (db-meta)
                          (when-not (supports-schemas) (name sname))
                          (when (supports-schemas) (name sname))
                          (name tname))))))

(defn unique-constraints
  "Returns a list of unique constraints for the specified schema and
  table names."
  [sname tname]
  (let [pkeys (primary-keys sname tname)]
    (map (fn [[cname cmeta]]
           (Constraint. (keyword cname)
                        (if (pkeys (keyword cname))
                          :primary-key
                          :unique)
                        {:columns
                         (vec (map #(-> % :column_name keyword)
                                   cmeta))}))
         (indexes sname tname #(-> % :non_unique not)))))

(defn constraints
  "Returns a list of constraints for the specified schema and table
  names. Supports only unique constraints at the moment."
  [sname tname]
  (unique-constraints sname tname))

;;;; Columns analysis

(defvar- dtypes-aliases
  {:bit    :boolean
   :bool   :boolean
   :bpchar :char
   :bytea  :blob
   :datetime :timestamp
   :float4 :real
   :float8 :double
   :image  :blob
   :int    :integer
   :int2   :smallint
   :int4   :integer
   :int8   :bigint
   :ntext  :nclob
   :text   :clob})

(defn analyze-data-type
  "Returns an abstract data-type definition given a column meta-data."
  [col-meta]
  (let [dtype (-> col-meta :type_name as-keyword)
        dtype (dtypes-replace dtypes-aliases dtype)]
    (DataType.
     dtype
     (case dtype
       :varchar [(:column_size col-meta)]
       []))))

(defn analyze-expression
  "Returns the Clojure equivalent of the given SQL expression. Just a
  stub currently."
  [expr]
  (when expr
    (cond (re-find #"(.*)::(.*)" expr)
          (let [[_ & [value dtype]] (first (re-seq #"(.*)::(.*)" expr))]
            (read-string (replace (str value) \' \")))
          (re-find #"(\w+)(\(\))?" expr)
          (let [[[_ func]] (re-seq #"(\w+)(\(\))?" expr)]
            (keyword func))
          :else (str expr))))

(defn analyze-column
  "Returns an abstract column definition given a column meta-data."
  [col-meta]
  (let [auto-inc (= (:is_autoincrement col-meta) "YES")]
    (Column. (-> col-meta :column_name keyword)
             (analyze-data-type col-meta)
             (when-not auto-inc
               (analyze-expression (:column_def col-meta)))
             auto-inc
             (= (:is_nullable col-meta) "NO")
             [])))

(defn columns
  "Returns a list of abstract column definitions for the specified
  schema and table names."
  [sname tname]
  (map analyze-column
       (resultset-seq
        (.getColumns (db-meta)
                     (when-not (supports-schemas) (name sname))
                     (when (supports-schemas) (name sname))
                     (name tname)
                     nil))))

;;;; Tables analysis

(defn analyze-table
  "Returns the abstract table definition for the specified schema and
  table names."
  [sname tname]
  (schema/table* tname
                 (into {} (map #(vector (:cname %) %)
                               (columns sname tname)))
                 (into {} (map #(vector (:cname %) %)
                               (constraints sname tname)))
                 {}))

(defn tables
  "Returns a list of abstract table definitions for the specified schema name."
  [sname]
  (map #(analyze-table sname (-> % :table_name keyword))
       (resultset-seq
        (.getTables (db-meta)
                    (when-not (supports-schemas) (name sname))
                    (when (supports-schemas) (name sname))
                    nil
                    (into-array ["TABLE"])))))

;;;; Catalogs analysis

(defn catalogs
  "Returns a list of catalog names as keywords."
  []
  (map #(-> % :table_cat keyword)
       (doall (resultset-seq (.getCatalogs (db-meta))))))

;;;; Schemas analysis

(defn schemas
  "Returns a list of schema names as keywords."
  []
  (cond (supports-schemas)
        (map #(-> % :table_schem keyword)
             (doall (resultset-seq (.getSchemas (db-meta)))))
        (supports-catalogs) (catalogs)))

(defn analyze-schema
  "Returns the abstract schema definition for the specified schema name
  using the given connection-info if specified or the default one."
  [sname & [connection-info]]
  (let [sname (keyword sname)
        analyze-schema* #(apply schema/schema sname {} (tables sname))]
    (with-db-meta connection-info
      (if-let [schemas (schemas)]
        (when ((set schemas) sname)
          (analyze-schema*))
        (analyze-schema*)))))
