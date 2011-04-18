; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.metadata
  "Helpers to query the database's meta-data."
  (:require (clojure.contrib.sql [internal :as sqlint])
            (lobos [compiler :as compiler]
                   [connectivity :as conn]
                   [schema :as schema]))
  (:use (clojure.contrib [def :only [defvar-]]))
  (:import (java.sql DatabaseMetaData)))

;; -----------------------------------------------------------------------------

;; ## Database Metadata

(defvar- *db-meta* nil)
(defvar- *db-meta-spec* nil)

(defn db-meta
  "Returns the binded DatabaseMetaData object found in *db-meta* or get
  one from the default connection if not available."
  []
  (or (when (conn/connection)
        (.getMetaData (conn/connection)))
      *db-meta*
      (conn/with-connection :default-connection
        (.getMetaData (conn/connection)))))

(defn db-meta-spec
  []
  (or (:db-spec sqlint/*db*)
      *db-meta-spec*
      (conn/get-db-spec :default-connection)))

(defmacro with-db-meta
  "Evaluates body in the context of a new connection or a named global
  connection to a database then closes the connection while binding its
  DatabaseMetaData object to *db-meta*."
  [connection-info & body]
  `(if ~connection-info
     (conn/with-connection ~connection-info
       (binding [*db-meta-spec* (conn/get-db-spec ~connection-info)
                 *db-meta* (.getMetaData (conn/connection))]
         ~@body))
     (do ~@body)))

;; -----------------------------------------------------------------------------

;; ## Helpers

(defn raw-query [sql-string]
  (with-open [stmt (.createStatement (conn/connection))]
    (let [resultset (try (.executeQuery stmt sql-string)
                         (catch Exception _))]
      (when resultset
        (doall (resultset-seq resultset))))))

(defmacro query-schema [table & [conditions]]
  `(let [db-spec# (db-meta-spec)]
     (require (symbol (str "lobos.backends."
                           (:subprotocol db-spec#))))
     (raw-query 
      (str "select * from information_schema." (name ~table)
           (when '~conditions
             (str
              " where "
              (compiler/compile
               (schema/build-definition (schema/expression ~conditions)
                                        db-spec#))))))))

;; -----------------------------------------------------------------------------

;; ## Predicates

(defn supports-catalogs
  "Returns the term used for catalogs if the underlying database supports
  that concept."
  []
  (when (.supportsCatalogsInDataManipulation (db-meta))
    (.getCatalogTerm (db-meta))))

(defn supports-schemas
  "Returns the term used for schemas if the underlying database supports
  that concept."
  []
  (when (.supportsSchemasInDataManipulation (db-meta))
    (.getSchemaTerm (db-meta))))

;; -----------------------------------------------------------------------------

;; ## Database Objects

(defn catalogs
  "Returns a list of catalog names as keywords."
  []
  (map #(-> % :table_cat keyword)
       (doall (resultset-seq (.getCatalogs (db-meta))))))

(defn schemas
  "Returns a list of schema names as keywords."
  []
  (cond (supports-schemas)
        (map #(-> % :table_schem keyword)
             (doall (resultset-seq (.getSchemas (db-meta)))))
        (supports-catalogs) (catalogs)))

(defn tables
  "Returns a list of table names as keywords for the specified schema."
  [sname]
  (map #(-> % :table_name keyword)
       (resultset-seq
        (.getTables (db-meta)
                    (when-not (supports-schemas) (name sname))
                    (when (supports-schemas) (name sname))
                    nil
                    (into-array ["TABLE"])))))

(defn primary-keys
  "Returns primary key names as a set of keywords for the specified
  table."
  [sname tname]
  (set
   (map #(-> % :pk_name keyword)
        (resultset-seq
         (.getPrimaryKeys (db-meta)
                          (when-not (supports-schemas) (name sname))
                          (when (supports-schemas) (name sname))
                          (name tname))))))

(defn indexes-meta
  "Returns metadata maps for indexes of the specified table. Results are
  sorted by ordinal position, grouped by index name and can be filtered
  by the given function f. Doesn't returns indexes that have
  tableIndexStatistic type."
  [sname tname & [f]]
  (try
    (group-by :index_name
      (filter #(and (> (:type %) DatabaseMetaData/tableIndexStatistic)
                    ((or f identity) %))
              (resultset-seq
               (.getIndexInfo (db-meta)
                              (when-not (supports-schemas) (name sname))
                              (when (supports-schemas) (name sname))
                              (name tname)
                              false
                              false))))
    (catch java.sql.SQLException _ nil)))

(defn references-meta
  "Returns metadata maps for cross reference of the specified foreign
  table. Results are sorted by their ordinal position and grouped by
  foreign key name."
  [sname tname]
  (group-by :fk_name
    (resultset-seq
     (.getImportedKeys (db-meta)
                       (when-not (supports-schemas) (name sname))
                       (when (supports-schemas) (name sname))
                       (name tname)))))

(defn columns-meta
  "Returns metadata maps for each columns of the specified table."
  [sname tname]
  (resultset-seq
   (.getColumns (db-meta)
                (when-not (supports-schemas) (name sname))
                (when (supports-schemas) (name sname))
                (name tname)
                nil)))
