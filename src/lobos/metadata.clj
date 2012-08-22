; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.metadata
  "Helpers to query the database's meta-data."
  (:require (lobos [compiler :as compiler]
                   [connectivity :as conn]
                   [schema :as schema]))
  (:import (java.sql DatabaseMetaData)))

;; -----------------------------------------------------------------------------

;; ## Database Metadata

(def ^{:dynamic true :private true} *db-meta* nil)
(def ^{:dynamic true :private true} *db-meta-spec* nil)

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
  (or (conn/jdbc-db-spec)
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

(defmacro meta-call [method sname & args]
  `(doall
    (resultset-seq
     (~method (db-meta)
              (when (and ~sname (not (supports-schemas))) (name ~sname))
              (when (and ~sname (supports-schemas)) (name ~sname))
              ~@args))))

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

(defn default-schema []
  (when (supports-schemas)
    (->> (doall (resultset-seq (.getSchemas (db-meta))))
         (filter :is_default)
         first
         :table_schem
         keyword)))

(defn tables
  "Returns a list of table names as keywords for the specified schema."
  [& [sname]]
  (map #(-> % :table_name keyword)
       (meta-call .getTables sname nil (into-array ["TABLE"]))))

(defn primary-keys
  "Returns primary key names as a set of keywords for the specified
  table."
  [sname tname]
  (set
   (map #(-> % :pk_name keyword)
        (meta-call .getPrimaryKeys sname (name tname)))))

(defn indexes-meta
  "Returns metadata maps for indexes of the specified table. Results are
  sorted by ordinal position, grouped by index name and can be filtered
  by the given function f. Doesn't returns indexes that have
  tableIndexStatistic type."
  [sname tname & [f]]
  (try
    (group-by :index_name
      (filter #(let [type (:type %)
                     ;; HACK: new MySQL drivers return a string here!
                     type (if (string? type)
                            (Integer/parseInt type)
                            (short type))]
                 (and (> type DatabaseMetaData/tableIndexStatistic)
                      ((or f identity) %)))
              (meta-call .getIndexInfo sname (name tname) false false)))
    (catch java.sql.SQLException _ nil)))

(defn references-meta
  "Returns metadata maps for cross reference of the specified foreign
  table. Results are sorted by their ordinal position and grouped by
  foreign key name."
  [sname tname]
  (group-by :fk_name
    (meta-call .getImportedKeys sname (name tname))))

(defn columns-meta
  "Returns metadata maps for each columns of the specified table."
  [sname tname]
  (meta-call .getColumns sname (name tname) nil))
