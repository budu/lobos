;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.metadata
  "Helpers to query the database's meta-data."
  (:require (lobos [connectivity :as conn]))
  (:use (clojure.contrib [def :only [defvar-]])))

;;;; Metadata

(defvar- *db-meta* nil)

(defn db-meta
  "Returns the binded DatabaseMetaData object found in *db-meta* or get
  one from the default connection if not available."
  []
  (or *db-meta*
      (conn/with-connection :default-connection
        (.getMetaData (conn/connection)))))

(defmacro with-db-meta
  "Evaluates body in the context of a new connection or a named global
  connection to a database then closes the connection while binding its
  DatabaseMetaData object to *db-meta*."
  [db-spec & body]
  `(if ~db-spec
     (conn/with-connection ~db-spec
       (binding [*db-meta* (.getMetaData (conn/connection))]
         ~@body))
     (do ~@body)))

;;;; Predicates

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
