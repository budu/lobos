; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.backends.h2
  "Compiler implementation for H2."
  (:refer-clojure :exclude [compile defonce])
  (:require (lobos [schema :as schema]))
  (:use (clojure [string :only [split]])
        (clojure.contrib [def :only [defvar-]])
        (lobos analyzer compiler connectivity metadata utils))
  (:import (lobos.ast AlterRenameAction
                      AutoIncClause
                      CreateSchemaStatement
                      DataTypeClause
                      DropStatement)
           (lobos.schema ForeignKeyConstraint
                         UniqueConstraint)))

;; -----------------------------------------------------------------------------

;; ## Analyzer

(defmethod analyze [:h2 UniqueConstraint]
  [_ sname tname cname meta]
  (let [columns (split (:column_list meta) #",")]
    (UniqueConstraint.
     (make-index-name tname :unique columns)
     :unique
     columns)))

(defmethod analyze [:h2 :constraints]
  [_ sname tname]
  (concat
   (map (fn [meta] (analyze UniqueConstraint sname tname
                            (-> meta :constraint_name keyword)
                            meta))
        (query-schema :constraints (= :CONSTRAINT_TYPE "UNIQUE")))
   (map (fn [[cname meta]] (analyze ForeignKeyConstraint cname meta))
        (references-meta sname tname))))

;; -----------------------------------------------------------------------------

;; ## Compiler

(defmethod compile [:h2 DataTypeClause]
  [expression]
  (let [{:keys [dtype args options]} expression]
    (unsupported (= dtype :binary)
      "Use varbinary instead.")
    (unsupported (:time-zone options)
      "Time zones not supported.")
    (str (as-sql-keyword dtype) (as-list args))))

(defmethod compile [:h2 AutoIncClause]
  [_]
  "AUTO_INCREMENT")

(defmethod compile [::standard CreateSchemaStatement]
  [statement]
  (let [{:keys [db-spec sname elements]} statement
        [elements foreign-keys] (extract-foreign-keys elements)
        alters (map compile (build-alter-add-statements
                             (assoc db-spec :schema sname)
                             foreign-keys))]
    (conj alters
          (str "CREATE SCHEMA "
               (apply join "\n" (conj (map compile elements)
                                      (as-identifier db-spec sname)))))))

(defmethod compile [:h2 CreateSchemaStatement]
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

(defmethod compile [:h2 DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (join \space
      "DROP"
      (as-sql-keyword otype)
      (as-identifier db-spec oname :schema)
      (when (and behavior (#{:table} otype))
        (as-sql-keyword behavior)))))

(defmethod compile [:h2 AlterRenameAction]
  [action]
  (let [{:keys [db-spec element]} action
        old-name (:cname element)
        new-name (:others element)]
    (format "ALTER COLUMN %s RENAME TO %s"
            (as-identifier db-spec old-name)
            (as-identifier db-spec new-name))))
