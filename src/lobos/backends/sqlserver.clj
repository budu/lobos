;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.backends.sqlserver
  "Compiler implementation for SQL Server."
  (:refer-clojure :exclude [compile])
  (:require (lobos [schema :as schema]))
  (:use (clojure.contrib [def :only [defvar-]])
        lobos.analyzer
        lobos.compiler
        lobos.metadata
        lobos.utils)
  (:import (lobos.ast AutoIncClause
                      DataTypeExpression
                      DropStatement
                      Identifier)
           (lobos.schema DataType)))

;;;; Analyzer

(defvar- analyzer-data-type-aliases
  {:bit :boolean
   :datetime2 :timestamp
   :image :blob
   :int :integer
   :ntext :nclob
   :text :clob})

(defmethod analyze [:microsoft-sql-server DataType]
  [_ column-meta]
  (let [dtype (-> column-meta :type_name as-keyword)
        [dtype options] (if (= dtype :datetimeoffset)
                          [:timestamp {:time-zone true}]
                          [dtype nil])
        dtype (first (replace analyzer-data-type-aliases
                              [dtype]))]
    (schema/data-type
     dtype
     (analyze-data-type-args dtype column-meta)
     options)))

;;;; Compiler

(defmethod compile [:sqlserver Identifier]
  [identifier]
  (let [{:keys [db-spec value level]} identifier
        schema (:schema db-spec)]
    (if (and (= level :schema) schema)
      (str (when schema (str (as-identifier db-spec schema) "."))
           (as-identifier db-spec value))
      (as-str \[ value \]))))

(defvar- compiler-data-type-aliases
  {:blob      :image
   :boolean   :bit
   :clob      :text
   :double    :float
   :nclob     :ntext
   :timestamp :datetime2})

(defmethod compile [:sqlserver DataTypeExpression]
  [expression]
  (let [{:keys [dtype args options]} expression
        {:keys [collate time-zone]} options
        dtype (if (and (= dtype :timestamp) time-zone)
                :datetimeoffset
                (first (replace compiler-data-type-aliases [dtype])))
        args (if (#{:image :ntext :text} dtype) [] args)]
    (unsupported (and (= dtype :time) time-zone)
      "Time zone unsupported for time data type.")
    (join \space
      (concat
       [(str (as-sql-keyword dtype)
             (as-list args))]
       (when collate ["COLLATE" (as-str collate)])))))

(defmethod compile [:sqlserver AutoIncClause]
  [_]
  "IDENTITY")

(defmethod compile [:sqlserver DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement
        sql-string (join \space
                         ["DROP"
                          (as-sql-keyword otype)
                          (as-identifier db-spec oname :schema)])]
    (join \;
      (concat
       (when (and (= otype :schema)
                  (= behavior :cascade))
         (for [element (with-db-meta db-spec
                         (-> (analyze-schema oname) :elements keys))]
           (compile (schema/build-drop-statement
                     (schema/table element)
                     :cascade
                     (assoc db-spec :schema oname)))))
       [sql-string]))))
