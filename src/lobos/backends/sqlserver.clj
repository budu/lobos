; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.backends.sqlserver
  "Compiler implementation for SQL Server."
  (:refer-clojure :exclude [compile defonce])
  (:require clojure.string
            (lobos [schema :as schema]))
  (:use (clojure.contrib [def :only [defvar-]])
        lobos.analyzer
        lobos.compiler
        lobos.metadata
        lobos.utils)
  (:import (lobos.ast AlterAddAction
                      AlterDropAction
                      AlterModifyAction
                      AlterRenameAction
                      AlterTableStatement
                      AutoIncClause
                      DataTypeClause
                      DropStatement
                      IdentifierExpression)
           (lobos.schema DataType)))

;; -----------------------------------------------------------------------------

;; ## Analyzer

(defmethod analyze [:microsoft-sql-server nil]
  [_ expr]
  (when expr
    (cond (re-find #"^\(\((\d+)\)\)$" expr)
          (let [[[_ n]] (re-seq #"(\w+)(\(\))?" expr)]
            (Integer/parseInt n)))))

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

;; -----------------------------------------------------------------------------

;; ## Compiler

(defmethod compile [:sqlserver IdentifierExpression]
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

(defmethod compile [:sqlserver DataTypeClause]
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
      (str (as-sql-keyword dtype) (as-list args))
      (when collate (str "COLLATE " (as-str collate))))))

(defmethod compile [:sqlserver AutoIncClause]
  [_]
  "IDENTITY")

;; HACK: quick fix for len function
(defmethod compile [:sqlserver lobos.ast.CheckConstraintDefinition]
  [definition]
  (let [{:keys [db-spec cname condition identifiers]} definition
        identifiers-re (re-pattern (apply join \| identifiers))
        {:keys [stmt env]} condition
        condition (clojure.string/replace (first stmt)
                                          identifiers-re
                                          #(as-identifier db-spec %))
        condition (apply format
                         (.replace condition "?" "%s")
                         (map #(if (string? %)
                                 (str \' % \')
                                 %) env))]
    (join \space
      "CONSTRAINT"
      (as-identifier db-spec cname)
      "CHECK"
      (.replace condition "length" "len"))))

(defmethod compile [:sqlserver DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement
        sql-string (join \space
                     "DROP"
                     (as-sql-keyword otype)
                     (as-identifier db-spec oname :schema))]
    (apply join \;
      (conj (when (and (= otype :schema)
                       (= behavior :cascade))
              (vec (for [element (with-db-meta db-spec
                                   (-> (analyze-schema oname) :elements keys))]
                     (compile (schema/build-drop-statement
                               (schema/table element)
                               :cascade
                               (assoc db-spec :schema oname))))))
            sql-string))))

(defmethod compile [:sqlserver AlterModifyAction]
  [action]
  (let [{:keys [db-spec element]} action
        default (:default element)
        cname (when default
                (make-constraint-name (:tname element)
                                      "default"
                                      (list (:cname element))))]
    (cond (= default :drop)
          (join \space
                "DROP CONSTRAINT"
                (as-identifier db-spec cname))
          default
          (join \space
                "ADD CONSTRAINT"
                (as-identifier db-spec cname)
                "DEFAULT"
                (compile default)
                "FOR"
                (as-identifier db-spec (:cname element)))
          :else (unsupported "Only set/drop default supported."))))

(defmethod compile [:sqlserver AlterRenameAction]
  [action]
  (let [{:keys [db-spec element]} action]
    (format "EXEC sp_rename '%s', '%s', 'COLUMN';"
            (join \.
                  (as-identifier db-spec (:sname element))
                  (as-identifier db-spec (:tname element))
                  (as-identifier db-spec (:cname element)))
            (as-str (:others element)))))

(defmethod compile [:sqlserver AlterTableStatement]
  [statement]
  (let [{:keys [db-spec tname action element]} statement
        element (assoc element :sname (:schema db-spec) :tname tname)]
    (if (= action :rename)
      (compile (AlterRenameAction. db-spec element))
      (join \space
            "ALTER TABLE"
            (as-identifier db-spec tname :schema)
            (case action
                  :add    (compile (AlterAddAction. db-spec element))
                  :drop   (compile (AlterDropAction. db-spec element))
                  :modify (compile (AlterModifyAction. db-spec element)))))))
