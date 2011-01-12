;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.compiler
  "The compiler multimethod definition, an default implementation and
  some helpers functions."
  (:refer-clojure :exclude [compile replace])
  (:require (lobos [ast :as ast]))
  (:use (clojure [string :only [replace]])
        lobos.utils)
  (:import (java.lang UnsupportedOperationException)
           (lobos.ast AlterAddAction
                      AlterDropAction
                      AlterModifyAction
                      AlterRenameAction
                      AlterTableStatement
                      AutoIncClause
                      CheckConstraintDefinition
                      ColumnDefinition
                      CreateTableStatement
                      CreateSchemaStatement
                      DataTypeExpression
                      DropStatement
                      ForeignKeyConstraintDefinition
                      Identifier
                      Mode
                      UniqueConstraintDefinition
                      ValueExpression)))

(declare compile)

;;;; Helpers

(defn as-identifier
  "Constructs an Identifier ast object and compile it."
  [db-spec name & [level]]
  (compile (Identifier. db-spec name level)))

(defn unsupported
  "Throws an UnsupportedOperationException using the given message. Can
  be given a test, in which case it throw an exception only if it's
  true."
  ([msg] (unsupported true msg))
  ([test msg]
     (when test
       (throw (UnsupportedOperationException. (str msg))))))

(defn mode [db-spec] (Mode. db-spec))

;;;; Compiler

(def backends-hierarchy
  (atom (-> (make-hierarchy)
            (derive :h2 ::standard)
            (derive :mysql ::standard)
            (derive :postgresql ::standard)
            (derive :sqlite ::standard)
            (derive :sqlserver ::standard))))

(defmulti compile
  "Compile the given statement."
  (fn [stmt]
    [(keyword (or (-> stmt :db-spec :subprotocol)
                  ::standard))
     (type stmt)])
  :hierarchy backends-hierarchy)

;;;; Default compiler

(defmethod compile [::standard Mode] [_] nil)

(defmethod compile [::standard Identifier]
  [identifier]
  (let [{:keys [db-spec value level]} identifier
        schema (:schema db-spec)]
    (if (and (= level :schema) schema)
      (str (when schema (str (as-identifier db-spec schema) "."))
           (as-identifier db-spec value))
      (as-str \" value \"))))

;;; Expressions

(defmethod compile [::standard ValueExpression]
  [expression]
  (let [{:keys [specification]} expression]
    (cond (keyword? specification) (str (as-sql-keyword specification))
          (string? specification) (str "'" specification "'")
          :else specification)))

(defmethod compile [::standard DataTypeExpression]
  [expression]
  (let [{:keys [dtype args options]} expression
        {:keys [encoding collate time-zone]} options]
    (join \space
      (str (as-sql-keyword dtype) (as-list args))
      (when encoding ["CHARACTER SET" (as-str encoding)])
      (when collate ["COLLATE" (as-str collate)])
      (when time-zone ["WITH TIME ZONE"]))))

;;; Clauses

(defmethod compile [::standard AutoIncClause]
  [_]
  "GENERATED ALWAYS AS IDENTITY")

;;; Definitions

(defmethod compile [::standard ColumnDefinition]
  [definition]
  (let [{:keys [db-spec cname data-type default
                auto-inc not-null others]} definition]
    (apply join \space
      (as-identifier db-spec cname)
      (compile data-type)
      (when default  (str "DEFAULT " (compile default)))
      (when auto-inc (compile auto-inc))
      (when not-null "NOT NULL")
      others)))

(defmethod compile [::standard UniqueConstraintDefinition]
  [definition]
  (let [{:keys [db-spec cname ctype columns]} definition]
    (join \space
      "CONSTRAINT"
      (as-identifier db-spec cname)
      (as-sql-keyword ctype)
      (as-list (map (partial as-identifier db-spec) columns)))))

(defmethod compile [::standard ForeignKeyConstraintDefinition]
  [definition]
  (let [{:keys [db-spec cname columns parent-table parent-columns match
                triggered-actions]} definition]
    (join \space
      "CONSTRAINT"
      (as-identifier db-spec cname)
      "FOREIGN KEY"
      (as-list (map (partial as-identifier db-spec) columns))
      "REFERENCES"
      (as-identifier db-spec parent-table :schema)
      (as-list (map (partial as-identifier db-spec) parent-columns))
      (when match (str "MATCH " (as-sql-keyword match)))
      (when (contains? triggered-actions :on-delete)
        (str "ON DELETE " (as-sql-keyword (:on-delete triggered-actions))))
      (when (contains? triggered-actions :on-update)
        (str "ON UPDATE " (as-sql-keyword (:on-update triggered-actions)))))))

(defmethod compile [::standard CheckConstraintDefinition]
  [definition]
  (let [{:keys [db-spec cname condition identifiers]} definition
        identifiers-re (re-pattern (apply join \| identifiers))
        {:keys [stmt env]} condition
        condition (replace (first stmt)
                           identifiers-re
                           #(as-identifier db-spec %))
        condition (apply format
                         (replace condition "?" "%s")
                         (map #(if (string? %)
                                 (str \' % \')
                                 %) env))]
    (join \space
      "CONSTRAINT"
      (as-identifier db-spec cname)
      "CHECK"
      condition)))

;;; Statements

(defmethod compile [::standard CreateSchemaStatement]
  [statement]
  (let [{:keys [db-spec sname elements]} statement]
    (str "CREATE SCHEMA "
         (apply join "\n\n" (conj (map compile elements)
                                  (as-identifier db-spec sname))))))

(defmethod compile [::standard CreateTableStatement]
  [statement]
  (let [{:keys [db-spec tname elements]} statement]
    (format "CREATE TABLE %s %s"
            (as-identifier db-spec tname :schema)
            (or (as-list (map compile elements))
                "()"))))

(defmethod compile [::standard DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (join \space
      "DROP"
      (as-sql-keyword otype)
      (as-identifier db-spec oname :schema)
      (as-sql-keyword behavior))))

;;; Alter statement and actions

(defmethod compile [::standard AlterAddAction]
  [action]
  (let [{:keys [db-spec element]} action
        is-column (instance? ColumnDefinition element)]
    (join \space
          "ADD"
          (compile element))))

(defmethod compile [::standard AlterDropAction]
  [action]
  (let [{:keys [db-spec element]} action
        is-column (instance? ColumnDefinition element)]
    (join \space
          "DROP"
          (if is-column
            "COLUMN"
            "CONSTRAINT")
          (as-identifier db-spec (:cname element)))))

(defmethod compile [::standard AlterModifyAction]
  [action]
  (let [{:keys [db-spec element]} action
        default (:default element)]
    (if default
      (join \space
            "ALTER COLUMN"
            (as-identifier db-spec (:cname element))
            (if (= default :drop)
              "DROP DEFAULT"
              (str "SET DEFAULT " (compile default))))
      (unsupported "Only set/drop default supported."))))

(defmethod compile [::standard AlterRenameAction]
  [action]
  (let [{:keys [db-spec element]} action]
     (unsupported "Rename action not supported.")))

(defmethod compile [::standard AlterTableStatement]
  [statement]
  (let [{:keys [db-spec tname action element]} statement
        element (assoc element :sname (:schema db-spec) :tname tname)]
    (join \space
          "ALTER TABLE"
          (as-identifier db-spec tname :schema)
          (case action
            :add    (compile (AlterAddAction. db-spec element))
            :drop   (compile (AlterDropAction. db-spec element))
            :modify (compile (AlterModifyAction. db-spec element))
            :rename (compile (AlterRenameAction. db-spec element))))))
