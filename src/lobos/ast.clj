; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.ast
  "Abstract SQL syntax tree for the DDL part of the language."
  (:use clojure.template)
  (:import (java.io Writer)))

;; -----------------------------------------------------------------------------

;; ## AST Records

;; ### Special Records

(defrecord Mode
  [db-spec])

;; ### Expression Records

(defrecord ScalarExpression
  [db-spec scalar])

(defrecord IdentifierExpression
  [db-spec name qualifiers])

(defrecord FunctionExpression
  [db-spec name args])

(defrecord OperatorExpression
  [db-spec op left right])

;; ### Clause Records

(defrecord AutoIncClause
  [db-spec])

(defrecord DataTypeClause
  [db-spec dtype args options])

;; ### Definition Records

(defrecord ColumnDefinition
  [db-spec cname data-type default auto-inc not-null others])

(defrecord ConstraintDefinition
  [db-spec cname])

(defrecord UniqueConstraintDefinition
  [db-spec cname ctype columns])

(defrecord ForeignKeyConstraintDefinition
  [db-spec cname columns parent-table parent-columns match triggered-actions])

(defrecord CheckConstraintDefinition
  [db-spec cname condition])

;; ### Statement Records

(defrecord CreateSchemaStatement
  [db-spec sname elements])

(defrecord CreateTableStatement
  [db-spec tname elements])

(defrecord CreateIndexStatement
  [db-spec iname tname columns options])

(defrecord DropStatement
  [db-spec otype oname behavior options])

(defrecord AlterTableStatement
  [db-spec tname action element])

;; ### Alter Action Records

(defrecord AlterAddAction
  [db-spec element])

(defrecord AlterDropAction
  [db-spec element])

(defrecord AlterModifyAction
  [db-spec element])

(defrecord AlterModifyDataTypeAndOptionsAction
  [db-spec element])

(defrecord AlterModifyDefaultAction
  [db-spec element])

(defrecord AlterRenameAction
  [db-spec element])


;; -----------------------------------------------------------------------------

;; ## Definations Print Method

(defn print-without-db-spec [r, ^Writer w]
  (.write w "#")
  (.write w (.getName (class r)))
  (print-method (dissoc r :db-spec) w))

(do-template [record]
  (defmethod print-method record [r, ^Writer w]
    (print-without-db-spec r w))
  Mode
  ScalarExpression
  IdentifierExpression
  FunctionExpression
  OperatorExpression
  AutoIncClause
  DataTypeClause
  ColumnDefinition
  ConstraintDefinition
  UniqueConstraintDefinition
  ForeignKeyConstraintDefinition
  CheckConstraintDefinition
  CreateSchemaStatement
  CreateTableStatement
  CreateIndexStatement
  DropStatement
  AlterTableStatement
  AlterAddAction
  AlterDropAction
  AlterModifyAction
  AlterModifyDataTypeAndOptionsAction
  AlterModifyDefaultAction
  AlterRenameAction)

;; -----------------------------------------------------------------------------

;; ## Import Helpers

(defn import-expressions
  "Import all expression AST records into the calling namespace."
  []
  (import
   '(lobos.ast ScalarExpression
               IdentifierExpression
               FunctionExpression
               OperatorExpression)))

(defn import-clauses
  "Import all clause AST records into the calling namespace."
  []
  (import
   '(lobos.ast AutoIncClause
               DataTypeClause)))

(defn import-definitions
  "Import all definition AST records into the calling namespace."
  []
  (import
   '(lobos.ast ColumnDefinition
               ConstraintDefinition
               UniqueConstraintDefinition
               ForeignKeyConstraintDefinition
               CheckConstraintDefinition)))

(defn import-statements
  "Import all statement AST records into the calling namespace."
  []
  (import
   '(lobos.ast CreateSchemaStatement
               CreateTableStatement
               CreateIndexStatement
               DropStatement
               AlterTableStatement)))

(defn import-actions
  "Import all action AST records into the calling namespace."
  []
  (import
   '(lobos.ast AlterAddAction
               AlterDropAction
               AlterModifyAction
               AlterModifyDataTypeAndOptionsAction
               AlterModifyDefaultAction
               AlterRenameAction)))

(defn import-all
  "Import all AST records into the calling namespace."
  []
  (import '(lobos.ast Mode))
  (import-expressions)
  (import-clauses)
  (import-definitions)
  (import-statements)
  (import-actions))
