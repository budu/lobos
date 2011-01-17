; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.ast
  "Abstract SQL syntax tree for the DDL part of the language.")

;; ### Special Records

(defrecord Mode
  [db-spec])

;; ### Atomic Records

(defrecord Identifier
  [db-spec value level])

(defrecord Literal
  [db-spec value])

;; ### Expression Records

(defrecord DataTypeExpression
  [db-spec dtype args options])

;; Not going down the rabbit hole yet!
(defrecord ValueExpression
  [db-spec specification])

;; ### Clause Records

(defrecord AutoIncClause
  [db-spec])

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
  [db-spec cname condition identifiers])

;; ### Statement Records

(defrecord CreateSchemaStatement
  [db-spec sname elements])

(defrecord CreateTableStatement
  [db-spec tname elements])

(defrecord DropStatement
  [db-spec otype oname behavior])

(defrecord AlterTableStatement
  [db-spec tname action element])

;; ### Alter Action Records

(defrecord AlterAddAction
  [db-spec element])

(defrecord AlterDropAction
  [db-spec element])

(defrecord AlterModifyAction
  [db-spec element])

(defrecord AlterRenameAction
  [db-spec element])

;; ## Helpers

(defn import-all
  "Import all AST record into the calling namespace."
  []
  (import
   '(lobos.ast AlterAddAction
               AlterDropAction
               AlterModifyAction
               AlterRenameAction
               AlterTableStatement
               AutoIncClause
               CheckConstraintDefinition
               ColumnDefinition
               ConstraintDefinition
               CreateTableStatement
               CreateSchemaStatement
               DataTypeExpression
               DropStatement
               ForeignKeyConstraintDefinition
               Identifier
               Literal
               Mode
               UniqueConstraintDefinition
               ValueExpression)))
