;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.ast
  "Abstract SQL syntax tree for the DDL part of the language.")

;;;; Special records

(defrecord Mode
  [db-spec])

;;;; Atomic records

(defrecord Identifier
  [db-spec value level])

(defrecord Literal
  [db-spec value])

;;;; Expression records

(defrecord DataTypeExpression
  [db-spec dtype args options])

;; not going down the rabbit hole yet!
(defrecord ValueExpression
  [db-spec specification])

;;; Clause records

(defrecord AutoIncClause
  [db-spec])

;;;; Definition records

(defrecord ColumnDefinition
  [db-spec cname data-type default auto-inc not-null others])

(defrecord UniqueConstraintDefinition
  [db-spec cname ctype columns])

(defrecord ForeignKeyConstraintDefinition
  [db-spec cname columns foreign-table foreign-columns match triggered-actions])

(defrecord CheckConstraintDefinition
  [db-spec cname condition identifiers])

;;;; Statement records

(defrecord CreateSchemaStatement
  [db-spec sname elements])

(defrecord CreateTableStatement
  [db-spec tname elements])

(defrecord AlterTableStatement
  [db-spec tname action])

(defrecord DropStatement
  [db-spec otype oname behavior])
