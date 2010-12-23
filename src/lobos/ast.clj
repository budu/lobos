;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.ast
  "Abstract SQL syntax tree for the DDL part of the language.")

;;;; Atomic records

(defrecord Identifier [value])

(defrecord Literal [value])

;;;; Expression records

(defrecord DataTypeExpression [dtype args])

;; not going down the rabbit hole yet!
(defrecord ValueExpression [specification])

;;;; Definition records

(defrecord ColumnDefinition
  [cname data-type default identity not-null others])

(defrecord UniqueConstraintDefinition
  [cname ctype columns])

(defrecord ForeignKeyConstraintDefinition
  [cname columns refered match-type triggered-action])

(defrecord CheckConstraintDefinition
  [cname predicate])

;;;; Statement records

(defrecord CreateSchemaStatement [db-spec sname elements])

(defrecord CreateTableStatement [db-spec tname elements])

(defrecord AlterTableStatement [db-spec tname action])

(defrecord DropStatement [db-spec otype oname behavior])
