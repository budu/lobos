;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.backends.h2
  "Compiler implementation for H2."
  (:refer-clojure :exclude [compile])
  (:use (clojure.contrib [def :only [defvar-]])
        lobos.compiler
        lobos.utils)
  (:import (lobos.ast AutoIncClause
                      CreateSchemaStatement
                      DataTypeExpression
                      DropStatement)))

;;;; Compiler

(defmethod compile [:h2 DataTypeExpression]
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

(defmethod compile [:h2 CreateSchemaStatement]
  [statement]
  (let [{:keys [db-spec sname elements]} statement]
    (apply join ";\n\n"
      (conj (map (comp compile
                       #(assoc-in % [:db-spec :schema] sname))
                 elements)
            (str "CREATE SCHEMA "
                 (as-identifier db-spec sname))))))

(defmethod compile [:h2 DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (join \space
      "DROP"
      (as-sql-keyword otype)
      (as-identifier db-spec oname :schema)
      (when (and behavior (#{:table} otype))
        [(as-sql-keyword behavior)]))))
