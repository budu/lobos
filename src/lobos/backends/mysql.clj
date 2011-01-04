;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.backends.mysql
  "Compiler implementation for MySQL."
  (:refer-clojure :exclude [compile])
  (:use (clojure.contrib [def :only [defvar-]])
        (clojure [string :only [join]])
        lobos.compiler
        lobos.utils)
  (:import (lobos.ast AutoIncClause
                      CreateSchemaStatement
                      CreateTableStatement
                      DataTypeExpression
                      DropStatement
                      Identifier)))

(defmethod compile [:mysql Identifier]
  [identifier]
  (let [{:keys [db-spec value level]} identifier
        schema (:schema db-spec)]
    (if (and (= level :schema) schema)
      (str (when schema (str (as-identifier db-spec schema) "."))
           (as-identifier db-spec value))
      (as-str \` value \`))))

(defvar- dtypes-aliases
  {:clob :text})

(defmethod compile [:mysql DataTypeExpression]
  [expression]
  (let [{:keys [dtype args]} expression]
    (str (as-sql-keyword (dtypes-replace dtypes-aliases dtype))
         (as-list args))))

(defmethod compile [:mysql AutoIncClause]
  [_]
  "AUTO_INCREMENT")

(defmethod compile [:mysql CreateSchemaStatement]
  [statement]
  (let [{:keys [db-spec sname elements]} statement]
    (join ";\n\n"
          (conj (map (comp compile
                           #(assoc-in % [:db-spec :schema] sname))
                     elements)
                (str "CREATE SCHEMA "
                     (as-identifier db-spec sname))))))

(defmethod compile [:mysql DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (join \space
      (concat
       ["DROP"
        (as-sql-keyword otype)
        (as-identifier db-spec oname :schema)]
       (when (and behavior (#{:table} otype))
         [(as-sql-keyword behavior)])))))
