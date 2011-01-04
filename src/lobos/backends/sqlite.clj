;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.backends.sqlite
  "Compiler implementation for SQLite."
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

(defmethod compile [:sqlite Identifier]
  [identifier]
  (as-str (:value identifier)))

#_(
(defvar- dtypes-aliases ;; TMP
  {:clob :text})

(defmethod compile [:sqlite DataTypeExpression] ;; TMP
  [expression]
  (let [{:keys [dtype args]} expression]
    (str (as-sql-keyword (dtypes-replace dtypes-aliases dtype))
         (as-list args))))
)

(defmethod compile [:sqlite AutoIncClause]
  [_]
  "AUTOINCREMENT")

(defmethod compile [:sqlite CreateSchemaStatement]
  [statement]
  (let [{:keys [db-spec sname elements]} statement]
    (join ";\n\n"
          (map (comp compile
                     #(assoc-in % [:db-spec :schema] sname))
               elements))))

(defmethod compile [:sqlite DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (when (#{:table} otype)
      (join \space
        ["DROP"
         (as-sql-keyword otype)
         (as-identifier db-spec oname)]))))
