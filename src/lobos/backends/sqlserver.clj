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
  (:use (clojure.contrib [def :only [defvar-]])
        (clojure [string :only [join]])
        lobos.compiler
        lobos.utils)
  (:import (lobos.ast AutoIncClause
                      DataTypeExpression
                      DropStatement
                      Identifier)))

(defmethod compile [:sqlserver Identifier]
  [identifier]
  (let [{:keys [db-spec value level]} identifier
        schema (:schema db-spec)]
    (if (and (= level :schema) schema)
      (str (when schema (str (as-identifier db-spec schema) "."))
           (as-identifier db-spec value))
      (as-str \[ value \]))))

(defvar- dtypes-aliases
  {:blob :image
   :boolean :bit
   :clob :text
   :timestamp :datetime})

(defmethod compile [:sqlserver DataTypeExpression]
  [expression]
  (let [{:keys [dtype args]} expression
        [dtype args] (if (= dtype :double)
                       [:float [53]]
                       [dtype args])]
    (str (as-sql-keyword (dtypes-replace dtypes-aliases dtype))
         (as-list args))))

(defmethod compile [:sqlserver AutoIncClause]
  [_]
  "IDENTITY")

(defmethod compile [:sqlserver DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (join \space
      ["DROP"
       (as-sql-keyword otype)
       (as-identifier db-spec oname :schema)])))
