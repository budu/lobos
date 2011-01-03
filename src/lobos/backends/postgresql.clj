;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.backends.postgresql
  "Compiler implementation for PostgreSQL."
  (:refer-clojure :exclude [compile])
  (:use (clojure.contrib [def :only [defvar-]])
        (clojure [string :only [join]])
        lobos.compiler
        lobos.utils)
  (:import (lobos.ast ColumnDefinition
                      DataTypeExpression)))

(defvar- dtypes-aliases
  {:blob :bytea
   :double :double-precision})

(defmethod compile [:postgresql DataTypeExpression]
  [expression]
  (let [{:keys [dtype args]} expression]
    (str (as-sql-keyword (dtypes-replace dtypes-aliases dtype))
         (as-list args))))

(defmethod compile [:postgresql ColumnDefinition]
  [definition]
  (let [{:keys [db-spec cname data-type default
                auto-inc not-null others]} definition]
    (join \space
      (concat
       [(as-identifier db-spec cname)
        (if auto-inc
          "SERIAL"
          (compile data-type))]
       (when default  ["DEFAULT" (compile default)])
       (when not-null ["NOT NULL"])
       others))))
