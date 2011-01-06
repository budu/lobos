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
        lobos.analyzer
        lobos.compiler
        lobos.utils)
  (:import (lobos.ast ColumnDefinition
                      DataTypeExpression)
           (lobos.schema DataType)))

;;;; Analyzer

(defvar- analyzer-data-type-aliases
  {:bool :boolean
   :bpchar :char
   :bytea :blob
   :float4 :real
   :float8 :double
   :int2 :smallint
   :int4 :integer
   :int8 :bigint
   :text :nclob})

(defmethod analyze [:postgresql DataType]
  [_ column-meta]
  (let [dtype (-> column-meta :type_name as-keyword)
        dtype (first (replace analyzer-data-type-aliases
                              [dtype]))]
    (DataType.
     dtype
     (case dtype
       :varchar [(:column_size column-meta)]
       []))))

;;;; Compiler

(defvar- compiler-data-type-aliases
  {:blob :bytea
   :clob :text
   :double :double-precision
   :nclob :text})

(defmethod compile [:postgresql DataTypeExpression]
  [expression]
  (let [{:keys [dtype args]} expression]
    (str (as-sql-keyword
          (first (replace compiler-data-type-aliases [dtype])))
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
