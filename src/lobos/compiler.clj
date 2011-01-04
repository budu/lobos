;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.compiler
  "The compiler multimethod definition, an default implementation and
  some helpers functions."
  (:refer-clojure :exclude [compile replace])
  (:require (lobos [ast :as ast]))
  (:use (clojure [string :only [join
                                replace
                                upper-case]]))
  (:import (lobos.ast AutoIncClause
                      ColumnDefinition
                      CreateTableStatement
                      CreateSchemaStatement
                      DataTypeExpression
                      DropStatement
                      Identifier
                      UniqueConstraintDefinition
                      ValueExpression)))

(declare compile)

;;;; Helpers

(defn as-str ; taken from clojure.contrib.string
  "Like clojure.core/str, but if an argument is a keyword or symbol,
  its name will be used instead of its literal representation."
  ([] "")
  ([x] (if (instance? clojure.lang.Named x)
         (name x)
         (str x)))
  ([x & ys]
     ((fn [^StringBuilder sb more]
        (if more
          (recur (. sb (append (as-str (first more)))) (next more))
          (str sb)))
      (new StringBuilder ^String (as-str x)) ys)))

(defn as-list
  "Returns the given collection parenthesized string with its items
  separated by commas. Apply as-str to coll items."
  [coll]
  (when (not-empty coll)
    (format "(%s)" (join ", " (map as-str coll)))))

(defn as-sql-keyword
  "Returns the given string, symbol or keyword as an upper-cased string
  and replace dashes with spaces."
  [s]
  (replace (-> s as-str upper-case) \- \space))

(defn as-identifier
  "Constructs an Identifier ast object and compile it."
  [db-spec name & [level]]
  (compile (Identifier. db-spec name level)))

(defn unsupported
  "Throws an UnsupportedOperationException using the given message."
  [msg]
  (throw (java.lang.UnsupportedOperationException. msg)))

;;;; Compiler

(def backends-hierarchy
  (atom (-> (make-hierarchy)
            (derive :h2 ::standard)
            (derive :mysql ::standard)
            (derive :postgresql ::standard)
            (derive :sqlite ::standard))))

(defmulti compile
  "Compile the given statement."
  (fn [stmt]
    [(keyword (or (-> stmt :db-spec :subprotocol)
                  ::standard))
     (type stmt)])
  :hierarchy backends-hierarchy)

;;;; Default compiler

(defmethod compile [::standard Identifier]
  [identifier]
  (let [{:keys [db-spec value level]} identifier
        schema (:schema db-spec)]
    (if (and (= level :schema) schema)
      (str (when schema (str (as-identifier db-spec schema) "."))
           (as-identifier db-spec value))
      (as-str \" value \"))))

;;; Expressions

(defmethod compile [::standard ValueExpression]
  [expression]
  (let [{:keys [specification]} expression]
    (cond (keyword? specification) (str (as-sql-keyword specification))
          (string? specification) (str "'" specification "'")
          :else specification)))

(defmethod compile [::standard DataTypeExpression]
  [expression]
  (let [{:keys [dtype args]} expression]
    (str (as-sql-keyword dtype) (as-list args))))

;;; Clauses

(defmethod compile [::standard AutoIncClause]
  [_]
  "GENERATED ALWAYS AS IDENTITY")

;;; Definitions

(defmethod compile [::standard ColumnDefinition]
  [definition]
  (let [{:keys [db-spec cname data-type default
                auto-inc not-null others]} definition]
    (join \space
      (concat
       [(as-identifier db-spec cname)
        (compile data-type)]
       (when default  ["DEFAULT" (compile default)])
       (when auto-inc [(compile auto-inc)])
       (when not-null ["NOT NULL"])
       others))))

(defmethod compile [::standard UniqueConstraintDefinition]
  [definition]
  (let [{:keys [db-spec cname ctype columns]} definition
        spec (join \space
               [(as-sql-keyword ctype)
                (as-list (map (partial as-identifier db-spec) columns))])]
    (if cname
      (join \space [(as-identifier db-spec cname) spec])
      spec)))

;;; Statements

(defmethod compile [::standard CreateSchemaStatement]
  [statement]
  (let [{:keys [db-spec sname elements]} statement]
    (str "CREATE SCHEMA "
         (join "\n\n" (conj (map compile elements)
                            (as-identifier db-spec sname))))))

(defmethod compile [::standard CreateTableStatement]
  [statement]
  (let [{:keys [db-spec tname elements]} statement]
    (format "CREATE TABLE %s %s"
            (as-identifier db-spec tname :schema)
            (or (as-list (map compile elements))
                "()"))))

(defmethod compile [::standard DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (join \space
      (concat
       ["DROP"
        (as-sql-keyword otype)
        (as-identifier db-spec oname :schema)]
       (when behavior [(as-sql-keyword behavior)])))))
