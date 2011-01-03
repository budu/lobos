;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.schema
  "The abstract schema data-structure and some function to help creating
  one."
  (:refer-clojure :exclude [bigint boolean char double float])
  (:require (lobos [ast :as ast]))
  (:import (lobos.ast AutoIncClause
                      ColumnDefinition
                      CreateTableStatement
                      CreateSchemaStatement
                      DataTypeExpression
                      DropStatement
                      UniqueConstraintDefinition
                      ValueExpression)))

;;;; Helpers

(defn conj-when
  "Like conj but if test is false returns coll untouched."
  [coll test x & xs]
  (if test
    (apply conj coll x xs)
    coll))

;;;; Protocols

(defprotocol Buildable
  (build-definition [this db-spec]))

(defprotocol Creatable
  (build-create-statement [this db-spec]))

(defprotocol Dropable
  (build-drop-statement [this behavior db-spec]))

;;;; Common exceptions

(defn name-required [name otype]
  (when-not name
    (throw (IllegalArgumentException.
            (format "A % definition needs at least a name."
                    otype)))))

;;;; Definition predicate

(defn definition?
  "Returns true if the given object is a definition."
  [o]
  (isa? (type o) ::definition))

;;;; Constraint definition

(defrecord Constraint [cname ctype specification]
  Buildable
  
  (build-definition [this db-spec]
    (condp contains? ctype
      #{:unique :primary-key}
      (UniqueConstraintDefinition.
       db-spec
       cname
       ctype
       (:columns specification)))))

(defn constraint
  "Constructs an abstract constraint definition and add it to the given
  table."
  [table constraint-name constraint-type specification]
  (update-in table [:constraints] conj
             [constraint-name
              (Constraint. constraint-name
                           constraint-type
                           specification)]))

(defn unique-constraint
  "Constructs an abstract unique (or primary-key depending on the given
  type) constraint definition and add it to the given table."
  [table constraint-type name-or-column columns]
  (let [named (contains? (-> table :columns) name-or-column)
        constraint-name (when named name-or-column)
        columns (if named
                  columns
                  (conj columns name-or-column))]
    (constraint table
                constraint-name
                constraint-type
                {:columns (vec columns)})))

(defn primary-key
  "Constructs an abstract primary key constraint definition and add it
  to the given table."
  [table name-or-column & columns]
  (unique-constraint table :primary-key name-or-column columns))

(defn unique
  "Constructs an abstract unique constraint definition and add it to the
  given table."
  [table name-or-column & columns]
  (unique-constraint table :unique name-or-column columns))

;;;; Data-type definition

(defrecord DataType [dtype args])

(defn data-type [dtype & args]
  (DataType. dtype (vec args)))

;;;; Column definition

(defrecord Column [cname data-type default auto-inc not-null others]
  Buildable
  
  (build-definition [this db-spec]
    (ColumnDefinition.
     db-spec
     cname
     (DataTypeExpression.
      db-spec
      (:dtype data-type)
      (:args data-type))
     (when default (ValueExpression. db-spec default))
     (when auto-inc (AutoIncClause. db-spec))
     not-null
     others)))

(defn column*
  "Constructs an abstract column definition."
  [column-name data-type options]
  (let [default  (first (filter vector? options))
        others   (vec (filter string? options))
        not-null (clojure.core/boolean (:not-null options))
        auto-inc (clojure.core/boolean (:auto-inc options))]
    (Column. column-name
             data-type
             (second default)
             auto-inc
             not-null
             others)))

(defn column
  "Constructs an abstract column definition and add it to the given
  table."
  [table column-name data-type options]
  (let [options  (set options)]
    (name-required column-name "column")
    (#(cond (options :primary-key) (primary-key % column-name)
            (options :unique) (unique % column-name)
            :else %)
     (update-in table [:columns] conj
                [column-name (column* column-name data-type options)]))))

;;;; Typed column definition

;;; Typed column helpers

(defn def-typed-columns*
  "Helper for macros that create typed columns definitions."
  [names args body]
  `(do
     ~@(for [n names]
         `(defn ~n
            ~(format (str "Constructs an abstract %s column definition and"
                          " add it to the given table.")
                     (name n))
            ~args
            ~@(body n)))))

(defmacro def-simple-typed-columns
  "Defines typed columns for simple data-types taking no arguments."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & options]
    #(list `(column ~'table
                    ~'column-name
                    (data-type ~(keyword %))
                    ~'options))))

(defmacro def-numeric-like-typed-columns
  "Defines numeric-like typed columns."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & [precision scale & options]]
    #(list
      `(let ~'[dargs (-> []
                         (conj-when (integer? precision) precision)
                         (conj-when (integer? scale) scale))
               options (-> options
                           (conj-when (not (integer? precision)) precision)
                           (conj-when (not (integer? scale)) scale))]
         (column ~'table
                 ~'column-name
                 (apply data-type ~(keyword %) ~'dargs)
                 ~'options)))))

(defmacro def-length-bounded-typed-columns
  "Defines length-bounded typed columns."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & [length & options]]
    #(list
      `(let ~'[dargs (conj-when [] (integer? length) length)
               options (conj-when options (not (integer? length)) length)]
         (column ~'table
                 ~'column-name
                 (apply data-type ~(keyword %) ~'dargs)
                 ~'options)))))

;;; Numeric types

(def-simple-typed-columns
  smallint
  integer
  bigint)

(def-numeric-like-typed-columns
  numeric
  decimal)

(def-simple-typed-columns
  real
  double)

(defn float
  "Constructs an abstract float column definition and add it to the
  given table."
  [table column-name & [precision & options]]
  (let [dargs (conj-when [] (integer? precision) precision)
        options (conj-when options (not (integer? precision)) precision)]
    (column table column-name (apply data-type :float dargs) options)))

;;; Character types

(def-length-bounded-typed-columns
  char
  varchar)

(def-simple-typed-columns
  clob)

;;; Binary data type

(def-simple-typed-columns
  blob)

;;; Boolean type

(def-simple-typed-columns
  boolean)

;;; Data/time types

(def-simple-typed-columns
  timestamp)

;;;; Table definition

(defrecord Table [name columns constraints options]
  Creatable Dropable
  
  (build-create-statement [this db-spec]
    (CreateTableStatement.
     db-spec
     name
     (map #(build-definition (second %) db-spec)
          (concat columns constraints))))

  (build-drop-statement [this behavior db-spec]
    (DropStatement. db-spec :table name behavior)))

(defn table*
  "Constructs an abstract table definition."
  [table-name columns constraints options]
  (name-required table-name "table")
  (Table. table-name columns constraints options))

(defmacro table
  "Constructs an abstract table definition containing the given
  elements."
  [name & elements]
  `(-> (table* ~name {} {} {}) ~@elements))

;;;; Schema definition

(defrecord Schema [sname elements options]
  Creatable Dropable
  
  (build-create-statement [this db-spec]
    (CreateSchemaStatement.
     db-spec
     sname
     (map #(build-create-statement (second %) db-spec) elements)))

  (build-drop-statement [this behavior db-spec]
    (DropStatement. db-spec :schema sname behavior)))

(defn schema?
  "Returns true if the given object is a Schema."
  [o]
  (isa? (type o) Schema))

(defn schema
  "Constructs an abstract schema definition."
  [schema-name & [options-or-element & elements]]
  (name-required schema-name "schema")
  (let [options (when-not (definition? options-or-element)
                  options-or-element)
        elements (if options
                   elements
                   (when elements
                     (conj elements options-or-element)))]
    (Schema.
     schema-name
     (into (sorted-map)
           (map #(vector (:name %) %) elements))
     (or options {}))))

;;;; Definitions hierarchy

(derive Constraint ::definition)
(derive DataType ::definition)
(derive Column ::definition)
(derive Table ::definition)
(derive Schema ::definition)
