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
  (:refer-clojure :exclude [bigint boolean char double float time replace])
  (:require clojureql.predicates
            lobos.ast)
  (:use (clojure [walk :only [postwalk-replace]]
                 [string :only [replace]])
        (clojure.contrib [def :only [defalias]])
        lobos.utils)
  (:import (lobos.ast AlterTableStatement
                      AutoIncClause
                      CheckConstraintDefinition
                      ColumnDefinition
                      CreateTableStatement
                      CreateSchemaStatement
                      DataTypeExpression
                      DropStatement
                      ForeignKeyConstraintDefinition
                      UniqueConstraintDefinition
                      ValueExpression)))

;;;; Protocols

(defprotocol Alterable
  (build-alter-statement [this action db-spec]))

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

(defrecord UniqueConstraint [cname ctype columns]
  Buildable
  
  (build-definition [this db-spec]
    (UniqueConstraintDefinition.
     db-spec
     cname
     ctype
     columns)))

(defn unique-constraint
  "Constructs an abstract unique (or primary-key depending on the given
  type) constraint definition and add it to the given table."
  [table name type columns]
  (let [name (or name (make-constraint-name table type columns))]
    (update-in table [:constraints] conj
               [name (UniqueConstraint. name type (vec columns))])))

(defn primary-key
  "Constructs an abstract primary key constraint definition and add it
  to the given table."
  ([table columns] (primary-key table nil columns))
  ([table name columns]
     (unique-constraint table name :primary-key columns)))

(defn unique
  "Constructs an abstract unique constraint definition and add it to the
  given table."
  ([table columns] (unique table nil columns))
  ([table name columns]
     (unique-constraint table name :unique columns)))

(defrecord ForeignKeyConstraint
  [cname columns parent-table parent-columns match triggered-actions]
  Buildable

  (build-definition [this db-spec]
    (ForeignKeyConstraintDefinition.
     db-spec
     cname
     columns
     parent-table
     parent-columns
     match
     triggered-actions)))

(defn foreign-key
  "Constructs an abstract foreign key constraint definition and add it
  to the given table."
  {:arglists '([table name? columns parent-table parent-columns? match?
                & triggered-actions])}
  [table & args]
  (let [[constraint-name args] (optional keyword? args)
        columns                (first args)
        parent-table           (second args)
        args                   (nnext args)
        [parent-columns args]  (optional vector? args)
        parent-columns         (or parent-columns columns)
        [match args]           (optional #{:full :partial :simple} args)
        triggered-actions      (apply hash-map args)
        constraint-name        (or constraint-name
                                   (make-constraint-name table "fkey" columns))]
    (update-in table [:constraints] conj
               [constraint-name
                (ForeignKeyConstraint. constraint-name
                                       columns
                                       parent-table
                                       parent-columns
                                       match
                                       triggered-actions)])))

(defrecord CheckConstraint
  [cname condition identifiers]
  Buildable

  (build-definition [this db-spec]
    (CheckConstraintDefinition.
     db-spec
     cname
     condition
     identifiers)))

(defn check*
  "Constructs an abstract check constraint definition and add it to the
  given table."
  [table constraint-name condition identifiers]
  (update-in table [:constraints] conj
             [constraint-name
              (CheckConstraint. constraint-name
                                condition
                                identifiers)]))

(defmacro check
  "Constructs an abstract check constraint definition and add it to the
  given table. Replace core predicates by custom one from ClojureQL
  predicates namespace."
  [table constraint-name condition]
  `(check*
    ~table
    ~constraint-name
    ~(postwalk-replace
      '{=   clojureql.predicates/=*
        !=  clojureql.predicates/!=*
        <   clojureql.predicates/<*
        >   clojureql.predicates/>*
        <=  clojureql.predicates/<=*
        >=  clojureql.predicates/>=*
        and clojureql.predicates/and*
        or  clojureql.predicates/or*
        not clojureql.predicates/not*
        in  clojureql.predicates/in}
      condition)
    ~(capture-keywords condition)))

;;;; Data-type definition

(defrecord DataType [dtype args options])

(defn data-type [dtype & [args options]]
  (DataType. dtype (vec args)
             (merge {:time-zone nil
                     :collate nil
                     :encoding nil}
                     options)))

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
      (:args data-type)
      (:options data-type))
     (if (= default :drop)
       default
       (when default (ValueExpression. db-spec default)))
     (when auto-inc (AutoIncClause. db-spec))
     not-null
     others)))

(defn column*
  "Constructs an abstract column definition."
  [column-name data-type options]
  (let [{:keys [default encoding collate]}
        (into {} (filter vector? options))
        data-type (when data-type
                    (update-in data-type [:options]
                               (partial merge-with #(or %1 %2))
                               {:encoding encoding
                                :collate collate
                                :time-zone ((set options) :time-zone)}))
        others     (vec (filter string? options))
        option-set (set options)
        not-null   (clojure.core/boolean (:not-null option-set))
        auto-inc   (clojure.core/boolean (:auto-inc option-set))]
    (Column. column-name
             data-type
             default
             auto-inc
             not-null
             others)))

(defn column
  "Constructs an abstract column definition and add it to the given
  table. Also creates and add the appropriate column constraints.

  It also can be used in alter modify and rename actions. In that
  case, if data-type is :to, it acts as a column rename clause and if
  data-type is :drop-default, it acts as a column drop default clause."
  {:arglists '([table column-name data-type? options])}
  [table column-name & options]
  (name-required column-name "column")
  (let [[data-type options] (optional #(instance? DataType %) options)
        option-set (when (seq? options) (set options))
        add-constraint #(cond (:primary-key option-set)
                              (primary-key % [column-name])
                              (:unique option-set)
                              (unique % [column-name])
                              :else %)]
    (add-constraint
     (update-in table [:columns] conj
                [column-name
                 (case (first options)
                   :to (Column. column-name nil nil nil nil (second options))
                   :drop-default (Column. column-name nil :drop nil nil nil)
                   (column* column-name data-type options))]))))

;;;; Typed column definition

;;; Typed column helpers

(defn def-typed-columns*
  "Helper for macros that create typed columns definitions."
  [names args dargs options]
  `(do
     ~@(for [n names]
         `(defn ~n
            ~(format (str "Constructs an abstract %s column definition and"
                          " add it to the given table.")
                     (name n))
            ~args
            (let [dargs# ~dargs
                  options# ~options]
              (apply column
                     ~'table
                     ~'column-name
                     (data-type ~(keyword n) dargs#)
                     options#))))))

(defmacro def-simple-typed-columns
  "Defines typed columns for simple data-types taking no arguments."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & options]
    '[]
    'options))

(defmacro def-numeric-like-typed-columns
  "Defines numeric-like typed columns."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & [precision scale & options]]
    '(-> []
         (conj-when (integer? precision) precision)
         (conj-when (integer? scale) scale))
    '(-> options
         (conj-when (not (integer? precision)) precision)
         (conj-when (not (integer? scale)) scale))))

(defmacro def-optional-precision-typed-columns
  "Defines typed columns with optional precision."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & [precision & options]]
    '(conj-when [] (integer? precision) precision)
    '(conj-when options (not (integer? precision)) precision)))

(defmacro def-optional-length-typed-columns
  "Defines optionally length-bounded typed columns."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & [length & options]]
    '(conj-when [] (integer? length) length)
    '(conj-when options (not (integer? length)) length)))

(defmacro def-length-bounded-typed-columns
  "Defines length-bounded typed columns."
  [& names]
  (def-typed-columns*
    names
    '[table column-name length & options]
    '(conj-when [] (integer? length) length)
    '(conj-when options (not (integer? length)) length)))

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
  double-precision)

(defalias double double-precision)

(def-optional-precision-typed-columns
  float)

;;; Character types

(def-optional-length-typed-columns
  char
  nchar
  clob
  nclob)

(def-length-bounded-typed-columns  
  varchar
  nvarchar)

;;; Binary data type

(def-optional-length-typed-columns
  binary
  blob)

(def-length-bounded-typed-columns
  varbinary)

;;; Boolean type

(def-simple-typed-columns
  boolean)

;;; Data/time types

(def-simple-typed-columns
  date)

(def-optional-precision-typed-columns
  time
  timestamp)

;;;; Table definition

(defrecord Table [name columns constraints options]
  Alterable Creatable Dropable

  (build-alter-statement [this action db-spec]
    (let [elements (map #(build-definition (second %) db-spec)
                        (concat columns constraints))]
      (for [element elements]
        (AlterTableStatement.
         db-spec
         name
         action
         element))))

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
  [table-name & [columns constraints options]]
  (name-required table-name "table")
  (Table. table-name
          (or columns {})
          (or constraints {})
          (or options {})))

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
  {:arglists '([schema-name options? & elements])}
  [schema-name & args]
  (name-required schema-name "schema")
  (let [[options elements] (optional (comp not definition?) args)]
    (Schema.
     schema-name
     (into (sorted-map)
           (map #(vector (:name %) %) elements))
     (or options {}))))

;;;; Definitions hierarchy

(derive UniqueConstraint ::definition)
(derive DataType ::definition)
(derive Column ::definition)
(derive Table ::definition)
(derive Schema ::definition)
