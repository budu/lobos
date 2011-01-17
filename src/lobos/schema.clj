; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.schema
  "This namespace include the abstract schema data-structures, an
  handful of helpers to create them and the protocol to build the into
  an abstract syntax tree of implementation agnostic SQL
  statements. Abstract schema data-structures can be divided in two
  categories.

  First, schema elements which include the `schema` and the `table`
  element definitions. Those can be created or dropped, also the `table`
  element can be altered.

  Then there's the table elements that serves to define tables. There's
  the completly abstract `column` and `constraint` elements, which are
  only meant to be used directly with the alter action. Each of them
  have more specialized function to help you define tables, like the
  `unique`, `primary-key`, `foreign-key` and `check` constraints
  definitons and the typed data definitions."
  (:refer-clojure :exclude [defonce replace
                            bigint boolean char double float time])
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
                      ConstraintDefinition
                      CreateTableStatement
                      CreateSchemaStatement
                      DataTypeExpression
                      DropStatement
                      ForeignKeyConstraintDefinition
                      UniqueConstraintDefinition
                      ValueExpression)))

;; -----------------------------------------------------------------------------

;; ## Protocols

;; The Alterable protocol add the possibility of building alter
;; statements from an object implementing it. *For internal use*.
(defprotocol Alterable
  (build-alter-statement [this action db-spec]))

;; The Buildable protocol is currently used only by table elements.
;; *For internal use*.
(defprotocol Buildable
  (build-definition [this db-spec]))

;; The Creatable protocol add the possibility of building create
;; statements from an object implementing it. *For internal use*.
(defprotocol Creatable
  (build-create-statement [this db-spec]))

;; The Dropable protocol add the possibility of building drop
;; statements from an object implementing it. *For internal use*.
(defprotocol Dropable
  (build-drop-statement [this behavior db-spec]))

;; -----------------------------------------------------------------------------

;; ## Common Exception

(defn name-required
  "Throws an IllegalArgumentException when the given name is nil with a
  default message using the given type of elements."
  [name etype]
  (when-not name
    (throw (IllegalArgumentException.
            (format "A % definition needs at least a name."
                    etype)))))

;; -----------------------------------------------------------------------------

;; ## Definition Predicate

(defn definition?
  "Returns true if the given object is an abstract schema element
  definition. *For internal use*."
  [o]
  (isa? (type o) ::definition))

;; -----------------------------------------------------------------------------

;; ## Constraint Definitions

;; `Constraint` records are only used to define unspecified constraint.
;; These type of constraints are useful with the alter drop action. They
;; can be be construct using the `constraint` function.
;; *For internal use*.
(defrecord Constraint [cname]
  Buildable

  (build-definition [this db-spec]
    (ConstraintDefinition. db-spec cname)))

(defn constraint
  "Constructs an unspecified abstract constraint definition and add it
  to the given table. To be used with alter action while dropping a
  constraint."
  [table name]
  (update-in table [:constraints] conj
             [name (Constraint. name)]))

;; `UniqueConstraint` records can be constructed using the `primary-key` or
;; `unique` functions. It can represent either a unique or primary key
;; constraint. *For internal use*.
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
  to the given table. If the name isn't specified, this constraint will
  be named using its specification."
  ([table columns] (primary-key table nil columns))
  ([table name columns]
     (unique-constraint table name :primary-key columns)))

(defn unique
  "Constructs an abstract unique constraint definition and add it to the
  given table. If the name isn't specified, this constraint will
  be named using its specification."
  ([table columns] (unique table nil columns))
  ([table name columns]
     (unique-constraint table name :unique columns)))

;; `ForeignKeyConstraint` record can be constructed using the
;; `foreign-key` function. *For internal use*.
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
  to the given table. The `columns` and `parent-table` arguments must be
  specified. If no `parent-columns` are specified, the `columns` will be
  used in its place.

  The `match` optional argument can be one of `:full`, `:partial` or
  `:simple`, but note that this isn't supported by most databases.

  You can specify `triggered-actions` with pairs of keyword, the first
  of the pairs must be one of `:on-delete` or `:on-update`, while the
  second one can be one of `:cascade`, `:set-null`, `:restrict`,
  `:set-default` or `:no-action`. The actions keywords are directly
  translated to SQL keywords, so you can specify custom ones if the
  database you're using provide more.

  If the name isn't specified, this constraint will be named
  using its specification."
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

;; `CheckConstraint` record can be constructed using the
;; `check` macro or the `chech*` function. *For internal use*.
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
  given table. The `constraint-name` argument is mandatory. For the
  condition argument, see ClojureQL predicates namespace. Also a list of
  identifiers used must be provided as keywords."
  [table constraint-name condition identifiers]
  (name-required constraint-name "check constraint")
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

;; -----------------------------------------------------------------------------

;; ## Data-type Definition

;; `DataType` records can be constructed using the `data-type` function.
;; *For internal use*.
(defrecord DataType [dtype args options])

(defn data-type
  "Constructs an abstract data-type definition using the given keyword
  `dtype`. Can also take an options list of arguments (`args`) and
  `options`."
  [dtype & [args options]]
  (DataType. dtype (vec args)
             (merge {:time-zone nil
                     :collate nil
                     :encoding nil}
                     options)))

;; -----------------------------------------------------------------------------

;; ## Column Definition

(defn datetime-now-alias
  "If the given default value, it will be replaced by the standard
  function returning the current time, date or timestamp depending on
  the specified data-type. *For internal use*."
  [dtype default]
  (if (= default :now)
    (or ({:date :current_date
          :time :current_time
          :timestamp :current_timestamp} dtype) default)
    default))

;; `Column` records can be constructed using the `column` function or
;; the more specific typed column functions. The `build-definition`
;; method will create the appropriate `DataTypeExpression` for data-type
;; definitions and `ValueExpression` for default values.
;; *For internal use*.
(defrecord Column [cname data-type default auto-inc not-null others]
  Buildable
  
  (build-definition [this db-spec]
    (let [{:keys [dtype args options]} data-type]
      (ColumnDefinition.
       db-spec
       cname
       (DataTypeExpression. db-spec dtype args options)
       (if (= default :drop)
         default
         (when default
           (ValueExpression. db-spec (datetime-now-alias dtype default))))
       (when auto-inc (AutoIncClause. db-spec))
       not-null
       others))))

(defn column*
  "Constructs an abstract column definition. It'll parse the column
  specific options. See the `column` function for more details.
  *For internal use*."
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
  table. Also creates and add the appropriate column constraints when
  these are specified as options. Here's a list of available options:

   * `:unique` which construct an unique constraint on that column
   * `:primary-key` which make the current column the primary key
   * `[:refer tname & options]` which add a foreign key constraint to
     the specified table. The options are the same as the `foreign-key`
     function with the expection that you can specify only one parent
     column.
   * `:not-null` prevents this column from being null
   * `:auto-inc` (for integers types) which makes it auto-populated with
     incremented integers
   * `[:encoding enc]` (for character types) determines which encoding to
     use if supported by the database. Also see the natianal character types.
   * `[:collate type]` (for character types) determines how equality is
     handled
   * :time-zone (for time types) determines if the type includes a time-zone

It also can be used in alter modify and rename actions. In that
  case, if data-type is :to, it acts as a column rename clause and if
  data-type is :drop-default, it acts as a column drop default clause."
  {:arglists '([table column-name data-type? options])}
  [table column-name & options]
  (name-required column-name "column")
  (let [[data-type options] (optional #(instance? DataType %) options)
        reference? #(and (vector? %) (= (first %) :refer))
        [ptable pcol & others] (->> options (filter reference?) first next)
        options (filter (comp not reference?) options)
        option-set (when (seq? options) (set options))
        add-constraint #(cond (:primary-key option-set)
                              (primary-key % [column-name])
                              (:unique option-set)
                              (unique % [column-name])
                              ptable
                              (apply foreign-key % [column-name] ptable
                                     (when pcol [pcol]) others)
                              :else %)]
    (add-constraint
     (update-in table [:columns] conj
                [column-name
                 (case (first options)
                   :to (Column. column-name nil nil nil nil (second options))
                   :drop-default (Column. column-name nil :drop nil nil nil)
                   (column* column-name data-type options))]))))

;; -----------------------------------------------------------------------------

;; ## Typed Column Definitions

;; Instead of calling the `column` option directly and including the
;; data-type argument, you can use typed column definition in which case
;; each types have their own functions.

;; ### Typed Column Helpers

(defn def-typed-columns*
  "Helper for macros that create typed columns definitions. It takes a
  sequence of names and define a function for each of them, a vector of
  arguments for those functions, `dargs` must specify how to handle
  these arguement and `options` must specify the generic column options.
  The optional `docs` arguement is appended to the generic docstring.
  *For internal use*."
  [names args dargs options & [docs]]
  `(do
     ~@(for [n names]
         `(defn ~n
            ~(format (str "Constructs an abstract %s column definition and"
                          " add it to the given table." docs)
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
  "Defines typed columns for simple data-types taking no arguments.
  *For internal use*."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & options]
    '[]
    'options))

(defmacro def-numeric-like-typed-columns
  "Defines numeric-like typed columns. These typed column funcitons can
  take an optional `precision` and `scale` argument. *For internal use*."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & [precision scale & options]]
    '(-> []
         (conj-when (integer? precision) precision)
         (conj-when (integer? scale) scale))
    '(-> options
         (conj-when (not (integer? precision)) precision)
         (conj-when (not (integer? scale)) scale))
    "Takes an optional `precision` and `scale` arguments."))

(defmacro def-optional-precision-typed-columns
  "Defines typed columns with optional precision. Used by `float` and
  time data-types. *For internal use*."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & [precision & options]]
    '(conj-when [] (integer? precision) precision)
    '(conj-when options (not (integer? precision)) precision)
    "Takes an optional `precision` argument."))

(defmacro def-optional-length-typed-columns
  "Defines optionally length-bounded typed columns. Used by binary and
  character types. *For internal use*."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & [length & options]]
    '(conj-when [] (integer? length) length)
    '(conj-when options (not (integer? length)) length)
    "Takes an optional `length` argument."))

(defmacro def-length-bounded-typed-columns
  "Defines length-bounded typed columns. Used by variable binary and
  character types. *For internal use*."
  [& names]
  (def-typed-columns*
    names
    '[table column-name length & options]
    '(conj-when [] (integer? length) length)
    '(conj-when options (not (integer? length)) length)
    "The `length` arguemnt is mandatory."))

;; ### Numeric Types

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

;; ### Character Types

(def-optional-length-typed-columns
  char
  nchar
  clob
  nclob)

(defalias text clob)

(defalias ntext nclob)

(def-length-bounded-typed-columns  
  varchar
  nvarchar)

;; ### Binary Types

(def-optional-length-typed-columns
  binary
  blob)

(def-length-bounded-typed-columns
  varbinary)

;; ### Boolean Type

(def-simple-typed-columns
  boolean)

;; ### Data/time Types

(def-simple-typed-columns
  date)

(def-optional-precision-typed-columns
  time
  timestamp)

;; -----------------------------------------------------------------------------

;; ## Table Definition

;; `Table` records can be constructed using the `table*` function or
;; the `table` macro. *For internal use*.
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
  "Constructs an abstract table definition. The `table-name` is
  mandatory."
  [table-name & [columns constraints options]]
  (name-required table-name "table")
  (Table. table-name
          (or columns {})
          (or constraints {})
          (or options {})))

(defmacro table
  "Constructs an abstract table definition containing the given
  elements. Takes an arbitrary number of table elements."
  [name & elements]
  `(-> (table* ~name {} {} {}) ~@elements))

;; -----------------------------------------------------------------------------

;; ## Schema Definition

;; `Schema` records can be constructed using the `schema` function.
;; *For internal use*.
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

;; -----------------------------------------------------------------------------

;; ## Definitions Hierarchy

;; The definition hierarchy makes it easy to test if an object represent
;; an abstract schema element definition. See the `definition?`
;; predicate.
(derive Constraint           ::definition)
(derive UniqueConstraint     ::definition)
(derive ForeignKeyConstraint ::definition)
(derive CheckConstraint      ::definition)
(derive DataType             ::definition)
(derive Column               ::definition)
(derive Table                ::definition)
(derive Schema               ::definition)
