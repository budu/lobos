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
  (:require (lobos [ast :as ast]))
  (:use (clojure [walk   :only [postwalk]]
                 [set    :only [union]]
                 [string :only [replace]])
        lobos.utils))

(ast/import-all)

;; -----------------------------------------------------------------------------

;; ## Protocols

(defprotocol Alterable
  "The Alterable protocol add the possibility of building alter
  statements from an object implementing it. *For internal use*."
  (build-alter-statement [this action db-spec]))

(defprotocol Buildable
  "The Buildable protocol is currently used only by table elements.
  *For internal use*."
  (build-definition [this db-spec]))

(defprotocol Creatable
  "The Creatable protocol add the possibility of building create
  statements from an object implementing it. *For internal use*."
  (build-create-statement [this db-spec]))

(defprotocol Dropable
  "The Dropable protocol add the possibility of building drop
  statements from an object implementing it. *For internal use*."
  (build-drop-statement [this behavior db-spec]))

;; -----------------------------------------------------------------------------

;; ## Common Exception

(defn name-required
  "Throws an IllegalArgumentException when the given name is nil with a
  default message using the given type of elements."
  [name etype]
  (when-not name
    (throw (IllegalArgumentException.
            (format "A %s definition needs at least a name."
                    etype)))))

;; -----------------------------------------------------------------------------

;; ## Definition Predicate

(defn definition?
  "Returns true if the given object is an abstract schema element
  definition. *For internal use*."
  [o]
  (isa? (type o) ::definition))

;; -----------------------------------------------------------------------------

;; ## Expression Definitions

(def ^{:doc "A set of symbol representing SQL infix operators."}
  sql-infix-operators
  '#{;; math operators
     + - * /
     ;; boolean operators
     < > <= >= = != or and in like})

(def ^{:doc "A set of symbol representing SQL prefix operators."}
  sql-prefix-operators
  '#{not})

(def ^{:doc "A set of symbol representing SQL functions."}
  sql-functions
  '#{;; string functions
     length lower position replace str subs trim upper
     ;; numeric functions
     abs ceil floor mod
     ;; datetime functions
     extract now current_date current_time current_timestamp})

(def sql-symbols
  (union sql-infix-operators
         sql-prefix-operators
         sql-functions))

(defrecord Expression [value]
  Buildable

  (build-definition [this db-spec]
    (postwalk
     #(do
        (cond (vector? %)
              (let [[f & n] %]
                (if (keyword? f)
                  (condp contains? (-> f name symbol)
                    sql-infix-operators
                    (OperatorExpression. db-spec f (first n) (next n))
                    sql-prefix-operators
                    (OperatorExpression. db-spec f nil n)
                    sql-functions
                    (FunctionExpression. db-spec f n))
                  %))
              (and (keyword? %)
                   (not (contains? sql-symbols (-> % name symbol))))
              (IdentifierExpression. db-spec % nil)
              (not (keyword? %))
              (ScalarExpression. db-spec %)
              :else %))
     value)))

(defn expression?
  "Returns true if the given object is an Expression."
  [o]
  (instance? Expression o))

(defmacro expression [form]
  `(Expression.
    ~(postwalk
      #(if (and (seq? %)
                (sql-symbols (first %)))
         (apply vector
                (keyword (first %))
                (rest %))
         %)
      form)))

;; -----------------------------------------------------------------------------

;; ## Index Definitions

(defrecord Index [iname tname columns options]
  Creatable Dropable

  (build-create-statement [this db-spec]
    (CreateIndexStatement. db-spec iname tname columns options))

  (build-drop-statement [this behavior db-spec]
    (DropStatement. db-spec :index iname nil {:tname tname})))

(defn index?
  "Returns true if the given object is an Index."
  [o]
  (instance? Index o))

(defn index
  "Constructs an index on the specified table and columns. Can take an
  optional index name and a set of options of which only :unique is
  available for now. This can be used inside or outside a table
  definition, in the later case you just have to provide the name of the
  table as a keyword."
  {:arglists '([table name? columns & options])}
  [table & args]
  (let [tname (if (keyword? table) table (:name table))
        [name args] (optional keyword? args)
        [columns & options] args
        name (or name (make-index-name tname
                                       (or ((set options) :unique)
                                           :index)
                                       columns))]
       (if (keyword? table)
         (Index. name tname columns options)
         (update-in table [:indexes] conj
                    [name (Index. name tname columns options)]))))

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

(defn constraint?
  "Returns true if the given object is a Constraint."
  [o]
  (instance? Constraint o))

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
  [table name type column-names]
  (let [name (or name (make-index-name table type column-names))]
    (update-in table [:constraints] conj
               [name (UniqueConstraint. name type (vec column-names))])))

(defn primary-key
  "Constructs an abstract primary key constraint definition and add it
  to the given table. If the name isn't specified, this constraint will
  be named using its specification."
  ([table column-names] (primary-key table nil column-names))
  ([table name column-names]
     (unique-constraint table name :primary-key column-names)))

(defn unique
  "Constructs an abstract unique constraint definition and add it to the
  given table. If the name isn't specified, this constraint will
  be named using its specification."
  ([table column-names] (unique table nil column-names))
  ([table name column-names]
     (unique-constraint table name :unique column-names)))

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
  {:arglists '([table name? column-names parent-table
                parent-column-names? match? & triggered-actions])}
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
                                   (make-index-name table "fkey" columns))]
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
  [cname condition]
  Buildable

  (build-definition [this db-spec]
    (CheckConstraintDefinition.
     db-spec
     cname
     (build-definition condition db-spec))))

(defn check*
  "Constructs an abstract check constraint definition and add it to the
  given table. The `constraint-name` argument is mandatory."
  [table constraint-name condition]
  (name-required constraint-name "check constraint")
  (update-in table [:constraints] conj
             [constraint-name
              (CheckConstraint. constraint-name
                                condition)]))

(defmacro check
  "Constructs an abstract check constraint definition and add it to the
  given table."
  [table constraint-name condition]
  `(check*
    ~table
    ~constraint-name
    (expression ~condition)))

;; -----------------------------------------------------------------------------

;; ## Data-type Definition

;; `DataType` records can be constructed using the `data-type` function.
;; *For internal use*.
(defrecord DataType [name args options])

(defn data-type?
  "Returns true if the given object is a DataType."
  [o]
  (instance? DataType o))

(defn data-type
  "Constructs an abstract data-type definition using the given keyword
  `name`. Can also take an options list of arguments (`args`) and
  `options`."
  [name & [args options]]
  (check-valid-options options :encoding :collate :time-zone)
  (update-options
   (DataType. name (vec args) {})
   options))

;; -----------------------------------------------------------------------------

;; ## Column Definition

(defn datetime-now-alias
  "If the given default value, it will be replaced by the standard
  function returning the current time, date or timestamp depending on
  the specified data-type. *For internal use*."
  [name default]
  (let [value (:value default)]
    (if (= value [:now])
      (Expression.
       (or ({:date [:current_date]
             :time [:current_time]
             :timestamp [:current_timestamp]} name) value))
      default)))

;; `Column` records can be constructed using the `column` function or
;; the more specific typed column functions. The `build-definition`
;; method will create the appropriate `DataTypeClause` for data-type
;; definitions and `*Expression` AST for default values.
;; *For internal use*.
(defrecord Column [cname data-type default auto-inc not-null others]
  Buildable

  (build-definition [this db-spec]
    (let [{:keys [name args options]} data-type]
      (ColumnDefinition.
       db-spec
       cname
       (when data-type
         (DataTypeClause. db-spec name args options))
       (if (= default :drop)
         :drop
         (when default
           (build-definition
            (datetime-now-alias name default)
            db-spec)))
       (when auto-inc (AutoIncClause. db-spec))
       not-null
       others))))

(defn column?
  "Returns true if the given object is a Column."
  [o]
  (instance? Column o))

(defmacro default [form]
  `[:default (expression ~form)])

(defn column*
  "Constructs an abstract column definition. It'll parse the column
  specific options. See the `column` function for more details.
  *For internal use*."
  [column-name data-type options]
  (let [[map-entries others] (separate vector? (filter identity options))
        [kw-options  others] (separate keyword? others)
        {:keys [default encoding collate] :as option-map}
        ;; HACK: trying to get refer option, but it's not a map entry and
        ;; it's actually consumed by `column`, will clean up later.
        (into {} (map (fn [[f & r]] [f (first r)]) map-entries))
        option-set (set kw-options)
        data-type (update-options data-type
                                  (assoc (select-keys option-map
                                                      [:encoding
                                                       :collate])
                                    :time-zone (option-set :time-zone)))
        not-null   (when (:null option-set) false)
        not-null   (if (and (nil? not-null) (:not-null option-set)) true not-null)
        auto-inc   (when (:auto-inc option-set) true)]
    (name-required column-name "column")
    (check-valid-options (into option-set (keys option-map))
                         :null :not-null :auto-inc :default :primary-key :unique
                         :encoding :collate :time-zone :refer)
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
     function with the exception that you can specify only one parent
     column.
   * `:null` allow null values
   * `:not-null` prevents this column from being null
   * `:auto-inc` (for integers types) which makes it auto-populated with
     incremented integers
   * `[:encoding enc]` (for character types) determines which encoding to
     use if supported by the database. Also see the natianal character types.
   * `[:collate type]` (for character types) determines how equality is
     handled
   * `:time-zone` (for time types) determines if the type includes a time-zone

  It also can be used in alter modify and rename actions. In that
  case, if data-type is :to, it acts as a column rename clause and if
  data-type is :drop-default, it acts as a column drop default clause."
  {:arglists '([table column-name data-type? & options])}
  [table column-name & options]
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
                   :drop-default (Column. column-name nil :drop nil nil [])
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
    " Takes an optional `precision` and `scale` arguments."))

(defmacro def-optional-precision-typed-columns
  "Defines typed columns with optional precision. Used by `float` and
  time data-types. *For internal use*."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & [precision & options]]
    '(conj-when [] (integer? precision) precision)
    '(conj-when options (not (integer? precision)) precision)
    " Takes an optional `precision` argument."))

(defmacro def-optional-length-typed-columns
  "Defines optionally length-bounded typed columns. Used by binary and
  character types. *For internal use*."
  [& names]
  (def-typed-columns*
    names
    '[table column-name & [length & options]]
    '(conj-when [] (integer? length) length)
    '(conj-when options (not (integer? length)) length)
    " Takes an optional `length` argument."))

(defmacro def-length-bounded-typed-columns
  "Defines length-bounded typed columns. Used by variable binary and
  character types. *For internal use*."
  [& names]
  (def-typed-columns*
    names
    '[table column-name length & options]
    '(conj-when [] (integer? length) length)
    '(conj-when options (not (integer? length)) length)
    " The `length` arguemnt is mandatory."))

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

(def double double-precision)

(def-optional-precision-typed-columns
  float)

;; ### Character Types

(def-optional-length-typed-columns
  char
  nchar
  clob
  nclob)

(def text clob)

(def ntext nclob)

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

(defn- build-table-elements [db-spec method & elements]
  (->> (apply concat elements)
       (map #(when (second %)
               (method (second %) db-spec)))
       (filter identity)))

;; `Table` records can be constructed using the `table*` function or
;; the `table` macro. *For internal use*.
(defrecord Table [name columns constraints indexes]
  Alterable Creatable Dropable

  (build-alter-statement [this action db-spec]
    (let [elements (build-table-elements db-spec
                                         build-definition
                                         columns
                                         constraints)]
      (for [element elements]
        (AlterTableStatement.
         db-spec
         name
         action
         element))))

  (build-create-statement [this db-spec]
    (conj
     (build-table-elements db-spec build-create-statement indexes)
     (CreateTableStatement.
      db-spec
      name
      (build-table-elements db-spec build-definition columns constraints))))

  (build-drop-statement [this behavior db-spec]
    (DropStatement. db-spec :table name behavior nil)))

(defn table?
  "Returns true if the given object is a Table."
  [o]
  (instance? Table o))

(defn table*
  "Constructs an abstract table definition. The `table-name` is
  mandatory."
  [name & [columns constraints indexes]]
  (name-required name "table")
  (Table. name
          (or columns {})
          (or constraints {})
          (or indexes {})))

(defmacro table
  "Constructs an abstract table definition containing the given
  elements."
  [name & elements]
  `(-> (table* ~name) ~@(reverse elements)))

;; -----------------------------------------------------------------------------

;; ## Schema Definition

;; `Schema` records can be constructed using the `schema` function.
;; *For internal use*.
(defrecord Schema [sname tables indexes options]
  Creatable Dropable

  (build-create-statement [this db-spec]
    (CreateSchemaStatement.
     db-spec
     sname
     (flatten
      (map #(build-create-statement (second %) db-spec)
           (concat tables indexes)))))

  (build-drop-statement [this behavior db-spec]
    (DropStatement. db-spec :schema sname behavior nil)))

(defn schema?
  "Returns true if the given object is a Schema."
  [o]
  (instance? Schema o))

(defn- filtered-elements->map
  [pred elements]
  (into (sorted-map)
        (map #(vector (:name %) %)
             (filter pred elements))))

(defn schema
  "Constructs an abstract schema definition."
  {:arglists '([schema-name options? & elements])}
  [schema-name & args]
  (name-required schema-name "schema")
  (let [[options elements] (optional (comp not definition?) args)]
    (Schema.
     schema-name
     (filtered-elements->map table? elements)
     (filtered-elements->map index? elements)
     (or options {}))))

;; -----------------------------------------------------------------------------

;; ## Definitions Hierarchy

;; The definition hierarchy makes it easy to test if an object represent
;; an abstract schema element definition. See the `definition?`
;; predicate.
(derive Index                ::definition)
(derive Constraint           ::definition)
(derive UniqueConstraint     ::definition)
(derive ForeignKeyConstraint ::definition)
(derive CheckConstraint      ::definition)
(derive DataType             ::definition)
(derive Column               ::definition)
(derive Table                ::definition)
(derive Schema               ::definition)
