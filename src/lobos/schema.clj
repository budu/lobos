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
  (require (lobos [ast :as ast])))

;;;; Protocols

(defprotocol Buildable
  (build-definition [this db-spec]))

(defprotocol Creatable
  (build-create-statement [this db-spec]))

(defprotocol Dropable
  (build-drop-statement [this behavior db-spec]))

;;;; Constraint definition

(defrecord Constraint [cname ctype specification]
  Buildable
  
  (build-definition [this db-spec]
    (condp contains? ctype
      #{:unique :primary-key}
      (lobos.ast.UniqueConstraintDefinition.
       db-spec
       cname
       ctype
       (:columns specification)))))

(defn constraint
  "Constructs an abstract constraint definition and add it to the given
  table."
  [table constraint-name constraint-type specification]
  (update-in table [:constraints] conj
             (lobos.schema.Constraint. constraint-name
                                       constraint-type
                                       specification)))

(defn unique-constraint
  "Constructs an abstract unique (or primary-key depending on the given
  type) constraint definition and add it to the given table."
  [table constraint-type name-or-column columns]
  (let [named (contains? (-> table :columns set) name-or-column)
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

;;;; Column definition

(defrecord DataType [dtype args])

(defrecord Column [cname data-type default auto-inc not-null others]
  Buildable
  
  (build-definition [this db-spec]
    (lobos.ast.ColumnDefinition.
     db-spec
     cname
     (lobos.ast.DataTypeExpression.
      db-spec
      (:dtype data-type)
      (:args data-type))
     (when default (lobos.ast.ValueExpression. db-spec default))
     (when auto-inc (lobos.ast.AutoIncClause. db-spec))
     not-null
     others)))

(defn column
  "Constructs an abstract column definition and add it to the given
  table."
  [table column-name data-type options]
  (let [default nil
        default  (first (filter vector? options))
        others   (vec (filter string? options))
        options  (set options)
        not-null (boolean (options :not-null))
        auto-inc (boolean (options :auto-inc))]
    (when-not column-name
      (throw (IllegalArgumentException.
              "A column definition needs at least a name.")))
    (#(cond (options :primary-key) (primary-key % column-name)
            (options :unique) (unique % column-name)
            :else %)
     (update-in table [:columns] conj
                (lobos.schema.Column. column-name
                                      data-type
                                      (second default)
                                      auto-inc
                                      not-null
                                      others)))))

(defn integer
  "Constructs an abstract integer column definition and add it to the
  given table."
  [table column-name & options]
  (let [dtype (lobos.schema.DataType. :integer nil)]
    (column table column-name dtype options)))

(defn varchar
  "Constructs an abstract varchar column definition and add it to the
  given table."
  [table column-name & [limit & options]]
  (let [limit (when (integer? limit) limit)
        dtype (lobos.schema.DataType. :varchar [limit])
        options (if limit
                  options
                  (conj options limit))]
    (column table column-name dtype options)))

(defn timestamp
  "Constructs an abstract timestamp column definition and add it to the
  given table."
  [table column-name & options]
  (let [dtype (lobos.schema.DataType. :timestamp nil)]
    (column table column-name dtype options)))

;;;; Table definition

(defrecord Table [name columns constraints options]
  Creatable Dropable
  
  (build-create-statement [this db-spec]
    (lobos.ast.CreateTableStatement.
     db-spec
     name
     (map #(build-definition % db-spec)
          (concat columns constraints))))

  (build-drop-statement [this behavior db-spec]
    (lobos.ast.DropStatement. db-spec :table name behavior)))

(defn table*
  "Constructs an abstract table definition."
  [table-name columns constraints options]
  (lobos.schema.Table. table-name columns constraints options))

(defmacro table
  "Constructs an abstract table definition containing the given
  elements."
  [name & elements]
  `(-> (table* ~name [] [] {}) ~@elements))

;;;; Schema definition

(defrecord Schema [sname elements options]
  Creatable Dropable
  
  (build-create-statement [this db-spec]
    (lobos.ast.CreateSchemaStatement.
     db-spec
     sname
     (map #(build-create-statement (second %) db-spec) elements)))

  (build-drop-statement [this behavior db-spec]
    (lobos.ast.DropStatement. db-spec :schema sname behavior)))

(defn schema?
  "Returns true if the given object is a Schema."
  [o]
  (isa? (type o) lobos.schema.Schema))

(defn schema
  "Constructs an abstract schema definition."
  [schema-name & [options & elements]]
  (lobos.schema.Schema.
   schema-name
   (into (sorted-map)
         (map #(vector (:name %) %) elements))
   (or options {})))
