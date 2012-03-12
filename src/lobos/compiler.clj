; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.compiler
  "This namespace include the `compile` multimethod, a default
  implementation based on the SQL standard and some helpers
  functions. The compiler works on an AST defined in the `lobos.ast`
  namespace. Database specific implementations can be found in the
  `backends` directory."
  (:refer-clojure :exclude [compile defonce replace])
  (:require (lobos [ast :as ast]))
  (:use (clojure [string :only [replace]])
        lobos.utils)
  (:import (java.lang UnsupportedOperationException)))

(ast/import-all)

;; -----------------------------------------------------------------------------

;; ## Compiler

;; When no implementation specific method is found, fall back on the
;; default standard compiler.
(def backends-hierarchy
  (atom (-> (make-hierarchy)
            (derive :h2 ::standard)
            (derive :mysql ::standard)
            (derive :postgresql ::standard)
            (derive :sqlite ::standard)
            (derive :sqlserver ::standard))))

(defmulti compile
  "Compiles the given object into SQL."
  (fn [o]
    [(-> o :db-spec :subprotocol (or ::standard) keyword)
     (type o)])
  :hierarchy backends-hierarchy)

;; -----------------------------------------------------------------------------

;; ## Helpers

(defn as-identifier
  "Constructs an IdentifierExpression AST form and compiles it. Takes an
  optional `level` argument to determine which qualification level to
  output, only support `:schema` for now."
  [db-spec name & qualifiers]
  (compile (IdentifierExpression. db-spec name qualifiers)))

(defn unsupported
  "Throws an UnsupportedOperationException using the given message. Can
  be given a condition, in which case it throw an exception only if it's
  true."
  ([msg] (unsupported true msg))
  ([cond msg]
     (when cond
       (throw (UnsupportedOperationException. (str msg))))))

(defn mode
  "Constructs a Mode AST form with the specified `db-spec`."
  [db-spec]
  (Mode. db-spec))

(defn- extract-foreign-keys*
  "Used by the `extract-foreign-keys` function to extract foreign key
  constraints from the given `table`."
  [table]
  (let [fkey? #(instance? ForeignKeyConstraintDefinition %)
        foreign-keys (vector (:tname table)
                             (->> (:elements table)
                                  (filter fkey?)))
        table (update-in table [:elements]
               (fn [es] (filter #(not (fkey? %)) es)))]
    [table foreign-keys]))

(defn extract-foreign-keys
  "Given a collection of AST statements, extract all foreign key
  constraints for all tables and returns a vector composed of the
  statements (without the foreign keys) and a map of the foreign keys
  grouped by table name."
  [elements]
  (let [tables (filter #(instance? CreateTableStatement %) elements)
        others (filter #(not (instance? CreateTableStatement %)) elements)
        results (map extract-foreign-keys* tables)
        tables (map first results)
        foreign-keys (apply hash-map (apply concat (map second results)))]
    [(concat tables others)
     foreign-keys]))

(defn build-alter-add-statements
  "Helper used when compiling a create schema statement that require the
  foreign keys to be extracted and added individually using alter add
  statements."
  [db-spec m]
  (for [[tname elements] m element elements]
    (AlterTableStatement. db-spec tname :add element)))

;; -----------------------------------------------------------------------------

;; ## Default compiler

;; The default compiler is based on the SQL standard and does not prefer
;; any particular implementation.

;; Compiling `Mode` instance returns nil by default.
(defmethod compile [::standard Mode] [_] nil)

;; ### Expressions

;; Keywords will be made into SQL keywords using the `as-sql-keyword`
;; function while strings will be properly delimited by single quotes.
(defmethod compile [::standard ScalarExpression]
  [expression]
  (let [{:keys [scalar]} expression]
    (cond (keyword? scalar) (str (as-sql-keyword scalar))
          (string? scalar) (str "'" scalar "'")
          :else scalar)))

(defmethod compile [::standard IdentifierExpression]
  [identifier]
  (let [{:keys [db-spec name qualifiers]} identifier]
    (join* \. (->> (concat qualifiers [name])
                   (filter identity)
                   (map #(when % (as-str \" % \")))))))

(defmethod compile [::standard FunctionExpression]
  [function]
  (let [{:keys [db-spec name args]} function]
    (str (as-sql-keyword name)
         (as-list (map compile args)))))

(defmethod compile [::standard OperatorExpression]
  [operator]
  (let [{:keys [db-spec op left right]} operator]
    (if (vector? (first right))
      (str "("
           (compile left) " "
           (as-sql-keyword op) " "
           (as-list (map compile (first right)))
           ")")
      (str "(" (apply join
                      (str " " (as-sql-keyword op) " ")
                      (map compile (conj right left))) ")"))))

;; `DataTypeClause` instances are compiled with their `dtype`
;; property made into SQL keywords using the `as-sql-keyword`
;; function. If the data-type has an argument list, it will be passed
;; through the `as-list` function.
(defmethod compile [::standard DataTypeClause]
  [expression]
  (let [{:keys [dtype args options]} expression
        {:keys [encoding collate time-zone]} options]
    (join \space
      (str (as-sql-keyword dtype) (as-list args))
      (when encoding ["CHARACTER SET" (as-str encoding)])
      (when collate ["COLLATE" (as-str collate)])
      (when time-zone ["WITH TIME ZONE"]))))

;; ### Clauses

;; Standard `AutoIncClause` are using the `ALWAYS` variant.
(defmethod compile [::standard AutoIncClause]
  [_]
  "GENERATED ALWAYS AS IDENTITY")

;; ### Definitions

(defn not-null-option [not-null]
  (when-not (nil? not-null)
    (if not-null "NOT NULL" "NULL")))

;; `ColumnDefinition` instance will get their names made into SQL
;; identifiers using the `as-identifier` function. It's data-type will
;; get compiled with the appropriate method. The options will be added
;; in this order: default clause, auto-inc option and the not-null
;; constraint. Strings found in the others property will be added as
;; they are.
(defmethod compile [::standard ColumnDefinition]
  [definition]
  (let [{:keys [db-spec cname data-type default
                auto-inc not-null others]} definition]
    (apply join \space
      (as-identifier db-spec cname)
      (compile data-type)
      (when default  (str "DEFAULT " (compile default)))
      (when auto-inc (compile auto-inc))
      (not-null-option not-null)
      others)))

;; `UniqueConstraintDefinition` instances will get their names made into
;; SQL identifiers using the `as-identifier` function. The `ctype`
;; property must one of `:primary-key` or `:unique`.
(defmethod compile [::standard UniqueConstraintDefinition]
  [definition]
  (let [{:keys [db-spec cname ctype columns]} definition]
    (join \space
      "CONSTRAINT"
      (as-identifier db-spec cname)
      (as-sql-keyword ctype)
      (as-list (map (partial as-identifier db-spec) columns)))))

;; `ForeignKeyConstraintDefinition` instances are always compiled with
;; their full references clause. The options will be added in this
;; order: the match clause, the on-delete and the on-update triggered
;; actions.
(defmethod compile [::standard ForeignKeyConstraintDefinition]
  [definition]
  (let [{:keys [db-spec cname columns parent-table parent-columns match
                triggered-actions]} definition]
    (join \space
      "CONSTRAINT"
      (as-identifier db-spec cname)
      "FOREIGN KEY"
      (as-list (map (partial as-identifier db-spec) columns))
      "REFERENCES"
      (as-identifier db-spec parent-table (:schema db-spec))
      (as-list (map (partial as-identifier db-spec) parent-columns))
      (when match (str "MATCH " (as-sql-keyword match)))
      (when (contains? triggered-actions :on-delete)
        (str "ON DELETE " (as-sql-keyword (:on-delete triggered-actions))))
      (when (contains? triggered-actions :on-update)
        (str "ON UPDATE " (as-sql-keyword (:on-update triggered-actions)))))))

(defmethod compile [::standard CheckConstraintDefinition]
  [definition]
  (let [{:keys [db-spec cname condition]} definition]
    (join \space
      "CONSTRAINT"
      (as-identifier db-spec cname)
      "CHECK"
      (compile condition))))

;; ### Create and Drop Statements

;; The default create statement will extract all foreign key constraint
;; from its create table statements to be added afterward using alter
;; add statements.
(defmethod compile [::standard CreateSchemaStatement]
  [statement]
  (let [{:keys [db-spec sname elements]} statement
        [elements foreign-keys] (extract-foreign-keys elements)
        alters (map compile (build-alter-add-statements
                             (assoc db-spec :schema sname)
                             foreign-keys))]
    (conj alters
          (str "CREATE SCHEMA "
               (apply join "\n" (conj (map compile elements)
                                      (as-identifier db-spec sname)))))))

;; `CreateTableStatement` instances will properly compile all their
;; elements.
(defmethod compile [::standard CreateTableStatement]
  [statement]
  (let [{:keys [db-spec tname elements]} statement]
    (format "CREATE TABLE %s %s"
            (as-identifier db-spec tname (:schema db-spec))
            (or (as-list (map compile elements))
                "()"))))

(defmethod compile [::standard CreateIndexStatement]
  [statement]
  (let [{:keys [db-spec iname tname columns options]} statement
        index-column #(if (keyword? %)
                        (as-identifier db-spec %)
                        (let [col (first %)
                              options (set (rest %))]
                          (apply join \space
                            (as-identifier db-spec (first %))
                            (map as-sql-keyword options))))]
    (format "CREATE %sINDEX %s ON %s %s"
            (str (when ((set options) :unique) "UNIQUE "))
            (as-identifier db-spec iname)
            (as-identifier db-spec tname (:schema db-spec))
            (as-list (map index-column columns)))))

(defmethod compile [::standard DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (join \space
      "DROP"
      (as-sql-keyword otype)
      (as-identifier db-spec oname (:schema db-spec))
      (as-sql-keyword behavior))))

;; ### Alter Statement and Actions

(defmethod compile [::standard AlterAddAction]
  [action]
  (let [{:keys [db-spec element]} action
        element (assoc element :db-spec db-spec)]
    (join \space
          "ADD"
          (compile element))))

(defmethod compile [::standard AlterDropAction]
  [action]
  (let [{:keys [db-spec element]} action
        is-column (instance? ColumnDefinition element)]
    (join \space
          "DROP"
          (if is-column
            "COLUMN"
            "CONSTRAINT")
          (as-identifier db-spec (:cname element)))))

(defmethod compile [::standard AlterModifyDataTypeAndOptionsAction]
  [action]
  (let [{:keys [db-spec element]} action]
    (str "ALTER COLUMN " (compile element))))

(defmethod compile [::standard AlterModifyDefaultAction]
  [action]
  (let [{:keys [db-spec element]} action
        default (:default element)]
    (join \space
          "ALTER COLUMN"
          (as-identifier db-spec (:cname element))
          (if (= default :drop)
            "DROP DEFAULT"
            (str "SET DEFAULT " (compile default))))))

(defmethod compile [::standard AlterModifyAction]
  [action]
  (let [{:keys [db-spec element]} action
        {:keys [data-type default]} element]
    (cond
     data-type (compile (AlterModifyDataTypeAndOptionsAction. db-spec element))
     default   (compile (AlterModifyDefaultAction. db-spec element))
     :else     (unsupported
                "Only change data type or set/drop default supported."))))

;; `AlterRenameAction` instances aren't supported by the default
;; compiler.
(defmethod compile [::standard AlterRenameAction]
  [action]
  (let [{:keys [db-spec element]} action]
     (unsupported "Rename action not supported.")))

;; `AlterTableStatement` instances will get dispatched further into one
;; of the action supported: `AlterAddAction`, `AlterDropAction`,
;; `AlterModifyAction` or `AlterRenameAction`.
(defmethod compile [::standard AlterTableStatement]
  [statement]
  (let [{:keys [db-spec tname action element]} statement
        element (assoc element :sname (:schema db-spec) :tname tname)]
    (join \space
          "ALTER TABLE"
          (as-identifier db-spec tname (:schema db-spec))
          (case action
            :add    (compile (AlterAddAction. db-spec element))
            :drop   (compile (AlterDropAction. db-spec element))
            :modify (compile (AlterModifyAction. db-spec element))
            :rename (compile (AlterRenameAction. db-spec element))))))
