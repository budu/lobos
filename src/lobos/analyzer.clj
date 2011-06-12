; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.analyzer
  "Analyze a database's meta-data to contruct an abstract schema."
  (:refer-clojure :exclude [defonce replace])
  (:require (lobos [connectivity :as conn]
                   [schema :as schema]))
  (:use (clojure.contrib [def :only [defvar-]])
        (clojure [string :only [replace]])
        lobos.internal
        lobos.metadata
        lobos.utils)
  (:import (java.sql DatabaseMetaData)
           (lobos.schema Column
                         DataType
                         Expression
                         ForeignKeyConstraint
                         Index
                         Schema
                         Table
                         UniqueConstraint)))

;; -----------------------------------------------------------------------------

;; ## Analyzer

(def db-hierarchy
  (atom (-> (make-hierarchy)
            (derive :h2 ::standard)
            (derive :mysql ::standard)
            (derive :postgresql ::standard)
            (derive :sqlite ::standard)
            (derive :microsoft-sql-server ::standard))))

(defmulti analyze
  "Analyzes the specified part of a schema and returns its abstract
  equivalent."
  (fn [dispatch-val & args]
    (if (vector? dispatch-val)
      dispatch-val
      [(as-keyword (.getDatabaseProductName (db-meta)))
       dispatch-val]))
  :hierarchy db-hierarchy)

;; -----------------------------------------------------------------------------

;; ## Default Analyzer

(defmethod analyze [::standard Expression]
  [_ expr]
  (when expr
    (Expression.
     (cond (re-find #"^(.*)::(.*)$" expr)
           (let [[_ & [value dtype]] (first (re-seq #"(.*)::(.*)" expr))]
             (read-string (replace (str value) \' \"))) ;; HACK: to replace!
           (re-find #"^\d+$" expr) (Integer/parseInt expr)
           (re-find #"^(\w+)(\(\))?$" expr)
           (let [[[_ func]] (re-seq #"(\w+)(\(\))?" expr)]
             (keyword func))
           :else (str expr)))))

(defmethod analyze [::standard UniqueConstraint]
  [_ sname tname cname index-meta]
  (let [pkeys (primary-keys sname tname)]
    (UniqueConstraint.
     (keyword cname)
     (if (pkeys (keyword cname))
       :primary-key
       :unique)
     (vec (map #(-> % :column_name keyword)
               index-meta)))))

(def action-rules
  {DatabaseMetaData/importedKeyCascade    :cascade
   DatabaseMetaData/importedKeySetNull    :set-null
   DatabaseMetaData/importedKeyRestrict   :restrict
   DatabaseMetaData/importedKeySetDefault :set-default})

(defmethod analyze [::standard ForeignKeyConstraint]
  [_ cname ref-meta]
  (let [pcolumns (vec (map #(-> % :pkcolumn_name keyword)
                           ref-meta))
        fcolumns (vec (map #(-> % :fkcolumn_name keyword)
                           ref-meta))
        ptable (-> ref-meta first :pktable_name keyword)
        on-delete (-> ref-meta first :delete_rule action-rules)
        on-delete (when on-delete [:on-delete on-delete])
        on-update (-> ref-meta first :update_rule action-rules)
        on-update (when on-delete [:on-update on-update])]
    (ForeignKeyConstraint.
     (keyword cname)
     fcolumns
     ptable
     pcolumns
     nil
     (into {} [on-delete on-update]))))

(defmethod analyze [::standard :constraints]
  [_ sname tname]
  (concat
   (map (fn [[cname meta]] (analyze UniqueConstraint sname tname cname meta))
        (indexes-meta sname tname #(let [nu (:non_unique %)]
                                     (or (false? nu) (= nu 0)))))
   (map (fn [[cname meta]] (analyze ForeignKeyConstraint cname meta))
        (references-meta sname tname))))

(defmethod analyze [::standard Index]
  [_ sname tname iname index-meta]
  (let [pkeys (primary-keys sname tname)]
    (Index.
     (keyword iname)
     tname
     (vec (map #(-> % :column_name keyword)
               index-meta))
     (when (-> index-meta first :non_unique not)
       (list :unique)))))

(defmethod analyze [::standard :indexes]
  [_ sname tname]
  (map (fn [[iname meta]] (analyze Index sname tname iname meta))
       (indexes-meta sname tname)))

(defn analyze-data-type-args
  "Returns a vector containing the data type arguments for the given
  column meta data."
  [dtype column-meta]
  (condp contains? dtype
    #{:nvarchar :varbinary :varchar} [(:column_size column-meta)]
    #{:binary :blob :char :clob :nchar :nclob
      :float :time :timestamp} [(:column_size column-meta)]
    #{:decimal :numeric} (let [scale (:decimal_digits column-meta)]
                           (if (= scale 0)
                           [(:column_size column-meta)]
                           [(:column_size column-meta) scale]))
    []))

(defmethod analyze [::standard DataType]
  [_ column-meta]
  (let [dtype (-> column-meta :type_name as-keyword)]
    (schema/data-type
     dtype
     (analyze-data-type-args dtype column-meta))))

(defmethod analyze [::standard Column]
  [_ column-meta]
  (let [auto-inc (= (:is_autoincrement column-meta) "YES")]
    (Column. (-> column-meta :column_name keyword)
             (analyze DataType column-meta)
             (when-not auto-inc
               (analyze Expression (:column_def column-meta)))
             auto-inc
             (= (:is_nullable column-meta) "NO")
             [])))

(defmethod analyze [::standard Table]
  [_ sname tname]
  (schema/table* tname
                 (into {} (map #(let [c (analyze Column %)]
                                  [(:cname c) c])
                               (columns-meta sname tname)))
                 (into {} (map #(vector (:cname %) %)
                               (analyze :constraints sname tname)))
                 (into {} (map #(vector (:iname %) %)
                               (analyze :indexes sname tname)))))

(defmethod analyze [::standard Schema]
  [_ sname]
  (apply schema/schema sname {:db-spec (db-meta-spec)}
         (map #(analyze Table sname %)
              (tables sname))))

(defn analyze-schema
  [& args]
  {:arglists '([connection-info? sname?])}
  (let [[db-spec sname _] (optional-cnx-and-sname args)]
    (with-db-meta db-spec
      (let [sname (or (keyword sname)
                      (default-schema)
                      (first _))]
        (if-let [schemas (schemas)]
          (when (or (nil? sname)
                    ((set schemas) sname))
            (analyze Schema sname))
          (analyze Schema sname))))))
