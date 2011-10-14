; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.backends.sqlite
  "Compiler implementation for SQLite."
  (:refer-clojure :exclude [compile defonce])
  (:require (lobos [schema :as schema]))
  (:use (lobos analyzer compiler connectivity internal metadata utils))
  (:import (lobos.ast AlterTableStatement
                      AutoIncClause
                      CreateSchemaStatement
                      CreateTableStatement
                      DataTypeClause
                      DropStatement
                      IdentifierExpression)
           (lobos.schema DataType
                         ForeignKeyConstraint
                         Schema
                         UniqueConstraint)))

;; -----------------------------------------------------------------------------

;; ## Analyzer

(def ^{:private true} analyzer-data-type-aliases
  {:time-with-time-zone :time
   :timestamp-with-time-zone :timestamp})

(defmethod analyze [:sqlite DataType]
  [_ column-meta]
  (let [dtype (-> column-meta :type_name as-keyword)
        tz? #{:time-with-time-zone :timestamp-with-time-zone}
        [dtype options] (if (tz? dtype)
                          [dtype {:time-zone true}]
                          [dtype nil])
        dtype (first (replace analyzer-data-type-aliases [dtype]))
        args (analyze-data-type-args dtype column-meta)]
    (schema/data-type
     dtype
     (if (#{:decimal :numeric} dtype)
       [(first args)]
       args)
     options)))

(defmethod analyze [:sqlite UniqueConstraint]
  [_ sname tname cname index-meta]
  (let [columns (vec (map #(-> % :column_name keyword)
                          index-meta))]
    (UniqueConstraint.
     (make-index-name tname :unique columns)
     :unique
     columns)))

(defn- analyze-primary-keys [tname]
  (let [columns (reduce
                 #(conj %1 (-> %2 :column_name keyword))
                 []
                 (resultset-seq
                  (.getPrimaryKeys (db-meta) nil nil (name tname))))]
    (when (not-empty columns)
      [(UniqueConstraint.
        (make-index-name tname :primary-key columns)
        :primary-key
        columns)])))

(defn- analyze-foreign-keys [tname]
  (let [fks (group-by :id  (try
                             (raw-query (format "pragma foreign_key_list(%s);"
                                                (name tname)))
                             (catch Exception _)))]
    (for [fk fks]
      (let [fk (second fk)
            pcolumns (reduce #(conj %1 (-> %2 :to keyword)) [] fk)
            fcolumns (reduce #(conj %1 (-> %2 :from keyword)) [] fk)
            fk (first fk)
            ptable (keyword (:table fk))
            match (as-keyword (:match fk))
            match (when-not (= match :none) match)
            on-delete (as-keyword (:on_delete fk))
            on-delete (when-not (= on-delete :no-action) on-delete)
            on-delete (when on-delete [:on-delete on-delete])
            on-update (as-keyword (:on_update fk))
            on-update (when-not (= on-update :no-action) on-update)
            on-update (when on-delete [:on-update on-update])]
        (ForeignKeyConstraint.
         (make-index-name tname :fkey fcolumns)
         fcolumns
         ptable
         pcolumns
         match
         (into {} [on-delete on-update]))))))

(defn- analyze-unique [sname tname]
  (map (fn [[cname meta]] (analyze UniqueConstraint sname tname cname meta))
       (indexes-meta sname tname #(let [nu (:non_unique %)]
                                    (or (false? nu) (= nu 0))))))

(defmethod analyze [:sqlite :constraints]
  [_ sname tname]
  (concat (analyze-unique sname tname)
          (analyze-primary-keys tname)
          (analyze-foreign-keys tname)))

(defmethod analyze [:sqlite Schema]
  [_ sname]
  (let [db-spec (db-meta-spec)
        sname (or sname
                  (->> db-spec
                       :subname
                       (re-find #"^\./(.*)\.\w+$")
                       second))]
    (analyze [:lobos.analyzer/standard Schema] sname)))

;; -----------------------------------------------------------------------------

;; ## Compiler

(defmethod compile [:sqlite IdentifierExpression]
  [identifier]
  (as-str (:name identifier)))

(defmethod compile [:sqlite DataTypeClause]
  [expression]
  (let [{:keys [dtype args options]} expression
        {:keys [collate time-zone]} options]
    (unsupported (and (#{:decimal :numeric} dtype) (= (count args) 2))
      "Doesn't support scale argument.")
    (join \space
      (str (as-sql-keyword dtype) (as-list args))
      (when collate (str "COLLATE " (as-str collate)))
      (when time-zone "WITH TIME ZONE"))))

(defmethod compile [:sqlite AutoIncClause]
  [_]
  nil)

(defmethod compile [:sqlite CreateSchemaStatement]
  [statement]
  (let [{:keys [db-spec sname elements]} statement]
    (map compile elements)))

(defn- drop-schema [db-spec]
  (map (comp compile
             #(schema/build-drop-statement % :cascade db-spec)
             #(schema/table %)
             :name)
       (with-connection db-spec
         (raw-query
          (format "select name from sqlite_master where type <> 'index';")))))

(defmethod compile [:sqlite DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (if (= otype :schema)
      (drop-schema db-spec)
      (join \space
        "DROP"
        (as-sql-keyword otype)
        (as-identifier db-spec oname)))))

(defmethod compile [:sqlite AlterTableStatement]
  [statement]
  (unsupported "Alter statement unsupported."))
