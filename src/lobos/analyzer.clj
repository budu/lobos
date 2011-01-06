;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.analyzer
  "Analyze a database's meta-data to contruct an abstract schema."
  (:refer-clojure :exclude [replace])
  (:require (lobos [schema :as schema]))
  (:use (clojure.contrib [def :only [defvar-]])
        (clojure [string :only [replace]])
        lobos.metadata
        lobos.utils)
  (:import (lobos.schema Column
                         UniqueConstraint
                         DataType
                         Schema
                         Table)))

;;;; Analyzer

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
  (fn [klass & args]
    [(as-keyword (.getDatabaseProductName (db-meta)))
     klass])
  :hierarchy db-hierarchy)

;;;; Default analyzer

(defmethod analyze [::standard Object]
  [_ expr]
  (when expr
    (cond (re-find #"(.*)::(.*)" expr)
          (let [[_ & [value dtype]] (first (re-seq #"(.*)::(.*)" expr))]
            (read-string (replace (str value) \' \")))
          (re-find #"(\w+)(\(\))?" expr)
          (let [[[_ func]] (re-seq #"(\w+)(\(\))?" expr)]
            (keyword func))
          :else (str expr))))


(defmethod analyze [::standard UniqueConstraint]
  [_ sname tname cname index-meta]
  (let [pkeys (primary-keys sname tname)]
    (UniqueConstraint.
     (keyword cname)
     (if (pkeys (keyword cname))
       :primary-key
       :unique)
     {:columns
      (vec (map #(-> % :column_name keyword)
                index-meta))})))

(defn constraints [sname tname]
  (map (fn [[cname meta]] (analyze UniqueConstraint sname tname cname meta))
       (indexes-meta sname tname #(-> % :non_unique not))))

(defmethod analyze [::standard DataType]
  [_ column-meta]
  (let [dtype (-> column-meta :type_name as-keyword)]
    (DataType.
     dtype
     (case dtype
       :varchar [(:column_size column-meta)]
       []))))

(defmethod analyze [::standard Column]
  [_ column-meta]
  (let [auto-inc (= (:is_autoincrement column-meta) "YES")]
    (Column. (-> column-meta :column_name keyword)
             (analyze DataType column-meta)
             (when-not auto-inc
               (analyze Object (:column_def column-meta)))
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
                               (constraints sname tname)))))

(defmethod analyze [::standard Schema]
  [_ sname]
  (apply schema/schema sname {}
         (map #(analyze Table sname %)
              (tables sname))))

(defn analyze-schema
  [sname & [connection-info]]
  (let [sname (keyword sname)]
    (with-db-meta connection-info
      (if-let [schemas (schemas)]
        (when ((set schemas) sname)
          (analyze Schema sname))
        (analyze Schema sname)))))
