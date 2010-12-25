;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.schema
  (:use [lobos.schema] :reload)
  (:use [clojure.test]))

;;;; Constraint definition tests

(deftest test-constraint
  (is (= (constraint {} :foo :bar :baz)
         {:constraints (list (lobos.schema.Constraint. :foo :bar :baz))})
      "Constraint definition"))

(deftest test-unique-constraint
  (is (= (unique-constraint {} :foo :bar (list :baz))
         {:constraints (list (lobos.schema.Constraint.
                              nil
                              :foo
                              {:columns [:bar :baz]}))})
      "Unnamed unique constraint definition")
  (is (= (unique-constraint {:columns [:bar]} :foo :bar (list :baz))
         {:columns [:bar]
          :constraints (list (lobos.schema.Constraint.
                              :bar
                              :foo
                              {:columns [:baz]}))})
      "Named unique constraint definition"))

(deftest test-primary-key
  (is (= (primary-key {} :foo :bar :baz)
         {:constraints (list (lobos.schema.Constraint.
                              nil
                              :primary-key
                              {:columns [:foo :bar :baz]}))})
      "Primary key constraint definition"))

(deftest test-unique
  (is (= (unique {} :foo :bar :baz)
         {:constraints (list (lobos.schema.Constraint.
                              nil
                              :unique
                              {:columns [:foo :bar :baz]}))})
      "Unique constraint definition"))

;;;; Column definition tests

(def column-definition-stub
     (lobos.schema.Column. :foo nil nil false false []))

(deftest test-column
  (testing "Column definition"
    (is (thrown? IllegalArgumentException
                 (column {} nil nil nil))
        "Invalid column definition")
    (is (= (column {} :foo nil nil)
           {:columns (list column-definition-stub)})
        "Column with name and data-type")
    (is (= (column {} :foo nil [:auto-inc])
           {:columns (list (assoc column-definition-stub
                             :auto-inc true))})
        "Setting column as auto incremented")
    (is (= (column {} :foo nil [:not-null])
           {:columns (list (assoc column-definition-stub
                             :not-null true))})
        "Setting column as not null")
    (is (= (column {} :foo nil [[:default 0]])
           {:columns (list (assoc column-definition-stub
                             :default 0))})
        "Setting default value")
    (is (= (column {} :foo nil ["BAR"])
           {:columns (list (assoc column-definition-stub
                             :others ["BAR"]))})
        "Setting custom options")))

(deftest test-data-types
  (testing "Data type definition"
    (is (= (integer {} :foo)
           {:columns (list (assoc column-definition-stub
                             :data-type (lobos.schema.DataType.
                                         :integer
                                         nil)))})
        "Integer")
    (is (= (varchar {} :foo)
           {:columns (list (assoc column-definition-stub
                             :data-type (lobos.schema.DataType.
                                         :varchar
                                         nil)))})
        "Varchar")
    (is (= (varchar {} :foo 1)
           {:columns (list (assoc column-definition-stub
                             :data-type (lobos.schema.DataType.
                                         :varchar
                                         [1])))})
        "Varchar with limit")
    (is (= (timestamp {} :foo)
           {:columns (list (assoc column-definition-stub
                             :data-type (lobos.schema.DataType.
                                         :timestamp
                                         nil)))})
        "Timestamp")))

;;;; Table definition tests

(deftest test-table*
  (is (thrown? IllegalArgumentException
               (table* nil nil nil nil))
      "Invalid table definition")
  (is (= (table* :foo nil nil nil)
         (lobos.schema.Table. :foo nil nil nil))
      "Table definition"))

(deftest test-table
  (is (= (table :foo)
         (lobos.schema.Table. :foo [] [] {}))
      "Empty table")
  (is (= (table :foo (column :foo nil nil))
         (lobos.schema.Table. :foo [column-definition-stub] [] {}))
      "Table with column")
  (is (= (table :foo (constraint :foo :bar :baz))
         (lobos.schema.Table. :foo [] [(lobos.schema.Constraint.
                                        :foo
                                        :bar
                                        :baz)] {}))
      "Table with constraint"))

;;;; Schema definition tests

(deftest test-schema
  (is (thrown? IllegalArgumentException
               (schema nil))
      "Invalid schema definition")
  (is (= (schema :foo)
         (lobos.schema.Schema. :foo {} {}))
      "Empty schema")
  (is (= (schema :foo {:bar nil :baz nil})
         (lobos.schema.Schema. :foo {} {:bar nil
                                        :baz nil}))
      "Empty schema with option.")
  (is (= (schema :foo nil {:name :bar} {:name :baz})
         (lobos.schema.Schema. :foo {:bar {:name :bar}
                                     :baz {:name :baz}} {}))
      "Schema with elements"))
