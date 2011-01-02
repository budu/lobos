;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.schema
  (:refer-clojure :exclude [bigint boolean char double float])
  (:use [lobos.schema] :reload)
  (:use [clojure.test]))

;;;; Constraint definition tests

(deftest test-constraint
  (is (= (table :foo (constraint :foo :bar :baz))
         (table* :foo {} {:foo (lobos.schema.Constraint. :foo :bar :baz)} {}))
      "Constraint definition"))

(deftest test-unique-constraint
  (is (= (table :foo (unique-constraint :foo :bar (list :baz)))
         (table* :foo {}
                 {nil (lobos.schema.Constraint.
                       nil
                       :foo
                       {:columns [:bar :baz]})} {}))
      "Unnamed unique constraint definition")
  (is (= (table :foo (column :bar nil nil)
                (unique-constraint :foo :bar (list :baz)))
         (table* :foo {:bar (column* :bar nil nil)}
                 {:bar (lobos.schema.Constraint.
                        :bar
                        :foo
                        {:columns [:baz]})} {}))
      "Named unique constraint definition"))

(deftest test-primary-key
  (is (= (table :foo (primary-key :foo :bar :baz))
         (table* :foo {} {nil (lobos.schema.Constraint.
                               nil
                               :primary-key
                               {:columns [:foo :bar :baz]})} {}))
      "Primary key constraint definition"))

(deftest test-unique
  (is (= (table :foo (unique :foo :bar :baz))
         (table* :foo {} {nil (lobos.schema.Constraint.
                               nil
                               :unique
                               {:columns [:foo :bar :baz]})} {}))
      "Unique constraint definition"))

;;;; Column definition tests

(def column-definition-stub
     (lobos.schema.Column. :foo nil nil false false []))

(deftest test-column
  (testing "Column definition"
    (is (thrown? IllegalArgumentException
                 (column {} nil nil nil))
        "Invalid column definition")
    (is (= (table :foo (column :foo nil nil))
           (table* :foo {:foo column-definition-stub} {} {}))
        "Column with name and data-type")
    (is (= (table :foo (column :foo nil [:auto-inc]))
           (table* :foo {:foo (assoc column-definition-stub
                                :auto-inc true)} {} {}))
        "Setting column as auto incremented")
    (is (= (table :foo (column :foo nil [:not-null]))
           (table* :foo {:foo (assoc column-definition-stub
                                :not-null true)} {} {}))
        "Setting column as not null")
    (is (= (table :foo (column :foo nil [[:default 0]]))
           (table* :foo {:foo (assoc column-definition-stub
                                :default 0)} {} {}))
        "Setting default value")
    (is (= (table :foo (column :foo nil ["BAR"]))
           (table* :foo {:foo (assoc column-definition-stub
                                :others ["BAR"])} {} {}))
        "Setting custom options")))

(deftest test-typed-column
  (testing "Typed column definition"
    (is (= (table :foo (integer :foo))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :integer))} {} {}))
        "Integer")
    (is (= (table :foo (numeric :foo))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :numeric))} {} {}))
        "Numeric")
    (is (= (table :foo (numeric :foo 2))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :numeric 2))} {} {}))
        "Numeric with precision")
    (is (= (table :foo (numeric :foo 2 4))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :numeric 2 4))} {} {}))
        "Numeric with precision and scale")
    (is (= (table :foo (numeric :foo "BAR"))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :numeric)
                                :others ["BAR"])} {} {}))
        "Numeric with other option")
    (is (= (table :foo (float :foo))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :float))} {} {}))
        "Float")
    (is (= (table :foo (float :foo 1))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :float 1))} {} {}))
        "Float with precision")
    (is (= (table :foo (varchar :foo))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :varchar))} {} {}))
        "Varchar")
    (is (= (table :foo (varchar :foo 1))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :varchar 1))} {} {}))
        "Varchar with limit")
    (is (= (table :foo (varchar :foo "BAR"))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :varchar)
                                     :others ["BAR"])} {} {}))
        "Varchar with other option")
    (is (= (table :foo (timestamp :foo))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :timestamp))} {} {}))
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
         (lobos.schema.Table. :foo {} {} {}))
      "Empty table")
  (is (= (table :foo (column :foo nil nil))
         (lobos.schema.Table. :foo {:foo column-definition-stub} {} {}))
      "Table with column")
  (is (= (table :foo (constraint :foo :bar :baz))
         (lobos.schema.Table. :foo {} {:foo (lobos.schema.Constraint.
                                             :foo
                                             :bar
                                             :baz)} {}))
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
  (is (= (schema :foo (table :bar) (table :baz))
         (lobos.schema.Schema. :foo {:bar (table :bar)
                                     :baz (table :baz)} {}))
      "Schema with elements")
  (is (= (schema :foo {:bar nil :baz nil}
                 (table :bar)
                 (table :baz))
         (lobos.schema.Schema. :foo
                               {:bar (table :bar)
                                :baz (table :baz)}
                               {:bar nil
                                :baz nil}))
      "Schema with elements and options."))
