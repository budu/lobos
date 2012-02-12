;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.schema
  (:refer-clojure :exclude [bigint boolean char double float time])
  (:use clojure.test
        lobos.schema)
  (:import (lobos.schema CheckConstraint
                         Column
                         ForeignKeyConstraint
                         UniqueConstraint
                         DataType
                         Table
                         Schema)))

;;;; Constraint definition tests

(deftest test-primary-key
  (is (= (table :foo (primary-key [:a :b :c]))
         (table* :foo {} {:foo_primary_key_a_b_c
                          (UniqueConstraint.
                           :foo_primary_key_a_b_c
                           :primary-key
                           [:a :b :c])} {}))
      "Unnamed primary key constraint definition")
  (is (= (table :foo (primary-key :bar [:a :b :c]))
         (table* :foo {} {:bar
                          (UniqueConstraint. :bar :primary-key [:a :b :c])}))
      "Named primary key constraint definition"))

(deftest test-unique
  (is (= (table :foo (unique [:a :b :c]))
         (table* :foo {} {:foo_unique_a_b_c
                          (UniqueConstraint.
                           :foo_unique_a_b_c
                           :unique
                           [:a :b :c])} {}))
      "Unnamed unique constraint definition")
  (is (= (table :foo (unique :bar [:a :b :c]))
         (table* :foo {} {:bar (UniqueConstraint. :bar :unique [:a :b :c])}))
      "Named unique constraint definition"))

(def foreign-key-stub
  (ForeignKeyConstraint. :foo_fkey_a_b_c [:a :b :c] :bar [:a :b :c] nil {}))

(deftest test-foreign-key
  (is (= (table :foo (foreign-key [:a :b :c] :bar))
         (table* :foo {} {:foo_fkey_a_b_c foreign-key-stub}))
      "Foreign key constraint definition")
  (is (= (table :foo (foreign-key :foobar [:a :b :c] :bar))
         (table* :foo {} {:foobar (assoc foreign-key-stub
                                    :cname :foobar)}))
      "Foreign key constraint definition with name")
  (is (= (table :foo (foreign-key [:a :b :c] :bar [:d :e :f]))
         (table* :foo {} {:foo_fkey_a_b_c (assoc foreign-key-stub
                                            :parent-columns [:d :e :f])}))
      "Foreign key constraint definition with foreign columns")
  (is (= (table :foo (foreign-key [:a :b :c] :bar :full))
         (table* :foo {} {:foo_fkey_a_b_c (assoc foreign-key-stub
                                            :match :full)}))
      "Foreign key constraint definition with match")
  (is (= (table :foo (foreign-key [:a :b :c] :bar :on-delete :cascade))
         (table* :foo {} {:foo_fkey_a_b_c (assoc foreign-key-stub
                                            :triggered-actions
                                            {:on-delete :cascade})}))
      "Foreign key constraint definition with one triggered action")
  (is (= (table :foo (foreign-key [:a :b :c] :bar :on-delete :cascade
                                                  :on-update :restrict))
         (table* :foo {} {:foo_fkey_a_b_c (assoc foreign-key-stub
                                            :triggered-actions
                                            {:on-delete :cascade
                                             :on-update :restrict})}))
      "Foreign key constraint definition with two triggered actions")
  (is (= (table :foo (foreign-key :foobar [:a :b :c]
                                  :bar [:d :e :f]
                                  :full
                                  :on-delete :cascade
                                  :on-update :restrict))
         (table* :foo {} {:foobar (assoc foreign-key-stub
                                    :cname :foobar
                                    :parent-columns [:d :e :f]
                                    :match :full
                                    :triggered-actions
                                    {:on-delete :cascade
                                     :on-update :restrict})}))
      "Foreign key constraint definition with everything"))

(deftest test-check
  (is (= (-> (table :foo (check :bar (> :a 1))) :constraints :bar)
         (CheckConstraint. :bar (expression (> :a 1))))
      "Simple check constraint definition")
  (is (= (-> (table :foo (check :bar (and (> :a 1) (< :a 10)
                                          (or (= :b "foo")
                                              (= :b "bar"))
                                          (in :ab [1 2 3])))) :constraints :bar)
         (CheckConstraint. :bar
                           (expression (and (> :a 1) (< :a 10)
                                            (or (= :b "foo")
                                                (= :b "bar"))
                                            (in :ab [1 2 3])))))
      "Complex check constraint definition"))

;;;; Column and data-type definition tests

(def column-definition-stub
  (Column. :foo nil nil false false []))

(deftest test-column
  (testing "Column definition"
    (is (= (table :foo (column :foo nil nil))
           (table* :foo {:foo column-definition-stub} {} {}))
        "Column with name and data-type")
    (is (= (table :foo (column :foo nil :auto-inc))
           (table* :foo {:foo (assoc column-definition-stub
                                :auto-inc true)} {} {}))
        "Setting column as auto incremented")
    (is (= (table :foo (column :foo nil :not-null))
           (table* :foo {:foo (assoc column-definition-stub
                                :not-null true)} {} {}))
        "Setting column as not null")
    (is (= (table :foo (column :foo nil [:default 0]))
           (table* :foo {:foo (assoc column-definition-stub
                                :default 0)} {} {}))
        "Setting default value")
    (is (= (table :foo (column :foo nil "BAR"))
           (table* :foo {:foo (assoc column-definition-stub
                                :others ["BAR"])} {} {}))
        "Setting custom options")
    (is (= (table :foo (column :foo :to :bar))
           (table* :foo {:foo (assoc column-definition-stub
                                :others :bar)}))
        "Should set :others value to the new column name")
    (is (= (table :foo (column :foo :drop-default))
           (table* :foo {:foo (assoc column-definition-stub
                                :default :drop)}))
        "Should set :default value to :drop")))

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
                                :data-type (data-type :numeric [2]))} {} {}))
        "Numeric with precision")
    (is (= (table :foo (numeric :foo 2 4))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :numeric [2 4]))} {} {}))
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
                                :data-type (data-type :float [1]))} {} {}))
        "Float with precision")
    (is (= (table :foo (varchar :foo 1))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :varchar [1]))} {} {}))
        "Varchar with limit")
    (is (= (table :foo (varchar :foo 1 "BAR"))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :varchar [1])
                                     :others ["BAR"])} {} {}))
        "Varchar with other option")
    (is (= (table :foo (timestamp :foo))
           (table* :foo {:foo (assoc column-definition-stub
                                :data-type (data-type :timestamp))} {} {}))
        "Timestamp")))

;;;; Table definition tests

(deftest test-table*
  (is (= (table* :foo nil nil nil)
         (Table. :foo {} {} {}))
      "Table definition"))

(deftest test-table
  (is (= (table :foo)
         (Table. :foo {} {} {}))
      "Empty table")
  (is (= (table :foo (column :foo nil nil))
         (Table. :foo {:foo column-definition-stub} {} {}))
      "Table with column")
  (is (= (table :foo (unique :bar [:a]))
         (Table. :foo {} {:bar (UniqueConstraint. :bar :unique [:a])} {}))
      "Table with constraint"))

;;;; Schema definition tests

(deftest test-schema
  (is (= (schema :foo)
         (Schema. :foo {} {} {}))
      "Empty schema")
  (is (= (schema :foo {:bar nil :baz nil})
         (Schema. :foo {} {} {:bar nil
                              :baz nil}))
      "Empty schema with option.")
  (is (= (schema :foo (table :bar) (table :baz))
         (Schema. :foo {:bar (table :bar)
                        :baz (table :baz)} {} {}))
      "Schema with tables")
  (is (= (schema :foo {:bar nil :baz nil}
                 (table :bar)
                 (table :baz))
         (Schema. :foo
                  {:bar (table :bar)
                   :baz (table :baz)}
                  {}
                  {:bar nil
                   :baz nil}))
      "Schema with tables and options."))

;;;; Invalid definition tests

(deftest test-name-required
  (doseq [definition [#(column* nil nil nil)
                      #(table* nil nil nil nil)
                      #(schema nil)]]
    (is (thrown-with-msg? IllegalArgumentException
          #"A .*? definition needs at least a name."
          (definition))
        "Should throw an exception when no name is provided.")))

(deftest test-check-valid-options
  (doseq [definition [#(column* :foo nil [:uto-inc])
                      #(data-type :foo nil [:ncoding])]]
    (is (thrown-with-msg? IllegalArgumentException
          #".*? are invalid, only .*? options are valid."
          (definition))
        "Should throw an exception when options are mistyped.")))
