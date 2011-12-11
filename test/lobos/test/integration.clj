;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.integration
  (:refer-clojure :exclude [alter compile defonce drop
                            bigint boolean char double float time])
  (:use clojure.test
        (lobos analyzer connectivity core metadata schema test utils)
        (lobos.backends h2 mysql postgresql sqlite sqlserver))
  (:import (lobos.schema ForeignKeyConstraint
                         UniqueConstraint)))

;;;; Fixtures

(use-fixtures :once
  remove-tmp-files-fixture
  open-global-connections-fixture)

;;;; Tests

(deftest test-action-connectivity
  (let [test-action
        #(with-schema [lobos :lobos]
           ;; cannot rely on creating just a schema as some dbms ignore that action
           (create lobos (table :foo (integer :bar)))
           (= (-> :lobos analyze-schema :tables :foo)
              (table :foo (integer :bar))))]
    (is (thrown? Exception (test-action))
        "An exception should have been thrown because there are no connection")
    (is (with-connection h2-spec (test-action))
        "No exception should have been thrown when executing an action")))

(def-db-test test-create-and-drop-schema
  (when (with-db-meta (get-db-spec *db*)
          (or (supports-schemas)
              (supports-catalogs)))
    (with-schema [lobos :lobos]
      (is (= (inspect-schema) lobos)
          "A schema named 'lobos' should have been created")
      (drop *db* lobos)
      (is (not= (inspect-schema :sname) :lobos)
          "A schema named 'lobos' should have been dropped"))))

(def-db-test test-create-and-drop-schema-with-tables
  (when (with-db-meta (get-db-spec *db*)
          (or (supports-schemas)
              (supports-catalogs)))
    (with-schema [lobos :lobos]
      (is (= (inspect-schema) lobos)
          "A schema named 'lobos' should have been created")
      (create lobos (table :foo (integer :bar)))
      (drop *db* lobos :cascade)
      (is (not= (inspect-schema :sname) :lobos)
          "A schema named 'lobos' should have been dropped"))))

(def-db-test test-create-and-drop-table
  (with-schema [lobos :lobos]
    (create lobos (table :foo (integer :bar)))
    (is (= (inspect-schema :tables :foo)
           (table :foo (integer :bar)))
        "A table named 'foo' should have been created")
    (drop lobos (table :foo))
    (is (nil? (inspect-schema :tables :foo))
        "A table named 'foo' should have been dropped")))

(def-db-test test-create-and-drop-index
  (with-schema [lobos :lobos]
    (create lobos (table :foo (integer :bar)))
    (let [cname (make-index-name :foo :index [:bar])]
      (create lobos (index :foo [:bar]))
      (is (= (inspect-schema :tables :foo :indexes cname)
             (index :foo [:bar]))
          "An index named 'foo_index_bar' should have been created")
      (drop lobos (index :foo [:bar]))
      (is (empty? (inspect-schema :tables :foo :indexes))
          "An index named 'foo_index_bar' should have been dropped"))
    (drop lobos (table :foo))))

(def-db-test test-alter-table
  (with-schema [lobos :lobos]
    (let [actual #(-> % :tables :foo (assoc :indexes {}))]
      (create lobos (table :foo (integer :a)))
      (when-supported (alter lobos :add (table :foo (integer :b)))
        (is (= (actual (inspect-schema))
               (table :foo (integer :a) (integer :b)))
            "A column named 'b' should have been added to table 'foo'"))
      (when-supported (alter lobos :drop (table :foo (integer :b)))
        (is (= (actual (inspect-schema))
               (table :foo (integer :a)))
            "A column named 'b' should have been dropped from table 'foo'"))
      (when-supported (alter lobos :add (table :foo (unique [:a])))
        (is (= (actual (inspect-schema))
               (table :foo (integer :a :unique)))
            "A constraint named 'foo_unique_a' should have been added to table 'foo'"))
      (when-supported (alter lobos :drop (table :foo (unique [:a])))
        (is (= (actual (inspect-schema))
               (table :foo (integer :a)))
            "A constraint named 'foo_unique_a' should have been dropped from table 'foo'"))
      (when-supported (alter lobos :modify (table :foo (column :a (default 0))))
        (is (= (actual (inspect-schema))
               (table :foo (integer :a (default 0))))
            "A column default clause should have been set"))
      (when-supported  (alter lobos :modify (table :foo (column :a :drop-default)))
        (is (= (actual (inspect-schema))
               (table :foo (integer :a)))
            "A column default clause should have been dropped"))
      (when-supported  (alter lobos :rename (table :foo (column :a :to :b)))
        (is (= (actual (inspect-schema))
               (table :foo (integer :b)))
            "A column named 'a' should have been renamed to 'b'"))
      (drop lobos (table :foo)))))

(defn- eq [dtype]
  (first (replace {:float :double
                   :numeric :decimal
                   :char :nchar
                   :clob :nclob
                   :varchar :nvarchar}
                  [dtype])))

(def-db-test test-data-types
  (with-schema [lobos :lobos]
    (let [actual-type #(inspect-schema :tables :foo :columns :bar :data-type :dtype eq)
          expected-type #(-> % :data-type :dtype eq)
          actual-args #(inspect-schema :tables :foo :columns :bar :data-type :args)
          expected-args #(-> % :data-type :args)]
      
      (doseq [dtype [:smallint :integer :bigint
                     :numeric :decimal
                     :real :float :double
                     :char :nchar :clob :nclob
                     :binary :blob
                     :boolean
                     :date :time :timestamp]]
        (let [dtype (data-type dtype)]
          (when-supported (create lobos (table :foo (column :bar dtype)))
            (is (= (actual-type) (expected-type (column* :bar dtype [])))
                (str "Data type should match " (-> dtype :dtype name)))
            (drop lobos (table :foo)))))
      
      (doseq [dtype [:char :nchar :varchar :nvarchar
                     :binary :varbinary
                     :numeric :decimal]]
        (let [dtype (data-type dtype [3])]
          (when-supported (create lobos (table :foo (column :bar dtype nil)))
            (is (= (actual-type) (expected-type (column* :bar dtype [])))
                (str "Data type should match " (-> dtype :dtype name)))
            (is (= (actual-args) (expected-args (column* :bar dtype [])))
                (format "Data type arguments should match %s for %s"
                        (:args dtype)
                        (-> dtype :dtype name)))
            (drop lobos (table :foo)))))
      
      (let [dtype (data-type :numeric [8 3])]
        (when-supported (create lobos (table :foo (column :bar dtype)))
          (is (= (actual-type) (expected-type (column* :bar dtype [])))
              (str "Data type should match " (-> dtype :dtype name)))
          (is (= (actual-args) (expected-args (column* :bar dtype [])))
              (format "Data type arguments should match %s for %s"
                      (:args dtype)
                      (-> dtype :dtype name)))
          (drop lobos (table :foo))))

      (doseq [dtype [:time :timestamp]]
        (let [dtype (data-type dtype [] {:time-zone true})]
          (when-supported (create lobos (table :foo (column :bar dtype)))
            (is (= (actual-type) (expected-type (column* :bar dtype [])))
                (str "Data type should match " (-> dtype :dtype name)))
            (is (inspect-schema :tables :foo :columns :bar
                                :data-type :options :time-zone)
                (str "Timezone should be set for " (-> dtype :dtype name)))
            (drop lobos (table :foo))))))))

(def-db-test test-unique-constraint
  (with-schema [lobos :lobos]
    (doseq [ctype [:unique :primary-key]]
      (let [cname (make-index-name :foo ctype [:a])]
        (when-supported (create lobos (table :foo (integer :a ctype)))
          (is (= (inspect-schema :tables :foo :constraints cname)
                 (UniqueConstraint. cname ctype [:a]))
              (format "A %s constraint named '%s' should have been created"
                      (name ctype)
                      (name cname))))
        (drop lobos (table :foo))))))

(def-db-test test-foreign-key-constraint
  (with-schema [lobos :lobos]
    (let [cname (make-index-name :baz :fkey [:bar])]
      (when-supported (do (create lobos (table :foo (integer :bar :primary-key)))
                          (create lobos (table :baz
                                          (integer :bar)
                                          (foreign-key [:bar] :foo))))
        (is (= (inspect-schema :tables :baz :constraints cname)
               (ForeignKeyConstraint. cname [:bar] :foo [:bar] nil
                                      (if (= *db* :h2)
                                        {:on-delete :restrict :on-update :restrict}
                                        {})))
            (format "A foreign key constraint named '%s' should have been created"
                    (name cname))))
      (drop lobos (table :baz))
      (drop lobos (table :foo)))))

(def-db-test test-default-schema
  (when-not (= *db* :mysql)
    (create *db* (table :foo (integer :bar)))
    (is (= (-> *db* analyze-schema :tables :foo)
           (table :foo (integer :bar)))
        "A table named 'foo' should have been created in the default schema")
    (drop *db* (table :foo))))
