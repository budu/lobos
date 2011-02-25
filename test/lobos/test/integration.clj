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
        (lobos connectivity core metadata schema test utils)
        (lobos.backends h2 mysql postgresql sqlite sqlserver))
  (:import (lobos.schema ForeignKeyConstraint
                         UniqueConstraint)))

;;;; Fixtures

(use-fixtures :once
  remove-tmp-files-fixture
  open-global-connections-fixture)

;;;; Tests

(deftest test-action-connectivity
  (let [test-create-action
        #(try
           (let [lobos (create-schema :lobos)]
             (= (-> (create lobos (table :foo (integer :bar)))
                    :elements
                    :foo)
                (table :foo (integer :bar))))
           (finally (try (drop-schema :lobos :cascade)
                         (catch Exception _))))]
    (is (thrown? Exception (test-create-action))
        "Trying to create a table when no connection is available")
    (is (with-connection h2-spec (test-create-action))
        "Using with-connection to test if a table has been created")))

(def-db-test test-create-and-drop-schema
  (with-schema [lobos :lobos]
    (when (with-db-meta (-> lobos :options :db-spec)
            (or (supports-schemas)
                (supports-catalogs)))
      (is (= lobos (schema :lobos {:db-spec (get-db-spec *db*)}))
          "Checking if the schema has been created")
      (is (nil? (drop-schema lobos))
          "Checking if the schema has been dropped"))))

(def-db-test test-create-and-drop-table
  (with-schema [lobos :lobos]
    (create lobos (table :foo (integer :bar)))
    (is (= (-> (get-schema) :elements :foo)
           (table :foo (integer :bar)))
        "Checking if the table has been created")
    (is (nil? (-> (drop lobos (table :foo)) :elements :foo))
        "Checking if the table has been dropped")))

(def-db-test test-alter-table
  (with-schema [lobos :lobos]
    (let [actual #(-> % :elements :foo)]
      (create lobos (table :foo (integer :a)))
      (are-equal (actual (alter lobos :add (table :foo (integer :b))))
                 (table :foo (integer :a) (integer :b))
                 "Checking if the column has been added")
      (are-equal (actual (alter lobos :drop (table :foo (integer :b))))
                 (table :foo (integer :a))
                 "Checking if the column has been dropped")
      (are-equal (actual (alter lobos :add (table :foo (unique [:a]))))
                 (table :foo (integer :a :unique))
                 "Checking if the constraint has been added")
      (are-equal (actual (alter lobos :drop (table :foo (unique [:a]))))
                 (table :foo (integer :a))
                 "Checking if the constraint has been dropped")
      (are-equal (actual (alter lobos :modify
                                (table :foo (column :a (default 0)))))
                 (table :foo (integer :a (default 0)))
                 "Checking if the default clause has been set")
      (are-equal (actual (alter lobos :modify
                                (table :foo (column :a :drop-default))))
                 (table :foo (integer :a))
                 "Checking if the default clause has been dropped")
      (are-equal (actual (alter lobos :rename (table :foo (column :a :to :b))))
                 (table :foo (integer :b))
                 "Checking if the column has been renamed")
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
    (let [actual-type #(-> % :elements :foo :columns :bar :data-type :dtype eq)
          expected-type #(-> % :data-type :dtype eq)
          actual-args #(-> % :elements :foo :columns :bar :data-type :args)
          expected-args #(-> % :data-type :args)]
      
      (doseq [dtype [:smallint :integer :bigint
                     :numeric :decimal
                     :real :float :double
                     :char :nchar :clob :nclob
                     :binary :blob
                     :boolean
                     :date :time :timestamp]]
        (let [dtype (data-type dtype)]
          (test-with [lobos (create lobos (table :foo (column :bar dtype)))]
           (is (= (actual-type lobos) (expected-type (column* :bar dtype [])))
               (str "Data type not matching for " (-> dtype :dtype name)))
           (drop lobos (table :foo)))))
      
      (doseq [dtype [:char :nchar :varchar :nvarchar
                     :binary :varbinary
                     :numeric :decimal]]
        (let [dtype (data-type dtype [3])]
          (test-with [lobos (create lobos (table :foo (column :bar dtype nil)))]
           (is (= (actual-type lobos) (expected-type (column* :bar dtype [])))
               (str "Data type not matching for " (-> dtype :dtype name)))
           (is (= (actual-args lobos) (expected-args (column* :bar dtype [])))
               (format "Data type arguments not matching for %s %s"
                       (-> dtype :dtype name)
                       (:args dtype)))
           (drop lobos (table :foo)))))
    
      (let [dtype (data-type :numeric [8 3])]
        (test-with [lobos (create lobos (table :foo (column :bar dtype)))]
         (is (= (actual-type lobos) (expected-type (column* :bar dtype [])))
             (str "Data type not matching for " (-> dtype :dtype name)))
         (is (= (actual-args lobos) (expected-args (column* :bar dtype [])))
             (format "Data type arguments not matching for %s %s"
                     (-> dtype :dtype name)
                     (:args dtype)))
         (drop lobos (table :foo))))

      (doseq [dtype [:time :timestamp]]
        (let [dtype (data-type dtype [] {:time-zone true})]
          (test-with [lobos (create lobos (table :foo (column :bar dtype)))]
           (is (= (actual-type lobos) (expected-type (column* :bar dtype [])))
               (str "Data type not matching for " (-> dtype :dtype name)))
           (is (-> lobos :elements :foo :columns :bar :data-type :options
                   :time-zone)
               (str "Timezone not set for " (-> dtype :dtype name)))
           (drop lobos (table :foo))))))))

(def-db-test test-unique-constraint
  (with-schema [lobos :lobos]
    (doseq [ctype [:unique :primary-key]]
      (let [cname (make-constraint-name :foo ctype [:a])]
        (are-equal (-> lobos (create (table :foo (integer :a ctype)))
                       :elements :foo :constraints cname)
                   (UniqueConstraint. cname ctype [:a])
                   (format "Checking if %s constraint has been created"
                           (name ctype)))
        (drop lobos (table :foo))))))

(def-db-test test-foreign-key-constraint
  (with-schema [lobos :lobos]
    (let [cname (make-constraint-name :baz :fkey [:bar])]
      (are-equal (-> lobos
                     (create (table :foo (integer :bar :primary-key)))
                     (create (table :baz (integer :bar)
                                    (foreign-key [:bar] :foo)))
                     :elements :baz :constraints cname)
                 (ForeignKeyConstraint. cname [:bar] :foo [:bar] nil {})
                 "Checking if foreign key constraint has been created")
      (drop lobos (table :baz))
      (drop lobos (table :foo)))))
