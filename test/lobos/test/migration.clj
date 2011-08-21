;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.migration
  (:refer-clojure :exclude [complement])
  (:require (lobos [connectivity :as conn]
                   [schema :as schema]))
  (:use (clojure.java [io :only [delete-file]])
        clojure.test
        (lobos [internal :only [query]]
               migration
               test)))

;;;; Fixtures

(use-fixtures :once
  remove-tmp-files-fixture
  #(do (%) (conn/close-global nil true)))

;;;; Tests

(deftest test-reverse-rename
  (is (= (reverse-rename '(column :foo :to :bar))
         '(column :bar :to :foo))
      "Should reverse the column definition rename special case.")
  (is (= (reverse-rename '(alter :rename
                            (table :foo
                              (column :a :to :b)
                              (column :c :to :d))))
         '(alter :rename
            (table :foo
              (column :b :to :a)
              (column :d :to :c))))
      (str "Should reverse all column definitions rename special cases "
           "inside nested form.")))

(deftest test-complement-create
  (is (= (complement '(create (table :users (integer :id))))
         '(drop (table :users (integer :id))))
      "Should return a drop table action without cascade clause")
  (is (= (complement '(create db (schema :users)))
         '(drop db (schema :users) :cascade))
      "Should return a drop schema action with a cascade clause"))

(deftest test-complement-alter
  (is (= (complement '(alter :add (table :foo (integer :foo))))
         '(alter :drop (table :foo (integer :foo))))
      "Should return an alter add action"))

(deftest test-append-to-stash-file
  (delete-file *stash-file* true)
  (append-to-stash-file 'foo)
  (is (= (slurp *stash-file*) "\nfoo\n")
      "Should create a stash file containing 'foo'")
  (append-to-stash-file 'bar)
  (is (= (slurp *stash-file*) "\nfoo\n\nbar\n")
      "Should appende 'bar' to the stash file"))

(deftest test-clear-stash-file
  (delete-file *stash-file* true)
  (append-to-stash-file 'foo)
  (clear-stash-file)
  (is (= (slurp *stash-file*) "")
      "Should be empty"))

(deftest test-read-stash-file
  (delete-file *stash-file* true)
  (append-to-stash-file ':foo)
  (append-to-stash-file ':bar)
  (is (= (read-stash-file) [:foo :bar])
      "Should read the stash file forms into a vector"))

(deftest test-migrations-file
  (binding [*migrations-namespace* "foo.bar"]
    (is (= (.getPath (migrations-file))
           (-> "src/foo/bar.clj"
               (.replace \/ java.io.File/separatorChar)))
        (str "Should return a File object with a path to the migrations "
             "namespace"))))

(deftest test-create-mfile
  (delete-file (migrations-file) true)
  (create-mfile)
  (is (= (slurp (migrations-file))
         (.replace
          (slurp "test/lobos/test/sample_migrations.clj")
          "#_" ""))
      "Should create an empty migrations file"))

(deftest test-append-to-mfile
  (delete-file (migrations-file) true)
  (create-mfile)
  (append-to-mfile 'foo "this is a test" '(println "up"))
  (is (.endsWith
       (slurp (migrations-file))
       "(defmigration foo \"this is a test\" (up [] println \"up\"))\n")
      (str "Should create a migrations file ending with "
           "a migration with a docstring and an up clause"))
  (append-to-mfile 'bar nil '(println "up") '(println "down"))
  (is (.endsWith
       (slurp (migrations-file))
       (str "(defmigration bar (up [] println \"up\") "
            "(down [] println \"down\"))\n"))
      (str "Should append a migration without a docstring and "
           " an up and down clauses")))

(deftest test-list-migrations
  (delete-file (migrations-file) true)
  (is (nil? (list-migrations))
      "Should return nil")
  (doseq [name '(foo bar baz)]
    (append-to-mfile name nil '(println "up")))
  (is (= (count (list-migrations)) 3)
      "Should return a collection of three elements")
  (is (every? #(satisfies? Migration %) (list-migrations))
      "All items returned should satisfy the Migration protocol"))

(defmacro with-migrations-table [& body]
  `(conn/with-connection h2-spec
     (binding [*db* h2-spec]
       (with-schema [~'lobos :lobos]
         (create-migrations-table h2-spec :lobos)
         ~@body))))

(deftest test-create-migrations-table
  (with-migrations-table
    (is (= (inspect-schema :elements :lobos_migrations)
           (schema/table :lobos_migrations
                         (schema/varchar :name 255)))
        "A table named 'lobos_migrations' should be created")))

(deftest test-insert-and-delete-migrations
  (with-migrations-table
    (insert-migrations h2-spec :lobos 'foo)
    (is (= (query h2-spec :lobos :lobos_migrations)
           (list {:name "foo"}))
        "Should insert a migration entry named 'foo'")
    (insert-migrations h2-spec :lobos 'bar 'baz)
    (is (= (query h2-spec :lobos :lobos_migrations)
           (list {:name "foo"}
                 {:name "bar"}
                 {:name "baz"}))
        "Should insert two migration entries named 'bar' and 'baz'")
    (delete-migrations h2-spec :lobos 'foo)
    (is (= (query h2-spec :lobos :lobos_migrations)
           (list {:name "bar"} {:name "baz"}))
        "Should delete a migration entry named 'foo'")
    (delete-migrations h2-spec :lobos 'bar 'baz)
    (is (empty? (query h2-spec :lobos :lobos_migrations))
        "Should delete all migration entries")))

(deftest test-query-migrations-table
  (with-migrations-table
    (is (empty? (query-migrations-table h2-spec :lobos))
        "Should return an empty list")
    (insert-migrations h2-spec :lobos 'foo 'bar)
    (is (= (query-migrations-table h2-spec :lobos)
           (list "foo" "bar"))
        "Should return a list containing 'foo' and 'bar'")))

(deftest test-list-migrations-names
  (delete-file (migrations-file) true)
  (is (empty? (list-migrations-names))
      "Should return an empty list")
  (doseq [name '(foo bar baz)]
    (append-to-mfile name nil '(println "up")))
  (is (= (list-migrations-names) (list "foo" "bar" "baz"))
      "Should return a list containing 'foo' 'bar' and 'baz'"))
