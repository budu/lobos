;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.system
  (:refer-clojure :exclude [compile conj! disj! distinct drop sort take])
  (:require (clojure.contrib [sql :as sql])
            (lobos [compiler :as compiler]
                   [connectivity :as conn]))
  (:use clojure.test
        (lobos [core :only [create drop]] test)
        (lobos.backends h2 mysql postgresql sqlite sqlserver)
        lobos.test.sample-schema))

;;;; Fixtures

(defn create-schemas-for-all-db []
  (doseq [db (available-global-cnx)]
    (create db sample-schema)))

(defn drop-schemas-for-all-db []
  (doseq [db (available-global-cnx)]
    (drop db sample-schema :cascade)))

(defn use-sample-schema-fixture [f]
  (try (create-schemas-for-all-db)
       (f)
       (finally (drop-schemas-for-all-db))))

(use-fixtures :once
  remove-tmp-files-fixture
  open-global-connections-fixture)

(use-fixtures :each
  use-sample-schema-fixture)

;;;; Helpers

(defn table [name]
  (compiler/as-identifier
   (conn/get-db-spec *db*)
   name
   :lobos))

(defn identifier [name]
  (compiler/as-identifier (conn/get-db-spec *db*) name))

;;;; Tests

(def-db-test test-check-constraint
  (when-not (= *db* :mysql)
    (sql/with-connection (conn/get-db-spec *db*)
      (is (thrown? Exception
                   (sql/insert-records (table :users)
                                       {(identifier :name) "x"}))
          "An exception should have been thrown because of a check constraint")
      (is (nil? (sql/insert-records (table :users)
                                    {(identifier :name) "foo"}))
          "The insert statement should not throw an exception"))))

(def-db-test test-unique-constraint
  (sql/with-connection (conn/get-db-spec *db*)
    (sql/insert-records (table :users) {(identifier :name) "foo"})
    (is (thrown? Exception
                 (sql/insert-records (table :users)
                                     {(identifier :name) "foo"}))
        "An exception should have been thrown because of an unique constraint")
    (is (nil? (sql/insert-records (table :users)
                                  {(identifier :name) "bar"}))
        "The insert statement should not throw an exception")))

;;; Using hardcoded id is a bad idea!
(def-db-test test-foreign-key-constraint
  (when-not (= *db* :sqlite)
    (sql/with-connection (conn/get-db-spec *db*)
      (sql/insert-records (table :users) {(identifier :name) "foo"})
      (is (thrown? Exception
                   (sql/insert-records (table :posts)
                                       {(identifier :title) "foo"
                                        (identifier :user_id) 2}))
          "An exception should have been thrown because of a foreign key constraint")
      (is (nil? (sql/insert-records (table :posts)
                                    {(identifier :title) "foo"
                                     (identifier :user_id) 1}))
          "The insert statement should not throw an exception"))))
