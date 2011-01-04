;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.integration
  (:refer-clojure :exclude [bigint boolean char compile double drop float])
  (:use clojure.test
        (clojure [string :only [join]])
        (clojure.contrib [io :only [delete-file
                                    file]])
        (lobos schema analyzer compiler core connectivity)
        (lobos.backends h2 mysql postgresql sqlite)))

;;;; DB connection specifications

(def h2-spec
  {:classname   "org.h2.Driver"
   :subprotocol "h2"
   :subname     "./lobos"})

(def mysql-spec
  {:classname   "com.mysql.jdbc.Driver"
   :subprotocol "mysql"
   :user        "lobos"
   :password    "lobos"
   :subname     "//localhost:3306/"})

(def postgresql-spec
  {:classname   "org.postgresql.Driver"
   :subprotocol "postgresql"
   :user        "lobos"
   :password    "lobos"
   :subname     "//localhost:5432/lobos"})

(def sqlite
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "./lobos.sqlite3"
   :create      true})

(def db-specs [h2-spec
               mysql-spec
               postgresql-spec
               #_sqlite]) ; disabled until JDBC driver is fixed

(defn driver-available? [db-spec]
  (try
    (clojure.lang.RT/classForName (:classname db-spec))
    true
    (catch ClassNotFoundException e false)))

(def available-specs (filter driver-available? db-specs))

(defn available-global-cnx []
  (filter #(re-find #":test-.*" (str %))
    (keys @global-connections)))

(defn test-db-name [db-spec]
  (keyword (str "test-" (:subprotocol db-spec))))

(def *db* nil)

;;;; Helpers

(defmacro def-db-test [name & body]
  `(do ~@(for [db-spec available-specs]
           (let [db (test-db-name db-spec)]
             `(deftest ~(symbol (str name "-" (:subprotocol db-spec)))
                (when ((set (available-global-cnx)) ~db)
                  (binding [*db* ~db]
                    ~@body)))))))

(defmacro with-schema [[var-name sname] & body]
  `(try
     (let [~var-name (create-schema ~sname *db*)]
       ~@body)
     (finally (try (drop-schema ~sname :cascade *db*)
                   (catch Exception _#)))))

;;;; Fixtures

(def tmp-files-ext '(db sqlite3))

(defn remove-tmp-files []
  (let [current-dir (file-seq (file "."))
        p (str ".*\\." (join "|" tmp-files-ext))
        tmp? #(re-find (re-pattern p) (str %))]
    (doseq [tmp-file (filter tmp? current-dir)]
      (delete-file tmp-file true))))

(defn remove-tmp-files-fixture [f]
  (remove-tmp-files)
  (f)
  (remove-tmp-files))

(defn open-global-connections-fixture [f]
  (doseq [db-spec db-specs]
    (when-not (driver-available? db-spec)
      (println (format "*** Driver for %s isn't available: %s missing ***"
                       (:subprotocol db-spec)
                       (:classname db-spec)))))
  (doseq [db-spec available-specs]
    (try
      (open-global db-spec (test-db-name db-spec))
      (catch Exception _
        (println "*** Failed to connect to" (:subprotocol db-spec) "***"))))
  (f)
  (doseq [db (available-global-cnx)]
    (close-global db)))

(use-fixtures :once
  remove-tmp-files-fixture
  open-global-connections-fixture)

;;;; Tests

(def-db-test test-create-and-drop-schema
  (with-schema [lobos :lobos]
    (when (with-db-meta (-> lobos :options :db-spec)
            (or (supports-schemas)
                (supports-catalogs)))
      (is (= lobos (schema :lobos {:db-spec (get-db-spec *db*)}))
          "Checking if the schema has been created")
      (drop-schema lobos)
      (is (nil? (analyze-schema :lobos *db*))
          "Checking if the schema has been dropped"))))

(def-db-test test-create-and-drop-table
  (with-schema [lobos :lobos]
    (let [lobos (create lobos (table :foo (integer :bar)))]
      (is (= (-> lobos :elements :foo) (table :foo (integer :bar)))
          "Checking if the table has been created"))
    (let [lobos (drop lobos (table :foo))]
      (is (nil? (-> lobos :elements :foo))
          "Checking if the table has been dropped"))))

(def-db-test test-data-types
  (with-schema [lobos :lobos]
    (doseq [dtype [:smallint :integer :bigint :numeric :decimal :real :float
                   :double :char :clob :blob :boolean :timestamp]]
      (let [lobos (create lobos
                          (table :foo
                                 (column :bar (data-type dtype) nil)))]
        (is (= (-> lobos :elements :foo :columns :bar :data-type :dtype)
               (-> (column* :bar (data-type dtype) []) :data-type :dtype)))
        (drop lobos (table :foo))))
    (let [lobos (create lobos (table :foo (varchar :bar 1)))]
        (is (= (-> lobos :elements :foo :columns :bar :data-type :dtype)
               (-> (column* :bar (data-type :varchar 1) []) :data-type :dtype)))
        (drop lobos (table :foo)))))
