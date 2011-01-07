;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.integration
  (:refer-clojure :exclude [bigint boolean char compile double drop float time])
  (:use clojure.test
        (clojure.contrib [io :only [delete-file file]])
        (lobos analyzer compiler connectivity core metadata schema utils)
        (lobos.backends h2 mysql postgresql sqlite sqlserver))
  (:import (java.lang UnsupportedOperationException)))

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

(def sqlite-spec
  {:classname   "org.sqlite.JDBC"
   :subprotocol "sqlite"
   :subname     "./lobos.sqlite3"
   :create      true})

(def sqlserver-spec
  {:classname    "com.microsoft.sqlserver.jdbc.SQLServerDriver"
   :subprotocol  "sqlserver"
   :user         "lobos"
   :password     "lobos"
   :subname      "//localhost:1433"
   :databaseName "lobos"})

(def db-specs [h2-spec
               mysql-spec
               postgresql-spec
               sqlite-spec
               sqlserver-spec])

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

(defn first-non-runtime-cause [e]
  (if (isa? (type e) RuntimeException)
    (if-let [c (.getCause e)]
      (first-non-runtime-cause c)
      e)
    e))

(defmacro ignore-unsupported [& body]
  `(try (doall (do ~@body))
        (catch Exception e#
          (if (isa? (type (first-non-runtime-cause e#))
                    UnsupportedOperationException)
            nil
            (throw e#)))))

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
        p (str ".*\\." (apply join "|" tmp-files-ext))
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
      (println (format "WARNING: Driver for %s isn't available: %s missing"
                       (:subprotocol db-spec)
                       (:classname db-spec)))))
  (doseq [db-spec available-specs]
    (try
      (open-global db-spec (test-db-name db-spec))
      (catch Exception _
        (println "WARNING: Failed to connect to" (:subprotocol db-spec)))))
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

(defn- eq [dtype]
  (first (replace {:float :double
                   :numeric :decimal
                   :char :nchar
                   :clob :nclob
                   :varchar :nvarchar}
                  [dtype])))

(def-db-test test-data-types
  (with-schema [lobos :lobos]
    (doseq [dtype [:smallint :integer :bigint
                   :numeric :decimal
                   :real :float :double
                   :char :nchar :clob :nclob
                   :binary :blob
                   :boolean
                   :date :time :timestamp]]
      (let [dtype (data-type dtype)
            lobos (ignore-unsupported
                   (create lobos
                           (table :foo
                                  (column :bar dtype nil))))]
        (when lobos
          (is (= (eq (-> lobos :elements :foo :columns :bar :data-type :dtype))
                 (eq (-> (column* :bar dtype []) :data-type :dtype)))
              (str "Data type not matching for " (-> dtype :dtype name)))
          (drop lobos (table :foo)))))
    (doseq [dtype [:char :nchar :varchar :nvarchar
                   :binary :varbinary
                   :numeric :decimal]]
      (let [dtype (data-type dtype [3])
            lobos (ignore-unsupported
                   (create lobos
                           (table :foo
                                  (column :bar dtype nil))))]
        (when lobos
          (is (= (eq (-> lobos :elements :foo :columns :bar :data-type :dtype))
                 (eq (-> (column* :bar dtype []) :data-type :dtype)))
              (str "Data type not matching for " (-> dtype :dtype name)))
          (is (= (-> lobos :elements :foo :columns :bar :data-type :args)
                 (-> (column* :bar dtype []) :data-type :args))
              (format "Data type arguments not matching for %s %s"
                      (-> dtype :dtype name)
                      (:args dtype)))
          (drop lobos (table :foo)))))
    (let [dtype (data-type :numeric [8 3])
          lobos (ignore-unsupported
                 (create lobos
                         (table :foo
                                (column :bar dtype nil))))]
      (when lobos
        (is (= (eq (-> lobos :elements :foo :columns :bar :data-type :dtype))
               (eq (-> (column* :bar dtype []) :data-type :dtype)))
            (str "Data type not matching for " (-> dtype :dtype name)))
        (is (= (-> lobos :elements :foo :columns :bar :data-type :args)
               (-> (column* :bar dtype []) :data-type :args))
            (format "Data type arguments not matching for %s %s"
                    (-> dtype :dtype name)
                    (:args dtype)))
        (drop lobos (table :foo))))
    (doseq [dtype [:time :timestamp]]
      (let [dtype (data-type dtype [] {:time-zone true})
            lobos (ignore-unsupported
                   (create lobos
                           (table :foo
                                  (column :bar dtype nil))))]
        (when lobos
          (is (= (eq (-> lobos :elements :foo :columns :bar :data-type :dtype))
                 (eq (-> (column* :bar dtype []) :data-type :dtype)))
              (str "Data type not matching for " (-> dtype :dtype name)))
          (is (-> lobos :elements :foo :columns :bar :data-type :options
                  :time-zone)
              (str "Timezone not set for " (-> dtype :dtype name)))
          (drop lobos (table :foo)))))))
