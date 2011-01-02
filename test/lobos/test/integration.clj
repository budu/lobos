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
        (lobos schema analyzer compiler core)
        (lobos.backends h2 postgresql)))

;;;; DB connection specifications

(def h2-spec
  {:classname "org.h2.Driver"
   :subprotocol "h2"
   :subname "./lobos"})

(def postgresql-spec
  {:classname   "org.postgresql.Driver"
   :subprotocol "postgresql"
   :user        "lobos"
   :password    "lobos"
   :subname     "//localhost:5432/lobos"})

(def db-specs [h2-spec
               postgresql-spec])


(defn driver-available? [db-spec]
  (try
    (clojure.lang.RT/classForName (:classname db-spec))
    true
    (catch ClassNotFoundException e false)))

(def available-specs (filter driver-available? db-specs))

(def *db-spec* nil)

;;;; Helpers

(defmacro def-db-test [name & body]
  `(do ~@(for [db-spec available-specs]
           `(deftest ~(symbol (str name "-" (:subprotocol db-spec)))
              (binding [*db-spec* ~db-spec]
                ~@body)))))

(defmacro with-schema [[var-name sname] & body]
  `(let [~var-name (create-schema ~sname *db-spec*)]
     ~@body
     (drop-schema ~var-name)))

;;;; Fixtures

(def tmp-files-ext '(db))

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

(use-fixtures :once remove-tmp-files-fixture)

;;;; Tests

(def-db-test test-create-and-drop-schema
  (with-schema [lobos :lobos]
    (is (= lobos (schema :lobos {:db-spec *db-spec*}))
        "Checking if the schema has been created"))
  (is (nil? (analyze-schema :lobos *db-spec*))
      "Checking if the schema has been dropped"))

(def-db-test test-create-and-drop-table
  (with-schema [lobos :lobos]
    (let [lobos (create lobos (table :foo))]
      (is (= (-> lobos :elements :foo) (table :foo))
          "Checking if the table has been created"))
    (let [lobos (drop lobos (table :foo))]
      (is (nil? (-> lobos :elements :foo))
          "Checking if the table has been dropped"))))
