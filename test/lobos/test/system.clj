;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.system
  (:refer-clojure :exclude [compile conj! disj! distinct drop sort take])
  (:require (lobos [connectivity :as conn]))
  (:use clojure.test
        clojureql.core
        (lobos [core :only [create-schema drop-schema]] test)
        (lobos.backends h2 mysql postgresql sqlite sqlserver)
        lobos.test.sample-schema))

;;;; Fixtures

(defn create-schemas-for-all-db []
  (doseq [db (available-global-cnx)]
    (create-schema sample-schema db)))

(defn drop-schemas-for-all-db []
  (doseq [db (available-global-cnx)]
    (try (drop-schema sample-schema :cascade db)
         (catch Exception _))))

(defn use-sample-schema-fixture [f]
  (try (create-schemas-for-all-db)
       (f)
       (finally (drop-schemas-for-all-db))))

(defn open-cql-global-connections-fixture [f]
  (doseq [db (available-global-cnx)]
    (open-global db (conn/get-db-spec db)))
  (f)
  (doseq [db (available-global-cnx)]
    (close-global db)))

(use-fixtures :once
  remove-tmp-files-fixture
  open-global-connections-fixture
  open-cql-global-connections-fixture
  use-sample-schema-fixture)

;;;; Tests

(def-db-test test-check-constraint
  (when-not (= *db* :test-mysql)
    (is (thrown? Exception (conj! (table *db* :lobos.users) {:name "a"})))))
