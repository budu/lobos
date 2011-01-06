;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.connectivity
  (:use clojure.test
        lobos.connectivity))

(def *cnx*)

(defn cnx-fixture [f]
  (binding [*cnx* (reify java.sql.Connection (close [this] nil))
            clojure.contrib.sql.internal/get-connection (fn [& _] *cnx*)]
    (f)))

(use-fixtures :each cnx-fixture)

(deftest test-open-and-close-global
  (open-global {})
  (is (= @global-connections
         {:default-connection {:connection *cnx* :db-spec {}}})
      "Opening a default global connection")
  (close-global)
  (is (= @global-connections {})
      "Closing a default global connection")
  (open-global {} :foo)
  (is (= @global-connections
         {:foo {:connection *cnx* :db-spec {}}})
      "Opening a named global connection")
  (close-global :foo)
  (is (= @global-connections {})
      "Closing a named global connection")
  (open-global {} :foo)
  (is (thrown? Exception
               (open-global nil :foo))
      "Opening a global connection with a existing name")
  (close-global :foo)
  (is (thrown? Exception
               (close-global :foo))
      "Closing an inexistant global connection"))

(deftest test-with-named-connection
  (is (thrown? Exception
               (with-named-connection :test
                 #(identity)))
      "With inexistant named connection")
  (open-global {} :test)
  (is (= (with-named-connection :test
           #(connection))
         *cnx*)
      "With existant named connection")
  (close-global :test))

(deftest test-with-spec-connection
  (is (= (with-spec-connection {}
           #(connection))
         *cnx*)
      "With connection specification"))

(deftest test-with-connection
  (is (= (with-connection {}
           (connection))
         *cnx*)
      "With connection specification")
  (is (thrown? Exception
               (with-connection :test
                 nil))
      "With inexistant named connection")
  (open-global {} :test)
  (is (= (with-connection :test
           (connection))
         *cnx*)
      "With existant named connection")
  (close-global :test))

(deftest test-default-connection
  (open-global {})
  (is (= (default-connection)
         *cnx*)
      "Default connection")
  (close-global))

(deftest test-get-db-spec
  (is (= (get-db-spec {}) {})
      "Get db-spec from db-spec")
  (open-global {})
  (is (= (get-db-spec) {})
      "Get db-spec from global connection")
  (close-global))
