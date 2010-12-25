;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.connectivity
  (:use [lobos.connectivity] :reload)
  (:use [clojure.test]))

(deftest test-open-and-close-global
  (is (thrown? IllegalArgumentException
               (open-global nil))
      "Invalid db-spec")
  (let [cnx (reify java.sql.Connection (close [this] nil))]
    (binding [clojure.contrib.sql.internal/get-connection (fn [& _] cnx)]
      (open-global :stub)
      (is (= @global-connections
             {:default-connection {:connection cnx :db-spec :stub}})
          "Opening a default global connection")
      (close-global)
      (is (= @global-connections {})
          "Closing a default global connection")
      (open-global :stub :foo)
      (is (= @global-connections
             {:foo {:connection cnx :db-spec :stub}})
          "Opening a named global connection")
      (close-global :foo)
      (is (= @global-connections {})
          "Closing a named global connection")
      (open-global :stub :foo)
      (is (thrown? Exception
                   (open-global nil :foo))
          "Opening a global connection with a existing name")
      (close-global :foo)
      (is (thrown? Exception
                   (close-global :foo))
          "Closing an inexistant global connection"))))
