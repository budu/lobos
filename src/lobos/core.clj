;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.core
  "Main interface to interact with Lobos."
  (require (lobos [connectivity :as conn]
                  [compiler :as compiler]
                  [schema :as schema]))
  (use (clojure [pprint :only [pprint]])))

;;;; Globals

(def *debug* nil)

;;;; Actions

(defn drop-table
  "Builds a drop table statement."
  [tname & [behavior connection-info]]
  (schema/drop (schema/table tname) behavior connection-info))

(defn execute
  "Execute the given statement using the specified connection
  information or the bound one."
  [statement & [connection-info]]
  (let [sql-string (compiler/compile statement)]
    (conn/with-connection connection-info
      (with-open [stmt (.createStatement (conn/connection))]
        (.execute stmt sql-string))))
  nil)

(defn debug
  "Prints useful information on the given action/object combination."
  [action object & [args connection-info level]]
  (let [level (or level *debug* :output)
        ast (when-not (= :schema level)
              (apply action object (conj args connection-info)))]
    (case level
      :output (println (compiler/compile ast))
      :ast (do (println (type ast))
               (pprint ast))
      :schema (do (println (type object))
                  (pprint object)))))
