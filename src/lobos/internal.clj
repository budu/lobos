; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.internal
  (:refer-clojure :exclude [defonce])
  (:require (lobos [compiler :as compiler]
                   [connectivity :as conn]
                   [metadata :as metadata]
                   [schema :as schema]))
  (:use (clojure.tools [macro :only [name-with-attributes]])
        lobos.utils))

(defonce debug-level
  (atom nil)
  "This atom keeps the currently set debug level, see
  `set-debug-level`. *For internal use*.")

;; -----------------------------------------------------------------------------

;; ## Statement Execution Helpers

(defn autorequire-backend
  "Automatically require the backend for the given connection-info."
  [connection-info]
  (if (:subprotocol connection-info)
    (->> connection-info
         :subprotocol
         (str "lobos.backends.")
         symbol
         require)
    (throw (Exception. "The :subprotocol key is missing from db-spec."))))

(defn- execute*
  "Execute the given SQL string or sequence of strings. Prints them if
  the `debug-level` is set to `:sql`."
  [sql]
  (doseq [sql-string (if (seq? sql) sql [sql])]
    (when (= :sql @debug-level) (println sql-string))
    (with-open [stmt (.createStatement (conn/connection))]
      (.execute stmt sql-string))))

(defn execute
  "Executes the given statement(s) using the specified connection
  information, the bound one or the default connection. It will executes
  an extra *mode* statement if defined by the backend compiler. *For
  internal purpose*."
  [statements & [connection-info]]
  (let [statements (if (seq? statements)
                     statements
                     [statements])
        db-spec (conn/get-db-spec connection-info)
        mode (compiler/compile (compiler/mode db-spec))]
    (conn/with-connection connection-info
      (autorequire-backend connection-info)
      (when mode (execute* mode))
      (doseq [statement statements]
        (let [sql (if (string? statement)
                           statement
                           (compiler/compile statement))]
          (when sql (execute* sql)))))) nil)

;; -----------------------------------------------------------------------------

;; ## Optional Arguments Helpers

(defn optional-cnx-or-schema [args]
  (let [[cnx-or-schema args]
        (optional #(or (schema/schema? %)
                       (and (-> % schema/definition? not)
                            (conn/connection? %))) args)
        args* args
        schema (when (schema/schema? cnx-or-schema) cnx-or-schema)
        cnx (or (conn/find-connection)
                (-> schema :options :db-spec)
                (when-not schema cnx-or-schema)
                :default-connection)
        db-spec (merge (conn/get-db-spec cnx)
                       (when schema
                         {:schema (-> schema :sname name)}))]
    [db-spec schema args]))

(defn optional-cnx-and-sname [args]
  (let [[db-spec schema args] (optional-cnx-or-schema args)
        [sname args] (conn/with-connection db-spec
                       (optional #(or (nil? %)
                                      ((set (metadata/schemas)) %)) args))
        sname (or sname (:name schema))]
    [db-spec sname args]))

;; -----------------------------------------------------------------------------

;; ## DML Helpers

(defn raw-query [sql-string]
  (with-open [stmt (.createStatement (conn/connection))]
    (let [resultset (.executeQuery stmt sql-string)]
      (when resultset
        (doall (resultset-seq resultset))))))

(defn raw-update [sql-string]
  (with-open [stmt (.createStatement (conn/connection))]
    (.executeUpdate stmt sql-string)))

(defn sql-from-where [db-spec stmt sname tname where-expr]
  (do
    (require (symbol (str "lobos.backends."
                          (:subprotocol db-spec))))
      (str stmt
           " from "
           (compiler/as-identifier db-spec tname sname)
           (when where-expr
             (str
              " where "
              (compiler/compile
               (schema/build-definition where-expr db-spec)))))))

(defmacro query [db-spec sname tname & [conditions]]
  `(raw-query
    (sql-from-where ~db-spec "select *" ~sname ~tname
                    (when-not ~(nil? conditions)
                      (schema/expression ~conditions)))))

(defmacro delete [db-spec sname tname & [conditions]]
  `(raw-update
    (sql-from-where ~db-spec "delete" ~sname ~tname
                    (when-not ~(nil? conditions)
                      (schema/expression ~conditions)))))
