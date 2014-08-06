; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.connectivity
  "A set of connectivity functions."
  (:refer-clojure :exclude [defonce])
  (:use lobos.utils))

(try
  (require 'lobos.connectivity.jdbc-1)
  (catch Exception e
    (try
      (require 'lobos.connectivity.jdbc-2)
      (catch Exception e
        (do
          (ns-unalias 'lobos.connectivity 'sqlint)
          (require 'lobos.connectivity.jdbc-3))))))

;; -----------------------------------------------------------------------------

;; ## Globals

(defonce global-connections
  (atom {})
  "This atom contains a map of all opened global connections.")

;; -----------------------------------------------------------------------------

;; ## Helpers

(def find-connection sqlint/find-connection*)

(def connection sqlint/connection*)

(defn jdbc-db-spec [] (:db-spec sqlint/*db*))

(defn get-db-spec
  "Returns the associated db-spec or itself. *For internal use*."
  [& [connection-info]]
  (let [connection-info (or connection-info :default-connection)]
    (or (jdbc-db-spec)
        (if (keyword? connection-info)
          (-> @global-connections connection-info :db-spec)
          connection-info))))

(defn ^{:dynamic true} *get-cnx*
  "Replaces `get-connection` from `contrib.sql.internal`
  namespace. Dissociates the `:schema` key to prevent conflict."
  [db-spec]
  (let [db-spec (dissoc db-spec :schema)]
    (sqlint/get-connection db-spec)))

(defn connection?
  "Checks if the given argument is a named connection or a db-spec. As a
  db-spec is just a map, any map return true. *For internal use*."
  [cnx]
  (or ((set (keys @global-connections)) cnx)
      (map? cnx)))

;; -----------------------------------------------------------------------------

;; ## Global Connections

(defn close-global
  "Supplied with a keyword identifying a global connection, that
  connection is closed and the reference dropped. If a truthful silent
  argument is supplied, don't throw an execption if there's no such
  connection."
  [& [connection-name silent]]
  (let [connection-name (or connection-name :default-connection)
        cnx (connection-name @global-connections)]
    (if cnx
      (do
        (.close (:connection cnx))
        (swap! global-connections dissoc connection-name)
        true)
      (when-not silent
        (throw
         (Exception. (format "No global connection by that name is open: %s"
                             connection-name)))))))

(defn- open-global* [connection-name db-spec]
  (let [cnx (*get-cnx* db-spec)]
    (when-let [ac (-> db-spec :auto-commit)]
      (.setAutoCommit cnx ac))
    (swap! global-connections assoc
           (or connection-name :default-connection)
           {:connection cnx :db-spec db-spec})))

(defn open-global
  "Opens a global connection to the database specified by `db-spec`. If
  no `connection-name` is given, opens a default global connection.

  If a global connection by that name already exists and the db-spec is
  safe (see below), then an exeption will be thrown. When the db-spec is
  unsafe it will be closed if the old db-spec is different and the
  original connection is left untouched.

  A safe db-spec is a map that does not contain an :unsafe key set to a
  truthful value."
  ([db-spec] (open-global :default-connection db-spec))
  ([connection-name db-spec]
     (if-let [cnx (connection-name @global-connections)]
       (if (:unsafe (:db-spec cnx))
         (when-not (= (:db-spec cnx) db-spec)
           (close-global connection-name)
           (open-global* connection-name db-spec))
         (throw
          (Exception.
           (format "A global connection by that name already exists (%s)"
                   connection-name))))
       (open-global* connection-name db-spec))))

;; -----------------------------------------------------------------------------

;; ## With Connections

(defn with-named-connection
  "Evaluates func in the context of a named global connection to a
  database."
  [connection-name func]
  (io!
   (if-let [cnx (@global-connections connection-name)]
     (binding [sqlint/*db*
               (assoc sqlint/*db*
                 :connection (:connection cnx)
                 :level 0
                 :rollback (atom false)
                 :db-spec (:db-spec cnx))]
       (func))
     (throw
      (Exception.
       (format "No such global connection currently open: %s, only got %s"
               connection-name
               (vec (keys @global-connections))))))))

(defn with-spec-connection
  "Evaluates func in the context of a new connection to a database then
  closes the connection."
  [db-spec func]
  (with-open [cnx (*get-cnx* db-spec)]
    (binding [sqlint/*db* (assoc sqlint/*db*
                            :connection cnx
                            :level 0
                            :rollback (atom false)
                            :db-spec db-spec)]
      (when-let [ac (-> db-spec :auto-commit)]
        (.setAutoCommit cnx ac))
      (func))))

(defmacro with-connection
  "Evaluates body in the context of a new connection or a named global
  connection to a database then closes the connection if it's a new
  one. The connection-info parameter can be a keyword denoting a global
  connection or a map containing values for one of the following
  parameter sets:

   *  Factory:
     * `:factory` (required) a function of one argument, a map of params
     * (others) (optional) passed to the factory function in a map

   * DriverManager:
     * `:classname` (required) a String, the jdbc driver class name
     * `:subprotocol` (required) a String, the jdbc subprotocol
     * `:subname` (required) a String, the jdbc subname
     * (others) (optional) passed to the driver as properties.

   * DataSource:
     * `:datasource` (required) a javax.sql.DataSource
     * `:username` (optional) a String
     * `:password` (optional) a String, required if :username is supplied

   * JNDI:
     * `:name` (required) a String or javax.naming.Name
     * `:environment` (optional) a java.util.Map

   * Options (for ClojureQL):
     * `:auto-commit` (optional) a Boolean
     * `:fetch-size`  (optional) an integer"
  [connection-info & body]
  `(let [connection-info# (or ~connection-info :default-connection)]
     ((if (keyword? connection-info#)
        with-named-connection
        with-spec-connection) connection-info# (fn [] ~@body))))

(defn default-connection
  "Returns the default connection if it exists."
  []
  (try
    (with-named-connection :default-connection
      connection)
    (catch Exception _ nil)))
