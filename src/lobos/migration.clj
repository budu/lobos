; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.migration
  "Migrations support."
  (:refer-clojure :exclude [complement defonce replace])
  (:require (clojure.contrib [sql :as sql])
            (lobos [analyzer :as analyzer]
                   [compiler :as compiler]
                   [connectivity :as conn]
                   [schema :as schema]))
  (:use (clojure [walk :only [postwalk]])
        (clojure.java [io :only [file writer]])
        (clojure.contrib [def :only [defvar name-with-attributes]])
        (clojure pprint)
        lobos.internal
        lobos.utils)
  (:import (java.sql Timestamp)
           (java.util Date)))

;; -----------------------------------------------------------------------------

;; ## Globals

(def *record* :stash)

(def *stash-file* (file ".lobos_stash.clj"))

(def *src-directory* "src/")

(def *migrations-namespace* 'lobos.migrations)

(def *config-namespace* 'lobos.config)

(def *migrations-table* :lobos_migrations)

;; -----------------------------------------------------------------------------

;; ## Action Complement

(defn reverse-rename [form]
  (postwalk
   #(if (and (seq? %) (= 'column (first %)))
      (let [[elem from _ to] %]
        `(~elem ~to :to ~from))
      %)
   form))

(defn complement [action]
  (cond
    (= (first action) 'create)
    (if (= 'schema (-> action reverse first first))
      (concat ['drop] (rest action) [:cascade])
      (apply list 'drop (rest action)))
    (= (first action) 'alter)
    (let [[element subaction cnx-or-schema] (reverse (rest action))]
      (filter
       identity
       (case subaction
             :add (list 'alter cnx-or-schema :drop element)
             :rename (list 'alter cnx-or-schema :rename
                           (reverse-rename element))
             nil)))))

;; -----------------------------------------------------------------------------

;; ## Migration Protocol

(defvar migrations
  (atom [])
  "Used to agregate migrations in order of definition, *For internal
  use*.")

(defprotocol Migration
  "The migration protocol is meant to be reified into a single migration
  unit. See the defmigration macro, *For internal use*."
  (up [cnx-or-schema])
  (down [cnx-or-schema]))

(defn prepare-body
  "*For internal use*."
  [body]
  `(do ~@(rest body)))

(defmacro defmigration
  ""
  {:arglists '([name doc-string? attr-map? & bodies])}
  [name & args]
  (let [[name args] (name-with-attributes name args)
        [migrate-up migrate-down] args]
    `(do
       (def ~name
         (with-meta
           (reify Migration
                  (up [cnx-or-schema]
                      ~(prepare-body migrate-up))
                  (down [cnx-or-schema]
                        ~(prepare-body migrate-down)))
           (.meta #'~name)))
       (swap! migrations conj ~name))))

;; -----------------------------------------------------------------------------

;; ### File Helpers

(defn- append [file content]
  (make-parents file)
  (with-open [wtr (writer file :append true)]
    (.write wtr "\n")
    (pprint content wtr)))

;; ### Stash File Helpers

(defn append-to-stash-file [action]
  (append *stash-file* action))

(defn clear-stash-file []
  (when (.exists *stash-file*)
    (spit *stash-file* "")))

(defn read-stash-file []
  (when (.exists *stash-file*)
    (read-string (str \[ (slurp *stash-file*) \]))))

;; ### Migrations File Helpers

(defn migrations-file []
  (file *src-directory*
        (-> (str *migrations-namespace*)
            (.replace "." "/")
            (str ".clj"))))

(defn append-to-mfile [name msg up & [down]]
  (let [mfile (migrations-file)]
    (when-not (.exists (migrations-file))
      (append mfile `(~'ns ~*migrations-namespace*
                       (:refer-clojure :exclude [~'alter ~'defonce ~'drop
                                                 ~'bigint ~'boolean ~'char
                                                 ~'double ~'float ~'time])
                       (:use (~'lobos [~'migration :only [~'defmigration]]
                                      ~'core ~'schema)
                              ~*config-namespace*))))
    (append
     mfile
     `(~'defmigration ~name ~@(when msg [msg])
        (~'up [] ~@up)
        ~@(when down
            [`(~'down [] ~@down)])))))

(defn list-migrations []
  (when (.exists (migrations-file))
    (swap! migrations (constantly []))
    (use :reload *migrations-namespace*)
    @migrations))

;; ### Migrations Table Helpers

(defn create-migrations-table
  [db-spec sname]
  (when-not (-> (analyzer/analyze-schema db-spec sname)
                :elements
                *migrations-table*)
    (let [action (schema/table *migrations-table*
                               (schema/varchar :name 255))
          create-stmt (schema/build-create-statement action db-spec)]
      (execute create-stmt db-spec))))

(defn insert-migrations
  [db-spec sname & names]
  (when-not (empty? names)
    (sql/with-connection db-spec
      (sql/insert-rows
       (compiler/as-identifier db-spec *migrations-table* sname)
       (map str names)))))

(defn delete-migration
  [db-spec sname & names]
  (when-not (empty? names)
    (conn/with-connection db-spec
      (delete db-spec sname *migrations-table*
              (in :name (vec (map str names)))))))

(defn query-migrations-table
  [db-spec sname]
  (conn/with-connection db-spec
    (map :name (query db-spec sname *migrations-table*))))

;; ### Commands Helpers

(defn list-migrations-names []
  (map #(-> % meta :name str) (list-migrations)))

(defn print-migration [name-or-migration]
  (let [migration (if (var? name-or-migration)
                    name-or-migration
                    (->> name-or-migration
                         symbol
                         (ns-resolve *migrations-namespace*)))]
    (if migration
      (do
        (-> migration meta :name println)
        (when-let [doc (-> migration meta :doc)]
          (println "   " doc)))
      (println name-or-migration))))

(defn pending-migrations [db-spec sname]
  (exclude (query-migrations-table db-spec
                                   sname)
           (list-migrations-names)))

(defn do-migrations* [db-spec sname with names]
  (let [migrations (->> names
                        (map str)
                        (only (list-migrations-names))
                        (when->> (= with :down) reverse)
                        (map symbol)
                        (map (partial ns-resolve *migrations-namespace*))
                        (map var-get))]
    (binding [*record* nil]
      (doseq [migration migrations]
        (let [name (-> migration meta :name)]
          (print-migration name)
          (if (= with :up)
            (do
              (up migration)
              (insert-migrations db-spec sname name))
            (do
              (down migration)
              (delete-migration db-spec sname name))))))))

(defn dump* [db-spec sname name msg actions]
  (append-to-mfile name msg
                   actions
                   (->> actions
                        (map complement)
                        (filter identity)
                        seq))
  (insert-migrations db-spec sname name))

(defn record [action]
  (when *record*
    (append-to-stash-file action)))
