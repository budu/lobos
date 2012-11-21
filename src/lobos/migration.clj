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
  (:require (clojure.java [jdbc :as sql])
            (lobos [analyzer :as analyzer]
                   [compiler :as compiler]
                   [connectivity :as conn]
                   [schema :as schema]))
  (:use (clojure [walk :only [postwalk]])
        (clojure.java [io :only [file writer]])
        (clojure.tools [macro :only [name-with-attributes]])
        (clojure pprint)
        lobos.internal
        lobos.utils)
  (:import (java.sql Timestamp)
           (java.util Date)))

;; -----------------------------------------------------------------------------

;; ## Globals

(def ^{:dynamic true} *record* :stash)

(def ^{:dynamic true} *stash-file* (file ".lobos_stash.clj"))

(def ^{:dynamic true} *src-directory* "src/")

(def ^{:dynamic true} *migrations-namespace* 'lobos.migrations)

(def ^{:dynamic true} *config-namespace* 'lobos.config)

(def ^{:dynamic true} *migrations-table* :lobos_migrations)

(def ^{:dynamic true} *reload-migrations* true)

;; -----------------------------------------------------------------------------

;; ## Action Complement

(defn reverse-rename [form]
  (postwalk
   #(if (and (seq? %) (= 'column (first %)))
      (let [[elem from _ to] %]
        `(~elem ~to :to ~from))
      %)
   form))

(defmulti complement
  "Returns the complementary action to be use in the down part of a
  migration or nil."
  first)

(defmethod complement
  'create
  [action]
  (let [not-cons-or-seq? #(not (or (seq? %)
                                   (isa? (type %) clojure.lang.Cons)))
        args (->> (rest action)
                  (map #(if (seq? %)
                          (filter not-cons-or-seq? %)
                          %)))]
    (if (= 'schema (-> action reverse first first))
      (concat ['drop] args [:cascade])
      (apply list 'drop args))))

(defmethod complement
  'alter
  [action]
  (let [[element subaction cnx-or-schema] (reverse (rest action))]
    (filter
     identity
     (case subaction
           :add (list 'alter cnx-or-schema :drop element)
           :rename (list 'alter cnx-or-schema :rename
                         (reverse-rename element))
           nil))))

(defmethod complement :default [action]
  (println
   (format "WARNING: complement of %s not supported"
           (first action))))

;; -----------------------------------------------------------------------------

;; ## Migration Protocol

(def ^{:doc "Used to agregate migrations in order of definition,
  *For internal use*."}
  migrations
  (atom []))

(defprotocol Migration
  "The migration protocol is meant to be reified into a single migration
  unit. See the defmigration macro, *For internal use*."
  (up [_])
  (down [_]))

(defmacro defmigration
  "Defines a migration to be used by the migration commands. The code
   contained inside the up section is used to modify a database using
   Lobos' actions, while the down section revert those changes.

    (defmigration create-users
      (up (create (table :users (integer :id :primary-key))))
      (down (drop (table :users (integer :id :primary-key)))))"
  {:arglists '([name doc-string? attr-map? & bodies])}
  [name & args]
  (let [prepare-body #(conj (rest %) 'do)
        [name args] (name-with-attributes name args)
        [migrate-up migrate-down] args]
    `(do
       (def ~name
         (with-meta
           (reify Migration
                  (up [_] ~(prepare-body migrate-up))
                  (down [_] ~(prepare-body migrate-down)))
           (.meta #'~name)))
       (swap! migrations conj ~name))))

;; -----------------------------------------------------------------------------

;; ### File Helpers

(defn- append [file content]
  (make-parents file)
  (with-open [wtr (writer file :append true)]
    (.write wtr "\n")
    (pprint content wtr)))

(defn ns-file [ns]
  (file *src-directory*
        (-> (str ns)
            (.replace "." "/")
            (str ".clj"))))

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
  (ns-file *migrations-namespace*))

(defn create-mfile []
  (append
   (migrations-file)
   `(~'ns ~*migrations-namespace*
      (:refer-clojure :exclude [~'alter ~'defonce ~'drop
                                ~'bigint ~'boolean ~'char
                                ~'double ~'float ~'time])
      (:use (~'lobos [~'migration :only [~'defmigration]]
                     ~'core ~'schema)
            ~*config-namespace*))))

(defn append-to-mfile [name msg up & [down]]
  (when-not (.exists (migrations-file))
    (create-mfile))
  (append
   (migrations-file)
   `(~'defmigration ~name ~@(when msg [msg])
      (~'up [] ~@up)
      ~@(when down
          [`(~'down [] ~@down)]))))

(defn list-migrations []
  (if *reload-migrations*
    (when (.exists (migrations-file))
      (swap! migrations (constantly []))
      (use :reload *migrations-namespace*)
      @migrations)
    @migrations))

;; ### Migrations Table Helpers

(defn create-migrations-table
  [db-spec sname]
  (autorequire-backend db-spec)
  (when-not (-> (analyzer/analyze-schema db-spec sname)
                :tables
                *migrations-table*)
    (let [action (schema/table *migrations-table*
                               (schema/varchar :name 255))
          db-spec (assoc db-spec :schema sname)
          create-stmt (schema/build-create-statement action db-spec)]
      (execute create-stmt db-spec))))

(defn insert-migrations
  [db-spec sname & names]
  (when-not (empty? names)
    (sql/with-connection db-spec
      (apply
       sql/insert-rows
       (compiler/as-identifier db-spec *migrations-table* sname)
       (map (comp vector str) names)))))

(defn delete-migrations
  [db-spec sname & names]
  (when-not (empty? names)
    (conn/with-connection db-spec
      (delete db-spec sname *migrations-table*
              (in :name (vec (map str names)))))))

;; ### Commands Helpers

(defn record [action]
  (when *record*
    (append-to-stash-file action)))

(defn list-migrations-names []
  (map #(-> % meta :name str) (list-migrations)))

(defn query-migrations-table
  [db-spec sname]
  (conn/with-connection db-spec
    (map :name (query db-spec sname *migrations-table*))))

(defn pending-migrations [db-spec sname]
  (exclude (query-migrations-table db-spec
                                   sname)
           (list-migrations-names)))

(defn do-migrations [db-spec sname with names & [silent]]
  (let [filter-migs #(only % (list-migrations-names))
        migrations (->> names
                        (map str)
                        filter-migs
                        (when->> (= with :down) reverse)
                        (map symbol)
                        (map (partial ns-resolve *migrations-namespace*))
                        (map var-get))]
    (binding [*record* nil]
      (doseq [migration migrations]
        (let [name (-> migration meta :name)]
          (when-not silent
            (println name))
          (if (= with :up)
            (do
              (up migration)
              (insert-migrations db-spec sname name))
            (do
              (down migration)
              (delete-migrations db-spec sname name))))))))

(defn generate-migration* [db-spec sname name msg actions]
  (append-to-mfile name msg
                   actions
                   (->> actions
                        (map complement)
                        (filter identity)
                        seq))
  (when-not (empty? actions)
    (insert-migrations db-spec sname name)))
