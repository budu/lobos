; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.migration
  "Migrations support."
  (:refer-clojure :exclude [defonce replace])
  (:require (clojure.contrib [sql :as sql])
            (lobos [analyzer :as analyzer]
                   [compiler :as compiler]
                   [connectivity :as conn]
                   [schema :as schema]))
  (:use (clojure [string :only [replace]])
        (clojure.java [io :only [file
                                 make-parents
                                 writer]])
        (clojure.contrib [def :only [name-with-attributes]])
        (clojure pprint)
        lobos.internal
        lobos.utils)
  (:import (java.sql Timestamp)
           (java.util Date)))

;; -----------------------------------------------------------------------------

;; ## Globals

(def *record* :stash)

(def *default-directory*
     (replace "lobos/migrations/"
              "/"
              java.io.File/separator))

(def *stash-file* "stash.clj")

(def *migrations-table* :lobos_migrations)

;; -----------------------------------------------------------------------------

;; ## Helpers

(defn- ljust [s n p]
  (apply str s (repeat (- n (count (str s))) p)))

(defn- current-timestamp []
  (Thread/sleep 15)
  (-> (Date.)
      .getTime
      (Timestamp.)
      str
      (replace #"\D" "")
      (ljust 17 \0)))

;; -----------------------------------------------------------------------------

;; ## Helpers

;; ### File Helpers

(defn- append [file content]
  (make-parents file)
  (with-open [wtr (writer file :append true)]
    (.write wtr "\n")
    (pprint content wtr)))

;; ### Stash File Helpers

(defn stash-file []
  (file *default-directory*
        *stash-file*))

(defn append-to-stash-file [action]
  (append (stash-file) action))

(defn clear-stash-file []
  (when (.exists (stash-file))
    (spit (stash-file) "")))

(defn read-stash-file []
  (when (.exists (stash-file))
    (read-string (str \[ (slurp (stash-file)) \]))))

;; ### Migration Files Helpers

(defn list-mfiles []
  (->> *default-directory*
       file
       .listFiles
       (filter #(not= % (stash-file)))
       (sort)))

(defn msg->mfile [& msg]
  (file (str *default-directory*
             (apply join \_ (current-timestamp) msg)
             ".clj")))

(defn action->mfile [action]
  (let [[action & args] action
        [spec-or-schema args]  (optional symbol? args)
        [subaction args] (optional keyword? args)
        [element name] (first args)]
    (msg->mfile
     (or spec-or-schema "default")
     action
     (when subaction (as-str subaction))
     element
     (as-str name))))

(defn append-to-mfile [mfile actions]
  (append mfile `{:do ~actions}))

(defn mfile->version [mfile]
  (->> mfile
       .getName
       (re-find #"^[^_.]*")))

(defn version->migrations [version]
  (let [dir (replace *default-directory* "\\" "\\\\")
        re (re-pattern (str "^" dir version))]
    (->> (list-mfiles)
         (filter #(re-seq re (str %)))
         (map slurp)
         (map read-string)
         (map #(assoc % :version version)))))

;; ### Migrations Table Helpers

(defn create-migrations-table
  [db-spec sname]
  (when-not (-> (analyzer/analyze-schema db-spec sname)
                :elements
                *migrations-table*)
    (let [action (schema/table *migrations-table*
                               (schema/varchar :version 255))
          create-stmt (schema/build-create-statement action db-spec)]
      (execute create-stmt db-spec))))

(defn insert-versions
  [db-spec sname & versions]
  (when-not (empty? versions)
    (sql/with-connection db-spec
      (sql/insert-rows
       (compiler/as-identifier db-spec *migrations-table* sname)
       (map str versions)))))

(defn delete-versions
  [db-spec sname & versions]
  (when-not (empty? versions)
    (conn/with-connection db-spec
      (delete db-spec sname *migrations-table*
              (in :version (vec versions))))))

(defn query-migrations-table
  [db-spec sname]
  (conn/with-connection db-spec
    (map :version (query db-spec sname *migrations-table*))))

;; ### Commands Helpers

(defn pending-versions [db-spec sname]
  (->> (list-mfiles)
       (map mfile->version)
       (exclude (query-migrations-table db-spec
                                        sname))))

(defn do-migrations* [db-spec sname with versions]
  (let [migrations (->> versions
                        (map version->migrations)
                        flatten
                        (sort-by :version)
                        (when->> (= with :undo) reverse))]
    (binding [*record* nil]
      ;; TODO: transaction
      (doseq [migration migrations
              action (with migration)]
        (println (as-str with "ing") (:version migration))
        (eval action)
        ((if (= with :do)
           insert-versions
           delete-versions)
         db-spec sname (:version migration))))))

(defn dump* [db-spec sname mfile actions]
  (append-to-mfile mfile actions)
  (insert-versions db-spec sname (mfile->version mfile)))

(defn record [db-spec sname action]
  (cond
   (= *record* :stash) (append-to-stash-file action)
   (= *record* :auto) (dump* db-spec sname
                             (action->mfile action)
                             [action])))
