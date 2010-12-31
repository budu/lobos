;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.backends.h2
  "Compiler implementation for H2."
  (:refer-clojure :exclude [compile])
  (:require (lobos [compiler :as compiler]))
  (:use (clojure [string :only [join]])))

(defmethod compiler/compile [:h2 lobos.ast.CreateSchemaStatement]
  [statement]
  (let [{:keys [sname elements]} statement]
    (join ";\n\n"
          (conj (map (comp compiler/compile
                           #(assoc-in % [:db-spec :schema] sname))
                     elements)
                (compiler/as-str "CREATE SCHEMA " sname)))))

(defmethod compiler/compile [:h2 lobos.ast.DropStatement]
  [statement]
  (let [{:keys [db-spec otype oname behavior]} statement]
    (join \space
      (concat
       ["DROP"
        (compiler/as-sql-keyword otype)
        (compiler/as-schema-qualified-name db-spec oname)]
       (when (and behavior (#{:table} otype))
         [(compiler/as-sql-keyword behavior)])))))
