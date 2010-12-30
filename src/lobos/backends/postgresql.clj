;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.backends.postgresql
  "Compiler implementation for PostgreSQL."
  (:refer-clojure :exclude [compile])
  (:require (lobos [compiler :as compiler]))
  (:use (clojure [string :only [join]])))

(defmethod compiler/compile [:postgresql lobos.ast.ColumnDefinition]
  [definition]
  (let [{:keys [cname data-type default
                auto-inc not-null others]} definition]
    (join \space
      (concat
       [(compiler/as-str cname)
        (if auto-inc
          "SERIAL"
          (str (compiler/as-sql-keyword (:dtype data-type))
               (compiler/as-list (:args data-type))))]
       (when default  ["DEFAULT" (compiler/compile default)])
       (when not-null ["NOT NULL"])
       others))))
