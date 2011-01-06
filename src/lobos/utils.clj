;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.utils
  "Helpers used in unrelated namespaces."
  (:refer-clojure :exclude [replace])
  (:use (clojure [string :only [lower-case]])
        (clojure [string :only [replace]])))

(defn as-keyword [s]
  (-> s lower-case (replace #"[_ ]" "-") keyword))
