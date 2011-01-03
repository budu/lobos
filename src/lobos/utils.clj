;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.utils
  "Helpers used in unrelated namespaces.")

(defn dtypes-replace
  "Replaces the given data-type keyword if there's an alias found in
  the specified aliases."
  [aliases dtype]
  (if (contains? aliases dtype)
    (aliases dtype)
    dtype))
