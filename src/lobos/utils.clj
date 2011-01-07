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
  (:use (clojure [string :only [lower-case
                                replace
                                upper-case]])))

(defn join [separator coll]
  (clojure.string/join separator (filter identity coll)))

(defn as-str ; taken from clojure.contrib.string
  "Like clojure.core/str, but if an argument is a keyword or symbol,
  its name will be used instead of its literal representation."
  ([] "")
  ([x] (if (instance? clojure.lang.Named x)
         (name x)
         (str x)))
  ([x & ys]
     ((fn [^StringBuilder sb more]
        (if more
          (recur (. sb (append (as-str (first more)))) (next more))
          (str sb)))
      (new StringBuilder ^String (as-str x)) ys)))

(defn as-list
  "Returns the given collection parenthesized string with its items
  separated by commas. Apply as-str to coll items."
  [coll]
  (when (not-empty coll)
    (format "(%s)" (join ", " (map as-str coll)))))

(defn as-sql-keyword
  "Returns the given string, symbol or keyword as an upper-cased string
  and replace dashes with spaces."
  [s]
  (when s (replace (-> s as-str upper-case) \- \space)))

(defn as-keyword [s]
  (-> s lower-case (replace #"[_ ]" "-") keyword))

(defn make-constraint-name [table ctype columns]
  (keyword
   (replace (join "_" (conj (map name columns)
                            (name ctype)
                            (-> table :name name)))
             \- \_)))

(defn optional [pred? args]
  (if (pred? (first args))
    [(first args) (next args)]
    [nil args]))
