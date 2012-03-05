;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.utils
  "Helpers used in unrelated namespaces."
  (:refer-clojure :exclude [defonce replace])
  (:use (clojure [string :only [lower-case
                                replace
                                upper-case]]
                 [walk :only [postwalk]])
        (clojure.java [io :only [file]]))
  (:import java.io.File))

(defn join [separator & coll]
  (clojure.string/join separator (filter identity coll)))

(defn join* [separator coll]
  (apply join separator coll))

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
    (format "(%s)" (apply join ", " (map as-str coll)))))

(defn as-sql-keyword
  "Returns the given string, symbol or keyword as an upper-cased string
  and replace dashes with spaces."
  [s]
  (when s (replace (-> s as-str upper-case) \- \space)))

(defn as-keyword [s]
  (when s
    (-> s lower-case (replace #"[_ ]" "-") keyword)))

(defn make-index-name [table type columns]
  (keyword
   (replace (apply join "_" (conj (map name columns)
                                  (name type)
                                  (name (if (keyword? table)
                                          table
                                          (:name table)))))
             \- \_)))

(defn optional [pred? args]
  (if (pred? (first args))
    [(first args) (next args)]
    [nil args]))

(defn conj-when
  "Like conj but if test is false returns coll untouched."
  [coll test x & xs]
  (if test
    (apply conj coll x xs)
    coll))

(defn capture-keywords
  "Returns a set containing all keywords found in the given form as
  strings."
  [form]
  (let [acc (atom #{})
        f #(if (keyword? %2) (conj %1 (name %2)) %1)]
    (postwalk #(swap! acc f %) form)
    @acc))

(defmacro defonce
  "Same as defonce but with an optional docstring."
  ([name expr]
     (list 'clojure.core/defonce
           name
           expr))
  ([name expr doc]
     (list 'clojure.core/defonce
           (with-meta name (assoc (meta name) :doc doc))
           expr)))

(defn only [items from]
  (filter #((set items) %) from))

(defn exclude [items from]
  (filter #(not ((set items) %)) from))

(defn remove-nil-values [m]
  (apply dissoc m (for [[k v] m :when (nil? v)] k)))

(defmacro when->> ; inspired by pallet.thread-expr
  [condition & body]
  (let [arg (last body)
        body (butlast body)]
    `(let [arg# ~arg]
       (if ~condition
         (->> arg# ~@body)
         arg#))))

(defn make-parents ;; fixed from java.io
  [f & more]
  (let [parents (.getParentFile ^File (apply file f more))]
    (when parents
      (.mkdirs parents))))

(defn update-options
  "Takes an element and merge the given options map to its options
  field. It also discard nil values from the map."
  [element options]
  (when element
    (update-in element [:options]
               merge
               (remove-nil-values options))))

;; Taken from clojure.contrib.seq
(defn separate
  "Returns a vector:
   [ (filter f s), (filter (complement f) s) ]"
  [f s]
  [(filter f s) (filter (complement f) s)])

(defn check-valid-options
  "Throws an IllegalArgumentException when the given option map or set
  contains keys not listed as valid, else returns nil."
  [options & valid-keys]
  (when options
    (let [option-set (apply hash-set (cond (map? options) (keys options)
                                           (coll? options) options
                                           :else [options]))]
      (when-let [invalid-keys (seq (apply disj option-set valid-keys))]
        (throw
         (IllegalArgumentException.
          (format "%s are invalid, only %s options are valid."
                  (str invalid-keys)
                  (str valid-keys))))))))
