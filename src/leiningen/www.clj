; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns leiningen.www
  "Creates this project website."
  (:use (hiccup core page-helpers)
        cljss.core))

(defmacro defpage [pname [title & [filename]] & body]
  `(defn ~pname []
     {:filename ~(or filename (name pname))
      :title ~title
      :content (html [:div {:id ~(name pname)} ~@body])}))

(load-file "www/src/www.clj")

(defn www
  "Creates this project website."
  [project & args]
  (doseq [page pages]
    (let [{:keys [filename title content]} (page)]
      (spit (str "www/" filename ".html")
            (page-layout title content)))))
