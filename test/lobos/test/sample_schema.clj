;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.sample-schema
  (:refer-clojure :exclude [alter compile drop
                            bigint boolean char double float time])
  (:use (lobos core schema)))

(defn surrogate-key [table]
  (integer table :id :auto-inc :primary-key))

(defn datetime-tracked [table]
  (-> table
      (timestamp :updated_on)
      (timestamp :created_on (default (now)))))

(defn refer-to [table ptable]
  (let [cname (-> (->> ptable name butlast (apply str))
                  (str "_id")
                  keyword)]
    (integer table cname [:refer ptable :id :on-delete :set-null])))

(defmacro tbl [name & elements]
  `(-> (table ~name
         (surrogate-key)
         (datetime-tracked))
       ~@elements))

(defschema sample-schema :lobos
  
  (tbl :users
    (varchar :name 100 :unique)
    (check :name (> (length :name) 1)))

  (tbl :posts
    (varchar :title 200 :unique)
    (text :content)
    (refer-to :users))

  (tbl :comments
    (text :content)
    (refer-to :users)
    (refer-to :posts)))
