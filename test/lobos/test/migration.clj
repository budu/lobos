;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.migration
  (:refer-clojure :exclude [complement])
  (:use clojure.test
        lobos.migration))

(deftest test-complement-for-create-action-on-table
  (is (= '(drop db (table :users (integer :id)))
         (complement '(create db (table :users (integer :id)))))
      "Should generate drop table action without cascade clause"))

(deftest test-complement-for-create-action-on-schema
  (is (= '(drop db (schema :users) :cascade)
         (complement '(create db (schema :users))))
      "Should generate drop schema action with a cascade clause"))
