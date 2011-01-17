;; Copyright (c) Nicolas Buduroi. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 which can be found in the file
;; epl-v10.html at the root of this distribution. By using this software
;; in any fashion, you are agreeing to be bound by the terms of this
;; license.
;; You must not remove this notice, or any other, from this software.

(ns lobos.test.utils
  (:refer-clojure :exclude [defonce])
  (:use clojure.test
        lobos.utils))

(deftest test-as-str
  (are [s] (= s "foo")
       (as-str :foo)
       (as-str 'foo)
       (as-str "foo"))
  (is (= (as-str :foo 'bar "baz") "foobarbaz")))

(deftest test-as-list
  (is (= (as-list [:foo :bar :baz]) "(foo, bar, baz)")))

(deftest test-as-sql-keyword
  (is (= (as-sql-keyword :foo-bar) "FOO BAR")))
