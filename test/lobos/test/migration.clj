(ns lobos.test.migration
  (:refer-clojure :exclude [alter compile defonce drop
                            bigint boolean char double float time complement])
  (:use clojure.test
        lobos.migration))

(deftest test-complement-for-drop-table-statement
  (is (= '(drop db (table :users (integer :id))) (complement '(create db (table :users (integer :id)))))
      "should generate correct drop table statement"))

(deftest test-complement-for-drop-schema-statement
  (is (= '(drop db (schema :users) :cascade) (complement '(create db (schema :users))))
      "should generate correct drop schema statement with a cascade clause"))