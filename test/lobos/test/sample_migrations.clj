
#_(ns
 lobos.migrations
 (:refer-clojure
  :exclude
  [alter defonce drop bigint boolean char double float time])
 (:use
  (lobos [migration :only [defmigration]] core schema)
  lobos.config))
