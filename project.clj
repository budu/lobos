(defproject lobos "1.0.0-SNAPSHOT"
  :description
  "A library to create and manipulate SQL database schemas."
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/java.jdbc "0.0.7"]
                 [org.clojure/tools.macro "0.1.1"]]
  :dev-dependencies [[lein-clojars "0.7.0"]
                     [marginalia "0.6.0"]
                     [cljss "0.1.1"]
                     [hiccup "0.3.1"]
                     [com.h2database/h2 "1.3.160"]]
  :jar-exclusions [#"www.clj" #"config.clj" #"migrations.clj"])
