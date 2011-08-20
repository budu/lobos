(defproject lobos "0.8.0-SNAPSHOT"
  :description
  "A library to create and manipulate SQL database schemas."
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.3.2"]
                     [lein-clojars "0.7.0"]
                     [marginalia "0.6.0"]
                     [cljss "0.1.1"]
                     [hiccup "0.3.1"]
                     [com.h2database/h2 "1.3.158"]]
  :jar-exclusions [#"www.clj" #"config.clj" #"migrations.clj"])