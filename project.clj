(defproject lobos "0.7.0-SNAPSHOT"
  :description
  "A library to create and manipulate SQL database schemas."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [lobos/clojureql "1.1.0-SNAPSHOT"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [lein-clojars "0.6.0"]
                     [marginalia "0.3.2"]
                     [cljss "0.1.0"]
                     [hiccup "0.3.1"]
                     [clj-help "0.2.0"]]
  :jar-exclusions [#"www.clj"])
