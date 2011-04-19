(defproject lobos "0.7.0"
  :description
  "A library to create and manipulate SQL database schemas."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [lein-clojars "0.6.0"]
                     [marginalia "0.5.0"]
                     [cljss "0.1.1"]
                     [hiccup "0.3.1"]
                     [clj-help "0.2.0"]
                     [com.h2database/h2 "1.3.154"]]
  :jar-exclusions [#"www.clj"])
