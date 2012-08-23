(defproject org.clojars.jcrossley3/lobos "1.0.0-SNAPSHOT"
  :description
  "Compatible with both 0.1.x and 0.2.x versions of java.jdbc"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/java.jdbc "0.1.1"]
                 [org.clojure/tools.macro "0.1.1"]]
  :dev-dependencies [[lein-clojars "0.7.0"]
                     [lein-marginalia "0.6.1"]
                     [lein-multi "1.1.0"]
                     [cljss "0.1.1"]
                     [hiccup "0.3.1"]
                     [com.h2database/h2 "1.3.160"]]
  :multi-deps {"1.2" [[org.clojure/clojure "1.2.1"]
                      [org.clojure/java.jdbc "0.1.1"]
                      [org.clojure/tools.macro "0.1.1"]]}
  :jar-exclusions [#"www.clj" #"config.clj" #"migrations.clj"])
