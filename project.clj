(defproject org.clojars.acidlabs/korma "0.4.0"
  :description "Tasty SQL for Clojure"
  :url "https://github.com/acidlabs/Korma"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [c3p0/c3p0 "0.9.1.2"]
                 [org.clojure/java.jdbc "0.2.2"]]
  :codox {:exclude [korma.sql.engine korma.sql.fns korma.sql.utils]}
  :dev-dependencies [[postgresql "9.0-801.jdbc4"]])
