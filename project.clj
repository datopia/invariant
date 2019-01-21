(defproject invariant "0.1.0-SNAPSHOT"
  :description "Invariant verification in datalog."
  :url "https://github.com/datopia/invariant"
  :license {:name "MIT Licence"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [io.replikativ/datahike "0.1.3"]]
  :plugins [[lein-marginalia "0.9.1"]]

  :profiles {:dev {:dependencies [[com.datomic/datomic-free "0.9.5697"]]}})
