(defproject invariant "0.1.0-SNAPSHOT"
  :description "Invariant verification in datalog."
  :url "https://github.com/datopia/invariant"
  :license {:name "MIT Licence"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.reader "1.3.2"]
                 [io.replikativ/datahike "0.2.0-beta"]]
  :plugins [[lein-marginalia "0.9.1"]]

  :profiles {:dev {:dependencies [[com.datomic/datomic-free "0.9.5697"]]}})
