(defproject io.datopia/invariant "0.1.0-SNAPSHOT"
  :description  "Invariant verification in datalog."
  :url          "https://github.com/datopia/invariant"
  :license      {:name "MIT Licence"
                 :url  "http://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure      "1.10.0"]
                 [org.clojure/tools.reader "1.3.2"]
                 [io.replikativ/datahike   "0.3.7-SNAPSHOT"]
                 [com.taoensso/carmine     "3.1.0"]
                 [com.datomic/datomic-free "0.9.5697"       :scope "provided"]]
  :plugins      [[lein-marginalia "0.9.1"]]
  :profiles     {:dev {:source-paths ["test-support" "dev"]}})
