(ns invariant.test.util
  (:require [clojure.java.io :as io]))

(def read-resource (comp read-string slurp io/resource))
