(ns sc.util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

; Via https://stackoverflow.com/a/26372677
(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn slurp-resource
  "Slurp bytes from a file in project root's resources/"
  [filename]
  (-> filename io/resource slurp-bytes))

(defn in? 
  "Test if a is in coll"
  [coll a] 
  (some #(= a %) coll))

(def not-nil? (complement nil?))
