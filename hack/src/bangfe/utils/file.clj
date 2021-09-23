(ns bangfe.utils.file
  (:require [clojure.java.io :as io]))

(defn to-file
  "Write a lazy seq to a file"
  [seq path]
  (->>
   seq
   (interpose \newline)
   (apply str)
   (spit path)))

(defn load-lines
  [path]
  (with-open [rdr (clojure.java.io/reader path)]
    (doall (line-seq rdr))))