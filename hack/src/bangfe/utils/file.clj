(ns bangfe.utils.file
  (:require
   [clojure.string :refer [split]]
   [clojure.java.io :as io]))



(defn to-file
  "Write a lazy seq to a file"
  [seq path]
  (->>
   seq
   (interpose \newline)
   (apply str)
   (spit path)))

(defn is-vm-file?
  [file]
  (let [[_ ext] (split file #"\.")]
    (= ext "vm")))

(defn load-vm-files
  [path]
  (->> (file-seq (clojure.java.io/file path))
       (filter #(.isFile %))
       (map str)
       (filter #(is-vm-file? %))))

(defn load-lines
  [path]
  (let [files (load-vm-files path)
        lines (for [file files]
                (with-open [rdr (clojure.java.io/reader file)]
                  (doall (line-seq rdr))))]
    (-> lines
        flatten
        (vec))))

(defn is-directory?
  [path]
  (.isDirectory (clojure.java.io/file path)))

