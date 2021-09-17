(ns bangfe.nand-assembler
  (:require [clojure.java.io])
  (:gen-class))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(first (slurp "resources/Add.asm"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))

(with-open [rdr (clojure.java.io/reader "resources/Add.asm")]
  (first (line-seq rdr)))