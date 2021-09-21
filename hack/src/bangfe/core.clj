(ns bangfe.core
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(defn -main [& args]
  (let [file (-> (parse-opts args)
                 :arguments
                 first)]
    (println "Starting")
    (println file)))