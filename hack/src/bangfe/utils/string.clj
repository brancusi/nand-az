(ns bangfe.utils.string
  (:require [clojure.java.io]
            [clojure.string :refer [trim]]
            [tupelo.string :as ts]))

(defn strip-whitespace
  [lines]
  (->> lines
       (map (fn [line] (clojure.string/replace line #"//.+" "")))
       (map trim)
       (filter seq)))

