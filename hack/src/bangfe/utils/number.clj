(ns bangfe.utils.number)

(defn sanatize-number
  [n]
  (if (number? n)
    n
    (Integer/parseInt n)))