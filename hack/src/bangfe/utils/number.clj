(ns bangfe.utils.number)

(defn sanatize-number
  [n]
  (if (number? n)
    n
    (try
      (Integer/parseInt n)
      (catch Exception e))))

