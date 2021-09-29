(ns hooks.for-indexed
  (:require [clj-kondo.hooks-api :as api]
            [clojure.edn :as edn]))

(defn for-indexed [{:keys [:node]}]
  (let [[bindings & body] (-> node :children rest)
        [item index coll] (:children bindings)
        new-node (api/list-node
                  (list
                   (api/token-node 'for)
                   (api/vector-node [item coll])
                   (api/list-node
                    (list*
                     (api/token-node 'let)
                     (api/vector-node [index (api/token-node 0)])
                     body))))]
    (let [{:keys [:row :col]} (meta node)]
      (when (not (and item index coll))
        (api/reg-finding! {:message "You are missing arguments, supply item, index, and coll"
                           :type :for-indexed/missing-index
                           :row row
                           :col col})))
    {:node new-node}))
