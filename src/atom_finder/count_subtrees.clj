(ns atom-finder.count-subtrees
  (:require [atom-finder.util :refer :all]))

(defn count-nodes
  "count every syntax node type in a tree"
  [root]
  (let [local-count {(typename root) 1}
        child-count (->> root children (map count-nodes))
        all-count   (conj child-count local-count)]

  (reduce (partial merge-with +) all-count)))
