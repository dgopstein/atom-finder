(in-ns 'atom-finder.classifier)

(defn assignment-as-value-atom? [node]
  (and (not (any-pred? #(% node) [(partial instance? IASTExpressionList)
                                  (partial instance? IASTExpressionStatement)
                                  paren-node?]))
       (->> node
            value-consuming-children
            (map remove-wrappers)
            (any-pred? assignment?))))
