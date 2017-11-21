(in-ns 'atom-finder.classifier)

(defn used-as-rvalue?
  [f node]
  ; certain nodes can never "use" the value of an expression
  (and (not (any-pred? #(% node) [(partial instance? IASTExpressionList)
                                  (partial instance? IASTExpressionStatement)
                                  paren-node?]))
       (->> node
            value-consuming-children
            (map remove-wrappers)
            (any-pred? f)
            )))

(defn assignment-as-value-atom? [node] (used-as-rvalue? assignment? node))
