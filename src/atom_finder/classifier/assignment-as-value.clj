(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast
          IASTNode IASTBinaryExpression IASTExpressionList
          IASTExpressionStatement IASTForStatement))

(defn assignment-as-value-atom? [node]
  (and (not (any-pred? #(% node) [(partial instance? IASTExpressionList)
                                  (partial instance? IASTExpressionStatement)
                                  paren-node?]))
       (->> node
            value-consuming-children
            (map remove-wrappers)
            (any-pred? assignment?))))
