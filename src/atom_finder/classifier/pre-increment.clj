(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast
          IASTNode IASTBinaryExpression IASTExpressionList
          IASTExpressionStatement IASTForStatement IASTUnaryExpression))

(defmulti pre-*crement? "Is this node a pre-*crement expression" class)
(s/defmethod pre-*crement? :default :- s/Bool [node] false)
(s/defmethod pre-*crement? IASTUnaryExpression :- s/Bool
  [node]
  (contains? #{IASTUnaryExpression/op_prefixDecr
               IASTUnaryExpression/op_prefixIncr}
             (.getOperator node)))

(defn pre-*crement-atom? [node]
  ; certain nodes can never "use" the value of an expression
  (and (not (any-pred? #(% node) [(partial instance? IASTExpressionList)
                                  (partial instance? IASTExpressionStatement)
                                  paren-node?]))
       (->> node
            value-consuming-children
            (map remove-wrappers)
            (any-pred? pre-*crement?)
            )))
