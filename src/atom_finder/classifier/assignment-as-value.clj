(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast
          IASTNode IASTBinaryExpression IASTExpressionList
          IASTExpressionStatement IASTForStatement))

(defn assignment? 
  "Is this node an assignment expression"
  [node]
  (and (instance? IASTBinaryExpression node)
       (contains? #{IASTBinaryExpression/op_assign IASTBinaryExpression/op_binaryAndAssign IASTBinaryExpression/op_binaryOrAssign
                    IASTBinaryExpression/op_binaryXorAssign IASTBinaryExpression/op_divideAssign IASTBinaryExpression/op_minusAssign
                    IASTBinaryExpression/op_moduloAssign IASTBinaryExpression/op_multiplyAssign IASTBinaryExpression/op_plusAssign 
                    IASTBinaryExpression/op_shiftLeftAssign IASTBinaryExpression/op_shiftRightAssign}
             (.getOperator node))))

(defn assignment-as-value-atom? [node]
  (and (not (any-pred? #(% node) [(partial instance? IASTExpressionList)
                                  (partial instance? IASTExpressionStatement)
                                  paren-node?]))
       (->> node
            value-consuming-children
            (map remove-wrappers)
            (any-pred? assignment?))))
