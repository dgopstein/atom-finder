(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTBinaryExpression))

(defn infix-operator-precedence-atom?
  "Is this node a infix-operator-precedence-atom?"
  [node]
  (let [assign-ops
        #{ ;Binary operation with assignment operator should not be considered atom
          IASTBinaryExpression/op_assign IASTBinaryExpression/op_binaryAndAssign IASTBinaryExpression/op_binaryOrAssign
          IASTBinaryExpression/op_binaryXorAssign IASTBinaryExpression/op_divideAssign IASTBinaryExpression/op_minusAssign
          IASTBinaryExpression/op_moduloAssign IASTBinaryExpression/op_multiplyAssign IASTBinaryExpression/op_plusAssign 
          IASTBinaryExpression/op_shiftLeftAssign IASTBinaryExpression/op_shiftRightAssign}]
   (if (and (instance? IASTBinaryExpression node)
            (not (contains? assign-ops (.getOperator node))))

     (some #(and (instance? IASTBinaryExpression %)
                 (not= (.getOperator node) (.getOperator %)))
           (children node))

     false)))
