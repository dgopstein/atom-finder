(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTBinaryExpression IASTConditionalExpression)
        '(org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTConditionalExpression))

(defn non-associative?
  "Returns true if a non-associative operator, returns nil otherwise"
  [node]
  (let [non-associative-list
        #{ ;Binary operations that are non-associative
          IASTBinaryExpression/op_minus IASTBinaryExpression/op_divide IASTBinaryExpression/op_modulo
          IASTBinaryExpression/op_shiftLeft IASTBinaryExpression/op_shiftRight IASTBinaryExpression/op_greaterThan 
          IASTBinaryExpression/op_greaterEqual IASTBinaryExpression/op_lessThan IASTBinaryExpression/op_lessEqual CPPASTConditionalExpression}]
    (if (instance? IASTBinaryExpression node) (contains? non-associative-list (.getOperator node)) (contains? non-associative-list (type node)))))

(defn node-operator-equal?
  "compare types two nodes, if both binary expression then compare their operators"
  [node1 node2]
  (if (and (instance? IASTBinaryExpression node1) (instance? IASTBinaryExpression node2))
    (= (.getOperator node1) (.getOperator node2))
    (= (type node1) (type node2))))


(defn infix-operator-precedence-atom?
  "Is this node a infix-operator-precedence-atom?"
  [node]
  (if (and (not (nil? (get-precedence-level node)))
           (not (assignment-operator? node)))

   (if (non-associative? node)
     ;;Current node is a non-associative operator
     (some #(not (nil? (get-precedence-level %))) (children node))
     ;;Current node is an associative operator
     (some #(and (not (nil? (get-precedence-level %)))
                 (not (node-operator-equal? node %))) (children node)))

   false))
