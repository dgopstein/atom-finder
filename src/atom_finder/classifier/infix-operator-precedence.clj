(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTBinaryExpression IASTConditionalExpression IASTUnaryExpression)
        '(org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTConditionalExpression))

(defn non-associative?
  "Returns true if a non-associative operator, returns nil otherwise"
  [node]
  (let [non-associatives
        #{ ;Binary operations that are non-associative
          IASTBinaryExpression/op_minus IASTBinaryExpression/op_divide IASTBinaryExpression/op_modulo
          IASTBinaryExpression/op_shiftLeft IASTBinaryExpression/op_shiftRight IASTBinaryExpression/op_greaterThan 
          IASTBinaryExpression/op_greaterEqual IASTBinaryExpression/op_lessThan IASTBinaryExpression/op_lessEqual CPPASTConditionalExpression}]
    (contains? non-associatives (if (instance? IASTBinaryExpression node) (.getOperator node) (type node)))))

(defn operator-equal?
  "compare types two nodes, if both binary expression then compare their operators"
  [& nodes]
  (if (or (every? #(instance? IASTBinaryExpression %) nodes) (every? #(instance? IASTUnaryExpression %) nodes))
    (apply = (map (memfn getOperator) nodes))
    (apply = (map type nodes))))

(defn operator?
  "Is this node an operator?"
  [node] (precedence-level node))

(defn infix-operator-precedence-atom?
  "Is this node a infix-operator-precedence-atom?"
  [node]
  (and
   (operator? node)

   (not (assignment? node))

   (if (non-associative? node)

     ;;Current node is a non-associative operator
     (some operator? (children node))

     ;;Current node is an associative operator
     (some #(and (operator? %)
                 (not (operator-equal? node %))) (children node)))))

