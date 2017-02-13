(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTArraySubscriptExpression IBasicType))

(defn reversed-subscript-atom?
  "Is this node a reversed-subscript-atom?"
  [node] 
  (and (instance? IASTArraySubscriptExpression node)
       ;;the first child will be the element to the left of the squared bracket
       ;;.getExpressionType returns the type that the expression evaluates to
       ;;IBasicType covers types that can be used as indices
       (instance? IBasicType (.getExpressionType (first (children node))))))
