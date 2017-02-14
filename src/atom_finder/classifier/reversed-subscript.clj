(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTArraySubscriptExpression IBasicType))

(defn reversed-subscript-atom?
  "Is this node a reversed-subscript-atom?"
  [node] 
  (and (instance? IASTArraySubscriptExpression node)
       ;;the first and second children are the expressions outside and inside the brackets, respectively
       ;;IBasicType covers types that can be used as indices
       (instance? IBasicType  (.getExpressionType (first (children node))))
       (not (instance? IBasicType (.getExpressionType (last (children node)))))))
