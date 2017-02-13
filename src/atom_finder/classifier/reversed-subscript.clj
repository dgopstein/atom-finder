(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTArraySubscriptExpression))

(defn reversed-subscript-atom?
  "Is this node a reversed-subscript-atom?"
  [node] 
  (and (instance? IASTArraySubscriptExpression node)
       (instance? org.eclipse.cdt.core.dom.ast.IBasicType(.getExpressionType (first (children node))))))
