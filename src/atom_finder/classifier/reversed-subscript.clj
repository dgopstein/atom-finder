(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTArraySubscriptExpression IBasicType IQualifierType))

(defn basic-type? [node]
  (let [type (.getExpressionType node)]
    (or (instance? IBasicType  type)
        ; Types like 'const int' should be inspected to see their base type
        (and (instance? IQualifierType type)
             (instance? IBasicType (.getType type))))))

(defn reversed-subscript-atom?
  "Is this node a reversed-subscript-atom?"
  [node] 
  (and (instance? IASTArraySubscriptExpression node)
       ;;the first and second children are the expressions outside and inside the brackets, respectively
       ;;IBasicType covers types that can be used as indices
       (basic-type? (first (children node)))
       (not (basic-type? (last (children node))))))
