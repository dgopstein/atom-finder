(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTArraySubscriptExpression IBasicType IQualifierType))

(defn basic-expr-type? [node]
  (try
    (let [expr-type (.getExpressionType node)]
      (or (instance? IBasicType expr-type)
          ;; Types like 'const int' should be inspected to see their base expr-type
          (and (instance? IQualifierType expr-type)
               (instance? IBasicType (.getType expr-type)))))
    ;; Triggered by gcc/gcc/config/i386/i386.c [219 2 19 1 0 0 0 0]
    (catch java.lang.ArrayIndexOutOfBoundsException e
      (do
        ;;(println "Error getting expr-type for node")
        false))))

(defn reversed-subscript-atom?
  "Is this node a reversed-subscript-atom?"
  [node]
  (and (instance? IASTArraySubscriptExpression node)
       ;;the first and second children are the expressions outside and inside the brackets, respectively
       ;;IBasicType covers expr-types that can be used as indices
       (basic-expr-type? (first (children node)))
       (not (basic-expr-type? (last (children node))))))
