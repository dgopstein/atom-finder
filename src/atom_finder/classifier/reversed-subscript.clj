(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTArraySubscriptExpression))

(defn reversed-subscript-atom?
  "Is this node a reversed-subscript-atom?"
  [node] 
  (and (instance? IASTArraySubscriptExpression node)
       (instance? org.eclipse.cdt.core.dom.ast.IBasicType(.getExpressionType (first (children node))))))

(comment

(->>
  "reversed-subscript.c"
  resource-path
  tu
  (get-in-tree [5 2])
  children
  (filter #(#{"ExpressionStatement"} (typename %)))
  (map (partial get-in-tree [0 2]))
  (map (fn [node]
            [(= "int" (.toString (.getExpressionType (first (children node)))))
             (write-ast node)]))
 )
