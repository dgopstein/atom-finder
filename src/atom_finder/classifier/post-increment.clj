(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTForStatement))

(defmulti post-*crement? "Is this node a post-*crement expression" class)
(s/defmethod post-*crement? :default :- s/Bool [node] false)
(s/defmethod post-*crement? IASTUnaryExpression :- s/Bool
  [node]
  (contains? #{IASTUnaryExpression/op_postFixDecr
               IASTUnaryExpression/op_postFixIncr}
             (.getOperator node)))

(defn post-*crement-atom? [node]
  (and (not (instance? IASTExpressionStatement node))
       (let [++children (->> node
                             children
                             (filter post-*crement?))]
         (not (empty?
               (if (instance? IASTForStatement node)
                 (remove #{(.getIterationExpression node)} ++children)
                 ++children)))
         )))
