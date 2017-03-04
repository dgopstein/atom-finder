(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement))

(defmulti value-used? "Is the value of this expression node used" class)
(s/defmethod value-used? IASTNode :- s/Bool [node] false) ; Only expressions have values
(s/defmethod value-used? IASTExpression :- s/Bool [node]
  )

(s/def value-consumers #{})

(s/defn assignment-as-value-atoms [n] [])

(defmulti post-*crement? "Is this node a post-*crement expression" class)
(s/defmethod post-*crement? :default :- s/Bool [node] false)
(s/defmethod post-*crement? IASTUnaryExpression :- s/Bool
  [node]
  (contains? #{IASTUnaryExpression/op_postFixDecr IASTUnaryExpression/op_postFixIncr} (.getOperator node)))

(defn post-*crement-atom? [node]
  (and (not (instance? IASTExpressionStatement node))
       (->> node
            children
            (any-true? post-*crement?))
  ))
