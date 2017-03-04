(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTForStatement))

(defmulti value-used? "Is the value of this expression node used" class)
(s/defmethod value-used? IASTNode :- s/Bool [node] false) ; Only expressions have values
(s/defmethod value-used? IASTExpression :- s/Bool [node]
  )

(defmulti post-*crement? "Is this node a post-*crement expression" class)
(s/defmethod post-*crement? :default :- s/Bool [node] false)
(s/defmethod post-*crement? IASTUnaryExpression :- s/Bool
  [node]
  (contains? #{IASTUnaryExpression/op_postFixDecr IASTUnaryExpression/op_postFixIncr} (.getOperator node)))

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

(def post-*crement-atoms (default-finder post-*crement-atom?))


(let [node (->>
            ;"for (int i = 0; i < a; i++) ; // <false>"
            "for (int i = 1; i--; a++) ; // <true>"
            parse-stmt)]

         )))
