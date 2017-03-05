(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast
          IASTNode IASTBinaryExpression IASTExpressionList
          IASTExpressionStatement IASTForStatement IASTUnaryExpression))

(defmulti post-*crement? "Is this node a post-*crement expression" class)
(s/defmethod post-*crement? :default :- s/Bool [node] false)
(s/defmethod post-*crement? IASTUnaryExpression :- s/Bool
  [node]
  (contains? #{IASTUnaryExpression/op_postFixDecr
               IASTUnaryExpression/op_postFixIncr}
             (.getOperator node)))

(defn paren-node?
  "Does this node just represent ()'s"
  [node]
  (and (instance? IASTUnaryExpression node)
       (= (.getOperator node) IASTUnaryExpression/op_bracketedPrimary)))

(defn remove-wrappers
  "Drill down the tree past expressions that just return the value of their direct children"
  [node]
  (let [new-node
        (cond 
          ; comma operators only return the value of their second operand
          (instance? IASTExpressionList node) (last (children node))
          ; parenthesis just return their only child
          (paren-node? node) (.getOperand node)
          :else node)]

    (if (= node new-node)
      node
      (remove-wrappers new-node))))

(defn value-consuming-children
  "Return the children of this node who's values will be used. e.g. The first and third clauses of a for loop don't use the values of their expression, but the second one does, so return that one"
  [node]
  (condp instance? node
    IASTForStatement   [(.getConditionExpression node)]
    (children node)))

(defn post-*crement-atom? [node]
  ; certain nodes can never "use" the value of an expression
  (and (not (any-true? #(% node) [(partial instance? IASTExpressionList)
                                  (partial instance? IASTExpressionStatement)
                                  paren-node?]))
       (->> node
            value-consuming-children
            (map remove-wrappers)
            (any-true? post-*crement?)
            )))

(defn post-*crement-not-atom?
  "Is this node a post-*crement that isn't an atom?"
  [node]
  (and (post-*crement? node)
       (not (any-true? post-*crement-atom? (all-parents node)))))

; Check if we miss any instances of the atom:
;  (print-atoms-in-dir
;   (expand-home "~/opt/src/redis")
;   ;(map atom-lookup [:post-increment])
;   [{:name :post-*crement-not-atom, :finder
;     (atom-finder.classifier/default-finder post-*crement-not-atom?)}]
;   )
;(def psgl (read-string (slurp (clojure.java.io/file (ClassLoader/getSystemResource "data/redis_not_post_increment_2017-03-04.edn")))))
;(found-atom-source :post-*crement-not-atom psgl)
