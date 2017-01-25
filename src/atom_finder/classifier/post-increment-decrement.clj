(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTUnaryExpression
           IASTForStatement IASTExpressionStatement IASTConditionalExpression))


(defn post-increment-decrement?
  "Is this oprator post decrement or increment?"
  [node]
  (let [u-ops #{ ; postfix decrement or increment operators
                IASTUnaryExpression/op_postFixDecr       IASTUnaryExpression/op_postFixIncr}]
    (and (instance? IASTUnaryExpression node) (contains? u-ops (.getOperator node)))))
  

(defn post-increment-decrement-atom?
  "Does this AST node an statement that contains decrement and increment operators?"
  [node]
  (let [statement #{;statement types that might not cause confusion
                    IASTForStatement IASTExpressionStatement IASTConditionalExpression}]
      (cond
        (leaf? node) false
        
        (not-any? #(instance? % node) statement)
        (if(some true? (map post-increment-decrement? (children node))) true false)
        
        :else false)))
                                               

(defn post-increment-decrement-atoms
  "Return all instances of confusing decrement and increment operators in an AST"
  [node]
  (cond
    (leaf? node) nil
    (post-increment-decrement-atom? node) [node]
    :else (mapcat post-increment-decrement-atoms (children node))))
