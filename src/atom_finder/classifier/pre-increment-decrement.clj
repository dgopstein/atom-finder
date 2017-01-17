(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTUnaryExpression
           IASTForStatement IASTExpressionStatement))


(defn pre-increment-decrement?
  "Is this oprator post decrement or increment?"
  [node]
  (let [u-ops #{ ; prefix decrement and increment operators
                IASTUnaryExpression/op_prefixDecr       IASTUnaryExpression/op_prefixIncr}]
    (and (instance? IASTUnaryExpression node) (contains? u-ops (.getOperator node)))))
  

(defn pre-increment-decrement-atom?
  "Does this AST node an statement that contains decrement and increment operators?"
  [node]
  (let [statement #{;statement types that might not cause confusion
                    IASTForStatement IASTExpressionStatement}]
      (cond
        (leaf? node) false
        
        (not-any? #(instance? % node) statement)
        (some true? (map pre-increment-decrement? (children node)))
        
        :else false)))
                                               

(defn pre-increment-decrement-atoms
  "Return all instances of confusing decrement and increment operators in an AST"
  [node]
  (cond
    (leaf? node) nil
    (pre-increment-decrement-atom? node) [node]
    :else (mapcat pre-increment-decrement-atoms (children node))))
