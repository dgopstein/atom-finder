(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTUnaryExpression))


(defn decrement-increment?
  "Is this oprator decrement or increment?"
  [node]
  (let [u-ops #{ ; all decrement or increment operators
                IASTUnaryExpression/op_postFixDecr      IASTUnaryExpression/op_postFixIncr
                IASTUnaryExpression/op_prefixDecr       IASTUnaryExpression/op_prefixIncr}]
    (and (instance? IASTUnaryExpression node) (contains? u-ops (.getOperator node)))))
  

(defn decrement-increment-in-statement?
  "Does this AST node an statement that contains decrement and increment operators?"
  [node]
  (let [statement #{;statement types that might not cause confusion
                    "ForStatement" "ExpressionStatement"}]
      (cond
        (leaf? node) false
        (not (contains? statement (typename node))) (some true? (map decrement-increment? (children node)))
        :else false)))
          
(def decrement-increment-atom? decrement-increment-in-statement?)
                                               

(defn decrement-increment-atoms
  "Return all instances of confusing decrement and increment operators in an AST"
  [node]
  (cond
    (leaf? node) nil
    (decrement-increment-in-statement? node) [node]
    :else (mapcat decrement-increment-atoms (children node))))
