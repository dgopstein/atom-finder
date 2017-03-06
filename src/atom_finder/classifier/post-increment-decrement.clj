(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTUnaryExpression
           IASTForStatement IASTExpressionStatement))

(def u-ops-post 
  #{;postfix decrement and increment operators
     IASTUnaryExpression/op_postFixDecr       IASTUnaryExpression/op_postFixIncr})

(declare increment-decrement-atoms increment-decrement-atom?)

(defn post-increment-decrement-atom?
  "Does this AST node an statement that contains decrement and increment operators?"
  [node]
  (increment-decrement-atom? u-ops-post node))

(defn post-increment-decrement-atoms
  "Return all instances of confusing decrement and increment operators in an AST"
  [node]
  (increment-decrement-atoms u-ops-post node))

;;
;;MAIN LOGIC WRITTEN IN PRE-INCREMENT-DECREMENT.CLJ
;;


