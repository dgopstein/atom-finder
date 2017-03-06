(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTUnaryExpression
           IASTForStatement IASTExpressionStatement))

(def u-ops-pre 
  #{ ;prefix decrement and increment operators
     IASTUnaryExpression/op_prefixDecr       IASTUnaryExpression/op_prefixIncr})

(declare increment-decrement-atoms increment-decrement-atom?)


(defn pre-increment-decrement-atom?
  "Does this AST node an statement that contains decrement and increment operators?"
  [node]
  (increment-decrement-atom? u-ops-pre node))
                                               

(defn pre-increment-decrement-atoms
  "Return all instances of confusing decrement and increment operators in an AST"
  [node]
  (increment-decrement-atoms u-ops-pre node))


;;
;; BELOW ARE THE MAIN FUNCTIONS FOR INCREMENT DECREMENT CLASSIFIERS
;;


(defn increment-decrement?
  "Is this oprator post decrement or increment?"
  [u-ops node]
  (and (instance? IASTUnaryExpression node) (contains? u-ops (.getOperator node))))


(defn increment-decrement-atom?
  "Does this AST node an statement that contains decrement and increment operators?"
  [u-ops node]
  (let [statement #{;statement types that might not cause confusion
                    IASTForStatement IASTExpressionStatement IASTConditionalExpression}]
      (and
        (not-any? #(instance? % node) statement)
        (if(some true? (map (partial increment-decrement? u-ops) (children node))) true false))))


(defn increment-decrement-atoms
  "Main function for all increment and decrement atom finder"
  [u-ops node]
  (cond
    (leaf? node) nil
    (increment-decrement-atom? u-ops node) [node]
    :else (mapcat (partial increment-decrement-atoms u-ops) (children node))))
