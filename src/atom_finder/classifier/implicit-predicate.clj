(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast  IASTIfStatement IASTForStatement IASTWhileStatement IASTDoStatement IASTBinaryExpression IASTUnaryExpression))

(def implicit-types #{IASTIfStatement IASTForStatement IASTWhileStatement IASTDoStatement IASTConditionalExpression})
                                        ; If statement as an implicit type:  C99 6.8.4.1/1
                                        ; For/While/Do statements as implicit types:  C99 6.8.5/2
                                        ; Ternary operator as an implicit type:  C99 6.5.15/1

(declare implicit-expression?)

(defn remove-parentheses
  [expr]
  (cond
    (= IASTUnaryExpression/op_not (.getOperator expr)) true
    (= IASTUnaryExpression/op_bracketedPrimary (.getOperator expr))
    (not (implicit-expression? (.getOperand expr)))
    :else false))

(defn implicit-expression?
  "Does this expression have a logical operator in it?"
  [expr]
  (not (cond
         (instance? IASTBinaryExpression expr)
         (contains? logical-operators (.getOperator expr))
         (instance? IASTUnaryExpression  expr)
         (remove-parentheses expr)
         :else false)))

(defn implicit-predicate-atom?
  "Does this AST node have an implicit predicate atom"
  [node]
  (cond
    (instance? IASTForStatement node) (implicit-expression? (.getConditionExpression node))
    (instance? IASTIfStatement node) (implicit-expression? (.getConditionExpression node))
    (instance? IASTWhileStatement node) (implicit-expression? (.getCondition node))
    (instance? IASTDoStatement node) (implicit-expression? (.getCondition node))
    (instance? IASTConditionalExpression node) (implicit-expression? (.getLogicalConditionExpression node))
    :else false))
