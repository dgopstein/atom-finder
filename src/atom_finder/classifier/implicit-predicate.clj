(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast  IASTIfStatement IASTForStatement IASTWhileStatement IASTDoStatement IASTBinaryExpression))

(def logical-operators #{IASTBinaryExpression/op_equals
                        IASTBinaryExpression/op_greaterEqual
                        IASTBinaryExpression/op_greaterThan
                        IASTBinaryExpression/op_lessThan
                        IASTBinaryExpression/op_lessEqual
                        IASTBinaryExpression/op_logicalAnd
                        IASTBinaryExpression/op_logicalOr
                        IASTBinaryExpression/op_notequals})

(defn implicit-preciate-for-if-statement?
  "Is there an implicit predicate in this if or for statement"
  [node]
  (cond
    (instance? IASTBinaryExpression (.getConditionExpression node))
      (not (contains? logical-operators (.getOperator (.getConditionExpression node))))
    :else true))

(defn implicit-predicate-while-do-loop?
  "Is there an implicit predicate in this do-while loop"
  [node]
  (cond
    (instance? IASTBinaryExpression (.getCondition node))
      (not (contains? logical-operators (.getOperator (.getCondition node))))
    :else true))

(defn implicit-predicate-atom?
  "Does this AST node have an implicit predicate atom"
  [node]
  (cond
   (leaf? node) nil
   (instance? IASTForStatement node) (implicit-preciate-for-if-statement? node)
   (instance? IASTIfStatement node) (implicit-preciate-for-if-statement? node)
   (instance? IASTWhileStatement node) (implicit-predicate-while-do-loop? node)
   (instance? IASTDoStatement node) (implicit-predicate-while-do-loop? node)
   :else false))