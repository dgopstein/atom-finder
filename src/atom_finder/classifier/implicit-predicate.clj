(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast
          IASTIfStatement IASTForStatement IASTWhileStatement
          IASTDoStatement IASTBinaryExpression IASTUnaryExpression)
        '(org.eclipse.cdt.core.dom.ast IBasicType IBasicType$Kind))

(defn condition
  [node]
  ((condp instance? node
     IASTForStatement          (memfn getConditionExpression)
     IASTIfStatement           (memfn getConditionExpression)
     IASTWhileStatement        (memfn getCondition)
     IASTDoStatement           (memfn getCondition)
     IASTConditionalExpression (memfn getLogicalConditionExpression)
                               (constantly nil))
   node))

(defn non-bool-expr?
  [node]
  (when (instance? IASTExpression node)
    (some->> node
             .getExpressionType
             (#(when (instance? IBasicType %1) %1))
             .getKind
             (= IBasicType$Kind/eBoolean)
             not)))

(defn implicit-predicate-atom?
  "Does this AST node have an implicit predicate atom"
  [node]
  (when-let [cond-expr (condition node)]
    (non-bool-expr? cond-expr)))
