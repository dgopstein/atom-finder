
(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast  IASTExpressionStatement IASTNullStatement IASTIfStatement IASTForStatement IASTWhileStatement IASTDoStatement IASTSwitchStatement IASTCaseStatement IASTDefaultStatement)
        '(org.eclipse.cdt.core.dom.ast.cpp ICPPASTRangeBasedForStatement))

(defn omitted-curly-brace?
  "Is the optional curly brace for this statement node be omitted?"
  [node]
  (or (instance? IASTNullStatement node) (instance? IASTExpressionStatement node)))

(defn curly-braces-atom?
  "Does this AST node an statement that should be followed by curly braces?"
  [node]
  (let [omittable-types [IASTForStatement IASTIfStatement IASTWhileStatement IASTDoStatement  ICPPASTRangeBasedForStatement]]
  (cond
   (some #(instance? % node) omittable-types)
      (some #(omitted-curly-brace? %) (children node))

   (instance? IASTSwitchStatement node) (not(->> node
                                              ((partial get-in-tree [1]))
                                              .getSyntax
                                              .toString
                                              ((partial = "{"))))
   :else false)))

