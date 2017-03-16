
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
  (let [omittable-types [IASTForStatement IASTIfStatement IASTWhileStatement IASTDoStatement  ICPPASTRangeBasedForStatement]
        special-types [IASTCaseStatement IASTDefaultStatement]]
  (cond
   (some #(instance? % node) omittable-types)
      (some #(omitted-curly-brace? %) (children node))

   (instance? IASTSwitchStatement node) (not(->> node
                                              ((partial get-in-tree [1]))
                                              .getSyntax
                                              .toString
                                              ((partial = "{"))))
   (some #(instance? % node) special-types)
      nil
      
   :else false)))

(comment
 (defn curly-braces-atoms
   "Return all instances of omitted curly brace atoms in the node"
   [node]
   (let [child-atoms
         (mapcat (partial curly-braces-atoms) (children node))]
     (cond
      ;;(instance? IASTIfStatement node)?
      (instance? IASTSwitchStatement node) (swtich-curly-brace node child-atoms)
      :else (if (curly-braces-atom? node) 
              (conj child-atoms node)
              child-atoms))))
 )
;default case - 
