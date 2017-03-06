
(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast  IASTExpressionStatement IASTNullStatement IASTIfStatement IASTForStatement IASTWhileStatement IASTDoStatement IASTSwitchStatement)
        '(org.eclipse.cdt.core.dom.ast.cpp ICPPASTRangeBasedForStatement))


(defn omitted-curly-brace?
  "Is the optional curly brace for this statement node be omitted?"
  [node]
  (or (instance? IASTNullStatement node) (instance? IASTExpressionStatement node)))

(defn curly-braces-atom?
  "Does this AST node an statement that should be followed by curly braces?"
  [node]
  (let [omittable-types [IASTSwitchStatement IASTIfStatement IASTForStatement IASTWhileStatement IASTDoStatement  ICPPASTRangeBasedForStatement]]
  (and
    (some #(instance? % node) omittable-types)
    (some #(omitted-curly-brace? %) (children node)))))


 ;.getLeadingSyntax - USE WITH CAUTION


;=================================================================
;=================================================================
;=================================================================
;=================================================================
;=================================================================

(comment
(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTDefaultStatement  IASTExpressionStatement IASTNullStatement IASTIfStatement))

(defn omitted-curly-brace?
  "Is the optional curly brace for this statement node be omitted?"
  [node]
  (or (instance? IASTNullStatement node) (instance? IASTExpressionStatement node)))

(defn has-method?
  "Does this node contain the method?"
  [method node]
  (some? (some method (map :name (java-methods node)))))

(defn curly-braces-atom?
  "Does this AST node an statement that should be followed by curly braces?"
  [node]
      (cond
        (has-method? #{'getBody} node)
           (omitted-curly-brace? (.getBody node))
        
        ;if-statement-specific test 
        (has-method? #{'getThenClause} node)
           (omitted-curly-brace? (.getThenClause node))

        ;else-clause-specific test
        (and (instance? IASTIfStatement (.getParent node))(omitted-curly-brace? node))
           (= (.getElseClause (.getParent node)) node) 

        ;Default-and-case-statement-specific test
        :else false)))
