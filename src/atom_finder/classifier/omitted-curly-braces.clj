(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast  IASTExpressionStatement IASTNullStatement IASTIfStatement IASTForStatement IASTWhileStatement IASTDoStatement IASTSwitchStatement IASTCompoundStatement)
        '(org.eclipse.cdt.core.dom.ast.cpp ICPPASTRangeBasedForStatement))

(def omittable-types [IASTForStatement IASTWhileStatement IASTDoStatement  ICPPASTRangeBasedForStatement])

(defn omitted-curly-brace?
  "Is the optional curly brace for this statement node be omitted?"
  [node]
  (not-any? (partial instance? IASTCompoundStatement) (children node)))


(defn if-statement-omitted-curly-brace?
  "omitted-curly-brace? for if statements"
  [node]
  (or (not (instance? IASTCompoundStatement (.getThenClause node)))

      (let [else-clause (.getElseClause node)
            valid-else-types [IASTIfStatement IASTCompoundStatement]]
       (and (some? else-clause)
            (not-any? #(instance? % else-clause) valid-else-types)))))

(defn omitted-curly-braces-atom?
  "Does this AST node an statement that should be followed by curly braces?"
  [node]
  (cond
   (some #(instance? % node) omittable-types)
    (omitted-curly-brace? node)

   (instance? IASTIfStatement node) (if-statement-omitted-curly-brace? node)

   (instance? IASTSwitchStatement node) (not(->> node
                                                 (get-in-tree [1])
                                                 .getSyntax
                                                 .toString
                                                 (= "{")))
   :else false))

