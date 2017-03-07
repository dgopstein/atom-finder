(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

(def macro-expansions (comp (memfn getMacroExpansions) root-ancestor))
(def macro-definitions (comp (memfn getMacroDefinitions) root-ancestor))

(def macro-operator-precedence-atom? macro-def-precedence-atom?)
(defn macro-def-precedence-atom?
  "Is the definition of a macro prone to precedence issues"
  [node]
  (macro-definitions node)
  )

(s/defn multiline-macro? :- s/Bool
  [node :- IASTPreprocessorMacroDefinition]
  (let [loc (.getExpansionLocation node)]
    (< 1 (- (.getEndingLineNumber loc)
            (.getStartingLineNumber loc)))))

(defn all-multiline-macros [root]
  (filter multiline-macro? (macro-definitions root)))

(defn do-wrapped-macro? [node] false)

(defmulti dangerous-precedence-macro-def? class)
(defmethod dangerous-precedence-macro-def? ASTFunctionStyleMacroDefinition [node]
  false)

(defmethod dangerous-precedence-macro-def? ASTMacroDefinition [node]
  (and (multiline-macro? node)
       (not (do-wrapped-macro? node))))

(->> "#define f(x) x?x:x
#define f2(x) x?x:x
#define f3 x?x:x
int main() {

  f(2);
  f(3);
}"
     parse-source
     macro-definitions
     ;(map (memfn toString))
     ;(map (memfn getMacroDefinition))
     ;first
     pprint
     )
; => #object[org.eclipse.cdt.internal.core.parser.scanner.ASTMacroExpansion 0x3c885ca1 "f(2)"]
;IASTBinaryExpression/op_multiply


;(print-tree (parse-expr "!x*y"))
;(print-tree (parse-expr "!(x*y)"))
