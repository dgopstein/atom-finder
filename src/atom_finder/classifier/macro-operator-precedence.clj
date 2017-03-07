(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition))

(def macro-expansions (comp (memfn getMacroExpansions) root-ancestor))
(def macro-definitions (comp (memfn getMacroDefinitions) root-ancestor))

(def macro-operator-precedence-atom? macro-def-precedence-atom?)
(defn macro-def-precedence-atom?
  "Is the definition of a macro prone to precedence issues"
  [node]
  (macro-definitions node)
  )

(defn all-multiline-macros [root]
  (filter multiline-macro? (macro-definitions root)))
(defn multiline-macro? [node]
  (pprint node)
  )

(->> "#define f(x) x?x:x
#define f2(x) x?x:x
#define f3 x?x:x
int main() {

  f(2);
  f(3);
}"
     parse-source
     macro-definitions
     first
     .getExpansion
     parse-fragment
     )

(defn do-wrapped-macro? [node])

(defmulti dangerous-precedence-macro-def? class)
(defmethod dangerous-precedence-macro-def? ASTFunctionStyleMacroDefinition [node]
  )

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
