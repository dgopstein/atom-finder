(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition))

(def macro-expansions (comp (memfn getMacroExpansions) root-ancestor))

(->> "#define f(x) x?x:x
int main() {

  f(2);
  f(3);
}"
     parse-source
     macro-expansions
     ;(map (memfn toString))
     first
     )
;IASTBinaryExpression/op_multiply
