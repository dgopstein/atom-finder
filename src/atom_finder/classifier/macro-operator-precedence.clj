(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition))

(def macro-expansions (comp (memfn getMacroExpansions) root-ancestor))
(def ap (all-preprocessor atom-finder.constants/big-root))

(->> ap
    ;(filter (partial instance? IASTPreprocessorFunctionStyleMacroDefinition))
    (take 100)
    ;(map #(vector (.getExpansion %1) (.toString (.getName %1))))
    (map typename)
    ;pprint
    )

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
