(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTIdExpression IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

(s/defn type-conversion-atom? :- s/Bool [node :- IASTNode] false)
