(in-ns 'atom-finder.classifier)

(import '(org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTExpressionList))
(def comma-operator-atom? (partial instance? CPPASTExpressionList))
