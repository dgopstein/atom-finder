(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTConditionalExpression))

(def conditional-atom? (partial instance? IASTConditionalExpression))
