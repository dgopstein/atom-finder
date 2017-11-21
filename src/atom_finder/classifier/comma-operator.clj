(in-ns 'atom-finder.classifier)

(import '(org.eclipse.cdt.core.dom.ast IASTExpressionList))

(defn comma-operator-atom? [node] (instance? IASTExpressionList node))
