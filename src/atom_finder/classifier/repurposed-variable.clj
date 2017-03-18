(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode))

(->> "void F1(int V1) { V1 = 1; }"
     parse-source
     (get-in-tree [0])
     .getDeclarator
     .getFunctionScope
     (#(.getBindings % (IScope.ScopeLookupData. )))
     )

