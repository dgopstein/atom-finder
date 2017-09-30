(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTIdExpression IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement IASTLiteralExpression IASTPreprocessorMacroExpansion)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

(s/defn expansion-parent
  "The AST node that fully encloses a macro expansion"
  [expansion :- IASTPreprocessorMacroExpansion]
    (->> expansion location-parent parent parent))

(s/defn outer-macro-operator-atom? :- (s/maybe IASTNode)
  "Does this expansion lead to a confusion AST tree outside of itself"
  [expansion :- IASTPreprocessorMacroExpansion]
  (let [exp-node (expansion-parent expansion)]
    (when (not= (->> exp-node write-node)
                (->> exp-node write-ast parse-frag write-node))
      exp-node)))

(s/defn macro-outer-precedence-finder
  [root :- IASTTranslationUnit]
  (->> root
       .getMacroExpansions
       (keep outer-macro-operator-atom?)
       ))

(s/defn macro-inner-precedence-finder
  [root :- IASTTranslationUnit]
  [])

'(-<>> "macro-operator-precedence.c"
      resource-path
      parse-file
      macro-outer-precedence-finder
      (map write-ast)
      pprint)

; https://stackoverflow.com/questions/38059977/cant-call-public-method-of-non-public-class-public-google-gcloud-library

'(-<>> "macro-operator-precedence.c"
     resource-path
     parse-file
     (def root)
     .getMacroExpansions
     first
     ;)
     ;(invoke-private-method <> "getContext")
     ;(invoke-private-method <> "getExpansion"))
     ;(invoke-private-method <> "getSequenceLength")
     ;(invoke-private-method <> "getImageLocation")
     ;)
     .getImageLocation
     loc
     ;ppublic-methods
     ;)
     ;loc prn)

     location-parent
     parent write-ast
     parent write-ast parse-frag write-node
     ;parent (pap write-ast) loc prn
     tree-path
     )


'(-<> root .getMacroExpansions (nth 2) (def exp <>))
'exp
'
';; works for expressions outside the expansion, not the arguments
'(->> exp location-parent parent parent write-node)
'(->> exp location-parent parent parent write-ast parse-frag write-node)
'
'
'(->> root print-tree)
'(->> root (get-in-tree [0 2 1 0])) ; + - :len 7
'(->> root (get-in-tree [0 2 1 0 0]) loc :length) ; * - :len 7
'(->> root (get-in-tree [0 2 1 0 0 0]) loc :length) ; 3 - :len 1
'(->> root (get-in-tree [0 2 1 0 0 1]) loc :length) ; 2 - :len 5
'
'(->> root (get-in-tree [0 2 3 0]) write-node) ; 2 - :len 5

