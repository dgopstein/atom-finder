(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTIdExpression IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement IASTLiteralExpression IASTPreprocessorMacroExpansion)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

(defn non-trivial-parent
  "find the first parent that doesn't have the same offset/length as this node"
  [node]
  (->> node all-parents (remove (partial =by loc node)) first))

(s/defn expansion-parent
  "The AST node that fully encloses a macro expansion"
  [expansion :- IASTPreprocessorMacroExpansion]
    (->> expansion location-parent non-trivial-parent parent))

(s/defn outer-macro-operator-atom? :- (s/maybe IASTNode)
  "Does this expansion lead to a confusion AST tree outside of itself"
  [expansion :- IASTPreprocessorMacroExpansion]
  (let [exp-node (expansion-parent expansion)
        expanded   (->> exp-node expr-operator)
        unexpanded (->> exp-node write-ast parse-frag expr-operator)]
    (when (and expanded unexpanded (not= expanded unexpanded))
      exp-node)))

;(-<>> "macro-operator-precedence.c"
;      resource-path
;      parse-file)
'(->> "#define M2(x) x+1
     3*M2((5<4))"
     parse-frag
     root-ancestor ;.getMacroExpansions first location-parent)
;expansion-parent expr-operator)
      macro-outer-precedence-finder
      (map write-ast)
      pprint)

(s/defn macro-outer-precedence-finder
  [root :- IASTTranslationUnit]
  (->> root
       .getMacroExpansions
       (keep outer-macro-operator-atom?)
       ))

(s/defn macro-inner-precedence-finder
  [root :- IASTTranslationUnit]
  [])

'(-<>> "macro-operator-precedence.c" resource-path parse-file (def root))
'(-<> root .getMacroExpansions (nth 3) (def exp <>))

;(->> exp ((juxt str loc)) prn)
;(->> exp location-parent ((juxt str loc)) prn)
;(->> exp location-parent parent parent parent print-tree)
'(->> exp location-parent parent parent write-ast parse-frag write-node)
