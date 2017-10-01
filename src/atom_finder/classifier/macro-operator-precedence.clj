(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTIdExpression IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement IASTLiteralExpression IASTPreprocessorMacroExpansion cpp.ICPPASTTemplateId)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

(defn non-trivial-parent
  "find the first parent that doesn't have the same offset/length as this node"
  [node]
  (->> node all-parents (remove (partial =by loc node)) first))

(s/defn expansion-parent
  "The AST node that fully encloses a macro expansion"
  [expansion :- IASTPreprocessorMacroExpansion]
    (some->> expansion location-parent non-trivial-parent parent))

;; the template is being parsed as greater-than + int + less-than, just
;; ignore these some-how. maybe by ignoring every template in the expanded
;; case, or every tree (greater, X, lessthan) tree in the un-expanded?
(s/defn template-misparse
  "sometimes the parser screws up A<b>::c so check if that happened"
  [expanded :- IASTNode unexpanded :- ExprOperator]
  (and (->> unexpanded :name (#{:lessThan :greaterThan}))
       (->> expanded (filter-tree (partial instance? ICPPASTTemplateId)) empty? not)))
  ;[expanded :- IASTNode unexpanded :- IASTNode]
  ;(and (->> expanded pap (filter-tree (partial instance? ICPPASTTemplateId)) pap empty? not pprn)
  ;     (->> unexpanded write-ast (re-find #"< *\w+ *>") pprn)))

(s/defn outer-macro-operator-atom? :- (s/maybe IASTNode)
  "Does this expansion lead to a confusion AST tree outside of itself"
  [expansion :- IASTPreprocessorMacroExpansion]
  (let [exp-node (expansion-parent expansion)
        expanded   (some->> exp-node expr-operator)
        unexpanded (some->> exp-node safe-write-ast parse-frag expr-operator)]
    (when (and expanded unexpanded
               (not= expanded unexpanded)
               (not (template-misparse exp-node unexpanded)))
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

'((-<>> "macro-operator-precedence.c" resource-path parse-file))
'((-<>> "macro-operator-precedence.c" resource-path parse-file (def root)))
'((-<> root .getMacroExpansions (nth 10) (def exp <>)))
'((-<> root macro-outer-precedence-finder))
'((->> exp location-parent parent parent write-ast parse-frag write-node))
'((->> [{:name :m :finder macro-outer-precedence-finder}]
     (print-atoms-in-dir (->> "~/opt/src/gcc" expand-home))))
