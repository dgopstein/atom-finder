(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTIdExpression IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement IASTLiteralExpression IASTPreprocessorMacroExpansion cpp.ICPPASTTemplateId)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

(defn greatest-trivial-parent
  "find the highest parent that has the same offset/length as this node"
  [node]
  (->> node all-parents (take-while (partial =by loc node)) last))

(defn non-trivial-parent
  "find the first parent that doesn't have the same offset/length as this node"
  [node]
  (->> node greatest-trivial-parent parent))

(s/defn expansion-parent
  "The parent of the AST node that fully encloses a macro expansion"
  [expansion :- IASTPreprocessorMacroExpansion]
    (some->> expansion location-parent non-trivial-parent parent))

;; the template is being parsed as greater-than + int + less-than, just
;; ignore these some-how. maybe by ignoring every template in the expanded
;; case, or every tree (greater, X, lessthan) tree in the un-expanded?
(s/defn template-misparse?
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
               (not (template-misparse? exp-node unexpanded)))
      exp-node)))

(s/defn macro-outer-precedence-finder
  [root :- IASTTranslationUnit]
  (->> root .getMacroExpansions (keep outer-macro-operator-atom?)))

(defn parse-expansion-args
  [expansion]
  (let [arg-expr (some->> expansion str (re-find #"\w+\((.*)\)") second parse-frag)]
    (when arg-expr
      (if (instance? IASTExpressionList arg-expr)
        (children arg-expr)
        [arg-expr]))))

(s/defn inner-macro-operator-atom? :- (s/maybe IASTNode)
  "Does this expansion lead to a confusion AST tree outside of itself"
  [expansion :- IASTPreprocessorMacroExpansion]
  (let [;arg-nodes (parse-expansion-args expansion)
        expanded   (->> expansion location-parent greatest-trivial-parent)
        unexpanded (->> expansion .getMacroDefinition .getExpansion parse-frag)
        pruned-exp   (->> expanded   prune-terminals (tap pprint))
        pruned-unexp (->> unexpanded prune-terminals (tap pprint))]
    (when (not (atom-finder.tree-diff/tree=by (juxt class (comp pap expr-operator)) pruned-exp pruned-unexp))
      expanded)))

(->> ;"#define M2(x,y) x+y+1 \n 3%(M2(5<4,7>2))"
     "#define M2(x,y) x+y+1 \n 3%(M2(5,7))"
     ;;(->> "#define M2(x) x+1 \n M2(2)"
     parse-frag root-ancestor
     .getMacroExpansions first
     inner-macro-operator-atom?)


(extend-protocol atom-finder.util/ASTTree clojure.lang.ISeq
  (ast-node [lst] (first lst))
  (children [lst] (rest lst)))

(extend-protocol atom-finder.util/ASTTree nil
  (ast-node [lst] nil)
  (children [lst] '()))

(defn seq-tree
  "A tree of nested lists that represents the AST"
  [node]
  (cons node (map seq-tree (children node))))

(defn prune-seq-tree
  "Form new leaves at a given predicate"
  [f any-tree]
  (let [[x & xs] (if (seqable? any-tree) any-tree (seq-tree any-tree))]
    (cons x
          (->> xs (map #(if (f (ast-node %1)) '() %1))  (map #(if (= '() %1) '() (prune-seq-tree f %1)))))))

(defn prune-terminals
  [tree]
  (prune-seq-tree (fn [node] (any-pred? #(instance? % node) [IASTIdExpression IASTLiteralExpression])) tree))

(s/defn macro-inner-precedence-finder
  [root :- IASTTranslationUnit]
  (->> root .getMacroExpansions (keep inner-macro-operator-atom?)))

'((-<>> "macro-operator-precedence.c" resource-path parse-file))
'((-<>> "macro-operator-precedence.c" resource-path parse-file (def root)))
'((-<> root .getMacroExpansions (nth 10) (def exp <>)))
'((-<> root macro-outer-precedence-finder))
'((->> exp location-parent parent parent write-ast parse-frag write-node))
'((->> [{:name :m :finder macro-outer-precedence-finder}]
     (print-atoms-in-dir (->> "~/opt/src/gcc" expand-home))))
