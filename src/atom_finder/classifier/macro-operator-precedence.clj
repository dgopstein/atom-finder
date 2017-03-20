(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTIdExpression IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement IASTLiteralExpression)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

(def macro-expansions (comp (memfn getMacroExpansions) root-ancestor))
(def macro-definitions (comp (memfn getMacroDefinitions) root-ancestor))

(def unparsable-exp? (comp nil? parse-frag))

(s/defn atomic-node? :- s/Bool [node :- IASTNode]
  (any-pred? #(instance? % node) #{IASTLiteralExpression IASTIdExpression IASTName}))

; The macro doesn't contain any operators which might have precedence issues
(s/defn atomic-exp? :- s/Bool [exp :- String]
  (true? (some->> exp parse-frag atomic-node?)))

(s/defn paren-wrapped-exp? :- s/Bool [exp :- String]
  (not (false? (some->> exp parse-expr paren-node?))))

(s/defn do-wrapped-exp? :- s/Bool [exp :- String]
  (and (not (false? (some->> exp parse-stmt (instance? IASTDoStatement) not))) ; avoid trailing semicolon
       (not (false? (some->> (str exp ";") parse-stmt (instance? IASTDoStatement))))))

(def outer-wrapped-exp? (some-fn empty? atomic-exp? paren-wrapped-exp? do-wrapped-exp?))

(defmulti param-wrapped-macro? class)
(defmethod param-wrapped-macro? IASTPreprocessorMacroDefinition [node] true)
(defmethod param-wrapped-macro? IASTPreprocessorFunctionStyleMacroDefinition
  [node]
  (let [params      (set (map (memfn getParameter) (.getParameters node)))
        ast         (parse-frag (.getExpansion node))
        param-nodes (filter-tree #(and (instance? IASTName %1)
                                       (contains? params (.toString %1))) ast)]
    (every? #(paren-node? (ancestor 2 %)) param-nodes)))

(defmulti macro-def-precedence-atom? class)
(defmethod macro-def-precedence-atom? IASTPreprocessorFunctionStyleMacroDefinition [node]
  (not-any? #(% (.getExpansion node)) [empty? unparsable-exp?
                       (every-pred outer-wrapped-exp? (fn [_] (param-wrapped-macro? node)))]))
(defmethod macro-def-precedence-atom? IASTPreprocessorMacroDefinition [node]
  (not-any? #(% (.getExpansion node)) [empty? unparsable-exp? outer-wrapped-exp?]))

(defn macro-def-precedence-atoms
  "Is the definition of a macro prone to precedence issues"
  [node]
  (->> node
       macro-definitions
       (filter macro-def-precedence-atom?)))

; In the future we might test macro expansions in addition to definitions
(def macro-operator-precedence-atoms macro-def-precedence-atoms)
