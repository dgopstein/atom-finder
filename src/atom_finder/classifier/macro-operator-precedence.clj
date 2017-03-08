(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

(def macro-expansions (comp (memfn getMacroExpansions) root-ancestor))
(def macro-definitions (comp (memfn getMacroDefinitions) root-ancestor))

(def empty-macro? (comp empty? (memfn getExpansion)))

(s/defn paren-wrapped-macro? :- s/Bool
  [node :- IASTPreprocessorMacroDefinition]
  (let [exp (.getExpansion node)]
    (if (empty? exp)
      false
      (not (false? (some->> exp parse-expr paren-node?))))))

(s/defn do-wrapped-macro? :- s/Bool
  [node :- IASTPreprocessorMacroDefinition]
  (let [exp (.getExpansion node)]
    (if (empty? exp)
      false
      (and (not (false? (some->> exp parse-stmt (instance? IASTDoStatement) not))) ; avoid trailing semicolon
           (not (false? (some->> (str exp ";") parse-stmt (instance? IASTDoStatement))))))))

(defn all-do-wrapped-macros [root] (filter do-wrapped-macro? (macro-definitions root)))

(def outer-wrapped-macro? (some-fn empty-macro? paren-wrapped-macro? do-wrapped-macro?))
(def not-outer-wrapped-macro? (complement outer-wrapped-macro?))

(defmulti param-wrapped-macro? class)
(defmethod param-wrapped-macro? IASTPreprocessorMacroDefinition [node] true)
(defmethod param-wrapped-macro? IASTPreprocessorFunctionStyleMacroDefinition
  [node]
  (let [params      (set (map (memfn getParameter) (.getParameters node)))
        ast         (parse-frag (.getExpansion node))
        param-nodes (filter-tree #(and (instance? IASTName %1)
                                       (contains? params (.toString %1))) ast)]
    (every? #(paren-node? (ancestor 2 %)) param-nodes)))

(defn all-param-wrapped-macros [root] (filter param-wrapped-macro? (macro-definitions root)))
(def not-param-wrapped-macro? (complement param-wrapped-macro?))

(def unparsable-macro? (comp nil? parse-frag (memfn getExpansion)))

(defmulti macro-def-precedence-atom? class)
(defmethod macro-def-precedence-atom? IASTPreprocessorFunctionStyleMacroDefinition [node]
  (and (not (empty-macro? node))
       (not (unparsable-macro? node))
      ((some-fn not-outer-wrapped-macro? not-param-wrapped-macro?) node)))
(defmethod macro-def-precedence-atom? IASTPreprocessorMacroDefinition [node]
  (and (not (empty-macro? node))
       (not (unparsable-macro? node))
       (not-outer-wrapped-macro? node)))

(defn macro-def-precedence-atoms
  "Is the definition of a macro prone to precedence issues"
  [node]
  (->> node
       macro-definitions
       (filter macro-def-precedence-atom?)))

; In the future we might test macro expansions in addition to definitions
(def macro-operator-precedence-atoms macro-def-precedence-atoms)
