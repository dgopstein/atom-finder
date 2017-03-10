(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTIdExpression IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

(def macro-expansions (comp (memfn getMacroExpansions) root-ancestor))
(def macro-definitions (comp (memfn getMacroDefinitions) root-ancestor))

(def empty-macro? (comp empty? (memfn getExpansion)))

(s/defn atomic-node? :- s/Bool
  [node :- IASTNode]
  (any-pred? #(instance? % node) #{IASTLiteralExpression IASTIdExpression IASTName}))

; The macro doesn't contain any operators which might have precedence issues
(s/defn atomic-macro? :- s/Bool
  [node :- IASTPreprocessorMacroDefinition]
  (let [exp (.getExpansion node)]
    (if (empty? exp)
      false
      (true? (some->> exp parse-frag atomic-node?)))))

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

(def outer-wrapped-macro? (some-fn empty-macro? atomic-macro? paren-wrapped-macro? do-wrapped-macro?))
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

(->>
;"#define M16 1
;#define M17 \"abc\"
;#define M18 abc
;#define M19 ab.c
;#define M20 (ab.c)
"#define M1 1; 2"
parse-source
macro-definitions
first
;(map (memfn getExpansion))
;(map parse-expr)
;(map #(vector (typename %1) (typename (parent %1))))
;(map #(vector (typename %1) (map typename (children %1))))
;macro-def-precedence-atoms
atomic-macro?
;.getExpansion
;((fn [exp] (some->> exp parse-expr atomic-node?)))
)
