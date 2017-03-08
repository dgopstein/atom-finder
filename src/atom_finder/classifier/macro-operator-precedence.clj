(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

(def macro-expansions (comp (memfn getMacroExpansions) root-ancestor))
(def macro-definitions (comp (memfn getMacroDefinitions) root-ancestor))


; Check to see whether there are multiple lines in a macro definition
; TODO Technically this should check for multiple statements, not lines
(s/defn multiline-macro? :- s/Bool
  [node :- IASTPreprocessorMacroDefinition]
  (let [loc (.getExpansionLocation node)]
    (< 1 (- (.getEndingLineNumber loc)
            (.getStartingLineNumber loc)))))

(defn all-multiline-macros [root]
  (filter multiline-macro? (macro-definitions root)))

(s/defn paren-wrapped-macro? :- s/Bool
  [node :- IASTPreprocessorMacroDefinition]
  (let [exp (.getExpansion node)]
    (->> exp parse-stmt paren-node?)))

(s/defn do-wrapped-macro? :- s/Bool
  [node :- IASTPreprocessorMacroDefinition]
  (let [exp (.getExpansion node)]
    (and (->> exp parse-stmt (instance? IASTDoStatement) not) ; avoid trailing semicolon
         (->> (str exp ";") parse-stmt (instance? IASTDoStatement)))))

(defn all-do-wrapped-macros [root] (filter do-wrapped-macro? (macro-definitions root)))

(def not-outer-wrapped-macro? (comp not (some-fn paren-wrapped-macro? do-wrapped-macro?)))

(->> "
#define f4(x) do { x; } while(0)
#define f(x) x?x:x
#define f2(x) x?x:x
#define f5(x) (x)?x:x
#define f6(x) (x)?(x):(x)
#define f3 x?x:x
#define Z3(x, y) x*y
int main() {

  f(2);
  f(3);
}"
     parse-source
     macro-definitions
     (take 1)
     ;first
     ;.getExpansion
     ;parse-frag
     (map #(vector (.toString (.getName %1)) (param-wrapped-macro? %1)))
     )

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

(->> "a"
     parse-expr
     .getName
     .toString
     )

(def not-param-wrapped-macro? (comp not param-wrapped-macro?))

(defmulti macro-def-precedence-atoms? class)
(defmethod macro-def-precedence-atoms? ASTFunctionStyleMacroDefinition [node]
  ((some-fn not-outer-wrapped-macro? not-param-wrapped-macro?) node))

(defmethod macro-def-precedence-atoms? ASTMacroDefinition [node]
  (not-outer-wrapped-macro? node))

(defn dangerous-multiline? [node]
  (and (multiline-macro? node)
       (not (do-wrapped-macro? node))))

(defn macro-def-precedence-atoms
  "Is the definition of a macro prone to precedence issues"
  [node]
  (->> node
       macro-definitions
       (filter macro-def-precedence-atoms?)
       )
  )
(def macro-operator-precedence-atoms macro-def-precedence-atoms)
