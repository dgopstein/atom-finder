(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement)
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

(defn all-do-wrapped-macros [root]
  (filter do-wrapped-macro? (macro-definitions root)))

(def not-outer-wrapped-macro?
  (comp not (some-fn paren-wrapped-macro? do-wrapped-macro?)))

;(->> "
;#define f4(x) do { x; } while(0)
;#define f(x) x?x:x
;#define f2(x) x?x:x
;#define f3 x?x:x
;int main() {
;
;  f(2);
;  f(3);
;}"
;     parse-source
;     macro-definitions
;     ((flip nth) 2)
;     .getExpansion
;     parse-frag
;     write-ast
;     )

(s/defn arg-wrapped? :- s/Bool
  [node :- IASTPreprocessorFunctionStyleMacroDefinition]
  (let [params (.getParameters node)
        ast    (parse-frag (.getExpansion node))]
       (filter-tree ast)

  ))

(def not-arg-wrapped-macro? (comp not arg-wrapped?))

(defmulti dangerous-precedence-macro-def? class)
(defmethod dangerous-precedence-macro-def? ASTFunctionStyleMacroDefinition [node]
  ((some-fn not-outer-wrapped-macro? not-arg-wrapped-macro?) node))

(defmethod dangerous-precedence-macro-def? ASTMacroDefinition [node]
  (not-outer-wrapped-macro? node))

(defn dangerous-multiline? [node]
  (and (multiline-macro? node)
       (not (do-wrapped-macro? node))))

(defn macro-def-precedence-atoms
  "Is the definition of a macro prone to precedence issues"
  [node]
  (->> node
       macro-definitions
       (filter dangerous-precedence-macro-def?)
       )
  )
(def macro-operator-precedence-atoms macro-def-precedence-atoms)
