(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTIdExpression IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement IASTLiteralExpression IASTPreprocessorMacroExpansion cpp.ICPPASTTemplateId IASTCastExpression IASTFunctionCallExpression IASTEqualsInitializer IASTIfStatement IASTWhileStatement IASTForStatement IASTDoStatement IASTArraySubscriptExpression IASTExpressionList IASTFieldReference
IASTArrayModifier IASTBinaryExpression IASTCaseStatement IASTCastExpression IASTConditionalExpression IASTDoStatement IASTEnumerationSpecifier IASTExpressionStatement IASTFieldDeclarator IASTFieldReference IASTForStatement IASTFunctionCallExpression IASTIfStatement IASTInitializerExpression IASTReturnStatement IASTSimpleDeclSpecifier IASTSwitchStatement IASTUnaryExpression IASTWhileStatement IASTProblem)
        '(org.eclipse.cdt.core.dom.ast.cpp ICPPASTArrayDesignator ICPPASTConstructorChainInitializer ICPPASTConstructorInitializer ICPPASTDeleteExpression ICPPASTFunctionDeclarator ICPPASTNewExpression ICPPASTPackExpansionExpression ICPPASTSimpleTypeConstructorExpression ICPPASTTemplatedTypeTemplateParameter ICPPASTTypenameExpression)
        '(org.eclipse.cdt.core.dom.ast.gnu cpp.IGPPASTSimpleDeclSpecifier c.IGCCASTArrayRangeDesignator c.IGCCASTSimpleDeclSpecifier IGNUASTGotoStatement )
        '(org.eclipse.cdt.core.dom.ast.c ICASTArrayDesignator)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition)
        '(org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTUnaryExpression CPPASTExpressionList )
        )

(s/defn valid-node?
  [node :- (s/maybe IASTNode)]
  (and node (not (instance? IASTProblem node))))

;;https://stackoverflow.com/questions/9568050/in-clojure-how-to-write-a-function-that-applies-several-string-replacements
(s/defn replace-map
  "given an input string and a hash-map, returns a new string with all
   keys in map found in input replaced with the value of the key"
  [replacements :- {s/Str s/Str} target :- s/Str]
  (reduce (fn [target [pattern replacement]]
            (str/replace target (re-pattern pattern) replacement))
          target replacements))

(s/defn id-replace-map
  "replace identifiers"
  [replacements :- {s/Str s/Str} target :- s/Str]
  (replace-map
   (->> replacements
        ;; don't match ids inside other ids
        (map-keys #(str "(?<!\\w)" (java.util.regex.Pattern/quote %) "(?!\\w)"))
        (map-values str/re-quote-replacement))
   target))

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

(s/defn expansion-container
  "The AST node that fully encloses a macro expansion"
  [expansion :- IASTPreprocessorMacroExpansion]
    (some->> expansion location-parent greatest-trivial-parent))

(s/defn expansion-parent
  "The parent of the AST node that fully encloses a macro expansion"
  [expansion :- IASTPreprocessorMacroExpansion]
    (some->> expansion expansion-container parent parent))

;; the template is being parsed as greater-than + int + less-than, just
;; ignore these some-how. maybe by ignoring every template in the expanded
;; case, or every tree (greater, X, lessthan) tree in the un-expanded?
(s/defn template-misparse?
  "sometimes the parser screws up A<b>::c so check if that happened"
  [expanded :- IASTNode unexpanded :- ExprOperator]
  (and (->> unexpanded :name (#{:lessThan :greaterThan}))
       (->> expanded (filter-tree (partial instance? ICPPASTTemplateId)) empty? not)))

(defn parse-macro-def
  [macro-def]
  (let [[all name args body] (->> macro-def str (re-find #"^([^=(]*)(?:\(([^)]*)\))?=(.*)"))]
    {:name name
     :args (some-<>> args (str/split <> #",") (remove empty?))
     :body body}))

(s/defn macro-defs-by-name {s/Str IASTPreprocessorMacroDefinition}
  [root :- IASTTranslationUnit]
  (->> root .getMacroDefinitions (map (juxt #(-> % .getName str) identity)) (into {})))

;; This doesn't catch cases where Macros are redefined throughout the file
(s/defn macro-body-str
  "Expand the body of the macro defnition"
  [macro-exp :- IASTPreprocessorMacroExpansion]
  (->> macro-exp .getMacroDefinition parse-macro-def :body))

'((->> "
  #define N	x
  #define M(x)  N(x)
  int y = M();
" parse-source .getMacroExpansions first macro-body-str))

(s/defn substituting-macro?
  [exp :- IASTPreprocessorMacroExpansion]
  (->> exp macro-body-str (re-find #"#")))

(s/defn outer-macro-operator-atom? :- (s/maybe IASTNode)
  "Does this expansion lead to a confusion AST tree outside of itself"
  [expansion :- IASTPreprocessorMacroExpansion]
  (let [exp-node (expansion-parent expansion)
        expanded   (some->> exp-node expr-operator)
        unexpanded (some->> exp-node write-tree parse-frag expr-operator)]
    (when (and (not (substituting-macro? expansion))
               expanded unexpanded
               (not= expanded unexpanded)
               (not (template-misparse? exp-node unexpanded)))
      exp-node)))

(s/defn macro-outer-precedence-finder
  [root :- IASTTranslationUnit]
  (->> root .getMacroExpansions (keep outer-macro-operator-atom?)))

(s/defn expansion-args-tree
  "Take the arguments to a macro and parse them into ASTs"
  [exp :- IASTPreprocessorMacroExpansion]
  ;; todo, is there a function that does this regex already
  (let [arg-str  (some->> exp str (re-find #"(?s)\w+\((.*)\)") second)
        arg-expr (some->> arg-str parse-frag)]
    (when arg-expr
      (cond
        (= arg-str "")
          []
        (instance? IASTExpressionList arg-expr)   ;; multiple args
          (children arg-expr)
        (instance? IASTProblem arg-expr) ;; statements as args?
          (let [parsed-args (some-<>> arg-str (str/split <> #",") (map parse-frag))]
            (when (every? valid-node? parsed-args)
              parsed-args))
        :else [arg-expr]                          ;; single arg
        ))))

(s/defn expansion-args-str :- [s/Str]
  "Extract the code in the arguments to a macro"
  [exp :- IASTPreprocessorMacroExpansion]
  (->> exp expansion-args-tree (map write-tree)))

(s/defn id-name
  [node :- IASTNode]
  (cond (instance? IASTIdExpression node) (.getName node)
        (instance? IASTName node) node
        :else nil))

(s/defn -maybe-set-value!
  [getter setter node replacements]
  (when-let* [operand     (getter node)
              name        (id-name operand)
              replacement (-> name str replacements)
              _           (instance? IASTExpression replacement)
              ]
       (doto node
         (setter (.copy (cond (instance? IASTIdExpression operand) replacement
                              (instance? IASTName operand) (.getName replacement)))))))

(s/defn -maybe-set-args!
  [node :- IASTFunctionCallExpression replacements]
  (let [args (.getArguments node)]
    (->> args
         count
         range
         (map (fn [idx] (-maybe-set-value!
                         (fn [_] (nth args idx))
                         (fn [_ new-child] (aset args idx new-child))
                         node replacements)))
         dorun)

    (doto node (.setArguments args))))

(s/defn -maybe-set-expr-list!
  [node :- CPPASTExpressionList replacements]
  (doseq [expr (.getExpressions node)]
    (-maybe-set-value!
     (fn [node] expr)
     (fn [_ new-child] (.replace node expr new-child))
     node replacements))
  node)

(s/defn -maybe-set-operand!
  "Take an identifier, and replace it with its full tree"
  [method node replacements]
  (-maybe-set-value! #(call-method % (str 'get method))
                     #(call-method %1 (str 'set method) %2)
                     node replacements))

(s/defn -maybe-set-operands!
  [node replacements & methods]
  (->> methods
       (map #(-maybe-set-operand! %1 node replacements))
       doall
       (some identity)))
(def expression-sites [
[IASTArrayModifier "ConstantExpression"]
[IASTArraySubscriptExpression "Argument" "ArrayExpression"]
[IASTBinaryExpression "Operand1" "Operand2"]
[IASTCaseStatement "Expression"]
[IASTCastExpression "Operand"]
[IASTConditionalExpression "LogicalConditionExpression" "PositiveResultExpression" "NegativeResultExpression"]
[IASTDoStatement "Condition"]
[IASTEnumerationSpecifier "Value"]
[IASTEqualsInitializer "InitializerClause"]
[IASTExpressionStatement "Expression"]
[IASTFieldDeclarator "BitFieldSize"]
[IASTFieldReference "FieldOwner" "FieldName"]
[IASTForStatement "ConditionExpression" "IterationExpression"]
[IASTFunctionCallExpression "FunctionNameExpression"]
[IASTIfStatement "ConditionExpression"]
[IASTInitializerExpression "Expression"]
[IASTReturnStatement "ReturnValue"]
[IASTSimpleDeclSpecifier "DeclTypeExpression"]
[IASTSwitchStatement "ControllerExpression"]
[IASTUnaryExpression "Operand"]
[IASTWhileStatement "Condition"]
[ICASTArrayDesignator "SubscriptExpression"]
[ICPPASTArrayDesignator "SubscriptExpression"]
[ICPPASTConstructorChainInitializer "InitializerValue"]
[ICPPASTConstructorInitializer "Expression"]
[ICPPASTDeleteExpression "Operand"]
[ICPPASTFunctionDeclarator "NoexceptExpression"]
[ICPPASTNewExpression "NewPlacement" "NewInitializer" "PlacementArguments"]
[ICPPASTPackExpansionExpression  "Pattern"]
[ICPPASTSimpleTypeConstructorExpression "InitialValue"]
[ICPPASTTemplatedTypeTemplateParameter "DefaultValue"]
[ICPPASTTypenameExpression "InitialValue"]
[IGCCASTArrayRangeDesignator "RangeFloor" "RangeCeiling"]
[IGCCASTSimpleDeclSpecifier "TypeofExpression"]
[IGNUASTGotoStatement "LabelNameExpression"]
[IGPPASTSimpleDeclSpecifier "TypeofExpression"]
[IASTFunctionCallExpression -maybe-set-args!]
[IASTExpressionList         -maybe-set-expr-list!]
 ])

(s/defn -replace-identifier!
  "If this node is capable of causing syntax ambiguities,
   try replacing parts of it to observe the ambiguity"
  [node :- IASTNode replacements :- {s/Str IASTNode}]
  (->> expression-sites
       (some
        (fn [[type & methods]]
          (when (instance? type node)
            (if (instance? String (first methods))
              (apply -maybe-set-operands! node replacements methods)
              ((first methods) node replacements)))))))

(s/defn parse-macro
  [macro-exp :- IASTPreprocessorMacroExpansion]
  (let [macro-def (->> macro-exp .getMacroDefinition)
        body-str  (macro-body-str macro-exp)]
    {:def macro-def
     :exp macro-exp
     :params-str (->> macro-def .getParameters (map (memfn getParameter)))
     :args-str   (->> macro-exp expansion-args-str)
     :args-tree  (->> macro-exp expansion-args-tree)
     :body-str   body-str
     :body-tree  (parse-frag body-str)}))

(s/defn macro-replace-arg-str
  [macro-exp :- IASTPreprocessorMacroExpansion]
  (let [mac        (parse-macro macro-exp)
        param-args (zipmap (:params-str mac) (:args-str mac))]
    (->> mac :body-str (id-replace-map param-args) parse-frag)))

(defn paren-wrap
  [node]
  (CPPASTUnaryExpression. IASTUnaryExpression/op_bracketedPrimary (.copy node)))

(s/defn macro-replace-arg-tree
  [macro-exp :- IASTPreprocessorMacroExpansion]
  (let [mac        (parse-macro macro-exp)
        param-args (zipmap (:params-str mac) (:args-tree mac))
        new-body   (->> mac :body-tree .copy)
        ;; Add top-level parens so we can modify the root
        [body-handle body-getter] (if (instance? IASTExpression new-body) [(paren-wrap new-body) child] [new-body identity])
        name-sites (->> body-handle (filter-tree id-name) set)
        ]

    (when (not (or
           ;; don't bother trying to parse macros with nested macros
           ;; note - we could do this test at the beginning of the function
           ;; to save some computation
           (and
            (->> macro-exp .getNestedMacroReferences empty? not)
            (do (comment (println (str "Won't expand nested macros in: " (filename (:exp mac)) ":" (start-line (:exp mac)) " - " (write-tree (body-getter body-handle)))))
                    true))

           ;; replace all matching arguments
           (->> body-handle (filter-tree #(-replace-identifier! % param-args))
                (remove nil?) doall empty?)

           ;; if any un-matched args remain, bail out
           ;; TODO these are the false-negatives
           (and
            (->> body-handle (filter-tree #((-> mac :params-str set) (id-name %))) (exists? name-sites))
            (do (println (str "Couldn't expand: " (filename (:exp mac)) ":" (start-line (:exp mac)) " - " (write-tree (body-getter body-handle))))
                    true))
           ))
      (body-getter body-handle))))

(require '[atom-finder.tree-diff :refer :all])
(s/defn inner-macro-operator-atom? :- (s/maybe IASTNode)
  "Does this expansion lead to a confusion AST tree outside of itself"
  [exp :- IASTPreprocessorMacroExpansion]
  (when-let* [_  (not (substituting-macro? exp))
              _  (instance? IASTPreprocessorFunctionStyleMacroDefinition (.getMacroDefinition exp))
              replaced-str  (macro-replace-arg-str  exp)
              replaced-tree (macro-replace-arg-tree exp)
              ;replaced-str  (pap print-tree (macro-replace-arg-str  exp))
              ;replaced-tree (pap print-tree (macro-replace-arg-tree exp))
              _  (not (atom-finder.tree-diff/tree=by (juxt class expr-operator)
                       replaced-str
                       replaced-tree))
              ]
    (->> exp location-parent greatest-trivial-parent)))

(s/defn macro-inner-precedence-finder
  [root :- IASTTranslationUnit]
  (->> root .getMacroExpansions (keep inner-macro-operator-atom?)))

(s/defn macro-operator-precedence-atom?
  "Does this expansion lead to a confusion"
  [node]
  ((some-fn inner-macro-operator-atom? outer-macro-operator-atom?) node))

(s/defn macro-operator-precedence-finder
  [node :- IASTNode] ; IASTTranslationUnit]
  (->> node root-ancestor .getMacroExpansions (keep macro-operator-precedence-atom?)))
