(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTName IASTIdExpression IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement IASTLiteralExpression IASTPreprocessorMacroExpansion cpp.ICPPASTTemplateId)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition))

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

(defmulti macro-body-str class)
(defmethod macro-body-str IASTPreprocessorMacroExpansion [exp]
  (->> exp .getMacroDefinition macro-body-str))
(defmethod macro-body-str IASTPreprocessorMacroDefinition [macro-def]
  (-> macro-def str (str/split #"=" 2) second))

(s/defn substituting-macro?
  [exp :- IASTPreprocessorMacroExpansion]
  (->> exp macro-body-str (re-find #"#")))

(s/defn outer-macro-operator-atom? :- (s/maybe IASTNode)
  "Does this expansion lead to a confusion AST tree outside of itself"
  [expansion :- IASTPreprocessorMacroExpansion]
  (let [exp-node (expansion-parent expansion)
        expanded   (some->> exp-node expr-operator)
        unexpanded (some->> exp-node safe-write-ast parse-frag expr-operator)]
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
  (let [arg-expr (some->> exp str (re-find #"\w+\((.*)\)") second parse-frag)]
    (when arg-expr
      (if (instance? IASTExpressionList arg-expr)
        (children arg-expr)
        [arg-expr]))))

(s/defn expansion-args-str :- [s/Str]
  "Extract the code in the arguments to a macro"
  [exp :- IASTPreprocessorMacroExpansion]
  (->> exp expansion-args-tree (map safe-write-ast)))

(s/defn -maybe-set-operand!
  "Take an identifier, and replace it with its full tree"
  [method node replacements]
  (when-let* [operand     (call-method node (str 'get method))
              _           (instance? IASTIdExpression operand)
              replacement (-> operand .getName str replacements)
              _           (instance? IASTExpression replacement)
              ]
       (doto node
         (call-method (str 'set method) (.copy replacement)))))

(s/defn -replace-identifier!
  "If this node is capable of causing syntax ambiguities,
   try replacing parts of it to observe the ambiguity"
  [node :- IASTNode replacements :- {s/Str IASTNode}]
  (condp instance? node
    IASTUnaryExpression      (-maybe-set-operand! "Operand"  node replacements)
    IASTBinaryExpression (let [op1 (-maybe-set-operand! "Operand1" node replacements)
                               op2 (-maybe-set-operand! "Operand2" node replacements)]
                           (or op1 op2))
      nil))

(s/defn parse-macro
  [macro-exp :- IASTPreprocessorMacroExpansion]
  (let [macro-def (->> macro-exp .getMacroDefinition)
        body-str  (macro-body-str macro-def)]
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
    (-<>> mac :body-str (id-replace-map param-args) parse-frag)))

(s/defn macro-replace-arg-tree
  [macro-exp :- IASTPreprocessorMacroExpansion]
  (let [mac        (parse-macro macro-exp)
        param-args (zipmap (:params-str mac) (:args-tree mac))
        new-body   (-> mac :body-tree .copy)]

    (when (not (or
           ;; replace all matching arguments
           (->> new-body (filter-tree expr-operator)
                    (map #(-replace-identifier! % param-args)) (remove nil?) doall empty?)
           ;; if any un-matched args remain, bail out
           ;; TODO these are the false-negatives
           (->> new-body (filter-tree #(instance? IASTName %)) (exists? #(-> % str param-args)))))
      new-body)))

(require '[atom-finder.tree-diff :refer :all])
(s/defn inner-macro-operator-atom? :- (s/maybe IASTNode)
  "Does this expansion lead to a confusion AST tree outside of itself"
  [exp :- IASTPreprocessorMacroExpansion]
  (when-let* [_  (not (substituting-macro? exp))
              _  (instance? IASTPreprocessorFunctionStyleMacroDefinition (.getMacroDefinition exp))
              replaced-str  (macro-replace-arg-str  exp)
              replaced-tree (macro-replace-arg-tree exp)
              ;replaced-str  (pap write-ast (macro-replace-arg-str  exp))
              ;replaced-tree (pap write-ast (macro-replace-arg-tree exp))
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
  [root :- IASTTranslationUnit]
  (->> root .getMacroExpansions (keep macro-operator-precedence-atom?)))

'(->> "/Users/dgopstein/opt/src/redis/deps/hiredis/test.c"
     parse-file
     macro-operator-precedence-finder
     doall
     (map start-line)
     pprint
     )
