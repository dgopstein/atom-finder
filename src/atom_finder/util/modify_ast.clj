(in-ns 'atom-finder.util)

;; Update AST's in memory

(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTName IASTIdExpression IASTBinaryExpression IASTUnaryExpression IASTExpressionStatement IASTPreprocessorFunctionStyleMacroDefinition IASTDoStatement IASTLiteralExpression IASTPreprocessorMacroExpansion cpp.ICPPASTTemplateId IASTCastExpression IASTFunctionCallExpression IASTEqualsInitializer IASTIfStatement IASTWhileStatement IASTForStatement IASTDoStatement IASTArraySubscriptExpression IASTExpressionList IASTFieldReference
IASTArrayModifier IASTBinaryExpression IASTCaseStatement IASTCastExpression IASTConditionalExpression IASTDoStatement IASTEnumerationSpecifier IASTExpressionStatement IASTFieldDeclarator IASTFieldReference IASTForStatement IASTFunctionCallExpression IASTIfStatement IASTInitializerExpression IASTReturnStatement IASTSimpleDeclSpecifier IASTSwitchStatement IASTUnaryExpression IASTWhileStatement IASTProblem)
        '(org.eclipse.cdt.core.dom.ast.cpp ICPPASTArrayDesignator ICPPASTConstructorChainInitializer ICPPASTConstructorInitializer ICPPASTDeleteExpression ICPPASTFunctionDeclarator ICPPASTNewExpression ICPPASTPackExpansionExpression ICPPASTSimpleTypeConstructorExpression ICPPASTTemplatedTypeTemplateParameter ICPPASTTypenameExpression)
        '(org.eclipse.cdt.core.dom.ast.gnu cpp.IGPPASTSimpleDeclSpecifier c.IGCCASTArrayRangeDesignator c.IGCCASTSimpleDeclSpecifier IGNUASTGotoStatement )
        '(org.eclipse.cdt.core.dom.ast.c ICASTArrayDesignator)
        '(org.eclipse.cdt.internal.core.parser.scanner ASTFunctionStyleMacroDefinition ASTMacroDefinition)
        '(org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTUnaryExpression CPPASTExpressionList )
        )

(def -expr-getter-setter-suffixes
  [
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
   ])

(def -expr-getters-setters-by-type
  "A mapping between each (eclipse) type of Expression node
   and all the methods that get and set its children"
  (->> -expr-getter-setter-suffixes
       (map (fn [[type & suffixes]]
              [type
               {:getters (map (fn [sfx] #(call-method % (str 'get sfx))) suffixes)
                :setters (map (fn [sfx] #(call-method %1 (str 'set sfx) %2)) suffixes)}]))
       (into {})
       ))

(s/defn -function-call-getter-setter
  [fn-expr :- IASTFunctionCallExpression]
  (let [args (.getArguments fn-expr)
        unfrozen-fn-expr (.copy fn-expr)
        unfrozen-args (.getArguments unfrozen-fn-expr)]
    {:getters (map constantly args)
     :setters
     (->> unfrozen-args
          (map-indexed
           (fn [idx _]
             (fn [new-child]
               (aset unfrozen-args idx (.copy new-child))
               unfrozen-fn-expr))))}
    )
  )

(s/defn -expression-list-getter-setter
  [expr-list :- IASTExpressionList]
  (let [exprs (.getExpressions expr-list)
        unfrozen-expr-list (.copy expr-list)
        unfrozen-exprs (.getExpressions unfrozen-expr-list)]
    {:getters (map constantly exprs)
     :setters
     (->> unfrozen-exprs
          (map (fn [unfrozen-expr]
                 (fn [new-child]
                   (.replace unfrozen-expr-list unfrozen-expr (.copy new-child))
                   unfrozen-expr-list))))}
    )
  )

(s/defn expr-getters-setters :- {:getters [(s/=> IASTExpression)]
                                 :setters [(s/=> IASTExpression IASTExpression)]}
  "For a given expression AST node, return functions that get/set it's children.
   The getters operate on the original node, so that children may be matched by
   equality,and the setters operate on a copy, so that it may be mutated."
  [node :- IASTExpression]
  (condp instance? node
    IASTFunctionCallExpression (-function-call-getter-setter node)
    IASTExpressionList         (-expression-list-getter-setter node)
    (let [unfrozen-node (.copy node)
          getter-setter-map
            (->> -expr-getters-setters-by-type
            (filter #(instance? (first %) node))
            first last)]
          (->
           getter-setter-map
           (update :getters (partial map #(partial % node)))
           (update :setters (partial map (fn [setter]
                                           (fn [new-child]
                                             (setter unfrozen-node (.copy new-child))
                                             unfrozen-node))))
           ))))

(s/defn replace-expr ;:- IASTExpression
  "Update a child inside a parent node.
   Returns an updated copy of the parent."
  [parent :- IASTExpression
   old-child :- IASTExpression
   new-child :- IASTExpression]
  (let [{getters :getters setters :setters} (expr-getters-setters parent)
        old-idx (.indexOf (map #(%) getters) old-child)
        setter (nth setters old-idx)]

     (setter new-child)
    ))
