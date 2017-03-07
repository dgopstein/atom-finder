(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression))

(defn mutatable-op?
  "can this AST node change program state?"
  [node]
  (let [b-ops #{ ; binary operators with side-effects
          IASTBinaryExpression/op_assign          IASTBinaryExpression/op_binaryAndAssign
          IASTBinaryExpression/op_binaryXorAssign IASTBinaryExpression/op_divideAssign
          IASTBinaryExpression/op_minusAssign     IASTBinaryExpression/op_moduloAssign
          IASTBinaryExpression/op_multiplyAssign  IASTBinaryExpression/op_plusAssign
          IASTBinaryExpression/op_shiftLeftAssign IASTBinaryExpression/op_shiftRightAssign}
        u-ops #{ ; unary operators with side-effects
          IASTUnaryExpression/op_postFixDecr      IASTUnaryExpression/op_postFixIncr
          IASTUnaryExpression/op_prefixDecr       IASTUnaryExpression/op_prefixIncr}]
  (cond
    (leaf? node) false
    (instance? IASTBinaryExpression node) (contains? b-ops (.getOperator node))
    (instance? IASTUnaryExpression node) (contains? u-ops (.getOperator node))
    :else (= (typename node) "FunctionCallExpression"))))

(defn mutatable-expr?
  "can this expression contain a node that can change program state?"
  [node]

  (cond
    (leaf? node) false
    (mutatable-op? node) true
    :else (some? (some mutatable-expr? (children node)))))

(defn short-circuitable-op?
  "is this operator short-circuitable?"
  [node]
  (and (instance? IASTBinaryExpression node)
       (let [op-code (.getOperator node)]
         (or (= op-code IASTBinaryExpression/op_logicalOr)
             (= op-code IASTBinaryExpression/op_logicalAnd)))))

;; IEEE 9899 6.5.3.4.2 claims that sizeof does conditional evaluation as well.
(defn short-circuitable-expr?
  "can this AST node short-circuit?"
  [node]
  (and (short-circuitable-op? node)
       (mutatable-expr? (get-in-tree [1] node))))
(def logic-as-control-flow-atom? short-circuitable-expr?)

(defn logic-as-control-flow-atoms
  "Return all instances of logic-as-control-flow in an AST"
  [node]
  (cond
    (leaf? node) nil
    (short-circuitable-expr? node) [node]
    :else (mapcat logic-as-control-flow-atoms (children node))))
