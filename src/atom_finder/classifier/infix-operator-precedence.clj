(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTBinaryExpression IASTConditionalExpression IASTUnaryExpression IASTFieldReference)
        '(org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTConditionalExpression CPPASTExpressionList))

(def confusing-operator-combinations
  "if the combination of groups of operators exists in this set, then the combination is confusing"
  {[:multiply :add] [:arith_unary :add] [:arith_unary :multiply]
   [:and :or] [:not :and] [:not :or]
   [:not :compare] [:not :and] [:pointer :add] [:cond :cond]})

(def always-confusing-operator-groups
  "If any of these operator groups is used along with another operator, then it is confusing"
  #{:bitwise :non-asso :comma})

(def never-confusing-operator-groups
  "If any of these operator groups is used along with another operator, then it is not confusing"
  #{:field_ref})

(defmulti get-operator-group"Returns the name of the operator group that the node belongs to, nil not defined in this operator-group" class)
(defmethod get-operator-group :default [node]
  (let [operator-group-table
    {CPPASTConditionalExpression :cond
     CPPASTExpressionList :comma
     IASTFieldReference :field_ref}]
    (->> node type operator-group-table)))
(defmethod get-operator-group IASTUnaryExpression [node]
  (let [operator-group-table
    {IASTUnaryExpression/op_postFixDecr :arith_unary
     IASTUnaryExpression/op_postFixIncr :arith_unary
     IASTUnaryExpression/op_minus :arith_unary
     IASTUnaryExpression/op_plus :arith_unary
     IASTUnaryExpression/op_prefixDecr :arith_unary
     IASTUnaryExpression/op_prefixIncr :arith_unary
     IASTUnaryExpression/op_amper :pointer
     IASTUnaryExpression/op_star :pointer
     IASTUnaryExpression/op_not :not
     IASTUnaryExpression/op_tilde :bitwise}]
    (->> node .getOperator operator-group-table)))
(defmethod get-operator-group IASTBinaryExpression [node]
  (let [operator-group-table
    {IASTBinaryExpression/op_modulo :non-asso
     IASTBinaryExpression/op_multiply :multiply
     IASTBinaryExpression/op_divide :non-asso
     IASTBinaryExpression/op_plus :add
     IASTBinaryExpression/op_minus :non-asso
     IASTBinaryExpression/op_shiftLeft :bitwise
     IASTBinaryExpression/op_shiftRight :bitwise
     IASTBinaryExpression/op_greaterThan :compare
     IASTBinaryExpression/op_greaterEqual :compare 
     IASTBinaryExpression/op_lessThan :compare
     IASTBinaryExpression/op_lessEqual :compare
     IASTBinaryExpression/op_equals :compare
     IASTBinaryExpression/op_notequals :compare
     IASTBinaryExpression/op_binaryAnd :bitwise
     IASTBinaryExpression/op_binaryXor :bitwise
     IASTBinaryExpression/op_binaryOr :bitwise
     IASTBinaryExpression/op_logicalAnd :and
     IASTBinaryExpression/op_logicalOr :or}]
    (->> node .getOperator operator-group-table)))



(defn non-associative?
  "Returns true if a non-associative operator, returns nil otherwise"
  [node]
  (let [non-associatives
        #{ ;Binary operations that are non-associative
          IASTBinaryExpression/op_minus IASTBinaryExpression/op_divide IASTBinaryExpression/op_modulo
          IASTBinaryExpression/op_shiftLeft IASTBinaryExpression/op_shiftRight IASTBinaryExpression/op_greaterThan 
          IASTBinaryExpression/op_greaterEqual IASTBinaryExpression/op_lessThan IASTBinaryExpression/op_lessEqual CPPASTConditionalExpression}]
    (contains? non-associatives (if (instance? IASTBinaryExpression node) (.getOperator node) (type node)))))

(defn operator-equal?
  "compare types two nodes, if both binary expression then compare their operators"
  [& nodes]
  (if (or (every? #(instance? IASTBinaryExpression %) nodes) (every? #(instance? IASTUnaryExpression %) nodes))
    (apply = (map (memfn getOperator) nodes))
    (apply = (map type nodes))))

(defn operator?
  "Is this node an operator?"
  [node] (precedence-level node))

(defn operator-but-not-unary?
  "Is this node an operator?"
  [node]
  (if (not (instance? IASTUnaryExpression node))
    (precedence-level node)
    nil))

(defn unbracktted-unary-operator-in-children?
  "Is this node not a bracket and constains unary operator as a child?"
  [node]
  (and (not= "()" (write-node node))
       (not (empty? (filter-tree #(and (instance? IASTUnaryExpression %)
                                       (operator? %)) node)))))

(defn infix-operator-precedence-atom?
  "Is this node a infix-operator-precedence-atom?"
  [node]
  (and
   (operator? node)

   (not (assignment? node))

   (if (non-associative? node)

     ;;Current node is a non-associative operator
     (some operator? (children node))

     ;;Current node is an associative operator
     (or (and (instance? IASTBinaryExpression node)
              (unbracktted-unary-operator-in-children? (get-in-tree [0] node)))
         (some #(and (operator-but-not-unary? %)
                  (not (operator-equal? node %))) (children node))))))
(comment
 (->> "~\\opt\\src\\gcc\\"
      expand-home
      (pmap-dir-files #(->> % parse-file))
      (map (default-finder infix-operator-precedence-atom?))
                                        ;(remove nil?)     
      (take 10)
      println)

 (defn stuff
   [filename]
   (->> filename parse-file 
        ((default-finder infix-operator-precedence-atom?))
        (map #(vector (write-ast %)))))

 ;;(map #(vector filename (write-ast %)))

 (->> "~\\opt\\src\\gcc\\"
      expand-home
      (pmap-dir-files #(stuff %))
      (take 1000)
      flatten1
      (map println))
 )
