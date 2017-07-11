(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTBinaryExpression IASTConditionalExpression IASTUnaryExpression)
        '(org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTConditionalExpression CPPASTExpressionList CPPASTFieldReference))

(defmulti operator-group"Returns the name of the operator group that the node belongs to, nil not defined in this operator-group" class)
(defmethod operator-group :default [node]
  (let [operator-group-table
    {CPPASTConditionalExpression :cond
     CPPASTExpressionList :comma
     CPPASTFieldReference :field_ref}]
    (->> node type operator-group-table)))
(defmethod operator-group IASTUnaryExpression [node]
  (let [operator-group-table
    {IASTUnaryExpression/op_postFixDecr :de_incr
     IASTUnaryExpression/op_postFixIncr :de_incr
     IASTUnaryExpression/op_minus :arith_unary
     IASTUnaryExpression/op_plus :arith_unary
     IASTUnaryExpression/op_prefixDecr :de_incr
     IASTUnaryExpression/op_prefixIncr :de_incr
     IASTUnaryExpression/op_amper :pointer
     IASTUnaryExpression/op_star :pointer
     IASTUnaryExpression/op_not :not
     IASTUnaryExpression/op_tilde :bitwise}]
    (->> node .getOperator operator-group-table)))
(defmethod operator-group IASTBinaryExpression [node]
  (if (assignment? node)
    :assign

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
           (->> node .getOperator operator-group-table))))


(defn specific-confusing-operator-combination?
  "if the combination of groups of operators exists in this set, then the combination is confusing"
  [combination]
  (some #(= % combination) (map sort #{[:de_incr :pointer] [:multiply :add] [:arith_unary :add] [:arith_unary :multiply] [:and :add] [:and :multiply] [:and :arith_unary] [:or :add] [:or :multiply] [:or :arith_unary] [:or :and] [:not :add] [:not :multiply] [:not :arith_unary] [:not :and] [:not :or]
;;[:compare :and] [:compare :or] // Underclassify
[:compare :not] [:compare :compare] [:pointer :add] [:cond :arith_unary] [:cond :and] [:cond :or] [:cond :not] [:cond :compare] [:cond :cond] [:non-asso :add] [:non-asso :multiply] [:non-asso :arith_unary] [:non-asso :and] [:non-asso :or] [:non-asso :not] [:non-asso :non-asso] [:field_ref :pointer]})))

(def always-confusing-operator-groups
  "If any of these operator groups is used along with another operator, then it is confusing"
  #{:bitwise :comma})

(defn confusing-operator-combination?
  "Is this combination of the operator groups confusing?"
  [combination]
  (or (specific-confusing-operator-combination? combination)
      (some (partial contains? always-confusing-operator-groups) combination)))

(defn operator-group-pair?
  [node]
  (and (operator-group node)
       (some operator-group (children node))))

(defn group-pair
  "returns a collection of operator group pairs between the node and its children
  , if a second parameter is passed, use that as the collection instead of the children"
  ([node]
   (->> node
        children
        (map operator-group)
        (remove nil?)
        (map (fn [child-group] (sort [(operator-group node) child-group])))))

  ([node collection]
   (->> collection
        (map operator-group)
        (remove nil?)
        (map (fn [child-group] (sort [(operator-group node) child-group]))))))


;;
;;The following 3 functions are for the binary operator special case
;;
(defn rvalue-unary-operator?
  "Is this a unary operator that can operate on an rvalue? (! - + *)"
  [node]
  (let [node-group (operator-group node)]
    (or (= :arith_unary node-group)
        (= :not node-group)
        (= :pointer node-group))))

(defn unbracktted-unary-in-children
  "Is this node not a bracket and constains unary operator in its children?"
  [node]
  (if (not= "()" (write-node node))
      (filter-tree #(rvalue-unary-operator? %) node)))

(defn group-pairs-in-binary-operator
  "Special function for returning a collection of operator group in binary operator's children, binary operator should ignore some groups of operators in its second operand"
  [node]
  (->> (concat [(get-in-tree [0] node)
                (if (not (rvalue-unary-operator? (get-in-tree [1] node)))
                  (get-in-tree [1] node) nil)]

               (unbracktted-unary-in-children (get-in-tree [0] node)))
       (map operator-group)
       (remove nil?)
       (map (fn [child-group] (sort [(operator-group node) child-group])))))



(defn operator-precedence-atom?
  "Is this node an operator-precedence-atom?"
  [node]
  (and
   (operator-group-pair? node)

   (not (= :assign (operator-group node)))

   (if (instance? IASTBinaryExpression node)
     (some confusing-operator-combination? (group-pairs-in-binary-operator node))
     (some confusing-operator-combination? (group-pair node)))))
