(in-ns 'atom-finder.util)
(import '(org.eclipse.cdt.core.dom.ast
          IASTNode IASTExpression IASTExpressionList IASTUnaryExpression
          IASTBinaryExpression IASTLiteralExpression IASTForStatement
          IASTFunctionDefinition IASTArraySubscriptExpression IASTCastExpression
          IASTFunctionCallExpression IASTFieldReference IASTFunctionDefinition))

(require '[clj-cdt.expr-operator :refer :all])

(defn greatest-trivial-parent
  "find the highest parent that has the same offset/length as this node"
  [node]
  (->> node all-parents (take-while (partial =by loc node)) last))

(defn least-trivial-child
  "find the lowest only-child that has the same offset/length as this node"
  [node]
  (let [[kid kids] (children node)]
    (if (and kid (not kids)
                  (=by loc node kid))
      (recur kid) node)))

(defn function-node? [node] (instance? IASTFunctionDefinition node))

(defn flatten-tree-context
  ([node] (flatten-tree-context {:path []} node))
  ([context node]
   (-> (fn [idx node]
         (flatten-tree-context
          (merge context
                 {:path         (conj (:path context) idx)
                  :in-function? (or (:in-function? context) (function-node? node))})
                               node))
       (mapcat-indexed (children node))
       (conj [context node]))))

(defn filter-tree-context
  "Find every AST node that matches pred"
  [pred node]
  (->> node flatten-tree-context (filter pred)))

(defn with-context [finder] (fn [context node] (map #(assoc context :node %) (finder node))))

(s/defn potential-atom-nodes
  "places where atoms may be found"
  [node]
  (concat (if (root-node? node) (->> node all-macro-defs (map parse-macro-def-body)) '())
          (->> node flatten-tree (remove contained-by-macro-exp?))))

;(defn default-finder [classifier] (partial filter-tree classifier))
(defn default-finder [classifier] #(->> % potential-atom-nodes (filter classifier)))

(s/defn context-map :- {IASTNode {s/Any s/Any}}
  [root]
  (->> root
       flatten-tree-context
       (map (fn [[ctx node]] [node ctx]))
       (into {})))

(defmacro operation-classifier
  "Identify unary/binary nodes by their operation"
  [expression-type field-name]
  `(fn [node#]
     (and (instance? ~expression-type node#)
          (= (.getOperator node#) (. ~expression-type ~(symbol (str "op_" field-name)))))))

(def paren-node?       (operation-classifier IASTUnaryExpression  bracketedPrimary))
(def unary-minus-node? (operation-classifier IASTUnaryExpression  minus))
(def unary-plus-node?  (operation-classifier IASTUnaryExpression  plus))
(def assignment-node?  (operation-classifier IASTBinaryExpression assign))

(def literal-types {
  IASTLiteralExpression/lk_integer_constant :int
  IASTLiteralExpression/lk_float_constant   :float
  IASTLiteralExpression/lk_char_constant    :char
  IASTLiteralExpression/lk_string_literal   :string
  IASTLiteralExpression/lk_this             :this
  IASTLiteralExpression/lk_true             :true
  IASTLiteralExpression/lk_false            :false
  IASTLiteralExpression/lk_nullptr          :null})

(s/defn literal-type :- s/Keyword [node :- IASTExpression]
  (->> node .getKind literal-types))

(s/defn parse-binary-literal :- s/Int
  "Parse a binary literal from string"
  [s :- String]
  (-> s
      (str/replace #"[bB]" "")
      (Long/parseLong 2)))

(s/defn integral? :- s/Bool
  "Is the number represented by this string an integer (e.g. not a flaoting point number)"
  [s :- String]
  (not-any? #(re-find % s)
            [#"\."          ; decimal
             #"^[^xX]*[eE]" ; exponent character (not in hex)
             #"p"           ; exponent character (in hex number)
             ]))

(defmulti radix "In which base is the number specified" class)
(s/defmethod radix String :- s/Keyword
  [num :- s/Str]
  ; This is a simplification. See for details:
  ; cdt/core/org.eclipse.cdt.core/parser/org/eclipse/cdt/internal/core/dom/parser/cpp/CPPASTLiteralExpression.java
  (condp re-find num
    #"^[+-]?0[xX]" :hex
    #"^[+-]?0[bB]" :bin
    #"^[+-]?0([^0-9]|$)" :dec ; Matches "0", "0.1" This may not technically be true buuuut...
    #"^[+-]?0.[0-9]*[.eE]" :dec ; There is no octal float in C
    #"^[+-]?0"  :oct
                :dec
  ))

(s/defmethod radix IASTNode :- s/Keyword [n] :non-literal)

(s/defmethod radix IASTUnaryExpression :- (s/maybe s/Keyword) [node]
  (if (or (unary-minus-node? node) (unary-plus-node? node))
    (radix (.getOperand node))
    :non-numeric))

(s/defmethod radix IASTLiteralExpression :- (s/maybe s/Keyword) [node]
  (if (#{:int :float} (literal-type node))
    (radix (String. (.getValue node)))
    :non-numeric))

(s/defn numeric-literal? :- s/Bool
  [node :- IASTNode]
  (not (#{:non-literal :non-numeric} (radix node))))

(defmulti parse-numeric-literal "Parse any valid C++ numeric literal from a string for it's value" class)

(s/defmethod parse-numeric-literal :default :- (s/maybe s/Num) [node] nil)

(s/defmethod parse-numeric-literal IASTUnaryExpression :- (s/maybe s/Num) [node]
  (cond
    (unary-minus-node? node) (parse-numeric-literal (str "-" (.getOperand node)))
    (unary-plus-node?  node) (parse-numeric-literal (str "+" (.getOperand node)))
    :else nil))

(s/defmethod parse-numeric-literal IASTLiteralExpression :- (s/maybe s/Num) [node]
  (parse-numeric-literal (String. (.getValue node))))

(defn remove-suffix [num-str] (str/replace num-str #"[uUlL]*$" ""))
(defn remove-prefix [num-str] (str/replace num-str #"^0[bBxX]" ""))

(s/defmethod parse-numeric-literal String :- (s/maybe s/Num) [s-in]
  (let [s (remove-suffix s-in)] ; remove suffix
    (condp contains? (radix s)
      #{:oct :dec :hex}
        (if (integral? s)
          (try
            (Long/decode s)
            (catch NumberFormatException e
              (java.math.BigInteger. (remove-prefix s)
                                     ({:oct 8 :dec 10 :hex 16} (radix s)))))
          (Double/parseDouble s))
      #{:bin}
        (parse-binary-literal s)
          nil)))

;; https://stackoverflow.com/a/600306/
(defn power-of-2? [x]
  (zero? (bit-and x (dec x))))

;; 2^n or 2^(n-1)
(defn power-of-2-ish? [x]
  (or (power-of-2? x)
      (power-of-2? (inc x))))

(defn log2 [n] (/ (Math/log n) (Math/log 2)))
(def number-bits "How many bits are requited to store an integer value" log2)

(defn remove-wrappers
  "Drill down the tree past expressions that just return the value of their direct children"
  [node]
  (let [new-node
        (cond
          ; comma operators only return the value of their second operand
          (instance? IASTExpressionList node) (last (children node))
          ; parenthesis just return their only child
          (paren-node? node) (.getOperand node)
          :else node)]

    (if (= node new-node)
      node
      (remove-wrappers new-node))))

(defn value-consuming-children
  "Return the children of this node who's values will be used. e.g. The first and third clauses of a for loop don't use the values of their expression, but the second one does, so return that one"
  [node]
  (condp instance? node
    IASTForStatement   [(.getConditionExpression node)]
    (children node)))

(defn enclosing-function
  "find the nearest ancestor that's a function definition"
  [node]
  (if (or (nil? node)
          (function-node? node))
    node
    (enclosing-function (parent node))))

(defn toplevel-offset?
  "Check if an offset lives in the top level or if it's inside some other node"
  [root offset]
  (not-any? #(contains-offset? % offset) (children root)))

(s/defn intersects-line-range?
  "Does this node contain the given line range"
  [start-line :- s/Int end-line :- s/Int node :- IASTNode]
  (let [node-loc (loc node)]
    (and
     node-loc
     (<= start-line (:end-line node-loc))
     (<  (:start-line node-loc) end-line))))

(s/defn contained-by-line-range?
  "Does this line range contain the given node"
  [start-line :- s/Int end-line :- s/Int node :- IASTNode]
  (let [node-loc (loc node)]
    (and
     node-loc
     (<= start-line (:start-line node-loc))
     (<  (:end-line node-loc) end-line))))

(s/defn contains-line-range?
  "Does this node contain the given line range"
  [start-line :- s/Int end-line :- s/Int node :- IASTNode]
  (let [node-loc (loc node)]
    (and
     node-loc
     (>  start-line (:start-line node-loc))
     (>= (:end-line node-loc) end-line))))

(s/defn line-range-parent?
  [start-line :- s/Int end-line :- s/Int node :- IASTNode]
  (and
    (intersects-line-range? start-line end-line node)
    (not (contained-by-line-range? start-line end-line node))
    (every? (partial contained-by-line-range? start-line end-line) (children node))))

; TODO this is untested, also it should probably use binary search for efficiency
(s/defn line-range-parent
  "Search node for the parent of the line range"
  [start-line :- s/Int end-line :- s/Int root :- IASTNode]
  (let [kids      (children root)
        container (find-first (partial contains-line-range? start-line end-line) kids)]
    (if (nil? container)
      root
      (recur start-line end-line container))))

(defn in-function? [node] (ancestral-instance? IASTFunctionDefinition node))

(s/defn precedence-level
  [node :- IASTNode]
  (some->> node expr-operator :precedence))

(defn assignment?
  "Returns true if the operator is an assignment operator"
  [node]
  (let [assignment-list
        #{IASTBinaryExpression/op_assign IASTBinaryExpression/op_binaryAndAssign IASTBinaryExpression/op_binaryOrAssign IASTBinaryExpression/op_binaryXorAssign IASTBinaryExpression/op_divideAssign IASTBinaryExpression/op_minusAssign IASTBinaryExpression/op_moduloAssign IASTBinaryExpression/op_multiplyAssign IASTBinaryExpression/op_plusAssign IASTBinaryExpression/op_shiftLeftAssign IASTBinaryExpression/op_shiftRightAssign}]

    (and (instance? IASTBinaryExpression node) (contains? assignment-list (.getOperator node)))))

(def logical-operators #{IASTBinaryExpression/op_equals
                         IASTBinaryExpression/op_greaterEqual
                         IASTBinaryExpression/op_greaterThan
                         IASTBinaryExpression/op_lessThan
                         IASTBinaryExpression/op_lessEqual
                         IASTBinaryExpression/op_logicalAnd
                         IASTBinaryExpression/op_logicalOr
                         IASTBinaryExpression/op_notequals
                         IASTUnaryExpression/op_not})

(defn misparsed-template?
  "Templates are often parsed as less-than/greater-than, catch that occasion"
  [node]
  (and
   (some->> node expr-operator :name (= :greaterThan))
   (some (%->> expr-operator :name (= :lessThan)) (children node))))
