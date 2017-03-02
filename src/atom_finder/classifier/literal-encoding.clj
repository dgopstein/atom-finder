(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTLiteralExpression))

(defmulti radix "In which base is the number specified" class)
(s/defmethod radix String :- s/Keyword
  [num :- s/Str]
  ; This is a simplification. See for details:
  ; cdt/core/org.eclipse.cdt.core/parser/org/eclipse/cdt/internal/core/dom/parser/cpp/CPPASTLiteralExpression.java
  (condp re-find num
    #"^[+-]?0x" :hex
    #"^[+-]?0b" :bin
    #"^[+-]?0([^0-9]|$)" :dec ; Matches "0", "0.1" This may not technically be true buuuut...
    #"^[+-]?0.[0-9]*[.eE]" :dec ; There is no octal float in C
    #"^[+-]?0"  :oct
                :dec
  ))

(s/defmethod radix IASTNode :- s/Keyword [n] :non-literal)

(s/defmethod radix IASTLiteralExpression :- (s/maybe s/Keyword) [node]
  (let [numeric-literal-types
        #{IASTLiteralExpression/lk_float_constant
          IASTLiteralExpression/lk_integer_constant}]
    (or
      (and (numeric-literal-types (.getKind node))
           (radix (String. (.getValue node))))
      :non-numeric)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      contexts for literal encoding     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; - Decimals used in bitwise operations
; - Chars used in arithmetic operations? ('A' - 'a') is pretty legit though?

(defmulti bitwise-op? "Check if an ASTNode represents a bitwise operator" class)

(s/defmethod bitwise-op? IASTBinaryExpression :- s/Bool [node]
  (let [bitwise-ops #{
          IASTBinaryExpression/op_binaryAnd  IASTBinaryExpression/op_binaryAndAssign
          IASTBinaryExpression/op_binaryOr   IASTBinaryExpression/op_binaryOrAssign
          IASTBinaryExpression/op_binaryXor  IASTBinaryExpression/op_binaryXorAssign
          IASTBinaryExpression/op_shiftLeft  IASTBinaryExpression/op_shiftLeftAssign
          IASTBinaryExpression/op_shiftRight IASTBinaryExpression/op_shiftRightAssign
                      }]
  (contains? bitwise-ops (.getOperator node))))

(s/defmethod bitwise-op? IASTUnaryExpression :- s/Bool [node]
  (contains? #{IASTUnaryExpression/op_tilde} (.getOperator node)))

(defmethod bitwise-op? :default [x] false)

(s/defn literal-encoding-atom? :- Boolean
  "Change of Literal Encoding atom classifier"
  [node :- IASTNode]
  (and (bitwise-op? node)
       (any-true? (children node)
  )))
