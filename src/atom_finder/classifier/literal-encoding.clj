(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTLiteralExpression))

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

(s/defn literal-encoding-atom? :- s/Bool
  "Change of Literal Encoding atom classifier"
  [node :- IASTNode]
  (and (bitwise-op? node)
       (any-pred?
        #(and (= :dec (radix %1))
              ; dec and oct are the same for numbers lower than 8
              ; so bitwise comparisons for literals lower than 8
              ; probably aren't confusing
              (>= (parse-numeric-literal (.toString %1)) 8))
         (children node)))
  )
