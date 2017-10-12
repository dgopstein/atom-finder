(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression IASTLiteralExpression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;      contexts for literal encoding     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; - Decimals used in bitwise operations
;   - Assume numbers 2^n or 2^(n-1) are ok, even if expressed in decimal
; - Chars used in arithmetic operations?
;   - ('A' - 'a') is pretty legit though?
;     - but it IS hard to calculate in your head if you don't have an ascii table)

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

(defn shift-op? [node]
  (and (instance? IASTBinaryExpression node)
    (let [bitwise-ops
          #{IASTBinaryExpression/op_shiftLeft  IASTBinaryExpression/op_shiftLeftAssign
            IASTBinaryExpression/op_shiftRight IASTBinaryExpression/op_shiftRightAssign}]
      (contains? bitwise-ops (.getOperator node)))))

(s/defn literal-encoding-atom? :- s/Bool
  "Change of Literal Encoding atom classifier"
  [node :- IASTNode]
  (and (bitwise-op? node)
       (any-pred?
        (fn [kid]
          (and (= :dec (radix kid))
               (let [num (or (parse-numeric-literal kid)
                             ;; this is a sloppy shotgun that doesn't work inside macro-expansion
                             (parse-numeric-literal (write-ast kid)))]
                 ;; Exceptions - decimal numbers that are ok in bitwise ops
                 (not (or
                  ;; dec and oct are the same for numbers lower than 8
                  ;; so bitwise comparisons for literals lower than 8
                  ;; probably aren't confusing
                  (> 8 (Math/abs num))
                  ;; 2^n and 2^(n-1) integers are widely memorized in decimal
                  ;; so ignore literals like 15, 128, 65536
                  (and (integer? num) (power-of-2-ish? num)))))))
        (if (shift-op? node)
          [(.getOperand1 node)]
          (children node))
        ))
  )
