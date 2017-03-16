(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast
          IASTNode IASTBinaryExpression IASTExpressionList
          IASTExpressionStatement IASTForStatement IASTUnaryExpression))

(defmulti post-*crement? "Is this node a post-*crement expression" class)
(s/defmethod post-*crement? :default :- s/Bool [node] false)
(s/defmethod post-*crement? IASTUnaryExpression :- s/Bool
  [node]
  (contains? #{IASTUnaryExpression/op_postFixDecr
               IASTUnaryExpression/op_postFixIncr}
             (.getOperator node)))

(defn post-*crement-atom? [node]
  ; certain nodes can never "use" the value of an expression
  (and (not (any-pred? #(% node) [(partial instance? IASTExpressionList)
                                  (partial instance? IASTExpressionStatement)
                                  paren-node?]))
       (->> node
            value-consuming-children
            (map remove-wrappers)
            (any-pred? post-*crement?)
            )))

(defn post-*crement-not-atom?
  "Is this node a post-*crement that isn't an atom?"
  [node]
  (and (post-*crement? node)
       (not (any-pred? post-*crement-atom? (all-parents node)))))

; Check if we miss any instances of the atom:
;  (print-atoms-in-dir
;   (expand-home "~/opt/src/redis")
;   ;(map atom-lookup [:post-increment])
;   [{:name :post-*crement-not-atom, :finder
;     (atom-finder.classifier/default-finder post-*crement-not-atom?)}]
;   )
;(def psgl (read-string (slurp (clojure.java.io/file (ClassLoader/getSystemResource "data/redis_not_post_increment_2017-03-04.edn")))))
;(found-atom-source :post-*crement-not-atom psgl)
