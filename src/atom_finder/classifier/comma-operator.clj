(in-ns 'atom-finder.classifier)

(import '(org.eclipse.cdt.core.dom.ast IASTExpressionList IASTFunctionCallExpression))

(require '[clj-cdt.expr-operator :refer :all])

(defn misparsed-template?
  "Templates are often parsed as less-than/greater-than, catch that occasion"
  [node]
  (and
   (some->> node expr-operator :name (= :greaterThan))
   (some (%->> expr-operator :name (= :lessThan)) (children node))))

(defn misparsed-nested-macro-arg?
  "When eclipse doesn't know a function is a macro, it doesn't know to pass printf arguments along for the ride"
  [node]
  (and (some->> node (instance? IASTFunctionCallExpression))
       (some->> node (get-in-tree [1]) expr-operator :name (= :bracketedPrimary))
       (some->> node (get-in-tree [1 0]) (instance? IASTExpressionList))
       ))

(defn comma-operator-atom? [node]
  (and
   (instance? IASTExpressionList node)
   (not-any? misparsed-template? (all-parents node)) ;; f<T>(a,b)
   (not-any? misparsed-nested-macro-arg? (all-parents node)) ;; M1((a, b))
   ))
