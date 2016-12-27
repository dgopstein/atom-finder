(in-ns 'atom-finder.classifier)

(defmulti radix "In which base is the number specified" class)
(s/defmethod radix String :- s/Keyword
  [num :- s/Str]
  ; This is a simplification. See for details:
  ; cdt/core/org.eclipse.cdt.core/parser/org/eclipse/cdt/internal/core/dom/parser/cpp/CPPASTLiteralExpression.java
  (condp (flip clojure.string/starts-with?) num
    "0x" :hex
    "0b" :bin
    "0"  :oct
         :dec
  ))
(s/defmethod radix IASTNode [n :- IASTNode]
  (radix (String. (.getValue n))))
