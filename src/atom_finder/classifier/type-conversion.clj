(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IBasicType IBasicType$Kind IASTNode IASTSimpleDeclSpecifier IASTDeclaration))

DeclSpec/eBoolean
TypeKind
IBasicType$Kind/eBoolean

(s/defn type-conversion-atom? :- s/Bool [node :- IASTNode] false)

(s/defrecord CoercionType
    [datatype :- s/Keyword modifiers :- #{s/Keyword} size :- s/Int])
(s/defrecord CoercionContext
    ;[node-type :- s/Keyword context-type :- CoercionType arg-type :- CoercionType])
    [node-type :- s/Keyword context-type arg-type])

(s/defrecord TD ;TypeDescription
    [number-type :- s/Keyword bits :- s/Int])
(def DeclSpec IASTSimpleDeclSpecifier)
(def TypeKind IBasicType$Kind)

(def type-components
  [
[IBasicType$Kind/eBoolean      IASTSimpleDeclSpecifier/t_bool          (TD. :int 1)]
[IBasicType$Kind/eChar         IASTSimpleDeclSpecifier/t_char          (TD. :int 8)]
[IBasicType$Kind/eChar16       IASTSimpleDeclSpecifier/t_char16_t      (TD. :int 16)]
[IBasicType$Kind/eChar32       IASTSimpleDeclSpecifier/t_char32_t      (TD. :int 32)]
[IBasicType$Kind/eDecimal128   IASTSimpleDeclSpecifier/t_decimal128    (TD. :real 128)]
[IBasicType$Kind/eDecimal32    IASTSimpleDeclSpecifier/t_decimal32     (TD. :real 32)]
[IBasicType$Kind/eDecimal64    IASTSimpleDeclSpecifier/t_decimal64     (TD. :real 64)]
[nil                           IASTSimpleDeclSpecifier/t_decltype      (TD. :decltype nil)]
[nil                           IASTSimpleDeclSpecifier/t_decltype_auto (TD. :decltype nil)]
[IBasicType$Kind/eDouble       IASTSimpleDeclSpecifier/t_double        (TD. :real 64)]
[IBasicType$Kind/eFloat        IASTSimpleDeclSpecifier/t_float         (TD. :real 32)]
[IBasicType$Kind/eFloat128     IASTSimpleDeclSpecifier/t_float128      (TD. :real 128)]
[IBasicType$Kind/eInt          IASTSimpleDeclSpecifier/t_int           (TD. :int 32)]
[IBasicType$Kind/eInt128       IASTSimpleDeclSpecifier/t_int128        (TD. :int 128)]
[IBasicType$Kind/eNullPtr      IASTSimpleDeclSpecifier/t_typeof        (TD. :typeof nil)]
[IBasicType$Kind/eUnspecified  IASTSimpleDeclSpecifier/t_unspecified   (TD. :unspecified nil)]
[IBasicType$Kind/eVoid         IASTSimpleDeclSpecifier/t_void          (TD. :void nil)]
[IBasicType$Kind/eWChar        IASTSimpleDeclSpecifier/t_wchar_t       (TD. :wchar nil)]
                 ])

(def decl-type (into {} (map #(into [] (vals (select-keys % [1 2]))) type-components)))
(def intl-type (into {} (map #(into [] (vals (select-keys % [0 2]))) type-components)))

(s/defn coercing-declaration; :- CoercionContext
  [node :- IASTDeclaration]
  (let [context-type (->> node .getDeclSpecifier .getType decl-type)
        arg-exprs (map #(->> % .getInitializer .getInitializerClause) (.getDeclarators node))
        arg-types (map #(->> % .getExpressionType .getKind intl-type) arg-exprs)]

    (or
      ; real -> int
      (and (#{:int} (:number-type context-type))
           (any-pred? #(#{:real} (:number-type %)) arg-types))

      ; large -> small (count the number of bits necessary to represent the number literal)
      (any-pred? (fn [[type expr]]
                   (and (numeric-literal? expr)
                        (#{:int} (:number-type type))
                        (< (:bits context-type) (number-bits (parse-numeric-literal (write-ast expr))))))
                 (map vector arg-types arg-exprs))
  )))

(->> "char V1 = -261"
     parse-expr
     coercing-declaration
     )
(numeric-literal? (parse-expr"261"))

(->> "-0x235"
     radix
     )

(->> "int *V1 = 1.99, v2 = -265"
     parse-expr
     .getDeclarators
     last
     .getInitializer
     .getInitializerClause
     )

; https://www.safaribooksonline.com/library/view/c-in-a/0596006977/ch04.html
(s/defn coercing-node? :- s/Bool
  "A node which can have children that implicitly convert their arguments to a different type"
  [node] false)
  ; declaration
  ; assignment
  ; function call
  ; return statement
  ; type-casting

  ; _Bool < char < short < int < long < long long
  ; float < double < long double

  ; char x = (unsigned char) -12
  ;)

(->> "V1 = 1.99"
     parse-expr
     ;children
     ;(map typename)
     ;last
     ;children
     ;(map write-ast)
     )
