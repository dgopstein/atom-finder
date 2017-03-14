(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IBasicType IBasicType$Kind IASTNode IASTSimpleDeclSpecifier IASTDeclaration))

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
[TypeKind/eBoolean      DeclSpec/t_bool          (TD. :int 1)]
[TypeKind/eChar         DeclSpec/t_char          (TD. :int 8)]
[TypeKind/eChar16       DeclSpec/t_char16_t      (TD. :int 16)]
[TypeKind/eChar32       DeclSpec/t_char32_t      (TD. :int 32)]
[TypeKind/eDecimal128   DeclSpec/t_decimal128    (TD. :real 128)]
[TypeKind/eDecimal32    DeclSpec/t_decimal32     (TD. :real 32)]
[TypeKind/eDecimal64    DeclSpec/t_decimal64     (TD. :real 64)]
[nil                           DeclSpec/t_decltype      (TD. :decltype nil)]
[nil                           DeclSpec/t_decltype_auto (TD. :decltype nil)]
[TypeKind/eDouble       DeclSpec/t_double        (TD. :real 64)]
[TypeKind/eFloat        DeclSpec/t_float         (TD. :real 32)]
[TypeKind/eFloat128     DeclSpec/t_float128      (TD. :real 128)]
[TypeKind/eInt          DeclSpec/t_int           (TD. :int 32)]
[TypeKind/eInt128       DeclSpec/t_int128        (TD. :int 128)]
[TypeKind/eNullPtr      DeclSpec/t_typeof        (TD. :typeof nil)]
[TypeKind/eUnspecified  DeclSpec/t_unspecified   (TD. :unspecified nil)]
[TypeKind/eVoid         DeclSpec/t_void          (TD. :void nil)]
[TypeKind/eWChar        DeclSpec/t_wchar_t       (TD. :wchar nil)]
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
      (any-pred? #(and (%1)
                       (< (:bits context-type) (number-bits (parse-numeric-literal %1)))) (map vector arg-types arg-exprs))
  )))

(->> "char V1 = 261"
     parse-expr
     ;coercing-declaration
     .toString
     )

(->> "int *V1 = 1.99, v2 = asdf"
     parse-expr
     .getDeclarators
     first
     .getInitializer
     .getInitializerClause
     .toString
     )

; https://www.safaribooksonline.com/library/view/c-in-a/0596006977/ch04.html
(s/defn coercing-node? :- s/Bool
  "A node which can have children that implicitly convert their arguments to a different type"
  [node])
  ; declaration
  ; assignment
  ; function call
  ; return statement
  ; type-casting

  ; _Bool < char < short < int < long < long long
  ; float < double < long double

  ; char x = (unsigned char) -12
  )

(->> "V1 = 1.99"
     parse-expr
     ;children
     ;(map typename)
     ;last
     ;children
     ;(map write-ast)
     )
