(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IBasicType IBasicType$Kind IASTNode IASTSimpleDeclSpecifier IASTDeclaration))

(s/defn type-conversion-atom? :- s/Bool [node :- IASTNode] false)

(s/defrecord CoercionType
    [datatype :- s/Keyword modifiers :- #{s/Keyword} size :- s/Int])
(s/defrecord CoercionContext
    ;[node-type :- s/Keyword context-type :- CoercionType arg-type :- CoercionType])
    [node-type :- s/Keyword context-type arg-type])

(s/defrecord TypeDescription
    [number-type :- s/Keyword bits :- s/Int])

(def type-components
  [
[IBasicType$Kind/eBoolean      IASTSimpleDeclSpecifier/t_bool          (TypeDescription. :int 1)]       ;[:bool nil]]
[IBasicType$Kind/eChar         IASTSimpleDeclSpecifier/t_char          (TypeDescription. :int 8)]         ;[:char 8]]
[IBasicType$Kind/eChar16       IASTSimpleDeclSpecifier/t_char16_t      (TypeDescription. :int 16)]        ;[:char 16]]
[IBasicType$Kind/eChar32       IASTSimpleDeclSpecifier/t_char32_t      (TypeDescription. :int 32)]        ;[:char 32]]
[IBasicType$Kind/eDecimal128   IASTSimpleDeclSpecifier/t_decimal128    (TypeDescription. :real 128)]    ;[:decimal 128]]
[IBasicType$Kind/eDecimal32    IASTSimpleDeclSpecifier/t_decimal32     (TypeDescription. :real 32)]     ;[:decimal 32]]
[IBasicType$Kind/eDecimal64    IASTSimpleDeclSpecifier/t_decimal64     (TypeDescription. :real 64)]     ;[:decimal 64]]
[nil                           IASTSimpleDeclSpecifier/t_decltype      (TypeDescription. :decltype nil)]   ;[:decltype nil]]
[nil                           IASTSimpleDeclSpecifier/t_decltype_auto (TypeDescription. :decltype nil)]   ;[:decltype nil]]
[IBasicType$Kind/eDouble       IASTSimpleDeclSpecifier/t_double        (TypeDescription. :real 64)]       ;[:float 64]]
[IBasicType$Kind/eFloat        IASTSimpleDeclSpecifier/t_float         (TypeDescription. :real 32)]       ;[:float 32]]
[IBasicType$Kind/eFloat128     IASTSimpleDeclSpecifier/t_float128      (TypeDescription. :real 128)]      ;[:float 128]]
[IBasicType$Kind/eInt          IASTSimpleDeclSpecifier/t_int           (TypeDescription. :int 32)]         ;[:int 32]]
[IBasicType$Kind/eInt128       IASTSimpleDeclSpecifier/t_int128        (TypeDescription. :int 128)]        ;[:int 128]]
[IBasicType$Kind/eNullPtr      IASTSimpleDeclSpecifier/t_typeof        (TypeDescription. :typeof nil)]     ;[:typeof nil]]
[IBasicType$Kind/eUnspecified  IASTSimpleDeclSpecifier/t_unspecified   (TypeDescription. :unspecified nil)];[:unspecified nil]]
[IBasicType$Kind/eVoid         IASTSimpleDeclSpecifier/t_void          (TypeDescription. :void nil)]       ;[:void nil]]
[IBasicType$Kind/eWChar        IASTSimpleDeclSpecifier/t_wchar_t       (TypeDescription. :wchar nil)]      ;[:wchar nil]]
                 ])

(def decl-type (into {} (map #(into [] (vals (select-keys % [1 2]))) type-components)))
(def intl-type (into {} (map #(into [] (vals (select-keys % [0 2]))) type-components)))

(s/defn coercing-declaration; :- CoercionContext
  [node :- IASTDeclaration]
  (let [context-type (->> node .getDeclSpecifier .getType decl-type)
        arg-types (map #(->> % .getInitializer .getInitializerClause
                               .getExpressionType .getKind intl-type)
                       (.getDeclarators node))]

    (or
      ; real -> int
      (and (#{:int} (:number-type context-type))
           (any-pred? #(#{:real} (:number-type %)) arg-types))

      ; large -> small
      (any-pred? #(< (:bits context-type) (:bits %)) arg-types)
  )))

(->> "char V1 = 261"
     parse-expr
     coercing-declaration
     )

(->> "int *V1 = 1.99, v2 = asdf"
     parse-expr
     .getDeclarators
     first
     .getInitializer
     .getInitializerClause
     .getExpressionType
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
