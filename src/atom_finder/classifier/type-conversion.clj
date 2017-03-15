(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IBasicType IBasicType$Kind IASTNode IASTSimpleDeclSpecifier IASTSimpleDeclaration IASTDeclaration IASTInitializerList))

(s/defn type-conversion-atom? :- s/Bool [node :- IASTNode] false)

(s/defrecord CoercionType
    [datatype :- s/Keyword modifiers :- #{s/Keyword} size :- s/Int])
(s/defrecord CoercionContext
    ;[node-type :- s/Keyword context-type :- CoercionType arg-type :- CoercionType])
    [node-type :- s/Keyword context-type arg-type])

(s/defrecord TD ;TypeDescription
    [number-type :- s/Keyword bits :- s/Int])

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

(s/defn decl-spec-range :- [(s/one s/Int "lower") (s/one s/Int "upper")]
  "Which values can safely be stored in a variable declared to this type"
  [node :- IASTSimpleDeclSpecifier]

  (let [unsigned (.isUnsigned node)
        bits (->> node .getType decl-type :bits)
        signed-range [(->> bits dec (Math/pow 2) bigint -)
                      (->> bits dec (Math/pow 2) bigint dec)]]

    (if unsigned
      (map #(- % (first signed-range)) signed-range)
      signed-range)))

(defmulti type-conversion-atom? class)
(s/defmethod type-conversion-atom? :default [node] false)
(s/defmethod type-conversion-atom? IASTSimpleDeclaration
  [node]
  (let [context-spec (.getDeclSpecifier node)
        context-type (->> context-spec .getType decl-type)
        [context-lower context-upper] (decl-spec-range context-spec)
        arg-exprs (keep #(some->> % .getInitializer .getInitializerClause) (.getDeclarators node))
        simple-arg-exprs (filter #(not (instance? IASTInitializerList %)) arg-exprs)
        arg-types (keep #(->> % .getExpressionType .getKind intl-type) simple-arg-exprs)]

    (or
      ; real -> int
      (and (#{:int} (:number-type context-type))
           (any-pred? #(#{:real} (:number-type %)) arg-types))

      ; large -> small
      ; signed -> unsigned (and the reverse)
      (any-pred? (fn [[type expr]]
                   (and (numeric-literal? expr)
                        (#{:int} (:number-type type))
                        (not (<= context-lower (parse-numeric-literal expr) context-upper))))
                 (map vector arg-types arg-exprs))
  )))

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
