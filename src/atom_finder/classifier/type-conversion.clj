(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IBasicType IBasicType$Kind IASTNode IASTSimpleDeclSpecifier IASTSimpleDeclaration IASTDeclaration IASTInitializerList ISemanticProblem)
        '(java.text ParseException))

(s/defn type-conversion-atom? :- s/Bool [node :- IASTNode] false)

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
(def basic-type (into {} (map #(into [] (vals (select-keys % [0 2]))) type-components)))

(defmulti unsigned? class)
(s/defmethod unsigned? IASTSimpleDeclaration [node] (->> node .getDeclSpecifier unsigned?))
(s/defmethod unsigned? IASTExpression [node] (->> node .getExpressionType unsigned?))
(s/defmethod unsigned? ISemanticProblem [node] nil) ; tread as false, but that's probably ok
(s/defmethod unsigned? :default [node] (->> node .isUnsigned))

(defmulti unify-type "Go from an arbitrary java node to it's type in clojure data" class)
(s/defmethod unify-type IBasicType :- (s/maybe TD) [node] (->> node .getKind basic-type))
(s/defmethod unify-type IASTSimpleDeclSpecifier :- (s/maybe TD) [node]
  (->> node .getType decl-type))
(s/defmethod unify-type IASTExpression [node]
  (merge (->> node .getExpressionType unify-type)
         {:unsigned? (unsigned? node) :val node}))
(s/defmethod unify-type ISemanticProblem [node]
  (throw (ParseException. (str "Expression type unknown (" (class node) ")") 0)))

(defmulti context-type class)
(s/defmethod context-type IASTBinaryExpression [node]
  (unify-type (.getOperand1 node)))
(s/defmethod context-type IASTSimpleDeclaration [node]
  (merge (->> node .getDeclSpecifier unify-type)
         {:unsigned? (unsigned? node)}))

(defmulti arg-types class)
(s/defmethod arg-types IASTBinaryExpression [node]
  (map unify-type [(.getOperand2 node)]))
(s/defmethod arg-types IASTSimpleDeclaration [node]
  (keep #(some->> % .getInitializer .getInitializerClause unify-type) (.getDeclarators node)))

(s/defn bit-range :- [(s/one s/Int "lower") (s/one s/Int "upper")]
  "Which values can safely be stored in a variable with this type"
  ([type] (bit-range (:unsigned? type) (:bits type)))
  ([unsigned? :- s/Bool bits :- s/Int]
   (let [signed-range [(->> bits dec (Math/pow 2) bigint -)
                       (->> bits dec (Math/pow 2) bigint dec)]]
     (if unsigned?
       (map #(- % (first signed-range)) signed-range)
       signed-range))))

(s/defn type-conversion? :- s/Bool
  [node]
  (try
    (let [context-type (->> node context-type)
          arg-types (->> node arg-types)] ; merge unsigned in
      (boolean (when (and context-type (not-empty arg-types))
                 (let [[context-lower context-upper] (bit-range context-type)]
                   (or
                                        ; real -> int
                    (and (#{:int} (:number-type context-type))
                         (any-pred? #(#{:real} (:number-type %)) arg-types))

                                        ; large -> small
                                        ; signed -> unsigned (and the reverse)
                    (any-pred? (fn [type]
                                 (and (numeric-literal? (:val type))
                                      (#{:int} (:number-type type))
                                      (not (<= context-lower (parse-numeric-literal (:val type)) context-upper))))
                               arg-types)
                    )))))
    (catch ParseException pe false) ; TODO make this more specific to unknown types
    ))

; https://www.safaribooksonline.com/library/view/c-in-a/0596006977/ch04.html
(s/defn type-conversion-atom? :- s/Bool
  "A node which can have children that implicitly convert their arguments to a different type"
  [node :- IASTNode]
  (cond
    (or (instance? IASTSimpleDeclaration node)
        (assignment-node? node))
           (type-conversion? node)
    :else false
    ))

  ; declaration
  ; assignment
  ; function call
  ; return statement
  ; type-casting

  ; _Bool < char < short < int < long < long long
  ; float < double < long double

  ; char x = (unsigned char) -12
