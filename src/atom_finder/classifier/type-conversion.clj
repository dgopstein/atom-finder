(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IBasicType IBasicType$Kind IASTNode IASTSimpleDeclSpecifier IASTSimpleDeclaration IASTDeclaration IASTInitializerList IProblemType))

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
(def basic-type (into {} (map #(into [] (vals (select-keys % [0 2]))) type-components)))
(def all-type (merge decl-type basic-type))

(defmulti unsigned? class)
(s/defmethod unsigned? IASTSimpleDeclaration [node] (->> node .getDeclSpecifier unsigned?))
(s/defmethod unsigned? IASTExpression [node] (->> node .getExpressionType unsigned?))
(s/defmethod unsigned? IProblemType [node] nil) ; tread as false, but that's probably ok
(s/defmethod unsigned? :default [node] (->> node .isUnsigned))

(defmulti unify-type "Go from an arbitrary java node to it's type in clojure data" class)
(s/defmethod unify-type IBasicType :- (s/maybe TD) [node] (->> node .getKind basic-type))
(s/defmethod unify-type IASTSimpleDeclSpecifier :- (s/maybe TD) [node]
  (->> node .getType decl-type))
(s/defmethod unify-type IASTSimpleDeclaration :- (s/maybe TD) [node]
  (merge (->> node .getDeclSpecifier unify-type)
         {:unsigned? (unsigned? node)}))
(s/defmethod unify-type IASTExpression :- (s/maybe TD) [node]
  (merge (->> node .getExpressionType unify-type)
         {:unsigned? (unsigned? node) :val node}))
(s/defmethod unify-type IProblemType [node]
  (throw (Exception. "Expression not parsable")))

(s/defn bit-range :- [(s/one s/Int "lower") (s/one s/Int "upper")]
  "Which values can safely be stored in a variable with this type"
  ([node :- IASTNode]
   (bit-range (unsigned? node) (->> node unify-type :bits)))
  ([unsigned? :- s/Bool bits :- s/Int]
   (let [signed-range [(->> bits dec (Math/pow 2) bigint -)
                       (->> bits dec (Math/pow 2) bigint dec)]]
     (if unsigned?
       (map #(- % (first signed-range)) signed-range)
       signed-range))))

(s/defn td-range :- [(s/one s/Int "lower") (s/one s/Int "upper")] [type :- TD]
  (bit-range (:unsigned? type) (:bits type)))

(s/defn type-conversion-declaration? :- s/Bool
  [node :- IASTSimpleDeclaration]
  (let [context-type (->> node unify-type)]
    (if (nil? context-type)
      false
      (let [[context-lower context-upper] (bit-range node)
            arg-exprs (keep #(some->> % .getInitializer .getInitializerClause) (.getDeclarators node))
            simple-arg-exprs (filter #(not (instance? IASTInitializerList %)) arg-exprs)
            arg-types (keep #(->> % .getExpressionType .getKind basic-type) simple-arg-exprs)]

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
         )))))

(defmulti unify-arg-types class)
(s/defmethod unify-arg-types IASTBinaryExpression [node]
  (map unify-type [(.getOperand2 node)]))


;(s/defn type-conversion-assignment? :- s/Bool
;  [node :- IASTBinaryExpression]
;  (let [context-type (->> node unify-type)]
;    (if (nil? context-type)
;      false
;      (let [[context-lower context-upper] (bit-range node)
;            arg-expr (->> node .getOperand2)
;            arg-type (->> arg-expr unify-type)]
;
;        (boolean
;         (or
;                                        ; real -> int
;          (and (#{:int}  (:number-type context-type))
;               (#{:real} (:number-type arg-type)))
;
;                                        ; large -> small
;                                        ; signed -> unsigned (and the reverse)
;          (and (numeric-literal? arg-expr)
;               (#{:int} (:number-type arg-type))
;               (not (<= context-lower (parse-numeric-literal arg-expr) context-upper)))))
;        ))))

{:unsigned :val}
(s/defn type-conversion? :- s/Bool
  [node]
  (let [context-type (->> node unify-type)
        arg-types (->> node unify-arg-types)] ; merge unsigned in
    (boolean (when (and context-type (not-empty arg-types))
      (let [[context-lower context-upper] (td-range context-type)]
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
         ))))))
(def type-conversion-assignment? type-conversion?)


(->> "int main() { unsigned int V1; V2 = -2; }"
    parse-source
    (get-in-tree [0 2 1 0]) ;.getOperand1 .getExpressionType type-range
    type-conversion-assignment?
    )

; https://www.safaribooksonline.com/library/view/c-in-a/0596006977/ch04.html
(s/defn type-conversion-atom? :- s/Bool
  "A node which can have children that implicitly convert their arguments to a different type"
  [node :- IASTNode]
  (cond
    (instance? IASTSimpleDeclaration node) (type-conversion-declaration? node)
    (assignment-node? node) (type-conversion-assignment? node)
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
  ;)
