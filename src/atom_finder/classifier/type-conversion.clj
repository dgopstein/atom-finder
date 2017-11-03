(in-ns 'atom-finder.classifier)
(import '(org.eclipse.cdt.core.dom.ast IBasicType IBasicType$Kind IASTNode IASTSimpleDeclSpecifier IASTElaboratedTypeSpecifier IASTSimpleDeclaration IASTDeclaration IASTDeclarator IASTEqualsInitializer IASTInitializerList IPointerType ISemanticProblem IASTFunctionCallExpression IFunctionType IASTReturnStatement IASTCastExpression)
        '(java.text ParseException)
        '(org.eclipse.cdt.internal.core.dom.parser.cpp.semantics EvalBinding))

(s/defn type-conversion-atom? :- s/Bool [node :- IASTNode] false)

(s/defrecord TD ;TypeDescription
    [number-type :- s/Keyword bits :- s/Int])
(def FullType {(s/required-key :number-type) s/Keyword
               (s/required-key :bits) (s/maybe s/Int)
               (s/optional-key :unsigned?) s/Bool
               (s/optional-key :val) IASTNode})

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

;(ns-unmap 'atom-finder.classifier 'unify-type)
(defmulti unify-type "Go from an arbitrary java node to it's type in clojure data" class)
(s/defmethod unify-type IBasicType :- (s/maybe FullType) [node]
  (some-> node .getKind basic-type (merge {:unsigned? (unsigned? node)})))
(s/defmethod unify-type IASTSimpleDeclSpecifier :- (s/maybe FullType) [node]
  (some-> node .getType decl-type (merge {:unsigned? (unsigned? node)})))
(s/defmethod unify-type IASTExpression :- (s/maybe FullType) [node]
  (some-> node .getExpressionType unify-type (merge {:val node})))
(s/defmethod unify-type ISemanticProblem [node]
  (throw (ParseException. (str "Expression type unknown (" (class node) ")") 0)))
(s/defmethod unify-type IASTElaboratedTypeSpecifier :- (s/maybe FullType) [node]
  ;; static const struct stream_vtable
  nil)
(s/defmethod unify-type IASTInitializerList :- (s/maybe FullType) [node]
  ;; x = { y = 1 }
  nil)
(s/defmethod unify-type IPointerType :- (s/maybe FullType) [node]
  ;; int x(char *y) { y; }
  ;;                 ^^^
  nil)
(s/defmethod unify-type :default [node] nil) ; TODO investigate that this doesn't generate a lot of false negatives

; https://www.safaribooksonline.com/library/view/c-in-a/0596006977/ch04.html
;(ns-unmap 'atom-finder.classifier 'context-types)
(defmulti context-types class)
(s/defmethod context-types :default [node] nil)
(s/defmethod context-types IASTFunctionCallExpression [node]
    (let [first-arg (->> node .getEvaluation .getArguments first)]
      (when (instance? EvalBinding first-arg)
        (let [type (->> first-arg .getBinding .getType)]
          (when (instance? IFunctionType type)
            (->> type .getParameterTypes (map unify-type)))))))
(s/defmethod context-types IASTBinaryExpression [node]
    [(->> node .getOperand1 .getExpressionType unify-type)])
(s/defmethod context-types IASTSimpleDeclaration [node]
  ;; Don't process any declaration specifiers which have an associated pointer *
  (->> node
       .getDeclarators
       (map (memfn getPointerOperators))
       (filter empty?)
       (map (fn [_] (->> node .getDeclSpecifier unify-type)))))
(s/defmethod context-types IASTReturnStatement [node]
    [(->> node enclosing-function (get-in-tree [0]) unify-type)])
(s/defmethod context-types IASTCastExpression [node]
  (when (->> node .getTypeId .getAbstractDeclarator .getPointerOperators empty?)
    [(->> node .getTypeId .getDeclSpecifier unify-type)]))

(defmulti arg-types class)
(s/defmethod arg-types :default [node] nil)
(s/defmethod arg-types IASTBinaryExpression [node]
    (map unify-type [(.getOperand2 node)]))
(s/defmethod arg-types IASTSimpleDeclaration [node]
  (keep #(some->> % .getInitializer
                  ((fn [init] (when (instance? IASTEqualsInitializer init) init)))
                  .getInitializerClause unify-type)
        (.getDeclarators node)))
(s/defmethod arg-types IASTFunctionCallExpression [node]
    (map unify-type (.getArguments node)))
(s/defmethod arg-types IASTReturnStatement [node]
    [(->> node .getReturnArgument unify-type)])
(s/defmethod arg-types IASTCastExpression [node]
    [(->> node .getOperand unify-type)])

(s/defn bit-range :- [(s/one s/Int "lower") (s/one s/Int "upper")]
  "Which values can safely be stored in a variable with this type"
  ([type] (bit-range (:unsigned? type) (:bits type)))
  ([unsigned? :- s/Bool bits :- s/Int]
   (let [signed-range [(->> bits dec (Math/pow 2) bigint -)
                       (->> bits dec (Math/pow 2) bigint dec)]]
     (if unsigned?
       (map #(- % (first signed-range)) signed-range)
       signed-range))))

(s/defn coercing-node? :- (s/maybe s/Bool)
  "Is one declaration/function-argument/etc coercing?"
  [c-type a-type]
  (when (not-any? nil? (concat [c-type a-type] (mapcat vals [c-type a-type])))
    (let [[context-lower context-upper] (bit-range c-type)]
      (boolean (or
                                        ; real -> int
                (and (#{:int} (:number-type c-type))
                     (#{:real} (:number-type a-type)))

                                        ; large -> small
                                        ; signed -> unsigned (and the reverse)
                (and (numeric-literal? (:val a-type))
                     (#{:int} (:number-type a-type))
                     (not (<= context-lower (parse-numeric-literal (:val a-type)) context-upper))))
               ))))

(s/defn type-conversion-atom? :- s/Bool
  [node]
  (try
    ;(println (write-tree node))
    (let [c-types (->> node context-types)
          a-types (->> node arg-types)
          types   (map vector c-types a-types)]
      (any-pred? #(apply coercing-node? %) types))
    (catch ParseException pe false)
    ;; sometimes .getEvaluation/getExpressionType barfs, see e.g. linux/drivers/gpu/drm/msm/hdmi/hdmi_hdcp.c:1020
    (catch ArrayIndexOutOfBoundsException e false)
    ))
