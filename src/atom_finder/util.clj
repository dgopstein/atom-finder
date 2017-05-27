(ns atom-finder.util
  (:require [clojure.reflect :as rflct]
            [clojure.string :as str]
            [schema.core :as s])
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import
           [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage cpp.ICPPASTNamespaceDefinition IASTCompositeTypeSpecifier ASTVisitor IASTNode IASTProblemStatement IASTName IASTProblem IASTProblemHolder IASTBinaryExpression IASTUnaryExpression IASTFieldReference IASTArraySubscriptExpression IASTCastExpression IASTFunctionCallExpression]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTProblemStatement CPPASTConditionalExpression CPPASTExpressionList]
           [org.eclipse.cdt.internal.core.parser.scanner ASTFileLocation]
           [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))

(s/set-fn-validation! true) ; Globally turn on schema validation

(defn load-cljs-in-dir
  [dir] ; e.g. "classifier/"
  (->> dir
       (str "atom_finder/")
       ClassLoader/getSystemResource
       clojure.java.io/file
       file-seq
       sort
       (map (memfn getName))
       (filter #(str/ends-with? % ".clj"))
       (map #(str/replace % #"\.clj$" ""))
       (map (partial str dir))
       (apply load)
  ))

(load-cljs-in-dir "util/")

;;Note: Operators missing from the list: (scope/ resolution ::) (memory allocation/ new new[] delete delete[]) (pointer-to-member/ ->* .*)
(defmulti precedence-level "returns the precedence level of the node" class)
(defmethod precedence-level :default [node]
  (let [precedence-list
        {IASTArraySubscriptExpression 2
         IASTFieldReference 2
         IASTCastExpression 2
         IASTFunctionCallExpression 2
         CPPASTConditionalExpression 15
         CPPASTExpressionList 16}]
    (precedence-list (type node))))
(defmethod precedence-level IASTUnaryExpression [node]
    (let [precedence-list
        {IASTUnaryExpression/op_postFixDecr 2 
         IASTUnaryExpression/op_postFixIncr 2
         IASTUnaryExpression/op_minus 3 
         IASTUnaryExpression/op_plus 3 
         IASTUnaryExpression/op_prefixDecr 3 
         IASTUnaryExpression/op_prefixIncr 3 
         IASTUnaryExpression/op_sizeof 3 
         IASTUnaryExpression/op_amper 3 
         IASTUnaryExpression/op_star 3 
         IASTUnaryExpression/op_not 3 
         IASTUnaryExpression/op_tilde 3
         IASTUnaryExpression/op_throw 15}]
    (precedence-list (.getOperator node))))
(defmethod precedence-level IASTBinaryExpression [node]
    (let [precedence-list
        {IASTBinaryExpression/op_modulo 5 
         IASTBinaryExpression/op_multiply 5
         IASTBinaryExpression/op_divide 5
         IASTBinaryExpression/op_plus 6 
         IASTBinaryExpression/op_minus 6
         IASTBinaryExpression/op_shiftLeft 7 
         IASTBinaryExpression/op_shiftRight 7
         IASTBinaryExpression/op_greaterThan 8 
         IASTBinaryExpression/op_greaterEqual 8 
         IASTBinaryExpression/op_lessThan 8 
         IASTBinaryExpression/op_lessEqual 8
         IASTBinaryExpression/op_equals 9 
         IASTBinaryExpression/op_notequals 9
         IASTBinaryExpression/op_binaryAnd 10
         IASTBinaryExpression/op_binaryXor 11
         IASTBinaryExpression/op_binaryOr 12
         IASTBinaryExpression/op_logicalAnd 13
         IASTBinaryExpression/op_logicalOr 14
         IASTBinaryExpression/op_assign 15 
         IASTBinaryExpression/op_binaryAndAssign 15 
         IASTBinaryExpression/op_binaryOrAssign 15
         IASTBinaryExpression/op_binaryXorAssign 15 
         IASTBinaryExpression/op_divideAssign 15 
         IASTBinaryExpression/op_minusAssign 15
         IASTBinaryExpression/op_moduloAssign 15 
         IASTBinaryExpression/op_multiplyAssign 15
         IASTBinaryExpression/op_plusAssign 15 
         IASTBinaryExpression/op_shiftLeftAssign 15
         IASTBinaryExpression/op_shiftRightAssign 15}]
    (precedence-list (.getOperator node))))

(defn assignment?
  "Returns true if the operator is an assignment operator"
  [node]
  (let [assignment-list 
        #{IASTBinaryExpression/op_assign IASTBinaryExpression/op_binaryAndAssign IASTBinaryExpression/op_binaryOrAssign IASTBinaryExpression/op_binaryXorAssign IASTBinaryExpression/op_divideAssign IASTBinaryExpression/op_minusAssign IASTBinaryExpression/op_moduloAssign IASTBinaryExpression/op_multiplyAssign IASTBinaryExpression/op_plusAssign IASTBinaryExpression/op_shiftLeftAssign IASTBinaryExpression/op_shiftRightAssign}]

    (and (instance? IASTBinaryExpression node) (contains? assignment-list (.getOperator node)))))
