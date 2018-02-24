(ns quark.tree-tokenizer
  (:require [atom-finder.util :refer :all]
            [schema.core :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [swiss.arrows :refer :all]
            )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression
    IASTExpression IASTStatement IASTTranslationUnit IASTName
    IASTExpressionList IASTExpressionStatement IASTForStatement
    IASTPreprocessorMacroDefinition IASTIfStatement IASTUnaryExpression
    IProblemBinding IProblemType]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
   [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]
   [org.eclipse.cdt.internal.core.parser.scanner ASTMacroDefinition]
   ))

(defn expr-typename
  [node]
  (let [expr-type (.getExpressionType node)]
    (condp instance? expr-type
        IProblemBinding "problem-binding"
        IProblemType "problem-type"
        (str expr-type))))

(defmulti to-poco "Convert IASTNode node to plain-old-clojure-object's" class)

(s/defmethod to-poco :default [node] [(typename node)])

(s/defmethod to-poco IASTExpression [node] [(typename node) (expr-typename node)])
(s/defmethod to-poco IASTUnaryExpression [node] [(typename node) (expr-typename node) (->> node expr-operator :name)])
(s/defmethod to-poco IASTBinaryExpression [node] [(typename node) (expr-typename node) (->> node expr-operator :name)])
(s/defmethod to-poco IASTName [node] [(typename node) (->> node node-name)])

(defn to-edn
  "Serialize AST node into an edn list"
  [node]
  (let [poco (to-poco node)]
    (cons poco (map to-edn (children node)))))


(->> "x" parse-frag expr-typename)
(->> "f(x)" parse-frag expr-typename)
(->> "1" parse-frag expr-typename)
(->> "{int x; x}" parse-frag (get-in-tree [1 0]) expr-typename)

(-<>>
 "gcc_cp_pt.c_92884c107e041201b33c5d4196fe756c716e8a0c" parse-resource
 ;"~/opt/src/gcc/gcc/c/c-array-notation.c" expand-home parse-file
 flatten-tree
 (drop 20035)
 (take 1)
 first
 (map write-tree)
 (map println)
 dorun)


 (get-in-tree [1 0])
 ((fn [node]
    (->> node write-ast println)
    (->> node to-edn prn)))
 time-mins)
