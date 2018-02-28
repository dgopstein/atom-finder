;; Convert a C/C++/Header file into an edn representation containing the
;; structure and (partial) type information of the AST

(ns quark.tree-tokenizer
  (:require [atom-finder.util :refer :all]
            [atom-finder.constants :refer :all]
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
  (log-err (str "Exception getting expression type for [file node] "
                [(filename node) (tree-path node)]) "---exception---"
           (let [expr-type (-> node .getExpressionType)]
             (condp instance? expr-type
               IProblemBinding "problem-binding"
               IProblemType "problem-type"
               (str expr-type)))))

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

'((->> "x" pap parse-frag expr-typename pprint))
'((->> "f(x)" pap parse-frag expr-typename pprint))
'((->> "1" pap parse-frag to-edn pprint))
'((->> "{int x; x;}" parse-frag to-edn pprint))
'((->> "\"abc\" + 2" parse-frag to-edn pprint))
'((->> "if(1) 'b';" parse-frag to-edn pprint))

(defn src-dir-to-edn
  [unexpanded-src-path unexpanded-out-path]
  (doseq [:let [src-path (expand-home unexpanded-src-path)
                out-path (expand-home unexpanded-out-path)]
          src-file (c-files src-path)
          :let [src-filename (.getAbsolutePath src-file)
                out-filename (str (str/replace src-filename src-path out-path) ".edn")]]
    (clojure.java.io/make-parents out-filename)
    (->> src-filename parse-file to-edn (spit out-filename))))

'((time-mins (src-dir-to-edn linux-path "tmp/src-to-edn/linux3")))

'((time-mins (src-dir-to-edn linux-path "tmp/src-to-edn/linux4")))


