(ns atom-finder.ast-spec
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTBinaryExpression IASTUnaryExpression]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTBinaryExpression CPPASTLiteralExpression])
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]
            [clojure.spec.test :as test]
            [clojure.string :as str]
            [atom-finder.util :refer :all]
            ))

(defn ast-node? [node] (instance? IASTNode node))
(s/def ::ast-node ast-node?)
(s/fdef leaves :args ::ast-node :ret (s/coll-of ::ast-node))

(s/def ::binary-expression (s/and ast-node? (partial instance? CPPASTBinaryExpression)))
(s/def ::literal-expression (s/and ast-node? (partial instance? CPPASTLiteralExpression)))

(def gen-literal-expression-args
 (gen/one-of
  [
   (gen/tuple (s/gen #{CPPASTLiteralExpression/lk_char_constant})
              (gen/char-ascii))
   (gen/tuple (s/gen #{CPPASTLiteralExpression/lk_float_constant})
              (gen/double))
   (gen/tuple (s/gen #{CPPASTLiteralExpression/lk_integer_constant})
              (s/gen (s/int-in -2147483648 2147483647)))
   (gen/tuple (s/gen #{CPPASTLiteralExpression/lk_string_literal})
              (gen/string))]))

(def gen-literal-expression
  (gen/fmap
   (fn [[type val]]
     (CPPASTLiteralExpression. type (.toCharArray (str val))))
   gen-literal-expression-args))

;(gen/sample gen-literal-expression-args 20)
;(gen/sample gen-literal-expression 10)
;
;(->>
; (s/exercise ::literal-expression 10 {::literal-expression (fn [] gen-literal-expression)})
; (map (comp write-ast first))
; )

