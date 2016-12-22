(ns atom-finder.ast-spec
  (:import [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTLiteralExpression])
  (:require [clojure.spec :as s]
            [clojure.spec.gen :as gen]))

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

(s/def ::literal-expression
  (s/with-gen
    (partial instance? CPPASTLiteralExpression)
    (fn [] gen-literal-expression)))
