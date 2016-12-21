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
;(s/exercise leaves)

(s/def ::binary-expression (s/and ast-node? (partial instance? CPPASTBinaryExpression)))
(s/def ::literal-expression (s/and ast-node? (partial instance? CPPASTLiteralExpression)))

;(defn gen-int-literal-expression
;  []
;  (->> (s/gen (s/int-in -2147483648 2147483647))
;       (gen/fmap #(->> (str %)
;                       (.toCharArray)
;                       (CPPASTLiteralExpression. CPPASTLiteralExpression/lk_integer_constant)))))

(defn gen-int-literal-expression
  []
  (->> (gen/tuple (s/int-in -2147483648 2147483647)
                    #{CPPASTLiteralExpression/lk_integer_constant CPPASTLiteralExpression/lk_float_constant})
       (gen/fmap (fn [[int type]] (->> (str int)
                       (.toCharArray) (CPPASTLiteralExpression. type))))))

;(gen-from [3 :arbitrary "elements"])

;(gen/generate gen-int-literal-expression)

;(map (comp write-ast first) (s/exercise ::literal-expression 10 {::literal-expression gen-int-literal-expression}))
