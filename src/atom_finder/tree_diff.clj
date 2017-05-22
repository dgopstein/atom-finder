(ns atom-finder.tree-diff
  (:require [atom-finder.util.util :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition]))

;(def a (parse-expr "int b = 1 + b++"))
;(def b (parse-expr "int b = 2 + b++"))
;(def c (parse-expr "int b = 1 * b++"))
;(def nodes [a b])
;(->> b children seq)

(defn tree= [& nodes]
  (->> nodes (map write-ast) set count (= 1)))

(defmulti node= (fn [a b] (and (= (class a) (class b)) (class a))))
(defmethod node= :default [a b] (and (nil? a) (nil? b)))
(defmethod node= false [a b] false)
(defmethod node= IASTNode [a b]
  (if (and (leaf? a) (leaf? b))
    (=by write-ast a b)
    true))
(defmethod node= IASTLiteralExpression [a b] (=by .toString a b))
(defmethod node= IASTBinaryExpression [a b] (=by .getOperator a b))
(defmethod node= IASTUnaryExpression [a b] (=by .getOperator a b))

(s/defn zip-trees [& nodes]
  (->> nodes (map children) transpose
       (mapcat (partial apply zip-trees))
       (cons nodes)))

(s/defn meaningful-change? [a :- IASTNode b :- IASTNode]
  (->> (zip-trees a b)
       (map (partial map #(if (leaf? %1) nil %1)))
       ;(tap #(doall (map (fn [x] (doall (map (fn [n] (println (write-ast n))) x))) %)))
       (every? (partial apply node=))
       not
       )
  )

;(apply meaningful-change? (map parse-frag ["1 +  2" "1 = 2"]))
;(apply meaningful-change? (map parse-frag ["1 +  2" "1 * 2"]))

;(node= (parse-frag "if (1) 2;") (parse-frag "if (2) 3;"))

;; two files
;; corresponding diffs
;; contains atom?
;; meaningful diffs?
