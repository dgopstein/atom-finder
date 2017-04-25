(ns atom-finder.tree-diff
  (:require [atom-finder.util :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition]))

(def a (parse-expr "int b = 1 + b++"))
(def b (parse-expr "int b = 2 + b++"))
(def c (parse-expr "int b = 1 * b++"))
(def nodes [a b])

(->> b children seq)


(s/defn zip-trees [& nodes]
  (->> nodes (map children) transpose
       (mapcat (partial apply zip-trees))
       (cons nodes)))

(s/defn meaningful-change? [a :- IASTNode b :- IASTNode]
  (->> (zip-trees a b)
       (tap #(doall (map (fn [x] (doall (map (fn [n] (println (write-ast n))) x))) %)))
       (every? (partial apply tree=))
       not
       )
  )

(defn tree= [& nodes]
  (->> nodes (map write-ast) set count (= 1)))

(defmulti node= (fn [a b] (and (= (class a) (class b)) (class a))))
(defmethod node= false [a b] false)
(defmethod node= IASTNode [a b] true)
(defmethod node= IASTBinaryExpression [a b] (= (.getOperator a) (.getOperator b)))

(meaningful-change?
(parse-frag "1 +  2")
(parse-frag "1 + 2"))


;; two files
;; corresponding diffs
;; contains atom?
;; meaningful diffs?
