(ns atom-finder.tree-diff
  (:require [atom-finder.util :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition]))

(defn tree= [& nodes]
  (->> nodes (map write-ast) set count (= 1)))

(defn node= [a b] (=by (and-then flatten-tree (partial map write-node-valueless)) a b))

(s/defn zip-trees [& nodes]
  (->> nodes (map children) transpose
       (mapcat (partial apply zip-trees))
       (cons nodes)))

(s/defn meaningful-change? [a :- IASTNode b :- IASTNode]
  (->> (zip-trees a b)
       (every? (partial apply node=))
       not
       )
  )

;; two files
;; corresponding diffs
;; contains atom?
;; meaningful diffs?
