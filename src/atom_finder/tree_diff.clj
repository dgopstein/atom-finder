(ns atom-finder.tree-diff
  (:require [atom-finder.util :refer :all]
            [atom-finder.tree-diff.difflib :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression
            IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression
            IASTExpressionList IASTForStatement IASTFunctionDefinition]))

(defn tree= [& nodes]
  (->> nodes (map write-ast) (apply =)))

(defn node= [a b]
  (when (and a b)
    (=by (and-then flatten-tree (partial map write-node-valueless)) a b)))

(s/defn zip-trees [& nodes]
  (->> nodes (map children) transpose
       (mapcat (partial apply zip-trees))
       (cons nodes)))

(s/defn meaningful-change? [a :- IASTNode b :- IASTNode]
  (->> (zip-trees a b)
       (every? (partial apply node=))
       not))

(s/defn tree=by
  "apply a function to every node of multiple trees until a difference occurs"
  [f & nodes] ; :- [atom-finder.util/ASTTree]]
  (or (every? (comp nil? ast-node) nodes)
      (and (apply = (map (comp f ast-node) nodes))
           (let [kids (map children nodes)]
             (and (apply = (map count kids))
                  (every? identity (apply (partial map (partial tree=by f)) kids)))))))

;; two files
;; corresponding diffs
;; contains atom?
;; meaningful diffs?
