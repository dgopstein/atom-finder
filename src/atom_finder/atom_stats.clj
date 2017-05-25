(ns atom-finder.atom-stats
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.tree-diff.difflib :refer :all]
   [atom-finder.constants :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [schema.core :as s]
   )
  (:use
   [clojure.pprint :only [pprint print-table]])
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression
    IASTBinaryExpression IASTLiteralExpression IASTExpressionList
    IASTForStatement IASTFunctionDefinition IASTComment]
   [difflib DiffUtils Delta Delta$TYPE]
   ))

(load-cljs-in-dir "atom_stats/")

(defn ba-counts
  [srcs atom]
  {:atom-count-before (->> srcs :atoms-before count)
   :atom-count-after  (->> srcs :atoms-after  count)})

(defn source-size-before-after [srcs atom]
  {:source-chars-before (-> srcs :source-before count)
   :source-chars-after  (-> srcs :source-after count)})

(defn ast-size [srcs atom]
  {:ast-size-before (-> srcs :ast-before flatten-tree count)
   :ast-size-after  (-> srcs :ast-after flatten-tree count)})

(defn atom-stats [] {
   :atom-counts-before-after ba-counts
   :source-size-before-after source-size-before-after
   :ast-size ast-size
   :added-comments added-comments-context
   :count (constantly 1)
   })
