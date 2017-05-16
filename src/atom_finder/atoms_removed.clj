(ns atom-finder.atoms-removed
  (:require [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.change-distiller :refer :all]
            [atom-finder.atom-patch :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition IASTComment]
           [difflib DiffUtils Delta Delta$TYPE]
           ))


;(def srcs
;  (let [pre-srcs (build-srcs (slurp-resource "atoms-removed-before.c") (slurp-resource "atoms-removed-after.c"))]
;    (merge
;     pre-srcs
;     {:atoms-before (->> pre-srcs :ast-before ((->> :post-increment atom-finder.classifier/atom-lookup :finder)))
;      :atoms-after (->> pre-srcs :ast-after ((->> :post-increment atom-finder.classifier/atom-lookup :finder)))
;       })
;     ))
;
;(let [corrs (tree-correspondences (:ast-before srcs) (:ast-after srcs))
;      l->r (left->right corrs)
;      ]
;
;  (->>
;   (:atoms-before srcs)
;   (map #(vector %1 (l->r %1)))
;   (map (partial map write-ast))
;   pprint
;  )
;  )

(defn atoms-removed
  [srcs]
   (atom-comments (->> srcs :atoms-after) (comments-added srcs)))
