(ns atom-finder.atoms-removed
  (:require [atom-finder.util.util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.comment_change :refer :all]
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
;(->>
; (diff-by #(pr-str (mapcat-tree typename %1)) (:atoms-before srcs) (:atoms-after srcs))
; (map (comp write-ast :revised))
; pprint
; )
;
;(map #(prn (mapcat-tree typename %1)) (:atoms-before srcs))
;
;(defn atoms-removed
;  [srcs]
;   (atom-comments (->> srcs :atoms-after) (comments-added srcs)))
