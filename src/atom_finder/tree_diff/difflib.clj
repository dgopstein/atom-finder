(ns atom-finder.tree-diff.difflib
  (:require [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition IASTComment]
           [difflib DiffUtils Delta Delta$TYPE]
           ))


(s/defn diff-by :- [{:delta Delta :original s/Any :revised s/Any}]
  [f cmnts-a :- [s/Any] cmnts-b :- [s/Any]]
  (->>
   (DiffUtils/diff (->> cmnts-a (map f)) (->> cmnts-b (map f)))
   .getDeltas
   (map (fn [c] {:delta c
                 :original (->> c .getOriginal .getPosition (safe-nth cmnts-a))
                 :revised (->> c .getRevised .getPosition (safe-nth cmnts-b))}))
   ))

(->> (diff-by write-node (->> "1 + 2 + 5" parse-frag flatten-tree-infixy) (->> "2 + 3 + 6" parse-frag flatten-tree-infixy))
     ;count
     ;(map :revised)
     ;(map write-node)
     (map :delta)
     )

(s/defn correspondence
  "Find a mapping between nodes of two ASTs"
  [a :- IASTNode b :- IASTNode]

  (diff (->> a flatten-tree) (->> flatten-tree b))
  )

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
