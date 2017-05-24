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

(def DiffMap {:delta Delta :original s/Any :revised s/Any s/Any s/Any})

(s/defn diff-by :- [DiffMap]
  [f a :- [s/Any] b :- [s/Any]]
    (->>
     (DiffUtils/diff (->> a (map f)) (->> b (map f)))
     .getDeltas
     (map (fn [c]
            (let [o-pos (->> c .getOriginal .getPosition)
                  o-len (->> c .getOriginal .size)
                  r-pos (->> c .getRevised  .getPosition)
                  r-len (->> c .getRevised  .size)]
              {:delta c
               :original (->> a (drop o-pos) (take o-len))
               :revised  (->> b (drop r-pos) (take r-len))
               })))
     ))

(s/defn correspondence [a b] ;:- [s/Any] b :- [s/Any] ]; diff-map :- [DiffMap]]
  (let [diff-map (diff-by write-node-valueless a b)
        old-lines (->> (remove (->> diff-map (mapcat :original) (into #{})) a))
        new-lines (->> (remove (->> diff-map (mapcat  :revised) (into #{})) b))]

    (map vector old-lines new-lines)
  ))

(->> a (map write-node))
(->> diff-map (map :original)
     first
     (map write-node)
     )
(->> diff-map (map :original) (map write-node))

(def a (->> "f(1 + 2 + 5)" parse-frag flatten-tree-infixy))
(def b (->> "2 + 3++ * 6"  parse-frag flatten-tree-infixy))

(->>
 (correspondence a b)
 (map (partial map write-node)))

(->> "2 + 3 + 6" parse-frag flatten-tree-infixy (map write-node-valueless))
(->> (diff-by write-node-valueless a b) (def diff-map))

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
