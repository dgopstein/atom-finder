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
