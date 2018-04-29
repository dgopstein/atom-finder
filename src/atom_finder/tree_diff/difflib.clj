(ns atom-finder.tree-diff.difflib
  (:require [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            [clj-cdt.clj-cdt :refer :all]
            [clj-cdt.writer-util :refer :all]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition IASTComment]
           [difflib DiffUtils Delta Delta$TYPE]
           ))

(def DiffMap {:delta Delta :original s/Any :revised s/Any s/Any s/Any})
(def diff-types {Delta$TYPE/INSERT :insert
                 Delta$TYPE/DELETE :delete
                 Delta$TYPE/CHANGE :change})

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
               :type (->> c .getType diff-types)
               })))
     ))

(s/defn diff-trees [a :- IASTNode b :- IASTNode]
  (diff-by write-node-valueless
           (flatten-tree-infixy a)
           (flatten-tree-infixy b)))

(s/defn correspondence
  ([a :- IASTNode b :- IASTNode]
   (correspondence (flatten-tree-infixy a) (flatten-tree-infixy b) (diff-trees a b)))
  ([a :- [s/Any] b :- [s/Any] diff-maps :- [DiffMap]]
    (map vector
         (remove (->> diff-maps (mapcat :original) (into #{})) a)
         (remove (->> diff-maps (mapcat  :revised) (into #{})) b))
  ))


(s/defn parse-diff ;:- difflib.Patch
  [s :- s/Str]
  (->> s
       ;; split the diff into multiple patch at "+++" as in
       ;; java-diff-utils/src/main/java/difflib/DiffUtils.java:54
       ((flip str/split) #"(?m)^(?=\+\+\+)")
       rest
       (map str/split-lines)
       (map #(DiffUtils/parseUnifiedDiff %))))
