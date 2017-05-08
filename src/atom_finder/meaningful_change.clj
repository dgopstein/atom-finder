(ns atom-finder.meaningful-change
  (:require [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition]
           ))


(def big-comments (->> atom-finder.constants/big-root all-comments (into [])))

(defn atom-comments
  "For every atom, collect the comments that are near them"
  [all-atoms root]
  (->>
   (loop [atoms all-atoms
          comments (all-comments root)
          atm-cmnts []]
     (if (or (empty? atoms) (empty? comments))
       atm-cmnts
       (let [look-behind 3 ; how far behind an atom an associated comment can be
             atom-line (end-line (first atoms))
             cmnt-line (end-line (first comments))
             past-comments #(<= (end-line %)  atom-line)]
         (recur (rest atoms)
                (drop-while past-comments comments)
                (conj atm-cmnts
                      [(first atoms)
                       (->> comments
                            (take-while past-comments)
                            (drop-while #(< (+ look-behind (end-line %)) atom-line)))]
                      )))))
   (filter (comp not empty? last))
   ))

(->> "atom-comments.c"
     parse-resource
     (#(atom-comments ((-> atom-lookup :post-increment :finder) %1) %1))
     (map (fn [[atm cmnts]] [(write-ast atm) (map str cmnts)]))
     pprint
     )

(->>
  {:comments-before (all-comments (parse-resource "meaningful-change-before.c"))
   :comments-after  (all-comments (parse-resource "meaningful-change-after.c" ))}
  :comments-before
  ;(take 2)
  ;(drop 1)
  (map (comp :line loc))
  )

(->> big-comments
     first)
