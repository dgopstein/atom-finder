(ns atom-finder.comment-change
  (:require [atom-finder.util :refer :all]
            [atom-finder.tree-diff.difflib :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition IASTComment]
           [difflib DiffUtils Delta Delta$TYPE]
           ))

(s/defn atom-comments
  "For every atom, collect the comments that are near them"
  ([atom-nodes :- [IASTNode] comments :- [IASTComment]]
    (->>
     (loop [atoms atom-nodes
            comments comments
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
     )))

(defn atom-finder-comments
  [atom root]
  (atom-comments ((:finder atom) root) (all-comments root)))

(s/defn comments-added
  "Which comments were added near atoms"
  [srcs]
  (->>
   (diff-by str (->> srcs :ast-before all-comments) (->> srcs :ast-after all-comments))
   (filter #(->> % :delta .getType (= Delta$TYPE/INSERT)))
   (mapcat :revised)
   ))

(defn atom-comments-added
  [srcs]
   (atom-comments (->> srcs :atoms-after) (comments-added srcs)))