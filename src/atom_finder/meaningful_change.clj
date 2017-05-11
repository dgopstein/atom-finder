(ns atom-finder.meaningful-change
  (:require [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.change-distiller :refer :all]
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

(s/defn diff-nodes :- [{:delta Delta :original s/Any :revised s/Any}]
  [cmnts-a :- s/Any cmnts-b :- s/Any]
  (->>
   (DiffUtils/diff (->> cmnts-a (map str)) (->> cmnts-b (map str)))
   .getDeltas
   (map (fn [c] {:delta c
                 :original (->> c .getOriginal .getPosition (nth cmnts-a))
                 :revised (->> c .getRevised .getPosition (nth cmnts-b))}))
   ))

(s/defn atom-comments-added
  "Which comments were added near atoms"
  [srcs atom]
  (->>
   (diff-nodes (->> srcs :ast-before all-comments) (->> srcs :ast-after all-comments))
   (filter #(->> % :delta .getType (= Delta$TYPE/INSERT)))
   (map :revised)
   (atom-comments (->> srcs :atoms-after))
   )
  )

;(->> "atom-comments.c"
;     parse-resource
;     (atom-finder-comments (-> atom-lookup :post-increment))
;     ;(#(atom-comments ((-> atom-lookup :post-increment :finder) %1) %1))
;     ;(map (fn [[atm cmnts]] [(write-ast atm) (map str cmnts)]))
;     (mapcat last)
;     (map end-line)
;     pprint
;     )
;
;(->> "atom-comments.c"
;     parse-resource)

;(->>
;  {:comments-before (all-comments (parse-resource "meaningful-change-before.c"))
;   :comments-after  (all-comments (parse-resource "meaningful-change-after.c" ))}
;  :comments-before
;  ;(take 2)
;  ;(drop 1)
;  (map (comp :line loc))
;  )
;
; (def big-comments (->> atom-finder.constants/big-root all-comments (into [])))
;(->> big-comments
;     first)
