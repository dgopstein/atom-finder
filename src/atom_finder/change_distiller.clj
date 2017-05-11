(ns atom-finder.change-distiller
  (:require [atom-finder.util :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [ch.uzh.ifi.seal.changedistiller.treedifferencing Node TreeDifferencer]
           [ch.uzh.ifi.seal.changedistiller.model.classifiers EntityType java.JavaEntityType]
           [difflib DiffUtils Delta Delta$TYPE]
           ))

(defn new-ccd [node label]
  (let [ccd (atom_finder.CDTChangeDistillerNode. node)]
    (doseq [child (children node)]
      (.add ccd (new-ccd child (typename child))))
    ccd))

(defn new-root [node]
  (let [ccd (new-ccd node "<new-root>")]
    (.setLabel ccd JavaEntityType/ROOT_NODE)
  ccd))

(s/defn tree-diff
  "Construct a TreeDifferencer and calculate its edit script"
  [left :- IASTNode right :- IASTNode]
  (let [tree-differ (TreeDifferencer.)]
    (.calculateEditScript tree-differ (new-root left) (new-root right))
    tree-differ
  ))

(defn correspondences
  [tree-differ]
  (->> (private-field tree-differ "fMatch")
       (map (fn [pair] [(.getLeft pair) (.getRight pair)]))
       (map (partial map (memfn node)))
    ))

(s/defn tree-correspondences
  "For two trees, associate each node with its closest pair in the other tree"
  [left :- IASTNode right :- IASTNode]
  (correspondences (tree-diff left right) "fMatch"))

(defn left->right
  [correspondences]
  (->> correspondences
       (map (partial into []))
       (into {})))

(def right->left (comp clojure.set/map-invert left->right))

;(def cmnts-a (->> srcs :ast-before all-comments))
;(def cmnts-b (->> srcs :ast-after all-comments))

(s/defn diff-nodes :- [{:delta Delta :original s/Any :revised s/Any}]
  [cmnts-a :- s/Any cmnts-b :- s/Any]
  (->>
   (DiffUtils/diff (->> cmnts-a (map str)) (->> cmnts-b (map str)))
   .getDeltas
   (map (fn [c] {:delta c
                 :original (->> c .getOriginal .getPosition (nth cmnts-a))
                 :revised (->> c .getRevised .getPosition (nth cmnts-b))}))
   ))

(->>
 (DiffUtils/diff
    (->> "meaningful-change-before.c" parse-resource all-comments (map str))
    (->> "meaningful-change-after.c" parse-resource all-comments (map str)))
 .getDeltas
  )

(->>
 (DiffUtils/diff '("axb" "lm" "cd" "ij" "ef") '("ab" "lm" "ef" "cd" "gh" "ij"))
 .getDeltas
 (map prn)
 )
