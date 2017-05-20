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

(s/defn tree-correspondences
  "For two trees, associate each node with its closest pair in the other tree"
  ([tree-differ :- TreeDifferencer]
   (->> (private-field tree-differ "fMatch")
        (map (fn [pair] [(.getLeft pair) (.getRight pair)]))
        (map (partial map (memfn node)))
        ))
  ([left :- IASTNode right :- IASTNode]
   (tree-correspondences (tree-diff left right) "fMatch")))

(defn left->right
  [correspondences]
  (->> correspondences
       (map (partial into []))
       (into {})))

(def right->left (comp clojure.set/map-invert left->right))
