(ns atom-finder.change-distiller
  (:require [atom-finder.util :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [ch.uzh.ifi.seal.changedistiller.treedifferencing Node TreeDifferencer]
           [ch.uzh.ifi.seal.changedistiller.model.classifiers EntityType java.JavaEntityType]))

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

(defn new-nodes [nodes]
  "A 1-level tree. One artificial root with the given nodes as children."
  (let [root (atom_finder.CDTChangeDistillerNode. (CPPASTTranslationUnit.))]
    (doseq [node nodes]
      (.add root (atom_finder.CDTChangeDistillerNode. node)))
    root))

(s/defn flat-diff
  "Diff two flat lists of nodes (useful for comments/macros)"
  [lefts :- [IASTNode] rights :- [IASTNode]]
  (let [tree-differ (TreeDifferencer.)]
    (.calculateEditScript tree-differ (new-nodes lefts) (new-nodes rights))
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

(->>
  (flat-diff
    (->> "meaningful-change-before.c" parse-resource all-comments)
    (->> "meaningful-change-after.c" parse-resource all-comments))
  correspondences
  (map (partial map str))
  pprint
  )
