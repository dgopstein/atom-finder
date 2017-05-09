(ns atom-finder.change-distiller
  (:require [atom-finder.util :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition]
           [ch.uzh.ifi.seal.changedistiller.treedifferencing Node TreeDifferencer]
           [ch.uzh.ifi.seal.changedistiller.model.classifiers EntityType java.JavaEntityType]))

;(for [child (.children (atom_finder.CDTChangeDistillerNode. (parse-frag "1 + 2")))] (pprint child))

;(def kids (.children (atom_finder.CDTChangeDistillerNode. (parse-frag "1 + 2"))))

(defn new-ccd [node label]
  (let [ccd (atom_finder.CDTChangeDistillerNode. node)]
    (doseq [child (children node)]
      ;(prn (str "adding " child))
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
  [left :- IASTNode right :- IASTNode]
  (->> (private-field (tree-diff left right) "fMatch")
       (map (fn [pair] [(.getLeft pair) (.getRight pair)]))
       (map (partial map (memfn node)))
    ))

(s/defn left->right
  [correspondences]
  (->> (tree-correspondences left-node right-node)
       (map (partial into []))
       (into {})))

(s/defn right->left
  [correspondences]
  (->> (tree-correspondences left-node right-node)
       (map (partial into []))
       (into {})))

;(def left-node (parse-frag "int x = 1 + 2; 3"))
;(def right-node (parse-frag "int x = -(1 * 3)"))
;
;(def left-to-right
;  (->>
;   (tree-correspondences left-node right-node)
;   (map (partial into []))
;   (into {})
;   ))
;
;(def right-to-left (clojure.set/map-invert left-to-right))
;
;(->> left-node
;     flatten-tree
;     (map left-to-right)
;     (map write-ast))
;
;(->> right-node
;     flatten-tree
;     (map right-to-left)
;     (map write-ast))
