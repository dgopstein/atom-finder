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


;ch.uzh.ifi.seal.changedistiller.treedifferencing
;
;(def node-left (atom_finder.CDTChangeDistillerNode. (parse-frag "int x = 1 + 2")))
;(def node-right (atom_finder.CDTChangeDistillerNode. (parse-frag "int x = -(1 * 3)")))
(def node-left (new-root (parse-frag "int x = 1 + 2")))
(def node-right (new-root (parse-frag "int x = -(1 * 3)")))
(def tree-diff (TreeDifferencer.))
(.calculateEditScript tree-diff node-left node-right)
(println (.getEditScript tree-diff))


(do
  (def node-left (->> "gcc_cp_pt.c_d430756d2dbcc396347bd60d205ed987716b5ae8_6000" parse-resource new-root))
  (def node-right (->> "gcc_cp_pt.c_92884c107e041201b33c5d4196fe756c716e8a0c_6000" parse-resource new-root)))
(def tree-diff (TreeDifferencer.))
(time (.calculateEditScript tree-diff node-left node-right))
(def fMatch (private-field tree-diff "fMatch"))
(def es (.getEditScript tree-diff))
(->> es
     (take 3)
     )

(->> fMatch
     (take 10)
     (map (fn [pair] [(.getLeft pair) (.getRight pair)]))
     (map (partial map (memfn node)))
     (map (partial map write-ast))
     pprint
     )
