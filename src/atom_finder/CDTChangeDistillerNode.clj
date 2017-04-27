(ns atom-finder.CDTChangeDistillerNode
  (:require [atom-finder.util :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition]
           [ch.uzh.ifi.seal.changedistiller.treedifferencing Node]
           [ch.uzh.ifi.seal.changedistiller.model.classifiers EntityType java.JavaEntityType])


  (:gen-class
   :extends      Node
   :state        state
   :init         init
   :constructors {[IASTNode] [EntityType String]}))

(defn -init
  [node]
  [[JavaEntityType/WILDCARD_TYPE (typename node)] node])

(defn gen-cd-node [node]
  (let [cd-node (atom-finder.CDTChangeDistillerNode. node)]
    (doseq [child (children node)]
      (.add cd-node (gen-cd-node child)))))

