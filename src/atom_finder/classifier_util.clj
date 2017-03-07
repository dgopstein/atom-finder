(ns atom-finder.classifier-util
  (:require [atom-finder.util :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTUnaryExpression]))

(defn default-finder [classifier] (partial filter-tree classifier))

(defn paren-node?
  "Does this node just represent ()'s"
  [node]
  (and (instance? IASTUnaryExpression node)
       (= (.getOperator node) IASTUnaryExpression/op_bracketedPrimary)))
