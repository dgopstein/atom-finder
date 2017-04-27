(ns atom-finder.change-distiller
  (:require [atom-finder.util :refer :all]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import [org.eclipse.cdt.core.dom.ast IASTNode IASTExpression IASTUnaryExpression IASTBinaryExpression IASTLiteralExpression IASTExpressionList IASTForStatement IASTFunctionDefinition]
           [ch.uzh.ifi.seal.changedistiller.treedifferencing Node]
           [ch.uzh.ifi.seal.changedistiller.model.classifiers EntityType java.JavaEntityType]))

;(for [child (.children (atom_finder.CDTChangeDistillerNode. (parse-frag "1 + 2")))] (pprint child))

;(def kids (.children (atom_finder.CDTChangeDistillerNode. (parse-frag "1 + 2"))))
