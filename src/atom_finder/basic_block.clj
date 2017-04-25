(ns atom-finder.basic-block
  (:require [schema.core :as s]
            [atom-finder.util :refer :all]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import
   [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage cpp.ICPPASTNamespaceDefinition IASTCompositeTypeSpecifier ASTVisitor IASTNode IASTProblem IASTExpressionStatement IASTNullStatement IASTIfStatement IASTForStatement IASTWhileStatement IASTDoStatement IASTSwitchStatement IASTCompoundStatement
    IASTConditionalExpression IASTFunctionDefinition IASTLabelStatement]))

(defn trunc
  [n s]
    (subs s 0 (min (count s) n)))

(->> "gcc_cp_pt.c_d430756d2dbcc396347bd60d205ed987716b5ae8" parse-resource
     flatten-tree
     (take 200)
     (map
      (fn [node]
        {:line (->> node loc :line)
         :new-block
         (any-pred? #(instance? % (parent node))
                    [IASTIfStatement IASTSwitchStatement IASTConditionalExpression
                     IASTLabelStatement IASTForStatement IASTDoStatement
                     IASTWhileStatement IASTFunctionDefinition])
         :parent (some->> node parent typename)
         :src (->> node safe-write-ast (trunc 20))
        }))
     (map prn)
     )
