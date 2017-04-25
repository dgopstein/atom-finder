(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.constants :refer :all]
            [atom-finder.util :refer :all]
            [atom-finder.classifier-util :refer :all]
            [atom-finder.results-util :refer :all]
            [atom-finder.atoms-in-dir :refer :all]
            [atom-finder.atom-patch :refer :all]
            [atom-finder.source-versions :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            ))

;(set! *warn-on-reflection* true)
(defn -main
  [& args]

  ;(->> (atom-patch/atoms-changed-all-commits gcc-repo atoms)
  ;     ;(map prn)
  ;     (take 10)
  ;     dorun)

  ; 48 hours
  ;(time (log-atoms-changed-all-commits "gcc-bugs-atoms_2017-03-28_2.edn" gcc-repo atoms))
)

;(->> "macro-operator-precedence_redis_2017-03-10_1.edn"
;     read-data
;     (take 30)
;     (found-atom-source :macro-operator-precedence)
;     ;sum-found-atoms
;     )

;sum-found-atoms => {:macro-operator-precedence 1760}
;sum-found-atoms => {:macro-operator-precedence  759} # After removing atomic macros (#define M1 123)

(import '(org.eclipse.cdt.core.dom.ast  IASTExpressionStatement IASTNullStatement IASTIfStatement IASTForStatement IASTWhileStatement IASTDoStatement IASTSwitchStatement IASTCompoundStatement)
20:35:            valid-else-types [IASTIfStatement IASTCompoundStatement]]
(->> "gcc_cp_pt.c_d430756d2dbcc396347bd60d205ed987716b5ae8" parse-resource
     (mapcat-tree
      (fn [node]
         (any-pred? #(instance? % node) [IfStatement FunctionDefinition])
        ))
     (take 20))
