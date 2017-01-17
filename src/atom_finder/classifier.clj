(ns atom-finder.classifier
  (:require [atom-finder.util :refer :all]
            [schema.core :as s]
            [clojure.pprint :refer [pprint]])
            
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode
    IASTExpression IASTStatement IASTTranslationUnit
    IASTPreprocessorMacroDefinition IASTIfStatement IASTBinaryExpression
    IASTCompoundStatement IASTForStatement IASTWhileStatement
    IASTExpressionStatement]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit 
    CPPASTExpressionList]
   [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]
   [org.eclipse.cdt.internal.core.parser.scanner ASTMacroDefinition]))

(s/set-fn-validation! true) ; Globally turn on schema validation

(->>
 ["preprocessor-in-statement" "logic-as-control-flow" "conditional"
  "literal-encoding" "comma" "pre-increment-decrement" "post-increment-decrement"
  "curly-braces"]
 (map (partial str "classifier/"))
 (apply load))
 

(defn default-finder [classifier] (partial atoms-in-tree classifier))

(def AtomName s/Keyword)
(def AtomClassifier s/Keyword)
(s/defrecord Atom [name classifier finder])

(defmacro ValidatedAtom
  "Creates an Atom record, with each function wrapped in Schema validation code"
  [name classifier finder]
  `(Atom. (s/validate AtomName ~name)
          (s/fn ~(symbol (str name "-classifier")) :- Boolean [node# :- IASTNode] (~classifier node#))
          (s/fn ~(symbol (str name "-finder")) :- [IASTNode] [node# :- IASTNode] (~finder node#))))
  

(def atoms
  [
   (ValidatedAtom :preprocessor-in-statement preprocessor-parent?             all-non-toplevel-preprocessors)
   (ValidatedAtom :logic-as-control-flow     logic-as-control-flow-atom?      logic-as-control-flow-atoms)
   (ValidatedAtom :conditional               conditional-atom?                (default-finder conditional-atom?))
   (ValidatedAtom :comma                     comma-atom?                      (default-finder comma-atom?))
   (ValidatedAtom :post-increment-decrement  post-increment-decrement-atom?   post-increment-decrement-atoms)
   (ValidatedAtom :pre-increment-decrement   pre-increment-decrement-atom?    pre-increment-decrement-atoms)
   (ValidatedAtom :curly-braces              curly-braces-atom?               curly-braces-atoms)])
  
   
  


(def atom-lookup (into {} (map #(vector (:name %1) %1) atoms)))
