(ns atom-finder.classifier
  (:require [atom-finder.util :refer :all]
            [schema.core :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            )
  (:import
   [org.eclipse.cdt.core.dom.ast IASTNode
    IASTExpression IASTStatement IASTTranslationUnit
    IASTPreprocessorMacroDefinition IASTIfStatement IASTBinaryExpression]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
   [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]
   [org.eclipse.cdt.internal.core.parser.scanner ASTMacroDefinition]))

(s/set-fn-validation! true) ; Globally turn on schema validation

(defn classifier-files []
  (->> "atom_finder/classifier/"
       ClassLoader/getSystemResource 
       clojure.java.io/file
       file-seq
       (map (memfn getName))
       (filter #(str/ends-with? % ".clj"))
       (map #(str/replace % #"\.clj$" ""))
       (map (partial str "classifier/"))
       ))

; Load all files in the classifier directory
(apply load (classifier-files))

(defn default-finder [classifier] (partial atoms-in-tree classifier))

(def AtomName s/Keyword)
(def AtomClassifier s/Keyword)
(s/defrecord Atom [name classifier finder])

(defmacro ValidatedAtom
  "Creates an Atom record, with each function wrapped in Schema validation code"
  [name classifier finder]
  `(Atom. (s/validate AtomName ~name)
          (s/fn ~(symbol (str name "-classifier")) :- Boolean [node# :- IASTNode] (~classifier node#))
          (s/fn ~(symbol (str name "-finder")) :- [IASTNode] [node# :- IASTNode] (~finder node#))
  ))

(def atoms
  [
   (ValidatedAtom :preprocessor-in-statement preprocessor-parent?        all-non-toplevel-preprocessors)
   (ValidatedAtom :logic-as-control-flow     logic-as-control-flow-atom? logic-as-control-flow-atoms)
   (ValidatedAtom :conditional               conditional-atom?           (default-finder conditional-atom?))
   (ValidatedAtom :reversed-subscript        reversed-subscript-atom?    (default-finder reversed-subscript-atom?))
   (ValidatedAtom :literal-encoding          literal-encoding-atom?      (default-finder literal-encoding-atom?))
   (ValidatedAtom :post-increment            post-*crement-atom?         (default-finder post-*crement-atom?))
   ]
  )


(def atom-lookup (into {} (map #(vector (:name %1) %1) atoms)))
