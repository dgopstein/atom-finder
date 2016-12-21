(ns atom-finder.classifier
  (:require [atom-finder.util :refer :all])
  (:import
   [org.eclipse.cdt.core.dom.ast
    IASTExpression IASTStatement IASTTranslationUnit
    IASTPreprocessorMacroDefinition IASTIfStatement IASTBinaryExpression]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
   [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]
   [org.eclipse.cdt.internal.core.parser.scanner ASTMacroDefinition]))

(load "preprocessor-in-statement")
(load "logic-as-control-flow")
(load "conditional")

(defn default-finder [classifier] (partial atoms-in-tree classifier))

(defrecord AtomClassifier [name classifier find-all])
(def atom-classifiers
  [
   (AtomClassifier. :preprocessor-in-statement preprocessor-parent?        all-non-toplevel-preprocessors)
   (AtomClassifier. :logic-as-control-flow     logic-as-control-flow-atom? logic-as-control-flow-atoms)
   (AtomClassifier. :conditional               conditional-atom?           (default-finder conditional-atom?))
   ])

(def atom-lookup (into {} (map #(vector (:name %1) %1) atom-classifiers)))
