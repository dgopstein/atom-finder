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
