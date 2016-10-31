(ns atom-finder.find-atom
  (:require [atom-finder.util :refer :all])
  (:import
   [org.eclipse.cdt.core.dom.ast IASTExpression IASTStatement IASTTranslationUnit IASTPreprocessorMacroDefinition IASTIfStatement]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
   [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]
   [org.eclipse.cdt.internal.core.parser.scanner ASTMacroDefinition]))

(defmulti side-effecting? class)
(defmethod side-effecting? :default [node] false)
(defmethod side-effecting? :default [node] false)

(defn contains-location?
  "Does this node contain the given offset/length"
  [root offset length]
  (let [root-loc (.getFileLocation root)
        root-offset (.getNodeOffset root-loc)
        root-length (.getNodeLength root-loc)]

    ;; The location/offset is fully contained in this node
    (and (<=    root-offset                 offset)
         (>= (+ root-offset root-length) (+ offset length)))))

(defn contains-offset?
  "Does this node contain the given offset"
  [root offset]
  (let [root-loc (.getFileLocation root)]
    (if (nil? root-loc)
      false
      (let [root-offset (.getNodeOffset root-loc)]
        ; (According to VisualVM) The dispatch on these methods
        ; is a CPU killer. Try to short-circuit if possible.
        (if (> root-offset offset)
          false
          (>= (+ root-offset (.getNodeLength root-loc)) offset))))))

(defn offset-parent
  "Find the AST node that contains the whole location offset
   Assumes that no children of a single parent overlap in terms of offset"
  [root offset]
  (let [kids      (children root)
        container (first (filter #(contains-offset? % offset) kids))]
    (if (nil? container)
      root
      (recur container offset))))

(defn toplevel-offset?
  "Check if an offset lives in the top level or if it's inside some other node"
  [root offset]
  (not-any? #(contains-offset? % offset) (children root)))

(defn macro-in-contexts
  "find a single macro in the given AST"
  [root macro classifier]
  (let [loc    (.getFileLocation macro)
        offset (.getNodeOffset loc)
        length (.getNodeLength loc)
        line   (.getStartingLineNumber loc)
               ;in-expr?  (not (instance? IASTTranslationUnit (offset-parent root offset)))
                                        ;in-expr?  (not (toplevel-offset? root offset))
        parent (offset-parent root offset)
        in-expr? (classifier parent)
        ret {:line line :offset offset :length length}
                ]
            
            [ret in-expr?]))

(defn preprocessors-in-contexts
  "return a list of all preprocessor directives inside of various contexts"
  [preprocessor-type context-classifier root]
    (keep (fn [[md in-expr?]] (if in-expr? md)) 
          (for [md (preprocessor-type root)]
            (macro-in-contexts root md context-classifier))))

(defn all-preprocessor [root] (.getAllPreprocessorStatements root))
(defn define-only [root] (filter #(instance? IASTPreprocessorMacroDefinition %) (all-preprocessor root)))

(defn define-in-contexts
  [context-classifier root]
  (preprocessors-in-contexts define-only context-classifier root))

(defn non-toplevel-classifier
  [parent]
  (not (instance? IASTTranslationUnit parent)))

(defn statement-expression-classifier
  [parent]
  (or (instance? IASTExpression parent)
      (instance? IASTStatement parent)))

(defn expression-classifier [parent]
  (instance? IASTExpression parent))

;; Technically we should see if any of the ancestors are If's, because it could be nested inside somehting else inside an if-statement
(defn if-body-classifier [parent]
  (instance? IASTIfStatement parent))
