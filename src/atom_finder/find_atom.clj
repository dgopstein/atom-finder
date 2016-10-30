(ns atom-finder.find-atom
  (:require [atom-finder.core :refer :all])
  (:import
           [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression IASTTranslationUnit]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))

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
  [root macro]
  (let [loc    (.getFileLocation macro)
                offset (.getNodeOffset loc)
                length (.getNodeLength loc)
                line   (.getStartingLineNumber loc)
               ;in-expr?  (not (instance? IASTTranslationUnit (offset-parent root offset)))
                in-expr?  (not (toplevel-offset? root offset))
                ret {:line line :offset offset :length length}
                ]
            
            [ret in-expr?]))

  (defn macros-in-contexts
  "return a list of all macros that are defined inside of expressiosn"
  [root]
    (keep (fn [[md in-expr?]] (if in-expr? md)) 
          (for [md (.getAllPreprocessorStatements root)]
            (macro-in-contexts root md))))

;(for [[md in-expr?] (macro-in-expressions root)]
;  [(.getStartingLineNumber (.getExpansionLocation md)) (.getRawSignature md) in-expr?])


;(macros-in-contexts (translation-unit (resource-path "if-starts-in-expression.c")))
;(count (.getAllPreprocessorStatements root))
