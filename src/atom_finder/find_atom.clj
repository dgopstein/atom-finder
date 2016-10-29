(ns atom-finder.find-atom
  (:require [atom-finder.core :refer :all])
  (:import
           [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression]
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

(defn location-parent
  "Find the AST node that contains the whole location offset/length
   Assumes that no children of a single parent overlap in terms of offset/location"
  [root offset length]
  (let [kids      (children root)
        container (first (filter #(contains-location? % offset length) kids))]
    (if (nil? container)
      root
      (recur container offset length))))

(defn macro-in-contexts
  "find a single macro in the given AST"
  [root macro]
  (let [loc    (.getExpansionLocation macro)
                offset (.getNodeOffset loc)
                length (.getNodeLength loc)
                line   (.getStartingLineNumber loc)
                in-expr?  (instance? IASTExpression (location-parent root offset length))
                ret {:line line :offset offset :length length}
                ]
            
            [ret in-expr?]
            )
  )

  (defn macros-in-contexts
  "return a list of all macros that are defined inside of expressiosn"
  [root]
    (keep (fn [[md in-expr?]] (if in-expr? md)) 
          (for [md (.getMacroDefinitions root)]
            (macro-in-contexts root md))))

;(for [[md in-expr?] (macro-in-expressions root)]
;  [(.getStartingLineNumber (.getExpansionLocation md)) (.getRawSignature md) in-expr?])


