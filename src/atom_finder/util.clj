(ns atom-finder.util
  (:require [clojure.reflect :as r]
            [clojure.string :as str]
            [schema.core :as s])
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import
   [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage cpp.ICPPASTNamespaceDefinition
    IASTCompositeTypeSpecifier ASTVisitor IASTNode IASTProblemStatement]
   [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTProblemStatement]
   [org.eclipse.cdt.internal.core.parser.scanner ASTFileLocation]
   [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))

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
