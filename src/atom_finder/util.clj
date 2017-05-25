(ns atom-finder.util
  (:require [clojure.reflect :as rflct]
            [clojure.string :as str]
            [schema.core :as s])
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import
   [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage cpp.ICPPASTNamespaceDefinition
    IASTCompositeTypeSpecifier ASTVisitor IASTNode IASTProblemStatement IASTName
    IASTBinaryExpression IASTProblem IASTProblemHolder]
   [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTProblemStatement]
   [org.eclipse.cdt.internal.core.parser.scanner ASTFileLocation]))

(s/set-fn-validation! true) ; Globally turn on schema validation

(defn load-cljs-in-dir
  [dir] ; e.g. "classifier/"
  (->> dir
       (str "atom_finder/")
       ClassLoader/getSystemResource
       clojure.java.io/file
       file-seq
       (map (memfn getName))
       (filter #(str/ends-with? % ".clj"))
       (map #(str/replace % #"\.clj$" ""))
       (map (partial str dir))
       (apply load)
  ))

(load-cljs-in-dir "util/")
