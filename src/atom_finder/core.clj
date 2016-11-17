(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.util :refer :all]
            [clojure.pprint :refer [pprint]]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            )
  (:import [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))

(defn -main
  [& args]

  (def root (tu (resource-path "if-starts-in-expression.c")))
  (println (count-expression-parents 2 root))
  ;(pprint (map reverse (sort-by last > (count-nodes-of-depth-in-dir 3 (expand-home "~/opt/src/the_silver_searcher/")))))
  ;(time (pprint (map reverse (sort-by last > (count-expression-parents-in-dir 3 (expand-home "~/opt/src/github-top-c"))))))

  (def github-top-c (map #(.getPath %) (.listFiles (io/file (expand-home "~/opt/src/github-top-c")))))

  ;; the place where all test-files live
  (def resource-dir (.getParent (java.io.File. (resource-path "define-in-if-loop.c"))))

  (defn PRINTLN [s]
    (println
     "\n==============================================================="
     s
     "\n==============================================================="))

  (PRINTLN "\nCount how many \"Preprocessor in Statement\" atoms are in the resource directory files")
  (preprocessor-in-dir resource-dir)

  (PRINTLN "\nPrint the AST of the file containing the atom")
  (print-tree (translation-unit (resource-path "macro-in-expression.c")))

  (PRINTLN "\nPrint the line of the atom in the file")
  (println (atom-finder.classifier/preprocessors-in-contexts define-only expression-classifier (tu (resource-path "macro-in-expression.c"))))

  (System/exit 0)
  )
