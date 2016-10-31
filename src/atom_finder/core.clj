(ns atom-finder.core
  (:require [atom-finder.find-atom :refer :all]
            [atom-finder.util :refer :all])
  (:import [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))


;(def filename "/Users/dgopstein/nyu/confusion/atoms/simplifications/2000-natori/nonconfusing.c")
;(def filename "/Users/dgopstein/nyu/confusion/atom-finder/src/atom_finder/macro-in-expression.c")

;(def root (translation-unit (resource-path "if-starts-in-expression.c")))

(defn find-atoms-in-file
  "Find every example of each of the given atoms in the given file"
  [atom filename]
  (let [root (translation-unit filename)]
    (atom root)))

(defn preprocessor-in-dir
  "Find all preprocessor directives not at the top level in directory"
  [dirname]
(time
 ; (prn (count (for [filename (map #(.getPath %) (c-files "/Users/dgopstein/opt/src/mono"))]
 (prn (count (pmap

              (fn [file]
                (let [filename (.getPath file)]
                  (try
                    (let [root (translation-unit filename)]
;                      (printf "%03d %s\n" (count (atom-finder.find-atom/macros-in-contexts root)) filename))
;                      (printf "%03d %s\n" (count (atom-finder.find-atom/preprocessors-in-statement-expression root)) filename))
;                      (printf "%03d %s\n" (count (atom-finder.find-atom/preprocessors-in-contexts root expression-classifier)) filename))
                                        ;                      (printf "%03d %s\n" (count (atom-finder.find-atom/define-in-contexts root expression-classifier)) filename))
                      (printf "%03d %s\n" (count (atom-finder.find-atom/preprocessors-in-contexts define-only if-body-classifier root)) filename))
                     (catch Exception e
                    (printf "-- exception parsing file: \"%s\"\n" filename))
                    (catch Error e
                      (printf "-- error parsing file: \"%s\"\n" filename))
                    )
                ))

              (c-files dirname)
              )))))

; (preprocessor-in-dir "/Users/dgopstein/opt/src/gcc")
; (preprocessor-in-dir "/Users/dgopstein/opt/src/gcc/gcc/testsuite/c-c++-common")
; (preprocessor-in-dir "/Users/dgopstein/nyu/confusion/atom-finder/src/test/resources")

;(atom-finder.find-atom/preprocessors-in-contexts define-only expression-classifier (tu "/Users/dgopstein/opt/src/gcc/gcc/testsuite/gcc.target/aarch64/aapcs64/abitest.h"))
; (atom-finder.find-atom/define-in-contexts (translation-unit "/Users/dgopstein/opt/src/gcc/gcc/testsuite/gcc.c-torture/compile/limits-exprparen.c") expression-classifier)
(atom-finder.find-atom/preprocessors-in-context define-only if-body-classifier (tu "/Users/dgopstein/opt/src/gcc/gcc/fortran/resolve.c"))

;(require '[clojure.contrib.repl-utils :as ru])
                                        ;(map #(-> % .getClass .getSimpleName) (.getAllPreprocessorStatements (translation-unit (resource-path "define-in-if-loop.c"))))
;(.getRawSignature (first (.getAllPreprocessorStatements (translation-unit (resource-path "define-in-if-loop.c")))))
;(instance? org.eclipse.cdt.core.dom.ast.IASTPreprocessorMacroDefinition  (first (.getAllPreprocessorStatements (translation-unit (resource-path "define-in-if-loop.c")))))
;(atom-finder.find-atom/macros-in-contexts (translation-unit (resource-path "union.c")))
;(get-in-tree  (translation-unit (resource-path "union.c")) [0 0 0])

(print-tree (translation-unit (resource-path "define-in-if-loop.c")))

(defn -main
  [& args]

  ;(print-tree root) 
  ;;(get-in-tree root [0 2 1 0 1]) ;; "%d %d\n""
  ;(.getLeadingSyntax (get-in-tree root [0 2 0 0 1 1 0 1]))
  ;(fn-pow #(.getNext %) (.getTrailingSyntax (get-in-tree root [0 2 0 0 1 1 0 0])) 5)
  ;(write (get-in-tree root [0 2 0 0 1 1 0 1]))
  ;(.getExpansionLocation (nth (.getMacroDefinitions root) 0))
  ;(.getNodeOffset (.getExpansionLocation (nth (.getMacroDefinitions root) 0)))
  ;(.getNodeLength (.getExpansionLocation (nth (.getMacroDefinitions root) 0)))
  ;(map #(.getNodeOffset %) (.getNodeLocations (get-in-tree root [0 2 0 0 1 1 0 ])))
  ;(map #(.getNodeLength %) (.getNodeLocations (get-in-tree root [0 2 0 0 1 1 0 ])))
  ;(map #(.getNodeLength %) (.getNodeLocations (get-in-tree root [0 2 0 0])))
  ;(write (get-in-tree root [0 2 1 ]))

  ;(def nine+seven-node (get-in-tree root [0 2 0 0 1 1 0 ]))
  ;(def printf-node (get-in-tree root [0 2 1 ]))


  ;(contains-location? root 41 1)
  ;(contains-location? nine+seven-node 41 1)
  ;(contains-location? printf-node 41 1)
  ;(write (location-parent root 41 1))


  ;(doseq [ancestor (filter-depth 3 root)]
  ;  (println (typename ancestor) (write ancestor) "\n-----------------\n"))

  )
