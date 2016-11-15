(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.count-subtrees :refer :all]
            [atom-finder.util :refer :all]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            )
  (:import [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))


(defn find-atoms-in-file
  "Find every example of each of the given atoms in the given file"
  [atom filename]
  (let [root (translation-unit filename)]
    (atom root)))

(defn preprocessor-in-dir
  "Find all preprocessor directives not at the top level in directory"
  [dirname]
(time
 (prn (count (pmap

              (fn [file]
                (let [filename (.getPath file)]
                  (try
                    (let [root (translation-unit filename)]
;                      (printf "%03d %s\n" (count (atom-finder.classifier/macros-in-contexts root)) filename))
;                      (printf "%03d %s\n" (count (atom-finder.classifier/preprocessors-in-statement-expression root)) filename))
;                      (printf "%03d %s\n" (count (atom-finder.classifier/preprocessors-in-contexts root expression-classifier)) filename))
                                        ;                      (printf "%03d %s\n" (count (atom-finder.classifier/define-in-contexts root expression-classifier)) filename))
                      (printf "%03d %s\n" (count (atom-finder.classifier/preprocessors-in-contexts define-only expression-classifier root)) filename))
                     (catch Exception e
                    (printf "-- exception parsing file: \"%s\"\n" filename))
                    (catch Error e
                      (printf "-- error parsing file: \"%s\"\n" filename))
                    )
                ))

              (c-files dirname)
              )))))

(defn count-nodes-in-dir
  "Find all preprocessor directives not at the top level in directory"
  [dirname]
  ;(time
  (reduce (partial merge-with +)
          (pmap
           (fn [file]
             (let [filename (.getPath file)]
               (try
                 (count-nodes (tu filename))
                 (catch Exception e (printf "-- exception parsing file: \"%s\"\n" filename))
                 (catch Error e     (printf "-- error parsing file: \"%s\"\n" filename))
                 )))

           (c-files dirname)
           )))
;)

(defn count-nodes-in-dirs
  [dirs]
  (csv/write-csv *out*
    (apply concat (for [dir dirs]
      (let [name (.getName (io/file dir))]
        (map #(cons name %) (count-nodes-in-dir dir))
        )))))

(defn -main
  [& args]

  (def root (tu (resource-path "if-starts-in-expression.c")))
  (def root (tu (expand-home "~/opt/src/gcc/libgcc/memmove.c")))
  (def root (tu (expand-home "~/opt/src/gcc/libgcc/unwind-dw2.c")))
  (def root (tu (expand-home "~/opt/src/emscripten/system/include/EGL/egl.h")))

  (def github-top-c (map #(.getPath %) (.listFiles (io/file (expand-home "~/opt/src/github-top-c")))))

  (pprint (map write (filter-type "IdExpression" root)))
  (pprint (map write (filter-type "Name" root)))
  (pprint (map write (filter-type "ExpressionStatement" root)))
  (pprint (map write (filter-type "FieldReference" root)))
  (pprint (map write (filter-type "SimpleDeclSpecifier" root)))
  (pprint (map write (filter-type "Declarator" root)))
  (pprint (map write (filter-type "SimpleDeclaration" root)))
  (pprint (map write (filter-type "NamedTypeSpecifier" root)))
  (pprint (map write (filter-type "ParameterDeclaration" root)))
  (pprint (map write (filter-type "CompoundStatement" root)))
  (pprint (map #(->> % .getFileLocation .getStartingLineNumber) (filter-type "Problem" root)))

  (->> (mapcat #(do (prn %1) (->> %1 .getPath tu (filter-type "Problem")))
               (c-files (expand-home "~/opt/src/emscripten/")))
       (map #(.getContainingFilename %))
       (take 10)
       )

  ; (time (count-nodes-in-dirs github-top-c))
  
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
