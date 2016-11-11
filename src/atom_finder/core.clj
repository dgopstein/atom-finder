(ns atom-finder.core
  (:require [atom-finder.classifier :refer :all]
            [atom-finder.util :refer :all])
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


(def ^:private running-jar
    "Resolves the path to the current running jar file."
  (-> :keyword class (.. getProtectionDomain getCodeSource getLocation getPath)))

(defn list-resources [path]
  (let [jar (java.util.jar.JarFile. path)
        entries (.entries jar)]
    (loop [result  []]
      (if (.hasMoreElements entries)
        (recur (conj result (.. entries nextElement getName)))
                result))))

(defn -main
  [& args]

  ;(def root (translation-unit (resource-path "if-starts-in-expression.c")))

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
