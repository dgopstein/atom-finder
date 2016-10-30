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

(defn c-files
  "Search directory structure for C-like files"
  [dirname]
  (let [dirfile  (clojure.java.io/file dirname)
        files (file-seq dirfile)
        exts #{"c" "cc" "cpp" "c++" "h" "hh" "hpp" "h++"}]

    (filter
     (fn [file]
       (and
        (exts (nth (re-find #".*\.([^.]+)" (.getName file)) 1 nil))
        (.isFile file))
       )
     files)))

(defn preprocessor-in-dir
  "Find all preprocessor directives not at the top level in directory"
  [dirname]
(time
 ; (prn (count (for [filename (map #(.getPath %) (c-files "/Users/dgopstein/opt/src/mono"))]
 (prn (count (for [filename (map #(.getPath %) (c-files dirname))]
               (try
                 (do ; (prn filename)
                   (let [root (translation-unit filename)]
                     (printf "%03d %s\n" (count (atom-finder.find-atom/macros-in-contexts root)) filename)))
                 (catch Exception e
                   (printf "-- error parsing file: \"%s\"\n" filename)
                   )))))))

; (preprocessor-in-dir "/Users/dgopstein/opt/src/gcc")

; (preprocessor-in-dir "/Users/dgopstein/opt/src/gcc/gcc/testsuite/c-c++-common")

; (atom-finder.find-atom/macros-in-contexts (translation-unit "/Users/dgopstein/opt/src/gcc/gcc/testsuite/c-c++-common/wdate-time.c"))

;(atom-finder.find-atom/macros-in-contexts (translation-unit "/Users/dgopstein/opt/src/mono/libgc/cord/cordbscs.c"))
;(atom-finder.find-atom/macros-in-contexts (translation-unit (resource-path "union.c")))
;(get-in-tree  (translation-unit (resource-path "union.c")) [0 0 0])

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
