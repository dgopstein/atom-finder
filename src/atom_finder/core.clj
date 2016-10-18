(ns atom-finder.core
  (:import [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor]))

(defn file-content
  "read in source file to be analyzed"
  [filename]
  (FileContent/createForExternalFileLocation filename))

(defn file-includes
  "list all includes in file"
  [filename]
  (let [definedSymbols {}
        includePaths (make-array String 0)
        info (new ScannerInfo definedSymbols includePaths)
        log (new DefaultLogService)
        emptyIncludes (IncludeFileContentProvider/getEmptyFilesProvider)
        opts 8
        translationUnit (.getASTTranslationUnit (GPPLanguage/getDefault)
                                                (file-content filename) info emptyIncludes nil opts log)]

    (.getIncludeDirectives translationUnit)))

(def filename "/home/dgopstein/nyu/confusion/atoms/simplifications/2000-natori/nonconfusing.c")

(defn -main
  [& args]
  (prn (file-content filename))
  (for [incld (file-includes filename)] (printf "include - %s\n" (.getName incld)))
  )
