(ns atom-finder.core
  (:import [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]))

(defn file-content
  "read in source file to be analyzed"
  [filename]
  (FileContent/createForExternalFileLocation filename))

(defn translation-unit
  [filename]
  (let [definedSymbols {}
        includePaths (make-array String 0)
        info (new ScannerInfo definedSymbols includePaths)
        log (new DefaultLogService)
        emptyIncludes (IncludeFileContentProvider/getEmptyFilesProvider)
        opts 8]

    (.getASTTranslationUnit (GPPLanguage/getDefault)
                            (file-content filename) info emptyIncludes nil opts log)))

(defn file-includes
  "list all includes in file"
  [filename]    
    (.getIncludeDirectives (translation-unit filename)))

;; (defn ast-visitor []
;;   (reify ASTVisitor
;;     (visit [this name]
;;       (printf "name: %s\n" name))))

(defmacro set-all! [obj m]
    `(do ~@(map (fn [e] `(set! (. ~obj ~(key e)) ~(val e))) m) ~obj))

(defn ast-visitor []
  (->
   (proxy [ASTVisitor] []
    (visit [name]
      (printf "name: %s\n" name) 3))

    (set-all! {shouldVisitNames        true
               shouldVisitDeclarations false
               shouldVisitDeclarators  true
               shouldVisitAttributes   true
               shouldVisitStatements   false
               shouldVisitTypeIds      true})))

(defn file-ast
  [filename]
  (let [visitor (ast-visitor)]
    (.accept (translation-unit filename) visitor)))

(defn print-tree [node index]
  (let [children (.getChildren node)
        print-contents? (and (not (instance? CPPASTTranslationUnit node))
                             (< (-> node .getFileLocation .getNodeLength) 30))
        offset (format " (offset: %s, %s)"
                       (-> node .getFileLocation .getNodeOffset)
                       (-> node .getFileLocation .getNodeLength))]

    (printf "%s -%s %s -> %s\n"
            (apply str (repeat index "  "))
            (-> node .getClass .getSimpleName)
            offset
            (if print-contents?
              (-> node .getRawSignature (.replaceAll "\n" " \\ "))
              (-> node .getRawSignature (.subSequence 0 5))))
    
    (for [iast-node children]
      (print-tree iast-node (inc index)))))

(def filename "/home/dgopstein/nyu/confusion/atoms/simplifications/2000-natori/nonconfusing.c")

(defn -main
  [& args]
  (prn (file-content filename))
  (for [incld (file-includes filename)] (printf "include - %s\n" (.getName incld)))

  (file-ast filename)
  (ast-visitor)
  (print-tree (translation-unit filename) 1)
  )
