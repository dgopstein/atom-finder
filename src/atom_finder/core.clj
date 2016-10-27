(ns atom-finder.core
  (:import [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]))

(defn file-content
  "read in source file to be analyzed"
  [filename]
  (FileContent/createForExternalFileLocation filename))

(defn translation-unit [filename]
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

(defmacro set-all! [obj m]
    `(do ~@(map (fn [e] `(set! (. ~obj ~(key e)) ~(val e))) m) ~obj))

(defn ast-visitor-proxy []
  (->
   (proxy [ASTVisitor] []
     (visit [name] (printf "name1: %s\n" name) true)
     (endVisit [name] (printf "name2: %s\n" name)))

   (set-all! {shouldVisitNames        true
              shouldVisitDeclarations true
              shouldVisitDeclarators  true
              shouldVisitAttributes   true
              shouldVisitStatements   true
              shouldVisitTypeIds      true})))

(defn visit-ast [filename]
  (let [visitor (ast-visitor-proxy)
        node (translation-unit filename)]
    (.accept node visitor)
    ;;(.startVisit visitor node)
    ))

(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
        (alength p)))

(defn pre-tree
  ([f node] (pre-tree f node 1))
  ([f node index]

   (let [children (.getChildren node)
         ret (case (arg-count f)
                   1 (f node)
                   2 (f node index))]

     (conj 
           (for [iast-node (.getChildren node)]
             (pre-tree f iast-node (inc index)))
           ret))))

(defn print-tree [node]
  (letfn 
      [(f [node index]
         (let [print-contents? (and (not (instance? CPPASTTranslationUnit node))
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
                     (-> node .getRawSignature (.subSequence 0 5))))))]
    
    (pre-tree f node)))

(defn depth [node]
  (inc
   (apply max 0
          (map depth
               (.getChildren node)))))

(defn leaf? [node] (empty? (.getChildren node)))

(defn leaves [node]
  (if (leaf? node)
    node
    (flatten (map leaves (.getChildren node)))))

(defn node-str [node]
  (let [stx (.getSyntax node)]
        (if (nil? stx)
          "nil"
          (.getImage stx))))

(map node-str (leaves root))

;; http://stackoverflow.com/questions/23178750/iteratively-apply-function-to-its-result-without-generating-a-seq
(defn fn-pow
  [f x n]
    (nth (iterate f x) n))

(defn ancestor
  "Get the nth grandparent of the node"
  [n node]
  (fn-pow #(.getParent %) node n))

(defn filter-size
  "Return every sub-tree of size n"
  [n node]

  (distinct (map #(ancestor n %) (leaves node))))

(filter-size 3 root)
  

(def filename "/Users/dgopstein/nyu/confusion/atoms/simplifications/2000-natori/nonconfusing.c")
(def root (translation-unit filename))

(defn -main
  [& args]
  (prn (file-content filename))
  (for [incld (file-includes filename)] (printf "include - %s\n" (.getName incld)))

  (visit-ast filename)
  (ast-visitor-proxy)
  (print-tree root)
  (pre-tree #(-> % .getClass .getSimpleName) root)
  )
