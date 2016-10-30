(ns atom-finder.util
  (:import
           [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression IASTTranslationUnit]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))

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

(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))


(defn children [node] (.getChildren node))

(defn pre-tree
  ([f node] (pre-tree f node 1))
  ([f node index]

   (let [kids (children node)
         ret (case (arg-count f)
                   1 (f node)
                   2 (f node index))]

     (conj 
           (doseq [iast-node kids]
             (pre-tree f iast-node (inc index)))
           ret))))

(defn print-tree [node]
  (letfn 
      [(f [node index]
         (let [offset (format " (offset: %s, %s)"
                              (-> node .getFileLocation .getNodeOffset)
                              (-> node .getFileLocation .getNodeLength))]
           
           (printf "%s -%s %s -> %s\n"
                   (apply str (repeat index "  "))
                   (-> node .getClass .getSimpleName)
                   offset
                   (-> node .getRawSignature
                       (str "           ")
                       (.subSequence 0 10)
                       (.replaceAll "\n" " \\ ")))))]
    
    (pre-tree f node)))

(defn depth [node]
  (inc
   (apply max 0
          (map depth
               (children node)))))

(defn leaf? [node] (empty? (children node)))

(defn leaves [node]
  (if (leaf? node)
    node
    (flatten (map leaves (children node)))))

(def writer (ASTWriter.))
(defn write [node] (.write writer node))

;; http://stackoverflow.com/questions/23178750/iteratively-apply-function-to-its-result-without-generating-a-seq
(defn fn-pow
  [f x n]
    (nth (iterate f x) n))

(defn ancestor
  "Get the nth grandparent of the node"
  [n node]
  (fn-pow #(.getParent %) node (dec n)))

(defn filter-depth
  "Return every sub-tree of size n"
  [n node]
  ;; start from the leaves of the tree and walk upwards n generations
  (let [candidates (distinct (map #(ancestor n %) (leaves node)))]
    ;; candidates may still have deeper branches than the one we came up from
    (filter #(= n (depth %)) candidates)))

(defn typename [node]
  (let [name (-> node .getClass .getSimpleName)]
    (nth (re-find #"CPPAST(.*)" name) 1)))

(defn resource-path
  "Find the path to a resource"
  [filename]
  (.getPath (clojure.java.io/resource filename)))

(defn get-in-tree
  "Find a value in the AST by indexes"
  [node indices]
  (if (empty? indices)
    node
    (recur (nth (children node) (first indices)) (rest indices))))


