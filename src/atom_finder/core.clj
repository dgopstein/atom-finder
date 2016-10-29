(ns atom-finder.core
  (:import [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression]
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

(defn filter-tree
  [node f]
  (let [kids  (children node)
        ret       (f node)
        kids-ret (for [iast-node kids]
                    (filter-tree iast-node f))]
        
    (if ret
      (flatten (conj ret kids-ret))
      kids-ret)))

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

(defn stringify
  "Convert multiple tree to string"
  [node])

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


;(def filename "/Users/dgopstein/nyu/confusion/atoms/simplifications/2000-natori/nonconfusing.c")
;(def filename "/Users/dgopstein/nyu/confusion/atom-finder/src/atom_finder/macro-in-expression.c")

;(def root (translation-unit (.getPath (clojure.java.io/resource "macro-in-expression.c"))))

(defn get-in-tree
  "Find a value in the AST by indexes"
  [node indices]
  (if (empty? indices)
    node
    (recur (nth (children node) (first indices)) (rest indices))))

(defn find-atoms-in-file
  "Find every example of each of the given atoms in the given file"
  [atom filename]
  (let [root (translation-unit filename)]
    (atom root)))

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
