(ns atom-finder.util
  (:import
           [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression IASTTranslationUnit]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))

(defmacro %w [& words]
    `(list ~@(map str (vec words))))

(defmulti translation-unit class)
(defmethod translation-unit java.io.File [file] (translation-unit (.getPath file)))
(defmethod translation-unit String [filename]
  (let [definedSymbols {}
        includePaths (make-array String 0)
        info (new ScannerInfo definedSymbols includePaths)
        log (new DefaultLogService)
        emptyIncludes (IncludeFileContentProvider/getEmptyFilesProvider)
        opts 8]

    (.getASTTranslationUnit (GPPLanguage/getDefault)
                            (FileContent/createForExternalFileLocation filename)
                            info emptyIncludes nil opts log)))

(def tu translation-unit)

(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))


(defn children [node] (.getChildren node))
(defn parent [node]
  (prn node)
  (or (.getParent node) node))

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
  (fn-pow parent node (dec n)))

(defn ancestral-instance?
  "Check whether any of this nodes ancestry are of the type listed"
  [type node]
  (if (nil? node)
    false
    (if (instance? type node)
      true
      (ancestral-instance? type (parent node)))))

(defn typename [node]
  (let [name (-> node .getClass .getSimpleName)]
    (nth (re-find #"CPPAST(.*)" name) 1)))

(defn filter-depth
  "Return every sub-tree of size n"
  [n node]
  ;; start from the leaves of the tree and walk upwards n generations
  (let [candidates (distinct (map #(ancestor n %) (leaves node)))]
    ;; candidates may still have deeper branches than the one we came up from
    (filter #(= n (depth %)) candidates)))

(defn filter-type
  "Return every example of type"
  [type node]
  (let [kids        (children node)
        kid-matches (mapcat (partial filter-type type) kids)
        matches     (filter #(= (typename %) type) kids)]
    (concat matches kid-matches)))

(defn filter-type-parent
  "Return the parent of each type"
  [type node]
  (->> node
       (filter-type type)
       (map parent)
       distinct))

(defn filter-instance
  [type node]
  ancestral-instance?
  (let [kids        (children node)
        kid-matches (mapcat (partial filter-instance type) kids)
        matches     (filter (partial instance? type) kids)]
    (concat matches kid-matches)))

(defn filter-instance-parent
  "Return the parent of each type"
  [type node]
  (->> node
       (filter-instance type)
       (map parent)
       distinct))

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

(clojure.java.io/resource "*")

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

(defn expand-home [s]
  (if (.startsWith s "~")
    (clojure.string/replace-first s "~" (System/getProperty "user.home"))
        s))

(defn pmap-dir-nodes
  "Apply a function to the AST of every c file in a directory"
  [f dirname]
          (pmap
           (fn [file]
             (let [filename (.getPath file)]
               (try
                 (f (tu filename))
                 (catch Exception e (printf "-- exception parsing file: \"%s\"\n" filename))
                 (catch Error e     (printf "-- error parsing file: \"%s\"\n" filename))
                 )))

           (c-files dirname)))

(defn write-tempfile
  [content]
  ; https://github.com/clojure-cookbook/clojure-cookbook/blob/master/04_local-io/4-10_using-temp-files.asciidoc
  (let [my-temp-file (java.io.File/createTempFile "filename" ".txt")]
    (with-open [file (clojure.java.io/writer my-temp-file)]
      (binding [*out* file]
        (print content)))

    my-temp-file))

(defn parse
  "Turn a string of C source code into an AST"
  [code]
  (->> code write-tempfile tu))

(defn parse-expr
  "Turn a single C expression into an AST"
  [code]
  (-> (str "int main() {\n" code ";\n}\n")
      parse
      (get-in-tree [0 2 0 0]))) 
