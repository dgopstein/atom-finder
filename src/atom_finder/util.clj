(ns atom-finder.util
  (:require [clojure.reflect :as r])
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import
           [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage ASTVisitor IASTExpression IASTTranslationUnit]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTTranslationUnit]
           [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))

;; print methods of java object
;; http://stackoverflow.com/questions/5821286/how-can-i-get-the-methods-of-a-java-class-from-clojure
(defn java-methods
  "list methods of java object"
  [obj]
  (->> obj
       r/reflect
       (:members)
       ;(filter :exception-types)
       (map #(dissoc % :exception-types))
       (map #(dissoc % :declaring-class))
       (sort-by :name)
       ))

(defn public-methods
  "list public methods of java object"
  [obj]
  (->> obj
       (java-methods)
       (filter #(:public (:flags %)))
       ))

(defn ppublic-methods
  "print public methods of java object"
  [obj]
  (->> obj
       (public-methods)
       (print-table)
       ))

(defmacro %w [& words]
    `(list ~@(map str (vec words))))

(defn tap [f x] (f x) x)
(defn pap [x] (tap prn x))

(def any-true? (comp boolean some))
(defn exists?
  ([lst] (any-true? true? lst))
  ([pred lst] (any-true? pred lst)))

(def range-from (partial iterate inc))

(defn map-values [f m]
  (reduce merge (map (fn [[k v]] {k (f v)}) m)))

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

(defn mem-tu
  "Create an AST from in-memory source (name is for documentation only)"
  [filename source]
  (let [definedSymbols {}
        includePaths (make-array String 0)
        info (new ScannerInfo definedSymbols includePaths)
        log (new DefaultLogService)
        emptyIncludes (IncludeFileContentProvider/getEmptyFilesProvider)
        opts 8]

    (.getASTTranslationUnit (GPPLanguage/getDefault)
                            (FileContent/create filename (.toCharArray source))
                            info emptyIncludes nil opts log)))

(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))


(defn children    [node] (.getChildren node))
(defn parent      [node] (.getParent node))
(defn safe-parent [node] (or (.getParent node) node))
(defn root-ancestor [node]
  (let [p (parent node)]
    (if (nil? p)
      node
      (recur p))))

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

(def ast-writer (ASTWriter.))
(defn write-ast [node] (.write ast-writer node))

;; http://stackoverflow.com/questions/23178750/iteratively-apply-function-to-its-result-without-generating-a-seq
(defn fn-pow
  [f x n]
    (nth (iterate f x) n))

; https://gist.github.com/micmarsh/bcbe19c9de8bb7a471bf
(defn flip [function]
  (fn
    ([] (function))
    ([x] (function x))
    ([x y] (function y x))
    ([x y z] (function z y x))
    ([a b c d] (function d c b a))
    ([a b c d & rest]
     (->> rest
          (concat [a b c d])
          reverse
          (apply function)))))

(defn ancestor
  "Get the nth grandparent of the node"
  [n node]
    (fn-pow parent node n))

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
  (let [candidates (distinct (map (partial ancestor n) (leaves node)))]
    ;; candidates may still have deeper branches than the one we came up from
    (filter #(= n (depth %)) candidates)))

(defn filter-tree
  "Return every example of type"
  [func node]
  (let [kids        (children node)
        kid-matches (mapcat (partial filter-tree func) kids)
        matches     (filter func kids)]
    (concat matches kid-matches)))

(defn filter-type
  "Return every example of type"
  [type node]
  (filter-tree #(= (typename %) type) node))

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

(defn count-nodes
  "Count the size of the ast"
  [node]
    (inc (reduce + (map count-nodes (children node)))))

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

(defn resource-path
  "Find the path to a resource"
  [filename]
  (.getPath (clojure.java.io/file (clojure.java.io/resource filename))))

(defn get-in-tree
  "Find a value in the AST by indexes"
  [indices node]
  (if (empty? indices)
    node
    (recur (rest indices) (nth (children node) (first indices)))))

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

(defn parse-source
  "Turn a string of C source code into an AST"
  [code]
  (mem-tu "anonymously-parsed-code.c" code))

(defn parse-expr
  "Turn a single C expression into an AST"
  [code]
  (->> (str "int main() {\n" code ";\n}\n")
      parse-source
      (get-in-tree [0 2 0 0]))) 

(defn parse-stmt
  "Turn a single C expression into an AST"
  [code]
  (->> (str "int main() {\n" code "\n}\n")
      parse-source
      (get-in-tree [0 2 0]))) 

(defn loc
  "Get location information about an AST node"
  [node]
  (let [loc    (.getFileLocation node)
        offset (.getNodeOffset loc)
        length (.getNodeLength loc)
        line   (.getStartingLineNumber loc)]
    {:line line :offset offset :length length}))

(defn errln "println to stderr" [s]
  (binding [*out* *err*] (println s)))

(defn atoms-in-tree
  "Return all instances of atom in an AST"
  [atom-classifier node]
  (let [child-atoms
        (mapcat (partial atoms-in-tree atom-classifier) (children node))]
    (if (atom-classifier node)
      (conj child-atoms node)
      child-atoms)))

(defn strict-get [m k]
  (if-let [[k v] (find m k)]
    v
        (throw (Exception. (str "Key Not Found " k)))))
