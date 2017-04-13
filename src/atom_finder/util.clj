(ns atom-finder.util
  (:require [clojure.reflect :as r]
            [clojure.string :as str]
            [schema.core :as s]
            )
  (:use     [clojure.pprint :only [pprint print-table]])
  (:import
           [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage cpp.ICPPASTNamespaceDefinition IASTCompositeTypeSpecifier ASTVisitor IASTNode IASTProblemStatement]
           [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
           [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTProblemStatement]
           [org.eclipse.cdt.internal.core.parser.scanner ASTFileLocation]
           [org.eclipse.cdt.internal.core.dom.rewrite.astwriter ASTWriter]))

;;;;;;
;;;   Completely generic utilities
;;;;;;

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

(defn errln [& s]
  (binding [*out* *err*]
      (println (apply str s))))

(defmacro %w [& words]
    `(list ~@(map str (vec words))))

(defn tap [f x] (f x) x)
(defn pap [x] (tap prn x)) ; print the value of variable and return it

; print the name and value of an expression
(defmacro pprn [x]
  `(let [y# ~x]
    (do
      (print (str ~(str x ": ") (prn-str y#)))
      y#)))

(def not-empty? (comp not empty?))

(defn sym-diff
  "Set symmetric difference - the opposite of the intersection"
  [& args]
  (clojure.set/difference
   (apply clojure.set/union args)
   (apply clojure.set/intersection args)))

(def any-pred? (comp boolean some))
(defn exists?
  ([lst] (any-pred? true? lst))
  ([pred lst] (any-pred? pred lst)))

(def range-from (partial iterate inc))

(defn distinct-by [f col]
  (map first (vals (group-by f col))))

(defn map-values [f m]
  (reduce merge (map (fn [[k v]] {k (f v)}) m)))

(defn slurp-lines [file]
    (str/split-lines (slurp file)))

(defn count-lines [str]
    (count (filter #{\newline} str)))

(defn close?
  "Are two numbers approximately equal"
  [tolerance x y]
     (< (Math/abs (- x y)) tolerance))

(defn strict-get
  "Lookup value in collection and throw exception if it doesn't exist"
  [m k]
  (if-let [[k v] (find m k)]
    v
        (throw (Exception. (str "Key Not Found " k)))))

; http://stackoverflow.com/questions/43213573/get-in-for-lists/43214175#43214175
(defn get-nth-in [init ks]
  (reduce
   (fn [a k]
     (if (associative? a)
       (get a k)
       (nth a k)))
   init ks))

(def flatten1 (partial apply concat))

(defn avg [seq1] (/ (reduce + seq1) (count seq1)))

(defn min-of [lst]
  "Min with a list argument"
  (if (empty? lst) nil
    (apply min lst)))

(defn max-of [lst]
  "Max with a list argument"
  (if (empty? lst) nil
    (apply max lst)))

(defn group-dissoc
  "Group a list of maps by a key, then dissoc that key"
  [key coll]
  (->> coll (group-by key) (map-values (partial map #(dissoc % key)))))

;;;;;;;;
;;   Specific to this project
;;;;;;;

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


(defn children    [^IASTNode node] (.getChildren node))
(defn parent      [^IASTNode node] (.getParent node))
(defn safe-parent [^IASTNode node] (or (.getParent node) node))
(defn root-ancestor [^IASTNode node]
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

(defn all-parents
  "Get the all grandparents of the node"
  [node]
  (take-while some? (iterate parent node)))

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
    (nth (re-find #"AST(.*)" name) 1)))

(defn filter-depth
  "Return every sub-tree of size n"
  [n node]
  ;; start from the leaves of the tree and walk upwards n generations
  (let [candidates (distinct (map (partial ancestor n) (leaves node)))]
    ;; candidates may still have deeper branches than the one we came up from
    (filter #(= n (depth %)) candidates)))

(defn filter-tree
  "Find every AST node that matches pred"
  [pred node]
  (concat
   (when (pred node) [node])
   (mapcat (partial filter-tree pred) (children node))))

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
  (some->> filename clojure.java.io/resource clojure.java.io/file .getPath))


(defn get-in-tree
  "Find a value in the AST by indexes"
  [indices node]
  (cond
    (nil? node) nil
    (empty? indices) node
    :else (recur (rest indices) (nth (children node) (first indices) nil))))

(defn expand-home [s]
  (if (clojure.string/starts-with? s "~")
    (str/replace-first s "~" (System/getProperty "user.home"))
        s))

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

(defn parse-stmt
  "Turn a single C statement into an AST"
  [code]
  (->> (str "int main() {\n" code "\n}\n")
      parse-source
      (get-in-tree [0 2 0])))

(defn parse-expr
  "Turn a single C expression into an AST"
  [code]
  (->> (str code ";")
      parse-stmt
      (get-in-tree [0])))

(def parse-file translation-unit)

(defn parse-resource
  "Parse a file in the resource directory"
  [filename]
  (->> filename resource-path parse-file))

(defn find-after
  "Take the element after the specified one"
  [coll elem]
  (->> (map vector coll (rest coll))
       (filter #(= elem (first %)))
       first
       last))

(def find-first (comp first (partial filter)))

(def map-kv (comp (partial into {}) (partial map)))

; core/org.eclipse.cdt.core/parser/org/eclipse/cdt/internal/core/dom/rewrite/changegenerator/ChangeGenerator.java:getNextSiblingNode(IASTNode node)
(s/defn next-sibling :- (s/maybe IASTNode)
  [node :- IASTNode]
  (let [parent-node (parent node)
        siblings
          (condp instance? parent-node
                ICPPASTNamespaceDefinition (.getDeclarations (cast ICPPASTNamespaceDefinition) true)
                IASTCompositeTypeSpecifier (.getDeclarations (cast IASTCompositeTypeSpecifier) true)
                (children parent-node))]

        (find-after siblings node)))

(s/defn stmt-str? :- s/Bool
  [code :- String]
  (let [stmt-parse (parse-stmt code)]
    ; if the code isn't a statement the next node will be a problem statement
    (and (not (nil? stmt-parse))
      (not (instance? CPPASTProblemStatement (next-sibling stmt-parse))))
  ))

(defn parse-frag
  "Turn a single C fragment (statement or expression) into an AST"
  [code]
  (let [parse-stmt-or-expr (fn [code] ((if (stmt-str? code) parse-stmt parse-expr) code))
        node1 (parse-stmt-or-expr code)]
    (if (instance? IASTProblemStatement node1)
      (parse-stmt-or-expr (str code ";"))
      node1)))

(defmulti loc "Get location information about an AST node" class)
(defmethod loc ASTFileLocation [l]
  (let [offset (.getNodeOffset l)
        length (.getNodeLength l)
        start-line (.getStartingLineNumber l)
        end-line  (.getEndingLineNumber l)]
    {:line start-line :offset offset :length length :start-line start-line :end-line end-line}))

(defmethod loc Object
  [node]
  (loc (.getFileLocation node)))

(defn errln "println to stderr" [s]
  (binding [*out* *err*] (println s)))

(defn all-preprocessor [node] (.getAllPreprocessorStatements (root-ancestor node)))

(defn print-node
  "Print the line that contains the node and the lines around it"
  [node]
  (let [line-num (.getStartingLineNumber (.getFileLocation node)) file-name (.getContainingFilename node)]
    (with-open [rdr (clojure.java.io/reader file-name)]
      (let [file-seq (line-seq rdr) total-line-num (count file-seq)]
       (println "===================================================")

       (if (>= (- line-num 2) 0) (println (str (- line-num 1) "    " (nth file-seq (- line-num 2)))))
       (println (str line-num ">>>>" (nth file-seq (- line-num 1))))
       (if (<= line-num total-line-num) (println (str (+ line-num 1) "    " (nth file-seq line-num))))

       (println "===================================================")))))

(defn pmap-dir-nodes
  "Apply a function to the AST of every c file in a directory"
  [f dirname]
          (pmap
           (fn [file]
             (let [filename (.getPath file)]
               (try
                 (f (parse-file filename))
                 (catch Exception e (printf "-- exception parsing file: \"%s\"\n" filename))
                 (catch Error e     (printf "-- error parsing file: \"%s\"\n" filename))
               )
             ))

           (c-files dirname)))


