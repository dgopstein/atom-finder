(in-ns 'atom-finder.util)

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
           ret)))
  ([f node index tree-path]

   (let [kids (children node)
         kids-last-index (count kids)
         ret (f node index tree-path)]

     (conj
           (doseq [[iast-node child-index] (map list kids (range 0 kids-last-index))]
             (pre-tree f iast-node (inc index) (conj tree-path child-index)))
           ret))))
(defn leaf? [node] (empty? (children node)))

(defn leaves [node]
  (if (leaf? node)
    node
    (flatten (map leaves (children node)))))

(defn all-parents
  "Get the all grandparents of the node"
  [node]
  (take-while some? (iterate parent node)))

(defn height
  "What is the furthest chain of children under this node"
  [node]
  (inc
   (apply max 0
          (map height
               (children node)))))

(defn depth "How many nodes lie between this one and the root"
  [node] (->> node all-parents count))

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

(defn filter-height
  "Return every sub-tree of size n"
  [n node]
  ;; start from the leaves of the tree and walk upwards n generations
  (let [candidates (distinct (map (partial ancestor n) (leaves node)))]
    ;; candidates may still have deeper branches than the one we came up from
    (filter #(= n (height %)) candidates)))

(defn flatten-tree [node]
  (conj (mapcat flatten-tree (children node)) node))

(defn flatten-tree-infixy [node]
  "Approximate an in-order flattening"
  (if (instance? IASTBinaryExpression node)
    (concat (flatten-tree-infixy (.getOperand1 node)) [node]
            (flatten-tree-infixy (.getOperand2 node)))
    (cons node (mapcat flatten-tree-infixy (children node)))))

(defn filter-tree
  "Find every AST node that matches pred"
  [pred node]
  (->> node flatten-tree (filter pred)))

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

(defn get-in-tree
  "Find a value in the AST by indexes"
  [indices node]
  (cond
    (nil? node) nil
    (empty? indices) node
    :else (recur (rest indices) (nth (children node) (first indices) nil))))

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

(def parse-file (comp translation-unit expand-home))

(defn parse-resource
  "Parse a file in the resource directory"
  [filename]
  (->> filename resource-path parse-file))

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
    {:line (long start-line)
     :offset (long offset) :end-offset (long (+ length offset)) :length (long length)
     :start-line (long start-line) :end-line (long end-line)}))

(defmethod loc Object
  [node]
  (loc (.getFileLocation node)))

(defmethod loc :default [node] nil) ; catch nulls

(def offset (comp :offset loc))
(def end-offset (comp :end-offset loc))
(def start-line (comp :start-line loc))
(def end-line (comp :end-line loc))
(defn lines [node]
  (if-let* [s (start-line node)
            e (end-line node)]
    (range s (inc e))
    []))

(defn filename [node] (.getFileName (.getFileLocation node)))

(defn all-preprocessor [node] (.getAllPreprocessorStatements (root-ancestor node)))

(defn all-comments [node] (->> node root-ancestor .getComments (into [])))

(defn print-node-context
  "Print the line that contains the node and the lines around it"
  ([node] (print-node-context 2 node))
  ([n-lines node]
   (with-open [rdr (clojure.java.io/reader (.getContainingFilename node))]
     (let [line-num  (start-line node)
           file-seq (line-seq rdr)
           first-line (max 0 (- line-num n-lines 1))
           lines-to-print (->> file-seq (drop first-line) (take (+ n-lines 1 n-lines)))]
       (println "===================================================")
       (doseq-indexed [line lines-to-print idx]
         (println (str (+ idx first-line) (if (= (+ idx first-line 1) line-num) " >> " "    ") line)))
       (println "===================================================")))))

(defn count-nodes
  "Count the size of the ast"
  [node]
    (inc (reduce + (map count-nodes (children node)))))

(defn contains-location?
  "Does this node contain the given offset/length"
  [root offset length]
  (let [root-loc (.getFileLocation root)
        root-offset (.getNodeOffset root-loc)
        root-length (.getNodeLength root-loc)]

    ;; The location/offset is fully contained in this node
    (and (<=    root-offset                 offset)
         (>= (+ root-offset root-length) (+ offset length)))))

(defn contains-offset?
  "Does this node contain the given offset"
  [node offset]
    (when-let [{node-offset :offset node-length :length} (some->> node loc)]
       (<= node-offset offset (+ node-offset node-length))))

(defn offset-parent?
  "True if this is deepest AST node that contains an offset"
  [node offset]
  (and
   (contains-offset? node offset)
   (not (exists? #(contains-offset? % offset) (children node)))))

'(s/defn search-tree-by :- (s/maybe IASTNode)
  "binary search the tree for val, after applying (f node)"
  [f :- (s/=> s/Any IASTNode) val :- s/Any root :- IASTNode]
  )

; https://stackoverflow.com/a/8950240
(s/defn binary-search-children-offset :- (s/maybe IASTNode)
  [target :- s/Int kids] ; :- [IASTNode]]
  (loop [l 0 h (unchecked-dec (count kids))]
    (if (<= h (inc l))
      (cond
        (== h -1) nil
        (contains-offset? (nth kids l) target) (nth kids l)
        (contains-offset? (nth kids h) target) (nth kids h)
        :else nil)
      (let [m (unchecked-add l (bit-shift-right (unchecked-subtract h l) 1))]
        (if (<= target (-> kids (nth m) end-offset))
          (recur l m)
          (recur (unchecked-inc m) h))))))

(s/defn binary-search-offset-parent :- (s/maybe IASTNode)
  "binary search the tree for an offset"
  [target :- s/Int root :- IASTNode]
  (let [{start :offset len :length} (loc root)]
    (when (<= start target (+ start len))
      (or (some->> (binary-search-children-offset target (children root))
                   (binary-search-offset-parent target)) ; make tail-recursive?
          root))))

(s/defn offset-parent
  "Find the AST node that contains the whole location offset
   Assumes that no children of a single parent overlap in terms of offset"
  ([root :- IASTNode offset :- s/Int] (binary-search-offset-parent offset root))
  ([node :- IASTNode] (parent node))) ;(offset-parent (root-ancestor node) (:offset (loc node)))))

(s/defn pmap-dir-trees
  "Apply a function to the root of the AST of every c file in a directory"
  [f :- (s/=> s/Any [IASTTranslationUnit]) dirname :- s/Str]
  (pmap-dir-files (comp f parse-file) dirname))

(s/defn pmap-dir-nodes
  "Apply a function to the every node of every c file in a directory"
  [f :- (s/=> s/Any [IASTTranslationUnit]) dirname :- s/Str]
  (flatten (pmap-dir-trees #(->> % flatten-tree (map f)) dirname)))
