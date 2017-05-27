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
           ret))))

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
    {:line start-line :offset offset :length length :start-line start-line :end-line end-line}))

(defmethod loc Object
  [node]
  (loc (.getFileLocation node)))

(defmethod loc :default [node] nil) ; catch nulls

(def offset (comp :offset loc))
(def start-line (comp :start-line loc))
(def end-line (comp :end-line loc))

(defn all-preprocessor [node] (.getAllPreprocessorStatements (root-ancestor node)))

(defn all-comments [node] (->> node root-ancestor .getComments (into [])))

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

(defn count-nodes
  "Count the size of the ast"
  [node]
    (inc (reduce + (map count-nodes (children node)))))

(s/defn search-tree-by :- (s/Maybe IASTNode)
  "binary search the tree for val, after applying (f node)"
  [f :- (s/=> s/Any IASTNode) val :- s/Any root :- IASTNode]


  )

(s/defn search-tree-offset-parent :- (s/maybe IASTNode)
  "binary search the tree for an offset"
  [target :- s/Int root :- IASTNode]
  (let [{start :offset len :length} (loc root)
        end (+ start len)
        kids (children root)]
    (when (<= start target end)
      (or (some->> (search-children-offset target kids)
                   (search-tree-offset-parent target)) ; make tail-recursive?
          root))))

16045
16103
(search-tree-offset-parent 16356 atom-finder.constants/big-root)

(s/defn search-children-offset :- (s/maybe IASTNode)
  [target :- s/Int kids] ; :- [IASTNode]]
  (pprn [target kids])
  (loop [l 0 h (unchecked-dec (count kids))]
    (pprn [l h])
    (if (<= h (inc l))
       (do ;(pprn [(-> kids (nth l) loc) target])
       (cond
         (== h -1) nil
         (contains-offset? (nth kids l) target) (nth kids l)
         (contains-offset? (nth kids h) target) (nth kids h)
         :else nil))
       (let [m (unchecked-add l (bit-shift-right (unchecked-subtract h l) 1))]
         (if (<= target (-> kids (nth m) offset))
           (recur l m)
           (recur (unchecked-inc m) h))))))

(search-children-offset 16045 (->> kid children first children))
(search-children-offset 16045 (children kid))

(->>
(search-children-offset 16045
(->> atom-finder.constants/big-root
     children
     ))
(def kid))

(defn binary-search
    "Finds earliest occurrence of x in xs (a vector) using binary search."
  ([xs x]
   (loop [l 0 h (unchecked-dec (count xs))]
     (if (<= h (inc l))
       (cond
         (== x (xs l)) l
         (== x (xs h)) h
         :else nil)
       (let [m (unchecked-add l (bit-shift-right (unchecked-subtract h l) 1))]
         (if (< (xs m) x)
           (recur (unchecked-inc m) h)
                        (recur l m)))))))

