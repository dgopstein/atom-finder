(in-ns 'atom-finder.util)

(import '(org.eclipse.cdt.internal.core.dom.rewrite
          ASTModificationStore astwriter.ASTWriter astwriter.ASTWriterVisitor
          commenthandler.NodeCommentMap changegenerator.ChangeGeneratorWriterVisitor)
        '(org.eclipse.cdt.core.dom.ast IASTPreprocessorStatement IASTPreprocessorMacroExpansion IASTPreprocessorMacroDefinition))

(import '(atom_finder SanitaryASTWriterVisitor))

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

;; mostly for use programmatically, tries to provide an
;; accurate representation of the AST
(defn write-ast [node]
  (condp instance? node
    IASTPreprocessorStatement (str node)
    (let [writer-visitor (SanitaryASTWriterVisitor.)]
      (.accept node writer-visitor)
      (.toString writer-visitor))))

;; mostly for human consumption, tries to provide an
;; informative representation of the AST
(defn write-tree [node]
  (if (nil? node)
    "<nil>"
    (try
      (condp instance? node
        IASTPreprocessorMacroExpansion  (->> node str)
        IASTPreprocessorMacroDefinition (->> node str)
      (write-ast node))
      (catch org.eclipse.cdt.internal.core.dom.rewrite.astwriter.ProblemRuntimeException e "<!!!>"))))

(defmacro should-visit! [writer val]
  "Tell the AST-visitor to visit no types of node"
  (let [fields
        ['shouldVisitArrayModifiers 'shouldVisitBaseSpecifiers
         'shouldVisitDeclarations 'shouldVisitDeclarators 'shouldVisitDeclSpecifiers
         'shouldVisitDecltypeSpecifiers 'shouldVisitExpressions
         'shouldVisitInitializers 'shouldVisitNames 'shouldVisitNamespaces
         'shouldVisitParameterDeclarations 'shouldVisitPointerOperators
         'shouldVisitStatements 'shouldVisitTemplateParameters
         'shouldVisitTranslationUnit 'shouldVisitTypeIds 'shouldVisitAttributes]]
    (concat '(do)
            (for [field fields]
              `(set! (. ~writer ~field) ~val))
            [writer])))

(defn visit-nothing! [writer] (should-visit! writer false))
(defn visit-everything! [writer] (should-visit! writer true))

(defmulti  fix-node "Remove any nil children" class)
(defmethod fix-node :default [node] node) ;; most nodes can be printed as-is
(defmethod fix-node IASTBinaryExpression
  [node]
  (if (.getOperand2 node)
    node
    (tap #(.setOperand2 %1 (.copy (.getOperand1 %1)))
         (.copy node)))) ;; if there's no second operand use the first one again

(defn SingleNodeVisitor []
  (let [modificationStore (ASTModificationStore.)
        commentMap (NodeCommentMap.)
        ;writer-visitor (ChangeGeneratorWriterVisitor. modificationStore nil commentMap)]
        ]
    (proxy [ChangeGeneratorWriterVisitor] [modificationStore nil commentMap]
      (visit [node]
        (visit-nothing! this)
        (proxy-super visit node))
      )))

(s/defn write-node-type :- s/Str [node :- (s/maybe IASTNode)] (str "<" (and node (typename node)) ">"))

(s/defn write-node-no-type :- s/Str [node :- IASTNode]
  (if (or (nil? node) (instance? IASTProblemHolder node))
    (write-node-type node)
    (let [writer-visitor (SingleNodeVisitor)]
      (.accept (fix-node node) writer-visitor)
      (str/replace (or (.toString writer-visitor) "") #"\s" ""))))

(s/defn write-node :- s/Str [node :- IASTNode]
      (let [node-str (write-node-no-type node)]
        (if (empty? node-str)
          (write-node-type node)
          node-str)))

(s/defn write-node-valueless :- s/Str
  "Don't write variable names, instead use their type"
  [node :- IASTNode]
  ((if (exists? #(instance? % node) [IASTName IASTLiteralExpression])
    write-node-type
    write-node)
   node))

(defmulti write-exp-node
  "Try to generate the syntax that would build this node
   if it were typed by a human rather than by a maco"
  class)

(s/defmethod write-exp-node IASTLiteralExpression :- s/Str [node]
  (-> node .getValue String.))

(s/defmethod write-exp-node IASTExpression :- s/Str [node]
  (-> node expr-operator :syntax (or "")))

(s/defmethod write-exp-node :default :- s/Str [node]
  "")

(defn print-tree [node]
  (pre-tree
   (fn [node index tree-path]
       (printf "%-60s %s\n"
               (str tree-path "  " (write-node-type node) "  " ((if intersects-macro-exp? write-exp-node write-node-no-type) node))
               (->> (select-keys (loc node) [:line :offset :length])
                    (map-keys (fn [k] (if (-> k name count (> 4)) (keyword (subs (name k) 0 3)) k))))
               ))
   node 1 []))

