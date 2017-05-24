(in-ns 'atom-finder.util)

(import '(org.eclipse.cdt.internal.core.dom.rewrite
          ASTModificationStore astwriter.ASTWriter astwriter.ASTWriterVisitor
          commenthandler.NodeCommentMap changegenerator.ChangeGeneratorWriterVisitor))

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

(def ast-writer (ASTWriter.))
(defn write-ast [node] (.write ast-writer node))

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

(pprint (macroexpand-1 '(should-visit! writer false)))

(defn visit-nothing! [writer] (should-visit! writer false))
(defn visit-everything! [writer] (should-visit! writer true))

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

(defn write-node [node]
  (let [writer-visitor (SingleNodeVisitor)]
		(when (not (nil? node))
			(.accept node writer-visitor))

		(let [node-str (str/replace (.toString writer-visitor) #"\s" "")]
      (if (empty? node-str)
        (str "<" (typename node) ">")
        node-str))
  ))

(gen-interface
 :name atom-finder.util.INodes
   :methods [[nodes [] clojure.lang.PersistentVector]])

(defn FlattenInorderVisitor []
  (let [ns (atom [])]
    (visit-everything!
     (proxy [ASTVisitor atom-finder.util.INodes] []
      (visit [node]
        (pprn node)
        (swap! ns #(conj % node))
        1
        )
      (nodes [] @ns)
        ))))

(defn flatten-tree-inorder [node]
  (let [flatten-visitor (FlattenInorderVisitor)]
		(when node
			(.accept node flatten-visitor))

    (.nodes flatten-visitor)
    ))

(->> "1 + 2"
     parse-frag
     ;flatten-tree
     flatten-tree-inorder
     ;(map write-node)
     )
